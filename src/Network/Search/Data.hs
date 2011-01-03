{-# LANGUAGE PatternGuards, TypeSynonymInstances #-}

module Network.Search.Data
       ( Searcher(..)
       , Searchable(..)
       , SearchParameter(..)
       , SearchFacet(..)
       , getFacetField
       , SearchQuery
       , FieldName
       , FieldValue
       , SortOrder(..)
       , SearchData(..)
       , SearchDoc(..)
       , SearchResult(..)
       , getFieldValue
       , getFieldValues
       , matchesFacet
       ) where

import Data.Ranged
import Data.Time

class Searcher t where
  query :: t -> SearchQuery -> IO(Maybe SearchResult)

class Searchable t where
  toSearchDoc :: t -> SearchDoc
  fromSearchDoc :: SearchDoc -> Maybe t

instance Searchable SearchDoc where
  toSearchDoc = id
  fromSearchDoc = Just

type FieldName = String
type FieldValue = SearchData
type SearchQuery = [SearchParameter]

data SortOrder = Asc | Desc
  deriving (Eq, Show)

data SearchParameter = SortParameter [(FieldName, SortOrder)]
                     | GroupBy [FieldName]
                     | PagingFilter Int Int -- perPage, numPage
                     | FacetFilter SearchFacet
                     | Keyword String
                     | FacetQueryStat SearchFacet -- 
                     | FacetFieldStat FieldName
--                     | FacetDateStat FieldName -- TODO: include 
  deriving (Eq, Show)

data SearchFacet = RangeFacet FieldName (Range FieldValue)
                 | ValueFacet FieldName FieldValue
  deriving (Eq, Show)

getFacetField :: SearchFacet -> FieldName
getFacetField (RangeFacet field _) = field
getFacetField (ValueFacet field _) = field

data SearchData = SearchInt Int
                | SearchFloat Float
                | SearchBool Bool
                | SearchStr String
                | SearchDate UTCTime
                | SearchArr [SearchData]
  deriving (Eq, Ord, Show)

instance DiscreteOrdered SearchData where
   adjacent _ _ = False
   adjacentBelow = const Nothing

type SearchDoc = [(String, SearchData)]

getFieldValue :: FieldName -> SearchDoc -> Maybe SearchData
getFieldValue _ [] = Nothing
getFieldValue targetName ((name, value):rest) | name == targetName = Just value
getFieldValue targetName ((name, value):rest) | name /= targetName = getFieldValue targetName rest

getFieldValues :: FieldName -> SearchDoc -> [SearchData]
getFieldValues _  [] = []
getFieldValues targetName ((name, SearchArr values):rest) | name == targetName = values ++ (getFieldValues targetName rest)
getFieldValues targetName ((name, value):rest) | name == targetName = value : (getFieldValues targetName rest)
getFieldValues targetName ((name, value):rest) | name /= targetName = getFieldValues targetName rest

matchesFacet :: SearchFacet -> SearchDoc -> Bool
matchesFacet _ [] = False
matchesFacet (RangeFacet fName fRange) ((name, SearchArr values):rest) | name == fName = or (map (rangeHas fRange) values) || matchesFacet (RangeFacet fName fRange) rest
matchesFacet (ValueFacet fName fValue) ((name, SearchArr values):rest) | name == fName = or (map (== fValue) values) || matchesFacet (ValueFacet fName fValue) rest
matchesFacet (RangeFacet fName fRange) ((name, value):rest) | name == fName = rangeHas fRange value || matchesFacet (RangeFacet fName fRange) rest
matchesFacet (ValueFacet fName fValue) ((name, value):rest) | name == fName = value == fValue || matchesFacet (ValueFacet fName fValue) rest
matchesFacet facet (_:rest) = matchesFacet facet rest

data SearchResult = SearchResult { resultDocs :: [SearchDoc]
                                 , resultCount :: Integer
                                 , resultFacets :: [(SearchFacet, Integer)]
                                 , resultRefinements :: [SearchParameter]
                                 }
  deriving (Eq, Show)

