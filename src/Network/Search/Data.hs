
module Network.Search.Data
       ( Searcher(..)
       , SearchParameter(..)
       , SearchFacet(..)
       , getFacetField
       , SearchQuery
       , FieldName
       , FieldValue
       , SortOrder
       , SearchData(..)
       , SearchDoc(..)
       , SearchResult(..)
       , getFieldValue
       , getFieldValues
       , matchesFacet
       ) where

import Data.UUID
import Data.Time

class Searcher t where
  query :: t -> SearchQuery -> IO(SearchResult)

class Searchable t where
  toSearchDoc :: t -> SearchDoc
  fromSearchDoc :: SearchDoc -> Maybe t

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
                     | FacetStat SearchFacet
  deriving (Eq, Show)

-- TODO: investigate a haskell Range type, if all SearchData elements form ranges, can operate on them (iterable)
data SearchFacet = RangeFacet FieldName FieldValue FieldValue
                 | ValueFacet FieldName FieldValue
  deriving (Eq, Show)

getFacetField :: SearchFacet -> FieldName
getFacetField (RangeFacet field _ _) = field
getFacetField (ValueFacet field _) = field

data SearchData = SearchId UUID
                | SearchInt Int
                | SearchFloat Float
                | SearchBool Bool
                | SearchStr String
                | SearchDate UTCTime
                | SearchArr [SearchData]
  deriving (Eq, Ord)

instance Show SearchData where
  show (SearchStr value) = value
  show (SearchId value) = show value
  show (SearchInt value) = show value
  show (SearchFloat value) = show value
  show (SearchBool value) = show value
  show (SearchDate value) = show value
  show (SearchArr value) = show value

type SearchDoc = [(String, SearchData)]

getFieldValue :: FieldName -> SearchDoc -> Maybe SearchData
getFieldValue _ [] = Nothing
getFieldValue targetName ((name, value):rest) | name == targetName = Just value
getFieldValue targetName ((name, value):rest) | name /= targetName = getFieldValue targetName rest

getFieldValues :: FieldName -> SearchDoc -> [SearchData]
getFieldValues _  [] = []
getFieldValues targetName ((name, value):rest) | name == targetName = value : (getFieldValues targetName rest)
getFieldValues targetName ((name, value):rest) | name /= targetName = getFieldValues targetName rest

matchesFacet :: SearchFacet -> SearchDoc -> Bool
matchesFacet _ [] = False
matchesFacet (RangeFacet fName lower upper) ((name, value):rest) | name == fName, lower <= value, value <= upper = True
matchesFacet (ValueFacet fName fValue) ((name, value):rest) | name == fName, value == fValue = True
matchesFacet facet (_:rest) = matchesFacet facet rest

data SearchResult = SearchResult { resultDocs :: [SearchDoc]
                                 , resultCount :: Integer
                                 , resultFacets :: [(SearchFacet, Integer)]
                                 , resultRefinements :: [SearchParameter]
                                 }
  deriving (Eq, Show)

