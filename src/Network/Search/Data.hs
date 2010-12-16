
module Network.Search.Data
       ( Searcher(..)
       , SearchParameter(..)
       , SearchFacet(..)
       , SearchQuery
       , FieldName
       , FieldValue
       , SortOrder
       , SearchData(..)
       , SearchDoc(..)
       , SearchResult(..)
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

data SearchFacet = RangeFacet FieldName FieldValue FieldValue
                 | ValueFacet FieldName FieldValue
  deriving (Eq, Show)

data SearchData = SearchId UUID
                | SearchInt Int
                | SearchFloat Float
                | SearchBool Bool
                | SearchStr String
                | SearchDate UTCTime
                | SearchArr [SearchData]
  deriving (Eq)

instance Show SearchData where
  show (SearchStr value) = value
  show (SearchId value) = show value
  show (SearchInt value) = show value
  show (SearchFloat value) = show value
  show (SearchBool value) = show value
  show (SearchDate value) = show value
  show (SearchArr value) = show value

type SearchDoc = [(String, SearchData)]

data SearchResult = SearchResult { resultDocs :: [SearchDoc]
                                 , resultCount :: Integer
                                 , resultFacets :: [(SearchFacet, Integer)]
                                 , resultRefinements :: [SearchParameter]
                                 }
  deriving (Eq, Show)

