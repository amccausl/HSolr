
module Network.Search.Data
       ( SearchParameter(..)
       , SearchData(..)
       , SearchDoc(..)
       , SearchResult(..)
       ) where

import Data.UUID
import Data.Time

class Searcher t a where
  runQuery :: t -> SearchQuery -> IO(SearchResult a)

type FieldName = String
type FieldValue = SearchData
type SearchQuery = [SearchParameter]

data SearchParameter = SortParameter [(FieldName, Bool)]
                     | GroupField FieldName
                     | PagingFilter Int Int
                     | FacetFilter SearchFacet
                     | Keyword String

data SearchFacet = RangeFacet FieldName FieldValue FieldValue
                 | ValueFacet FieldName FieldValue

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

data SearchResult = SearchResult t { resultName :: String
                                   , resultCount :: Int
                                   , resultFacets :: (Num n) => [(SearchFacet, n)]
                                   , resultScore :: Float
                                   , resultDocs :: [SearchDoc]
                                   , result :: [t]
                                   }
  deriving (Eq, Show)

