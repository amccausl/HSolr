
module Network.Search.Data
       ( SearchParameter(..)
       , SearchData(..)
       , SearchDoc(..)
       , SearchResult(..)
       ) where

import Data.UUID
import Data.Time

-- TODO: implement "Searcher" typeclass
-- runQuery :: [SearchParameter] -> IO( SearchResult )
-- 

-- class Searcher t
--   runSearch :: SearchQuery -> IO(SearchResult t)
--   

type FieldName = String
type FieldValue = SearchData

data SearchParameter = SortParameter FieldName Bool
                     | GroupField FieldName
                     | PagingFilter Int Int
                     | FacetFilter FieldName FieldValue
                     | FieldSearch FieldName FieldValue
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
                                   , resultScore :: Float
                                   , resultDocs :: [SearchDoc]
                                   , result :: [t]
                                   }
  deriving (Eq, Show)

