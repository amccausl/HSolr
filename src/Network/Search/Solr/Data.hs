
module Network.Search.Solr.Data
       ( SolrInstance(..)
       , SolrData(..)
       , SolrDoc(..)
       , SolrResult(..)
       ) where

import Data.UUID
import Data.Time

data SolrInstance a =
     SolrInstance { solrHost :: String
                  , solrPort :: Int
                  , solrImport :: a -> SolrDoc
                  , solrExport :: SolrDoc -> a
                  }

data SolrData = SolrId UUID
              | SolrInt Int
              | SolrFloat Float
              | SolrBool Bool
              | SolrStr String
              | SolrDate UTCTime
              | SolrArr [SolrData]
  deriving (Eq, Show)

type SolrDoc = [(String, SolrData)]

data SolrResult = SolrResult { resultName :: String
                             , resultCount :: Int
                             , resultScore :: Float
                             , resultDocs :: [SolrDoc]
                             }
  deriving (Eq, Show)

