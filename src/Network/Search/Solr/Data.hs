
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
  deriving (Eq)

instance Show SolrData where
  show (SolrId value) = show value
  show (SolrInt value) = show value
  show (SolrFloat value) = show value
  show (SolrBool value) = show value
  show (SolrStr value) = show value
  show (SolrDate value) = show value
  show (SolrArr value) = show value

type SolrDoc = [(String, SolrData)]

data SolrResult = SolrResult { resultName :: String
                             , resultCount :: Int
                             , resultScore :: Float
                             , resultDocs :: [SolrDoc]
                             }
  deriving (Eq, Show)

