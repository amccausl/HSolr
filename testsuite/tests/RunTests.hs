
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Network.Search.TestData as Data
import qualified Network.Search.TestSolr as Solr

main = defaultMain ( Data.tests ++ Solr.tests )

