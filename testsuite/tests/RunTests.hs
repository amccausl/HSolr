
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Network.Search.TestSolr as Solr

main = defaultMain [ testGroup "toQueryMap" [ testCase "sort1" Solr.test_toQueryMap_sort1
                                            , testCase "sort2" Solr.test_toQueryMap_sort2
                                            , testCase "sort3" Solr.test_toQueryMap_sort3
                                            ]
                   ]

