
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Network.Search.TestData as Data
import qualified Network.Search.TestSolr as Solr

main = defaultMain [ testGroup "Data:getFacetField" [ testCase "1" Data.test_getFacetField1
                                                    , testCase "2" Data.test_getFacetField2
                                                    ]
                   , testGroup "Data:getFieldValue" [ testCase "1" Data.test_getFieldValue1
                                                    , testCase "2" Data.test_getFieldValue2
                                                    ]
                   , testGroup "Data:getFieldValues" [ testCase "1" Data.test_getFieldValues1
                                                     , testCase "2" Data.test_getFieldValues2
                                                     ]
                   , testGroup "Data:matchesFacet" [ testCase "strArrayMatch" Data.test_matchesFacet_strArrayMatch
                                                   , testCase "strArrayMiss" Data.test_matchesFacet_strArrayMiss
                                                   , testCase "strMatch" Data.test_matchesFacet_strMatch
                                                   , testCase "strMiss" Data.test_matchesFacet_strMiss
                                                   ]
                   , testGroup "Solr:parseSolrResult" [ testCase "docs1" Solr.test_parseSolrResult_docs1
                                                      , testCase "count1" Solr.test_parseSolrResult_count1
                                                      , testCase "facet1" Solr.test_parseSolrResult_facet1
                                                      , testCase "refinements1" Solr.test_parseSolrResult_refinements1
                                                      ]
                   , testGroup "Solr:toQueryMap" [ testCase "sort1" Solr.test_toQueryMap_sort1
                                                 , testCase "sort2" Solr.test_toQueryMap_sort2
                                                 , testCase "sort3" Solr.test_toQueryMap_sort3
                                                 ]
                   ]

