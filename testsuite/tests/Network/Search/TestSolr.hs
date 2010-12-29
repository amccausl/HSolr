
module Network.Search.TestSolr where

-- Testing imports
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

-- Import common libraries to facilitate tests
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Time
import Locale

-- Import library to test
import Network.Search.Data
import Network.Search.Solr

solrInstance = SolrInstance { solrHost = "localhost"
                            , solrPort = 8080
                            }

-- * Test parseSolrResult
xmlTest1 = "<?xml version='1.0' encoding='UTF-8'?>\n\
\<response>\
\<lst name='responseHeader'>\
\ <int name='status'>0</int>\
\ <int name='QTime'>2</int>\
\ <lst name='params'>\
\  <str name='indent'>on</str>\
\  <str name='start'>0</str>\
\  <str name='q'>electronics</str>\
\  <str name='version'>2.2</str>\
\  <str name='rows'>10</str>\
\ </lst>\
\</lst>\
\<result name='response' numFound='2' start='0'>\
\ <doc>\
\  <arr name='cat'><str>electronics</str><str>connector</str></arr>\
\  <arr name='features'><str>car power adapter, white</str></arr>\
\  <str name='id'>F8V7067-APL-KIT</str>\
\  <bool name='inStock'>false</bool>\
\  <str name='manu'>Belkin</str>\
\  <date name='manufacturedate_dt'>2005-08-01T16:30:25Z</date>\
\  <str name='name'>Belkin Mobile Power Cord for iPod w/ Dock</str>\
\  <int name='popularity'>1</int>\
\  <float name='price'>19.95</float>\
\  <float name='weight'>4.0</float>\
\ </doc>\
\ <doc>\
\  <arr name='cat'><str>electronics</str><str>connector</str></arr>\
\  <arr name='features'><str>car power adapter for iPod, white</str></arr>\
\  <str name='id'>IW-02</str>\
\  <bool name='inStock'>false</bool>\
\  <str name='manu'>Belkin</str>\
\  <date name='manufacturedate_dt'>2006-02-14T23:55:59Z</date>\
\  <str name='name'>iPod &amp; iPod Mini USB 2.0 Cable</str>\
\  <int name='popularity'>1</int>\
\  <float name='price'>11.5</float>\
\  <float name='weight'>2.0</float>\
\ </doc>\
\</result>\
\</response>"

resultTest1 = parseSolrResult xmlTest1

test_parseSolrResult_docs1 = resultDocs resultTest1 @?= [ [ ("cat",SearchArr [SearchStr "electronics",SearchStr "connector"])
                                                          , ("features",SearchArr [SearchStr "car power adapter, white"])
                                                          , ("id",SearchStr "F8V7067-APL-KIT")
                                                          , ("inStock",SearchBool False)
                                                          , ("manu",SearchStr "Belkin")
                                                          , ("manufacturedate_dt",SearchDate (readTime defaultTimeLocale "%FT%TZ" "2005-08-01T16:30:25Z"))
                                                          , ("name",SearchStr "Belkin Mobile Power Cord for iPod w/ Dock")
                                                          , ("popularity",SearchInt 1)
                                                          , ("price",SearchFloat 19.95)
                                                          , ("weight",SearchFloat 4.0)
                                                          ]
                                                        , [ ("cat",SearchArr [SearchStr "electronics",SearchStr "connector"])
                                                          , ("features",SearchArr [SearchStr "car power adapter for iPod, white"])
                                                          , ("id",SearchStr "IW-02")
                                                          , ("inStock",SearchBool False)
                                                          , ("manu",SearchStr "Belkin")
                                                          , ("manufacturedate_dt",SearchDate (readTime defaultTimeLocale "%FT%TZ" "2006-02-14T23:55:59Z"))
                                                          , ("name",SearchStr "iPod & iPod Mini USB 2.0 Cable")
                                                          , ("popularity",SearchInt 1)
                                                          , ("price",SearchFloat 11.5)
                                                          , ("weight",SearchFloat 2.0)
                                                          ]
                                                        ]


-- * Test mkQueryRequest

-- * Test mkUpdateRequest

-- * Test toQueryMap

-- * Test toQueryMap

-- sort=inStock+asc,price+desc

-- single sort element should be added to map
test_toQueryMap_sort1 = toQueryMap Map.empty [SortParameter [("price", Desc)]] @?= Map.fromList [("sort", ["price desc"])]
-- should only sort by the most recent sort parameter
test_toQueryMap_sort2 = toQueryMap Map.empty [SortParameter [("price", Desc)], SortParameter [("inStock", Asc)]] @?= Map.fromList [("sort", ["inStock asc"])]
-- multiple value should be comma separated in result
test_toQueryMap_sort3 = toQueryMap Map.empty [SortParameter [("price", Desc), ("inStock", Asc)]] @?= Map.fromList [("sort", ["price desc,inStock asc"])]

-- ...&q=*:*&facet=true&facet.field=cat
-- ...&q=*:*&facet=true&facet.field=cat&facet.field=inStock
-- ...&q=ipod&facet=true&facet.query=price:[0 TO 100]&facet.query=price:[100 TO *]
-- ...&q=*:*&facet=true&facet.date=manufacturedate_dt&facet.date.start=2004-01-01T00:00:00Z&facet.date.end=2010-01-01T00:00:00Z&facet.date.gap=+1YEAR


