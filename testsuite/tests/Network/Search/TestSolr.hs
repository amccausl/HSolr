
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
import Network.HTTP (Request(..))

-- Import library to test
import Network.Search.Data
import Network.Search.Solr

tests = [ testGroup "Solr:parseSolrResult"  [ testCase "docs1" test_parseSolrResult_docs1
                                            , testCase "count1" test_parseSolrResult_count1
                                            , testCase "facet1" test_parseSolrResult_facet1
                                            , testCase "refinements1" test_parseSolrResult_refinements1
                                            ]
        , testGroup "Solr:mkQueryRequest"   [
                                            ]
        , testGroup "Solr:mkAddRequest"     [ testCase "add docs1" test_mkAddRequest_testAdd
                                            ]
        , testGroup "Solr:mkUpdateRequest"  [
                                            ]
        , testGroup "Solr:toQueryMap"       [ testCase "sort1" test_toQueryMap_sort1
                                            , testCase "sort2" test_toQueryMap_sort2
                                            , testCase "sort3" test_toQueryMap_sort3
                                            ]
        ]

-- * Shared data for testing
solrInstance = SolrInstance { solrHost = "localhost"
                            , solrPort = 8080
                            }

-- | Example response from Solr index for a search query
responseXml1 = --"<?xml version='1.0' encoding='UTF-8'?>\n\
    "\
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
\<lst name='facet_counts'>\
\ <lst name='facet_queries'/>\
\ <lst name='facet_fields'>\
\  <lst name='manu'>\
\    <int name='inc'>11</int>\
\    <int name='corsair'>4</int>\
\  </lst>\
\ </lst>\
\ <lst name='facet_dates'/>\
\</lst>\
\</response>"

-- | The internal SearchDoc representation of the xml response above
docs1 = [ [ ("cat",SearchArr [SearchStr "electronics",SearchStr "connector"])
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

-- | The xml to add the above documents to a Solr index
addXml1 = "<add>\
\<doc>\
  \<field name=\"cat\">electronics</field>\
  \<field name=\"cat\">connector</field>\
  \<field name=\"features\">car power adapter, white</field>\
  \<field name=\"id\">F8V7067-APL-KIT</field>\
  \<field name=\"inStock\">false</field>\
  \<field name=\"manu\">Belkin</field>\
  \<field name=\"manufacturedate_dt\">2005-08-01T16:30:25Z</field>\
  \<field name=\"name\">Belkin Mobile Power Cord for iPod w/ Dock</field>\
  \<field name=\"popularity\">1</field>\
  \<field name=\"price\">19.95</field>\
  \<field name=\"weight\">4</field>\
\</doc>\
\<doc>\
  \<field name=\"cat\">electronics</field>\
  \<field name=\"cat\">connector</field>\
  \<field name=\"features\">car power adapter for iPod, white</field>\
  \<field name=\"id\">IW-02</field>\
  \<field name=\"inStock\">false</field>\
  \<field name=\"manu\">Belkin</field>\
  \<field name=\"manufacturedate_dt\">2006-02-14T23:55:59Z</field>\
  \<field name=\"name\">iPod &amp; iPod Mini USB 2.0 Cable</field>\
  \<field name=\"popularity\">1</field>\
  \<field name=\"price\">11.50</field>\
  \<field name=\"weight\">2</field>\
\</doc>\
\</add>"

-- * Test parseSolrResult
resultTest1 = case parseSolrResult responseXml1 of
    Just result -> result

test_parseSolrResult_docs1 = resultDocs resultTest1 @?= docs1

test_parseSolrResult_count1 = resultCount resultTest1 @?= 2

test_parseSolrResult_facet1 = resultFacets resultTest1 @?= [ ( ValueFacet "manu" (SearchStr "inc"), 11 )
                                                           , ( ValueFacet "manu" (SearchStr "corsair"), 4 )
                                                           ]

test_parseSolrResult_refinements1 = resultRefinements resultTest1 @?= []

-- * Test mkQueryRequest

-- * Test mkAddRequest
test_mkAddRequest_testAdd = rqBody (mkAddRequest solrInstance docs1) @?= addXml1

-- * Test mkUpdateRequest

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


