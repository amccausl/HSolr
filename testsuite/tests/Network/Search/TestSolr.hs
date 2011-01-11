
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
import Data.Ranged
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
                                            , testCase "paging1" test_toQueryMap_paging1
                                            , testCase "paging2" test_toQueryMap_paging2
                                            , testCase "facetValue" test_toQueryMap_facetValue
                                            , testCase "facetRange" test_toQueryMap_facetRange
                                            , testCase "multipleFacets" test_toQueryMap_multipleFacets
                                            , testCase "facetStatValue" test_toQueryMap_facetStatValue
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

responseXml2 = "\n<response>\n<lst name=\"responseHeader\"><int name=\"status\">0</int><int name=\"QTime\">2</int><lst name=\"params\"><str name=\"facet\">true</str><str name=\"sort\">popularity desc,price asc</str><str name=\"start\">5</str><str name=\"q\">electronics</str><str name=\"facet.field\">cat</str><str name=\"fq\">popularity:[5 TO *]</str><str name=\"rows\">5</str></lst></lst><result name=\"response\" numFound=\"14\" start=\"5\"><doc><arr name=\"cat\"><str>electronics</str><str>graphics card</str></arr><arr name=\"features\"><str>ATI RADEON X1900 GPU/VPU clocked at 650MHz</str><str>512MB GDDR3 SDRAM clocked at 1.55GHz</str><str>PCI Express x16</str><str>dual DVI, HDTV, svideo, composite out</str><str>OpenGL 2.0, DirectX 9.0</str></arr><str name=\"id\">100-435805</str><bool name=\"inStock\">false</bool><str name=\"manu\">ATI Technologies</str><date name=\"manufacturedate_dt\">2006-02-13T00:00:00Z</date><str name=\"name\">ATI Radeon X1900 XTX 512 MB PCIE Video Card</str><int name=\"popularity\">7</int><float name=\"price\">649.99</float><float name=\"weight\">48.0</float></doc><doc><arr name=\"cat\"><str>electronics</str><str>hard drive</str></arr><arr name=\"features\"><str>7200RPM, 8MB cache, IDE Ultra ATA-133</str><str>NoiseGuard, SilentSeek technology, Fluid Dynamic Bearing (FDB) motor</str></arr><str name=\"id\">SP2514N</str><bool name=\"inStock\">true</bool><str name=\"manu\">Samsung Electronics Co. Ltd.</str><date name=\"manufacturedate_dt\">2006-02-13T15:26:37Z</date><str name=\"name\">Samsung SpinPoint P120 SP2514N - hard drive - 250 GB - ATA-133</str><int name=\"popularity\">6</int><float name=\"price\">92.0</float></doc><doc><arr name=\"cat\"><str>electronics</str><str>multifunction printer</str><str>printer</str><str>scanner</str><str>copier</str></arr><arr name=\"features\"><str>Multifunction ink-jet color photo printer</str><str>Flatbed scanner, optical scan resolution of 1,200 x 2,400 dpi</str><str>2.5\" color LCD preview screen</str><str>Duplex Copying</str><str>Printing speed up to 29ppm black, 19ppm color</str><str>Hi-Speed USB</str><str>memory card: CompactFlash, Micro Drive, SmartMedia, Memory Stick, Memory Stick Pro, SD Card, and MultiMediaCard</str></arr><str name=\"id\">0579B002</str><bool name=\"inStock\">true</bool><str name=\"manu\">Canon Inc.</str><str name=\"name\">Canon PIXMA MP500 All-In-One Photo Printer</str><int name=\"popularity\">6</int><float name=\"price\">179.99</float><float name=\"weight\">352.0</float></doc><doc><arr name=\"cat\"><str>electronics</str><str>monitor</str></arr><arr name=\"features\"><str>19\" TFT active matrix LCD, 8ms response time, 1280 x 1024 native resolution</str></arr><str name=\"id\">VA902B</str><bool name=\"inStock\">true</bool><str name=\"manu\">ViewSonic Corp.</str><str name=\"name\">ViewSonic VA902B - flat panel display - TFT - 19\"</str><int name=\"popularity\">6</int><float name=\"price\">279.95</float><float name=\"weight\">190.4</float></doc><doc><arr name=\"cat\"><str>electronics</str><str>hard drive</str></arr><arr name=\"features\"><str>SATA 3.0Gb/s, NCQ</str><str>8.5ms seek</str><str>16MB cache</str></arr><str name=\"id\">6H500F0</str><bool name=\"inStock\">true</bool><str name=\"manu\">Maxtor Corp.</str><date name=\"manufacturedate_dt\">2006-02-13T15:26:37Z</date><str name=\"name\">Maxtor DiamondMax 11 - hard drive - 500 GB - SATA-300</str><int name=\"popularity\">6</int><float name=\"price\">350.0</float></doc></result><lst name=\"facet_counts\"><lst name=\"facet_queries\"/><lst name=\"facet_fields\"><lst name=\"cat\"><int name=\"electronics\">14</int><int name=\"memory\">5</int><int name=\"card\">2</int><int name=\"drive\">2</int><int name=\"graphics\">2</int><int name=\"hard\">2</int><int name=\"monitor\">2</int><int name=\"camera\">1</int><int name=\"copier\">1</int><int name=\"multifunction\">1</int><int name=\"music\">1</int><int name=\"printer\">1</int><int name=\"scanner\">1</int><int name=\"connector\">0</int><int name=\"search\">0</int><int name=\"software\">0</int></lst></lst><lst name=\"facet_dates\"/></lst>\n</response>\n"

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
  \<field name=\"price\">11.5</field>\
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

pagingParam1 = PagingFilter 8 2
pagingParam2 = PagingFilter 10 1
-- encode basic paging parameter
test_toQueryMap_paging1 = toQueryMap Map.empty [pagingParam1] @?= Map.fromList [("start", ["8"]), ("rows", ["8"])]
-- merge 2 paging parameters
test_toQueryMap_paging2 = toQueryMap Map.empty [pagingParam1, pagingParam2] @?= Map.fromList [("start", ["0"]), ("rows", ["10"])]

facetParam1 = ValueFacet "section" (SearchInt 0)
facetParam2 = RangeFacet "popularity" (Range (BoundaryBelow (SearchInt 10)) (BoundaryAboveAll))
test_toQueryMap_facetValue = toQueryMap Map.empty [FacetFilter facetParam1] @?= Map.fromList [("fq", ["section:0"])]
test_toQueryMap_facetRange = toQueryMap Map.empty [FacetFilter facetParam2] @?= Map.fromList [("fq", ["popularity:[10 TO *]"])]
test_toQueryMap_multipleFacets = toQueryMap Map.empty [FacetFilter facetParam1, FacetFilter facetParam2] @?= Map.fromList [("fq", ["popularity:[10 TO *]", "section:0"])]

test_toQueryMap_facetStatValue = toQueryMap Map.empty [FacetFieldStat "cat"] @?= Map.fromList [("facet.field", ["cat"]), ("facet", ["true"])]

-- TODO: result grouping

-- ...&q=*:*&facet=true&facet.field=cat
-- ...&q=*:*&facet=true&facet.field=cat&facet.field=inStock
-- ...&q=ipod&facet=true&facet.query=price:[0 TO 100]&facet.query=price:[100 TO *]
-- ...&q=*:*&facet=true&facet.date=manufacturedate_dt&facet.date.start=2004-01-01T00:00:00Z&facet.date.end=2010-01-01T00:00:00Z&facet.date.gap=+1YEAR


