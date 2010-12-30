
module Network.Search.TestData where

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
import Data.Ranged

-- Import library to test
import Network.Search.Data
import Network.Search.Solr

tests = [ testGroup "Data:getFacetField" [ testCase "1" test_getFacetField1
                                         , testCase "2" test_getFacetField2
                                         ]
        , testGroup "Data:getFieldValue" [ testCase "1" test_getFieldValue1
                                         , testCase "2" test_getFieldValue2
                                         ]
        , testGroup "Data:getFieldValues" [ testCase "1" test_getFieldValues1
                                          , testCase "2" test_getFieldValues2
                                          ]
        , testGroup "Data:matchesFacet" [ testCase "strArrayMatch" test_matchesFacet_strArrayMatch
                                        , testCase "strArrayMiss" test_matchesFacet_strArrayMiss
                                        , testCase "strMatch" test_matchesFacet_strMatch
                                        , testCase "strMiss" test_matchesFacet_strMiss
                                        ]
        ]


-- * Data instances to test

doc1 = [ ("cat",SearchArr [SearchStr "electronics",SearchStr "connector"])
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

-- Int
int1 = SearchInt 1
int2 = SearchInt 2
int3 = SearchInt 0

-- Float
float1 = SearchFloat 19.95
float2 = SearchFloat 19.96
float3 = SearchFloat 19.94

-- Bool
boolT = SearchBool True
boolF = SearchBool False

-- Str
str1 = SearchStr "Belkin"
str2 = SearchStr "electronics"

-- Date

-- * Test getFacetField

test_getFacetField1 = getFacetField (RangeFacet "fieldName" (Range (BoundaryBelow int1) (BoundaryAbove int2))) @?= "fieldName"
test_getFacetField2 = getFacetField (ValueFacet "fieldName" int1) @?= "fieldName"

-- * Test getFieldValue

test_getFieldValue1 = getFieldValue "manu" doc1 @?= Just str1
test_getFieldValue2 = getFieldValue "missingName" doc1 @?= Nothing

-- * Test getFieldValues

test_getFieldValues1 = getFieldValues "price" doc1 @?= [SearchFloat 19.95]
test_getFieldValues2 = getFieldValues "cat" doc1 @?= [SearchStr "electronics",SearchStr "connector"]

-- * Test matchesFacet

test_matchesFacet_strArrayMatch = matchesFacet (ValueFacet "cat" str2) doc1 @?= True
test_matchesFacet_strArrayMiss = matchesFacet (ValueFacet "cat" str1) doc1 @?= False
test_matchesFacet_strMatch = matchesFacet (ValueFacet "manu" str1) doc1 @?= True
test_matchesFacet_strMiss = matchesFacet (ValueFacet "manu" str2) doc1 @?= False


