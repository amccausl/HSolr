
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

-- Import library to test
import Network.Search.Data
import Network.Search.Solr

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

