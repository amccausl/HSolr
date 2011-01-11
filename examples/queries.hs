
module Main (main) where

import Data.Ranged

import Network.Search.Data
import Network.Search.Solr

solr = SolrInstance { solrHost = "localhost"
                    , solrPort = 8080
                    }

testQuery :: SearchQuery
testQuery = [ SortParameter [("popularity", Desc), ("price", Asc)]
            , Keyword "electronics"
            , PagingFilter 5 2
            , FacetFilter (RangeFacet "popularity" (Range (BoundaryBelow (SearchInt 5)) (BoundaryAboveAll)))
--            , FacetFilter (ValueFacet "manu" (SearchStr "Belkin"))
            , FacetFieldStat "cat"
            ]

main = do result <- queryD solr testQuery
          print (result)

