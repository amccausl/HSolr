-- ------------------------------------------------------------

{- |
   Module     : Network.Search.Arrows
   Copyright  : Public Domain

   Maintainer : Alex McCausland (alex.mccausland@gmail.com)
   Stability  : experimental
   Portability: portable

   Define some basic arrows for generating and filtering searches.

   Each arrow in the ArrowedSearch typeclass has a form that can be used
   pre and post query, so that you can change the order of operations for
   performance, without changing the outcome.  Note that this is not
   always possible and will require that all fields are available from
   the solr index.
-}

-- ------------------------------------------------------------

module Network.Search.Arrows
       ( hasKeyword
       , hasFacet
       , hasValue
       , sortBy
       , groupBy
       , paging
       ) where

import Network.Search.Data
       ( FieldName
       , FieldValue
       , SortOrder
       , SearchQuery
       , SearchParameter(..)
       , SearchFacet(..)
       , getFacetField
       , SearchData(..)
       , SearchDoc
       , SearchResult(..)
       , Searcher(..)
       , getFieldValue
       , getFieldValues
       , matchesFacet
       )

import Control.Arrow
import Control.Arrow.ArrowIO

-- | the interface that defines operations that should be available in search.

class ArrowedSearch t where
  -- | search a data set by a given keyword
  -- | 
  -- | TODO: should use google keyword syntax
  hasKeyword :: (Arrow a) => String -> a t t

  -- | filter the SearchDocs by a given facet (either a range of value of a field)
  -- | If you use this function after the query has been run, it will remove the facet stat information
  hasFacet :: (Arrow a) => SearchFacet -> a t t

  -- | sort the results by a list of fields
  -- | be careful applying this after the search has been run as it needs to access all the records
  -- | TODO: by carefully examining the existing sort fields, depending on the new sort fields, it may not be neccessary to start from scratch
  -- | ie [a, b] -> [a, c], you would only have to examine all the elements that match on the first field at a time
  sortBy :: (Arrow a) => [(FieldName, SortOrder)] -> a t t

  -- | group the results into SearchDocs by a given list of fields.  Documents that match on the values for those fields will be added as 
  -- | SearchArr elements in a new doc.  This is also refered to as a "rollup".
  -- | TODO: this could be implemented making use of the sortBy arrow above (with the same optimizations for the operating set)
  groupBy :: (Arrow a) => [FieldName] -> a t t

  -- | pagingFilter perPage numPage
  -- | pages into the results, returning perPage items from numPage
  paging :: (Arrow a) => Int -> Int -> a t t

  -- | topN n
  -- | return the top documents in the set
  topN :: (Arrow a) => Int -> a t t
  topN n = paging n 1

  -- | adds statistics for # of items with a given facet
  facetStat :: (Arrow a) => SearchFacet -> a t t

hasValue :: (Arrow a) => FieldName -> (FieldValue -> Bool) -> a SearchResult SearchResult
hasValue field test = arr (hasValue' field test)

hasValue' :: FieldName -> (FieldValue -> Bool) -> SearchResult -> SearchResult
hasValue' field test input = SearchResult { resultDocs = docs
                                          , resultCount = fromIntegral (length docs)
                                          , resultFacets = []
                                          , resultRefinements = resultRefinements input
                                          }
 where docs = filter (\doc -> or (map (test) (getFieldValues field doc))) (resultDocs input)

-- Arrows to fetch results from a search service (should be done with a type class that is implemented in the solr module)

-- | solrFetch (IO)
fetch :: (ArrowIO a, Searcher s) => s -> a SearchQuery SearchResult
fetch searcher = arrIO (query searcher)
-- | solrFetchD (IO)
-- Run solr fetch with debugging information present
-- | solrFetchL (IO, returns lazy list of results)
-- Run solr fetch to return lazy list of results

-- Instance of SearchArrow for SearchQuery
instance ArrowedSearch SearchQuery where
  hasKeyword keyword = arr (++ [Keyword keyword])
  hasFacet facet = arr (++ [FacetFilter facet])
  sortBy fields = arr (++ [SortParameter fields])
  groupBy fields = arr (++ [GroupBy fields])
  paging perPage numPage = arr (++ [PagingFilter perPage numPage])

  facetStat facet = arr (++ [FacetStat facet])

-- Instance of SearchArrow for SearchResult
instance ArrowedSearch SearchResult where
  hasKeyword keyword = arr id
  hasFacet facet = arr (hasFacet' facet)
  sortBy fields = arr id
  groupBy fields = arr id
  paging perPage numPage = arr (resultPage perPage numPage)
  facetStat facet = arr id

hasFacet' :: SearchFacet -> SearchResult -> SearchResult
hasFacet' facet searchResult = SearchResult { resultDocs = results
                                            , resultCount = fromIntegral (length results)
                                            , resultFacets = []
                                            , resultRefinements = resultRefinements searchResult ++ [FacetFilter facet]
                                            }
  where results = filter (\doc -> matchesFacet facet doc) (resultDocs searchResult)

resultPage :: Int -> Int -> SearchResult -> SearchResult
resultPage perPage numPage searchResult = SearchResult { resultDocs =  results
                                                       , resultCount = resultCount searchResult
                                                       , resultFacets = resultFacets searchResult
                                                       , resultRefinements = resultRefinements searchResult ++ [PagingFilter perPage numPage]
                                                       }
  where results = ((take perPage) . (drop ((numPage - 1) * perPage)) . resultDocs) searchResult

