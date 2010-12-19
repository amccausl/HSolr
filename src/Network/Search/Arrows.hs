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

class ArrowedSearch t where
  -- | hasKeyword value
  hasKeyword :: (Arrow a) => String -> a t t
  -- | hasFacet searchFacet (facet is either field value or range)
  hasFacet :: (Arrow a) => SearchFacet -> a t t
  -- | sortBy field
  sortBy :: (Arrow a) => [(FieldName, SortOrder)] -> a t t
  -- | groupBy field
  groupBy :: (Arrow a) => [FieldName] -> a t t
  -- | pagingFilter perPage numPage
  paging :: (Arrow a) => Int -> Int -> a t t
  -- | topN n
  topN :: (Arrow a) => Int -> a t t
  topN n = paging n 1

  -- Arrows to change queries
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
  paging perPage numPage = arr id
  facetStat facet = arr id

hasFacet' :: SearchFacet -> SearchResult -> SearchResult
hasFacet' facet searchResult = SearchResult { resultDocs = results
                                            , resultCount = fromIntegral (length results)
                                            , resultFacets = []
                                            , resultRefinements = resultRefinements searchResult ++ [FacetFilter facet]
                                            }
  where results = filter (\doc -> matchesFacet facet doc) (resultDocs searchResult)

hasFacet'' :: SearchFacet -> FieldValue -> Bool
hasFacet'' (RangeFacet _ lower upper) value = value >= lower && value <= upper
hasFacet'' (ValueFacet _ targetValue) value = value == targetValue
