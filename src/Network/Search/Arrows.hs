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
       , SearchData(..)
       , SearchDoc
       , SearchResult(..)
       , Searcher
       )

import Control.Arrow

class ArrowedSearch t where
  -- | hasKeyword value
  hasKeyword :: String -> a t t
  -- | hasFacet searchFacet (facet is either field value or range)
  hasFacet :: SearchFacet -> a t t
  -- | sortBy field
  sortBy :: [(FieldName, SortOrder)] -> a t t
  -- | groupBy field
  groupBy :: FieldName -> a t t
  -- | pagingFilter perPage numPage
  paging :: Int -> Int -> a t t
  -- | topN n
  topN :: Int -> a t t
  topN n = paging n 1

  -- Arrows to change queries
  facetStat :: SearchFacet -> a t t

hasValue :: FieldName -> (FieldValue -> Bool) -> a SearchResult SearchResult
hasValue field test = arr (hasValue' field test)

hasValue' :: FieldName -> (FieldValue -> Bool) -> SearchResult -> SearchResult
hasValue' field test input = SearchResult { resultDocs = docs
                                          , resultCount = length docs
                                          , resultFacets = []
                                          , resultRefinements = resultRefinements input
                                          }
 where docs = filter (test . (getFieldValue field)) (resultDocs input)

-- Arrows to fetch results from a search service (should be done with a type class that is implemented in the solr module)

-- TODO: done by lifting the "runQuery" implementation of the given "Searcher" type
-- | solrFetch (IO)
fetch :: (Searcher s) => s -> IOStateArrow SearchQuery SearchResult
fetch searcher = arr (query searcher)
-- | solrFetchD (IO)
-- Run solr fetch with debugging information present
-- | solrFetchL (IO, returns lazy list of results)
-- Run solr fetch to return lazy list of results

-- Instance of SearchArrow for SearchQuery
instance ArrowedSearch SearchQuery where
  hasKeyword keyword = arr (++ (Keyword keyword))
  hasFacet facet = arr (++ (FacetFilter facet))
  sortBy fields = arr (++ (SortParameter fields))
  groupBy field = arr (++ (GroupBy [field]))
  paging perPage numPage = arr (++ (PagingFilter perPage numPage))

  facetStat facet = arr (++ (FacetStat facet))

-- Instance of SearchArrow for SearchResult
