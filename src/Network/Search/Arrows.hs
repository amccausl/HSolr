-- ------------------------------------------------------------

{- |
   Module     : Network.Search.Arrows
   Copyright  : Public Domain

   Maintainer : Alex McCausland (alex.mccausland@gmail.com)
   Stability  : experimental
   Portability: portable

   Define some basic arrows to generating and filtering search queries

   Each arrow has a form that can be used pre and post query, so that
   you can change the order of operations for performance, without
   changing the outcome.  Note that this is not always possible and will
   require that all fields are available from the solr index.
-}

-- ------------------------------------------------------------

module Network.Search.Arrows
       ( hasKeyword
       , hasFacet
       , hasValue
       , sortByAsc
       , sortByDesc
       , sortByAscC
       , sortbyDescC
       , groupBy
       , paging
       ) where

import Network.Search.Data
       ( FieldName
       , FieldValue
       , SearchParameter(..)
       , SearchFacet(..)
       , SearchData(..)
       , SearchDoc
       , SearchResult(..)
       )

import Control.Arrow

class ArrowedSearch t where
  -- | hasKeyword value
  hasKeyword :: String a t t
  -- | hasFacet searchFacet (facet is either field value or range)
  hasFacet :: SearchFacet -> a t t
  -- | sortByAsc field
  sortByAsc :: FieldName -> a t t
  -- | sortByDesc field
  sortByDesc :: FieldName -> a t t
  -- | sortByAscC field (composable, through use of the list arrow)
  sortByAscC :: FieldName -> a t t
  -- | sortByDescC field (composable, through use of the list arrow)
  sortByDescC :: FieldName -> a t t
  -- | groupBy field
  groupBy :: FieldName -> a t t
  -- | pagingFilter perPage numPage
  paging :: Int -> Int -> a t t
  -- | topN n
  topN :: Int -> a t t
  topN n = paging n 1

-- Arrows to change queries
addFacetStat :: SearchFacet -> a SearchQuery SearchQuery

-- Arrows to filter results
facetStat :: (Num n) => SearchFacet -> a SearchResult n

hasValue :: FieldName -> (FieldValue -> Bool) -> a SearchResult SearchResult

-- Arrows to fetch results from a search service (should be done with a type class that is implemented in the solr module)

-- TODO: done by lifting the "runSearch" implementation of the given "Searcher" type
-- | solrFetch (IO)
fetch :: a SearchQuery SearchResults
-- | solrFetchD (IO)
-- Run solr fetch with debugging information present
-- | solrFetchL (IO, returns lazy list of results)
-- Run solr fetch to return lazy list of results


-- Instance of SearchArrow for SearchQuery
instance ArrowedSearch SearchQuery where

-- Instance of SearchArrow for SearchResult
