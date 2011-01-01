\usepackage{fancyvrb}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\small}
\DefineVerbatimEnvironment{example}{Verbatim}{fontsize=\small}
\newcommand{\ignore}[1]{}


\begin{code}
{-# LANGUAGE NoMonomorphismRestriction, Arrows #-}

module Network.Search.Solr
       ( SolrInstance(..)
       , query
--       , add
--       , update
--       , deleteByQuery
       , deleteByID
       , commit
       , optimize

       , parseSolrResult
       , mkQueryRequest
       , mkUpdateRequest
       , toQueryMap
       ) where

-- Import data types
import Network.Search.Data
import Data.Time
import Locale
import qualified Data.Map as Map
import Data.Char (toLower)
import List (intersperse)

-- Import Networking modules
import Network.URI (URI(..), URIAuth(..), escapeURIString, isUnescapedInURI)
import Network.TCP as TCP
import Network.HTTP

-- Import XML manipulation
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree(..))

-- Constants
requestSize = 100  -- Do operations in groups of 100

-- Define datatype to represent a Solr server
data SolrInstance = SolrInstance { solrHost :: String
                                 , solrPort :: Int
                                 }

instance Searcher SolrInstance where
  query solr q = do
                 responseStr <- sendQueryRequest solr (mkQueryRequest solr (toQueryMap Map.empty q))
                 return (parseSolrResult responseStr)

-- Copy of implode function from PHP
implode :: [a] -> [[a]] -> [a]
implode glue = concat . intersperse glue

--add :: SolrInstance -> [SearchDoc] -> IO (String)
--add solr docs = sendUpdateRequest solr addXml
--  where addXml = runX (xshow (constA docs >>> (arrL id >>> mkDocs) >. wrapInTag "add"))

--update :: SolrInstance -> [SearchDoc] -> IO (String)
--update = add

--deleteByQuery :: SolrInstance a -> [QueryParameter] -> IO (String)
--deleteByQuery solr q =

deleteByID :: SolrInstance -> String -> IO (String)
deleteByID solr id = sendUpdateRequest solr (mkUpdateRequest solr ("<delete><id>" ++ id ++ "</id></delete>"))

commit :: SolrInstance -> IO (String)
commit solr = sendUpdateRequest solr (mkUpdateRequest solr "<commit/>")

optimize :: SolrInstance -> IO (String)
optimize solr = sendUpdateRequest solr (mkUpdateRequest solr "<optimize/>")

-- * HTTP functions

solrAuth :: SolrInstance -> Maybe URIAuth
solrAuth solr = Just (URIAuth "" host port)
  where host = solrHost solr
        port = show (solrPort solr)

-- * Functions to query a SolrInstance for documents

-- | Create a URI for a query of the SolrInstance with the given query string
queryURI :: SolrInstance -> String -> URI
queryURI solr query = URI "http:" (solrAuth solr) "/solr/select" query ""

-- | Create a Map to hold the parameters for a search query
toQueryMap :: Map.Map String [String] -> [SearchParameter] -> Map.Map String [String]
toQueryMap m [] = m
toQueryMap m ((SortParameter fields):rest) = toQueryMap (Map.insert "sort" [implode "," (map (encodeSort) fields)] m) rest
  where encodeSort (field, order) = field ++ " " ++ ((\(o:rest) -> (toLower o) : rest) (show order))
toQueryMap m ((Keyword k):rest) = toQueryMap (Map.insert "q" [k] m) rest

-- | Create an HTTP request from a Query Map
mkQueryRequest :: SolrInstance -> Map.Map String [String] -> Request_String
mkQueryRequest solr queryMap = Request { rqURI = queryURI solr queryStr :: URI
                                       , rqMethod = GET :: RequestMethod
                                       , rqHeaders = [] :: [Header]
                                       , rqBody = ""
                                       }
  where queryStr = toQueryStr queryMap

-- | Encode a query map to a query string
toQueryStr :: Map.Map String [String] -> String
toQueryStr queryMap = implode "&" (map (\(k, v) -> k ++ "=" ++ (escapeURIString (isUnescapedInURI) v)) (Map.foldWithKey (f) [] queryMap))
  where f k vs acc = acc ++ (map (\v -> (k, v)) vs)

-- | Send a Request to the SolrInstance
sendQueryRequest :: SolrInstance -> Request_String -> IO (String)
sendQueryRequest solr req = do
                            conn <- TCP.openStream (solrHost solr) (solrPort solr)
                            rawResponse <- sendHTTP conn req
                            body <- getResponseBody rawResponse
                            return body
                            --case rawResponse of
                            --   Right response -> return response
                            --   Left error -> return error

parseSolrResult :: String -> SearchResult
parseSolrResult responseStr = SearchResult { resultDocs = runLA (getChildren >>> isElem >>> hasName "doc" >>> processDoc) resultTag :: [SearchDoc]
                                           , resultCount = head (runLA (getAttrValue "numFound" >>> arr read) resultTag) :: Integer
                                           , resultFacets = runLA (getChildren >>> hasAttrValue "name" (== "facet_counts") >>> getFacets) xml :: [(SearchFacet, Integer)]
                                           , resultRefinements = [] :: [SearchParameter]
                                           }
  where xml = head (runLA xread responseStr)
        resultTag = head (runLA (getChildren >>> isElem >>> hasName "result") xml)
        metaTag = head (runLA (getChildren >>> isElem >>> hasName "lst") xml)

--parseSolrResult responseStr = head (runLA (xread >>> getSearchResult) (dropWhile (/= '\n') responseStr))

--getSearchResult :: (ArrowXml a) => a XmlTree SearchResult
--getSearchResult = proc x -> do
--                       d <- (getDocs >. id) -< x
--                       c <- getCount -< x
--                       f <- getFacets -< x
--                       r <- getRefinements -< x
--                       returnA -< SearchResult { resultDocs = d
--                                               , resultCount = c
--                                               , resultFacets = f
--                                               , resultRefinements = r
--                                               }

findResponse = getChildren >>> isElem >>> hasName "response"

getDocs :: (ArrowXml a) => a XmlTree SearchDoc
getDocs = getChildren >>>
    isElem >>> hasName "result" >>>
    getChildren >>>
    isElem >>> hasName "doc" >>>
    processDoc

processDoc :: (ArrowXml a) => a XmlTree SearchDoc
processDoc = (getChildren >>> processField) >. id
  where processField = (getAttrl >>> getChildren >>> getText) &&& getSolrData

getSolrData :: (ArrowXml a) => a XmlTree SearchData
getSolrData = processType "str" (\x -> SearchStr x)
          <+> processType "bool" (\x -> SearchBool (x == "true"))
          <+> processType "float" (\x -> SearchFloat (read x))
          <+> processType "date" (\x -> SearchDate (readTime defaultTimeLocale "%FT%TZ" x))
          <+> processType "int" (\x -> SearchInt (read x))
          <+> ((isElem >>> hasName "arr") >>> (getChildren >>> getSolrData) >. SearchArr)
  where processType t f = isElem >>> hasName t >>> getChildren >>> getText >>> arr f

getCount :: (ArrowXml a) => a XmlTree Integer
getCount = getChildren >>> isElem >>> hasName "result" >>> getAttrValue "numFound" >>> arr read

-- | Process the top level "facet_counts" lst xml element into the "facet_queries", "facet_fields", and "facet_dates" elements
getFacets :: (ArrowXml a) => a XmlTree (SearchFacet, Integer)
getFacets = getChildren >>> ( ( getFacetQueries )
                          <+> ( facetType "facet_fields" >>> getFacetFields )
                          <+> ( getFacetDates ) )
  where facetType name = isElem >>> hasAttrValue "name" (== name)


-- | Process "facet_queries" lst element
getFacetQueries :: (ArrowXml a) => a XmlTree (SearchFacet, Integer)
getFacetQueries = isRoot >>> constA (ValueFacet "testQueries" (SearchStr "testValue"), 1)

-- | Process "facet_fields" lst element
getFacetFields :: (ArrowXml a) => a XmlTree (SearchFacet, Integer)
getFacetFields = getChildren >>> (getAttrValue "name" &&& (getChildren >>> (getAttrValue "name" &&& (getChildren >>> getText))) >>>
        arr (\(catName, (catValue, facetCount)) -> (ValueFacet catName (SearchStr catValue), read facetCount)))

-- | Process "facet_dates" lst element
getFacetDates :: (ArrowXml a) => a XmlTree (SearchFacet, Integer)
getFacetDates = isRoot >>> constA (ValueFacet "testDates" (SearchStr "testValue"), 1)

getRefinements :: (ArrowXml a) => a XmlTree [SearchParameter]
getRefinements = constA []

-- * Functions to update documents in a SolrInstance

updateURI :: SolrInstance -> URI
updateURI solr = URI "http:" (solrAuth solr) "/solr/update" "" ""

mkUpdateRequest :: SolrInstance -> String -> Request_String
mkUpdateRequest solr msg = Request { rqURI = updateURI solr :: URI
                                   , rqMethod = POST :: RequestMethod
                                   , rqHeaders = [ Header HdrContentType   "text/xml; charset=utf-8"
                                                 , Header HdrContentLength (show (length msg))
                                                 ] :: [Header]
                                   , rqBody = msg
                                   }

sendUpdateRequest :: SolrInstance -> Request_String -> IO (String)
sendUpdateRequest solr req = do
                             conn <- TCP.openStream (solrHost solr) (solrPort solr)
                             print (rqBody req)
                             rawResponse <- sendHTTP conn req
                             body <- getResponseBody rawResponse
                             return body
                             --case rawResponse of
                             --   Right response -> return response
                             --   Left error -> return error

-- XML generation functions
wrapInTag tag = arr (NTree (XTag (mkName tag) []))

--mkDocs = (arrL id >>> arr solrImport >>> mkSolrData) >. wrapInTag "doc"
mkDocs = wrapInTag "doc"

mkSolrData :: ArrowXml a => a (String, SearchData) XmlTree
mkSolrData = mkelem "field" [attr "name" (arr nameHelper)] [arr solrDataHelper]
  where nameHelper (name, _) = NTree (XText name) []
        solrDataHelper (_, solrData) = NTree (XText (show solrData)) []

\end{code}
