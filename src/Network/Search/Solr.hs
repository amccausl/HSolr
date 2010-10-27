{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Network.Search.Solr
       ( SolrInstance(..)
       , sendRequest
--       , query
--       , add
--       , update
--       , delete
       , commit
       , optimize
       ) where

-- Import data types
import Network.Search.Solr.Data
import Data.UUID

-- Import Networking modules
import Network.URI (URI (..), parseURI, uriScheme, uriPath, uriQuery, uriFragment)
import Network.TCP as TCP
import Network.HTTP

-- Import XML manipulation
import Text.XML.HXT.Arrow

-- Constants
requestSize = 100  -- Do operations in groups of 100

--query :: SolrInstance a -> [QueryParameter] -> IO ([a])
--query_ :: 

--add :: SolrInstance a -> [a] -> IO (String)
-- pickle [a] -> xml -> add to <add> tag, post

--update :: SolrInstance a -> [a] -> IO (String)

--delete :: SolrInstance a -> [QueryParameter] -> IO (String)

--deleteByID :: SolrInstance a -> UUID -> IO (String)

commit :: SolrInstance a -> IO (String)
commit solr = sendRequest solr "<commit/>"

optimize :: SolrInstance a -> IO (String)
optimize solr = sendRequest solr "<optimize/>"

-- XML functions

-- Load "<result>" element

-- Load "<doc>" element
--getSolrDoc input = do
--    result <- (runX (readString [(a_validate,v_0)] input) >>> deep (isElem >>> hasName "doc"))
--    return result

-- HTTP functions

getUpdateURI :: SolrInstance a -> URI
getUpdateURI solr = case parseURI ("http://" ++ (solrHost solr) ++ ":" ++ (show (solrPort solr)) ++ "/solr/update") of
                        Just u -> u

getQueryURI :: SolrInstance a -> URI
getQueryURI solr = case parseURI ("http://" ++ (solrHost solr) ++ ":" ++ (show (solrPort solr)) ++ "/solr/select") of
                        Just u -> u

sendRequest :: SolrInstance a -> String -> IO (String)
sendRequest solr msg = do
                         conn <- TCP.openStream (solrHost solr) (solrPort solr)
                         print (rqBody req)
                         rawResponse <- sendHTTP conn req
                         body <- getResponseBody rawResponse
                         return body
                         --case rawResponse of
                         --   Right response -> return response
                         --   Left error -> return error
        where req = solrRequest solr msg

--solrRequest :: SolrInstance a -> String -> Request
solrRequest solr msg = Request { rqURI = getUpdateURI solr :: URI
                               , rqMethod = POST :: RequestMethod
                               , rqHeaders = [ Header HdrContentType   "text/xml; charset=utf-8"
                                             , Header HdrContentLength (show (length msg))
                                             ] :: [Header]
                               , rqBody = msg
                               }

