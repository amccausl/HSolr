{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Network.Search.Solr
       ( SolrInstance(..)
       , sendRequest
       , query
       , add
       , update
--       , deleteByQuery
       , deleteByID
       , commit
       , optimize
       ) where

-- Import data types
import Network.Search.Data
import Data.UUID

-- Import Networking modules
import Network.URI (URI (..), parseURI, uriScheme, uriPath, uriQuery, uriFragment)
import Network.TCP as TCP
import Network.HTTP

-- Import XML manipulation
import Text.XML.HXT.Core

-- Constants
requestSize = 100  -- Do operations in groups of 100

query :: SolrInstance a -> [SearchParameter] -> IO ([a])
query solr q = sendRequest solr (queryStr q)
-- todo: URL encode with query parameters

toMap :: SearchParameter -> (String, String)
toMap (Keyword keyword) = ("q", keyword)

queryStr :: [SearchParameter] -> String
queryStr sp = join '&' (map ((\(k, v) -> k ++ "=" ++ v) . toMap))

-- TODO: replace with intersperse
join :: String -> [String]
join glue (h:rest) = join' h glue rest
join glue [] = []

join' acc glue (h:rest) = join' (acc ++ (glue:h)) glue rest 
join' acc glue [] = acc

add :: SolrInstance a -> [a] -> IO (String)
add solr docs = sendRequest solr addXml
  where addXml = runX (xshow (constA docs >>> (arrL id >>> mkDocs) >. wrapInTag "add"))

update :: SolrInstance a -> [a] -> IO (String)
update = add

--deleteByQuery :: SolrInstance a -> [QueryParameter] -> IO (String)
--deleteByQuery solr q =

deleteByID :: SolrInstance a -> String -> IO (String)
deleteByID solr id = sendRequest solr ("<delete><id>" ++ id ++ "</id></delete>")

commit :: SolrInstance a -> IO (String)
commit solr = sendRequest solr "<commit/>"

optimize :: SolrInstance a -> IO (String)
optimize solr = sendRequest solr "<optimize/>"

-- XML generation functions
wrapInTag tag = arr (NTree (XTag (mkName tag) []))

mkDocs = (arrL id >>> arr solrImport >>> mkSolrData) >. wrapInTag "doc"
mkSolrData = mkelem "field"
             [attr "name" (arr (\(name, _) -> NTree (XText name) []))]
             [arr (\(_, solrData( -> NTree (XText (show solrData)) [])))]

-- HTTP functions

getUpdateURI :: SolrInstance a -> URI
getUpdateURI solr = case parseURI ("http://" ++ (solrHost solr) ++ ":" ++ (show (solrPort solr)) ++ "/solr/update") of
                        Just u -> u

getQueryURI :: SolrInstance a -> String -> URI
getQueryURI solr queryStr = case parseURI ("http://" ++ (solrHost solr) ++ ":" ++ (show (solrPort solr)) ++ "/solr/select" ++ queryStr) of
                                Just u -> u

sendUpdateRequest :: SolrInstance a -> String -> IO (String)
sendUpdateRequest solr msg = do
                             conn <- TCP.openStream (solrHost solr) (solrPort solr)
                             print (rqBody req)
                             rawResponse <- sendHTTP conn req
                             body <- getResponseBody rawResponse
                             return body
                             --case rawResponse of
                             --   Right response -> return response
                             --   Left error -> return error
  where req = solrRequest solr msg

sendQueryRequest :: SolrInstance a -> String -> IO (String)
sendQueryRequest solr msg = do
                             conn <- TCP.openStream (solrHost solr) (solrPort solr)
                             print (rqBody req)
                             rawResponse <- sendHTTP conn req
                             body <- getResponseBody rawResponse
                             return body
                             --case rawResponse of
                             --   Right response -> return response
                             --   Left error -> return error
  where req = solrQueryRequest solr msg

--solrRequest :: SolrInstance a -> String -> Request
solrUpdateRequest solr msg = Request { rqURI = getUpdateURI solr :: URI
                                     , rqMethod = POST :: RequestMethod
                                     , rqHeaders = [ Header HdrContentType   "text/xml; charset=utf-8"
                                                   , Header HdrContentLength (show (length msg))
                                                   ] :: [Header]
                                     , rqBody = msg
                                     }

--solrRequest :: SolrInstance a -> String -> Request
solrQueryRequest solr queryStr = Request { rqURI = getQueryURI solr queryStr :: URI
                                         , rqMethod = GET :: RequestMethod
                                         }


-- TODO: Remove SolrInstance and replace with an implementation of a Searcher typeclass

data SolrInstance a =
   SolrInstance { solrHost :: String
                , solrPort :: Int
                , solrImport :: a -> SolrDoc
                , solrExport :: SolrDoc -> a
                }

instance Searcher SolrInstance a where
  
