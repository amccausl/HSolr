{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Network.Search.Solr
       ( SolrInstance(..)
       , query
--       , add
--       , update
--       , deleteByQuery
       , deleteByID
       , commit
       , optimize
       ) where

-- Import data types
import Network.Search.Data
import Data.UUID
import Data.Time
import Locale
import List (intersperse)

-- Import Networking modules
import Network.URI (URI (..), URIAuth(..), parseURI, uriScheme, uriPath, uriQuery, uriFragment)
import Network.TCP as TCP
import Network.HTTP

-- Import XML manipulation
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree(..))

-- Constants
requestSize = 100  -- Do operations in groups of 100

-- Define datatype to represent a Solr server
data SolrInstance =
   SolrInstance { solrHost :: String
                , solrPort :: Int
                }

instance Searcher SolrInstance where
  query solr q = do
                 responseStr <- sendQueryRequest solr (queryStr q)
                 return (parseSolrResult responseStr)

-- todo: URL encode with query parameters
toMap :: SearchParameter -> (String, String)
toMap (Keyword keyword) = ("q", keyword)

queryStr :: [SearchParameter] -> String
queryStr sp = concat (intersperse "&" (map ((\(k, v) -> k ++ "=" ++ v) . toMap) sp))

parseSolrResult :: String -> SearchResult
parseSolrResult responseStr = SearchResult { resultDocs = runLA (xread >>> getDocs) (dropWhile (/= '\n') responseStr) :: [SearchDoc]
                                           , resultCount = 0 :: (Num n) => n
                                           , resultFacets = [] :: (Num n) => [(SearchFacet, n)]
                                           }

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
getSolrData = processType "str" (\x -> tryUUID x)
          <+> processType "bool" (\x -> SearchBool (x == "true"))
          <+> processType "float" (\x -> SearchFloat (read x))
          <+> processType "date" (\x -> SearchDate (readTime defaultTimeLocale "%FT%TZ" x))
          <+> processType "int" (\x -> SearchInt (read x))
          <+> ((isElem >>> hasName "arr") >>> (getChildren >>> getSolrData) >. SearchArr)
  where processType t f = isElem >>> hasName t >>> getChildren >>> getText >>> arr f
        tryUUID str = case fromString str of
            Just uuid -> SearchId uuid
            Nothing -> SearchStr str

-- TODO: update add to use SearchDoc types rather then typeclass
--add :: SolrInstance -> [SearchDoc] -> IO (String)
--add solr docs = sendUpdateRequest solr addXml
--  where addXml = runX (xshow (constA docs >>> (arrL id >>> mkDocs) >. wrapInTag "add"))

--update :: SolrInstance -> [SearchDoc] -> IO (String)
--update = add

--deleteByQuery :: SolrInstance a -> [QueryParameter] -> IO (String)
--deleteByQuery solr q =

deleteByID :: SolrInstance -> String -> IO (String)
deleteByID solr id = sendUpdateRequest solr ("<delete><id>" ++ id ++ "</id></delete>")

commit :: SolrInstance -> IO (String)
commit solr = sendUpdateRequest solr "<commit/>"

optimize :: SolrInstance -> IO (String)
optimize solr = sendUpdateRequest solr "<optimize/>"

-- XML generation functions
wrapInTag tag = arr (NTree (XTag (mkName tag) []))

--mkDocs = (arrL id >>> arr solrImport >>> mkSolrData) >. wrapInTag "doc"
mkDocs = wrapInTag "doc"

mkSolrData :: ArrowXml a => a (String, SearchData) XmlTree
mkSolrData = mkelem "field" [attr "name" (arr nameHelper)] [arr solrDataHelper]
  where nameHelper (name, _) = NTree (XText name) []
        solrDataHelper (_, solrData) = NTree (XText (show solrData)) []

-- HTTP functions

solrAuth :: SolrInstance -> Maybe URIAuth
solrAuth solr = Just (URIAuth "" host port)
  where host = solrHost solr
        port = show (solrPort solr)

updateURI :: SolrInstance -> URI
updateURI solr = URI "http:" (solrAuth solr) "/solr/update" "" ""

queryURI :: SolrInstance -> String -> URI
queryURI solr query = URI "http:" (solrAuth solr) "/solr/select" query ""

sendUpdateRequest :: SolrInstance -> String -> IO (String)
sendUpdateRequest solr msg = do
                             conn <- TCP.openStream (solrHost solr) (solrPort solr)
                             print (rqBody req)
                             rawResponse <- sendHTTP conn req
                             body <- getResponseBody rawResponse
                             return body
                             --case rawResponse of
                             --   Right response -> return response
                             --   Left error -> return error
  where req = mkUpdateRequest solr msg

sendQueryRequest :: SolrInstance -> String -> IO (String)
sendQueryRequest solr msg = do
                            conn <- TCP.openStream (solrHost solr) (solrPort solr)
                            rawResponse <- sendHTTP conn req
                            body <- getResponseBody rawResponse
                            return body
                            --case rawResponse of
                            --   Right response -> return response
                            --   Left error -> return error
  where req = mkQueryRequest solr msg


--solrRequest :: SolrInstance a -> String -> Request
mkUpdateRequest solr msg = Request { rqURI = updateURI solr :: URI
                                   , rqMethod = POST :: RequestMethod
                                   , rqHeaders = [ Header HdrContentType   "text/xml; charset=utf-8"
                                                 , Header HdrContentLength (show (length msg))
                                                 ] :: [Header]
                                   , rqBody = msg
                                   }

--solrRequest :: SolrInstance a -> String -> Request
mkQueryRequest solr queryStr = Request { rqURI = queryURI solr queryStr :: URI
                                       , rqMethod = GET :: RequestMethod
                                       , rqHeaders = [] :: [Header]
                                       , rqBody = ""
                                       }

