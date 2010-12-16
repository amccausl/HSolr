
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.SystemConfig
import Control.Arrow.ArrowList
import Network.Search.Data
import Data.Time
import Data.UUID
import Locale

play = runX (readDocument [] "testQuery.xml" >>> (findResponse >>> getDocs))

play2 = do
    xml <- readFile "testQuery.xml"
    print (runLA (xread >>> getDocs) (dropWhile (/= '\n') xml))

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

