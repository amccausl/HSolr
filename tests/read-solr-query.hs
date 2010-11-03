
import Text.XML.HXT.Arrow
import Control.Arrow.ArrowList
import Network.Search.Solr.Data
import Data.Time
import Data.UUID
import Locale

play = runX (readDocument [(a_validate,"0")] "testQuery.xml" >>> getDocs)

getDocs :: IOSArrow XmlTree SolrDoc
getDocs =
    getChildren >>>
    isElem >>> hasName "response" >>>
    getChildren >>>
    isElem >>> hasName "result" >>>
    getChildren >>>
    isElem >>> hasName "doc" >>>
    processDoc

processDoc :: IOSArrow XmlTree SolrDoc
processDoc = (getChildren >>> processField) >. id
  where processField = (getAttrl >>> getChildren >>> getText) &&& getSolrData

getSolrData :: IOSArrow XmlTree SolrData
getSolrData = processType "str" (\x -> tryUUID x)
          <+> processType "bool" (\x -> SolrBool (x == "true"))
          <+> processType "float" (\x -> SolrFloat (read x))
          <+> processType "date" (\x -> SolrDate (readTime defaultTimeLocale "%FT%TZ" x))
          <+> processType "int" (\x -> SolrInt (read x))
          <+> ((isElem >>> hasName "arr") >>> (getChildren >>> getSolrData) >. SolrArr)
  where processType t f = isElem >>> hasName t >>> getChildren >>> getText >>> arr f
        tryUUID str = case fromString str of
            Just uuid -> SolrId uuid
            Nothing -> SolrStr str

