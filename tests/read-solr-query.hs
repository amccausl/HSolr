
import Text.XML.HXT.Arrow
import Control.Arrow.ArrowList
import Network.Search.Solr.Data
import Data.Time
import Data.UUID
import Locale

play = runX (isolateDocs)

play2 = runX (loadResponse)

play3 = runX (isolateDocs >>> processDoc)

isolateDocs :: IOSArrow XmlTree XmlTree
isolateDocs =
    readDocument [(a_validate,"0")] "testQuery.xml" >>>
    getChildren >>>
    isElem >>> hasName "response" >>>
    getChildren >>>
    isElem >>> hasName "result" >>>
    getChildren >>>
    isElem >>> hasName "doc" 
--    >>> putXmlTree "-" >>>
--    getChildren >>> getSolrField

loadResponse :: IOSArrow XmlTree XmlTree
loadResponse =
    readDocument [(a_validate,"0")] "testQuery.xml" >>>
    getChildren >>>
    isElem >>> hasName "response" >>>
    getChildren >>>
    isElem >>> hasName "lst" >>>
    getChildren >>>
    putXmlTree "-"

processDoc :: IOSArrow XmlTree SolrDoc
processDoc = (getChildren >>> processField) >. id
  where processField = (getAttrl >>> getChildren >>> getText) &&& getSolrData

getSolrData :: IOSArrow XmlTree SolrData
getSolrData = (filterByType "str" >>> getText >>> arr (\x -> tryUUID x))
          <+> (filterByType "bool" >>> getText >>> arr (\x -> SolrBool (x == "true")))
          <+> (filterByType "float" >>> getText >>> arr (\x -> SolrFloat (read x)))
          <+> (filterByType "date" >>> getText >>> arr (\x -> SolrDate (readTime defaultTimeLocale "%FT%TZ" x)))
          <+> (filterByType "int" >>> getText >>> arr (\x -> SolrInt (read x)))
          <+> ((isElem >>> hasName "arr") >>> (getChildren >>> getSolrData) >. SolrArr)
  where filterByType t = isElem >>> hasName t >>> getChildren
        tryUUID str = case fromString str of
            Just uuid -> SolrId uuid
            Nothing -> SolrStr str

-- Doesn't collapse arrays
-- Doesn't split Docs
