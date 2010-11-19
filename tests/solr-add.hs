
import Text.XML.HXT.Arrow.XmlState.SystemConfig
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.TypeDefs
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs
import Control.Arrow.ArrowList
import Network.Search.Solr.Data
import Data.Time
import Data.UUID
import Locale

-- <add>
-- <doc>
--   <field name="id">TWINX2048-3200PRO</field>
--   <field name="name">CORSAIR  XMS 2GB (2 x 1GB) 184-Pin DDR SDRAM Unbuffered DDR 400 (PC 3200) Dual Channel Kit System Memory - Retail</field>
--   <field name="manu">Corsair Microsystems Inc.</field>
--   <field name="cat">electronics</field>
--   <field name="cat">memory</field>
--   <field name="features">CAS latency 2, 2-3-3-6 timing, 2.75v, unbuffered, heat-spreader</field>
--   <field name="price">185</field>
--   <field name="popularity">5</field>
--   <field name="inStock">true</field>
--   <field name="manufacturedate_dt">2006-02-13T15:26:37Z</field>
-- </doc>
-- </add>

testDocMem :: SolrDoc
testDocMem = [ ("id", SolrStr "TWINX2048-3200PRO")
             , ("name", SolrStr "CORSAIR  XMS 2GB (2 x 1GB) 184-Pin DDR SDRAM Unbuffered DDR 400 (PC 3200) Dual Channel Kit System Memory - Retail")
             , ("manu", SolrStr "Corsair Microsystems Inc.")
             , ("cat", SolrStr "electronics")
             , ("cat", SolrStr "memory")
             , ("features", SolrStr "CAS latency 2, 2-3-3-6 timing, 2.75v, unbuffered, heat-spreader")
             , ("price", SolrInt 185)
             , ("popularity", SolrInt 5)
             , ("inStock", SolrBool True)
             , ("manufacturedate_dt", SolrStr "2006-02-13T15:26:37Z") -- Should be date, check constructors
             ]

mkAddDocs :: ArrowXml a => a [SolrDoc] XmlTree
mkAddDocs = (arrL id >>> mkDocs) >. arr (NTree (XTag (mkName "add") []))

mkDocs :: ArrowXml a => a SolrDoc XmlTree
mkDocs = (arrL id >>> mkSolrData) >. arr (NTree (XTag (mkName "doc") []))

mkSolrData :: ArrowXml a => a (String, SolrData) XmlTree
mkSolrData = mkelem "field" [attr "name" (arr nameHelper)] [arr solrDataHelper]
  where nameHelper (name, _) = NTree (XText name) []
        solrDataHelper (_, solrData) = NTree (XText (show solrData)) []

play = runX (readDocument [] "mem.xml" >>> Text.XML.HXT.Core.getChildren >>> putXmlTree "-")

play2 = runX (constA [testDocMem, testDocMem] >>> mkAddDocs >>> putXmlTree "-" >>> writeDocument [withIndent yes] "hello.xml")

