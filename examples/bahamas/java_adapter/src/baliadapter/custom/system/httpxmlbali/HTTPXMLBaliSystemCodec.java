package baliadapter.custom.system.httpxmlbali;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXParseException;

import baliadapter.custom.codec.AssetTransfer;
import baliadapter.util.InputStreamUtils;

public class HTTPXMLBaliSystemCodec {

    public HTTPXMLBaliSystemCodec() {
        ;
    }

    public static HTTPXMLResponse getResponse(InputStream inputStream)
    throws Exception {

        ByteArrayInputStream bais = InputStreamUtils.getByteArrayInputStream(
                inputStream);

        boolean isXML = false;
        
        String responseCode = getResponseCode(bais);
        bais.reset();

        Collection<AssetTransfer> assets = null;
        try {
            assets = getAssets(bais);
            isXML = true;
            bais.reset();
        } catch(SAXParseException spe) {
            isXML = false;
        }
        return new HTTPXMLResponse(responseCode, assets, isXML);
    }

    private static Collection<AssetTransfer> getAssets(InputStream inputStream)
    throws Exception {
        Collection<AssetTransfer> assets = processDocument(
                createDocument(inputStream));
        inputStream.close();
        return assets;
    }

    private static String getResponseCode(InputStream inputStream)
    throws IOException {

        BufferedReader br = new BufferedReader(new InputStreamReader(
                inputStream));
        return br.readLine().split("=")[0];
    }

    private static Document createDocument(InputStream inputStream)
    throws Exception {

        Document document = null;
        try {

            DocumentBuilderFactory documentBuilderFactory =
                DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setCoalescing(true);
            documentBuilderFactory.setIgnoringComments(true);
            documentBuilderFactory.setIgnoringElementContentWhitespace(true);


            DocumentBuilder documentBuilder =
                documentBuilderFactory.newDocumentBuilder();
            document = documentBuilder.parse(inputStream);

        } catch(Exception e) {
            throw e;
        }
        return document;
    }

    private static Collection<AssetTransfer> processDocument(Document document) {

        Collection<AssetTransfer> assets = new ArrayList<AssetTransfer>();
        NodeList nodes = document.getChildNodes();

        if(nodes != null) {
            for (int i = 0; i < nodes.getLength(); i++) {
                Node node = nodes.item(i);

                if (node.getNodeType() != Node.ELEMENT_NODE)
                    continue;

                Element element = (Element)node;
                String tagname = element.getTagName();
                if ("Assets".equalsIgnoreCase(tagname)) {
                    assets.addAll(processAssets(element));
                }
            }
        }

        return assets;
    }

    private static Collection<AssetTransfer> processAssets(Element root) {
        Collection<AssetTransfer> assets = new ArrayList<AssetTransfer>();

        NodeList nodes = root.getChildNodes();

        if (nodes != null) {
            for (int i = 0; i < nodes.getLength(); i++) {
                Node node = nodes.item(i);

                if (node.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }

                Element element = (Element)node;
                String tagname = element.getTagName();

                if ("Asset".equalsIgnoreCase(tagname)) {
                    assets.add(processAsset(element));
                }
            }
        }
        return assets;
    }

    private static AssetTransfer processAsset(Element root) {

        String assetId = (String)root.getAttribute("Asset_ID");
        String creationDate = (String)root.getAttribute("Creation_Date");
        String title = getElementValue(root, "Title");
        String date = getElementValue(root, "Date");
        String summary = getElementValue(root, "Summary");
        String genre = getElementValue(root, "Genre");
        String rating = getElementValue(root, "Rating");
        String runTime =  getElementValue(root, "Run_Time");

        return new AssetTransfer(assetId, creationDate, title,
                date, summary, rating, genre, runTime);
    }

    private static String getElementValue(Element root, String name) {
        try {
            NodeList list = root.getElementsByTagName(name);
            Node node = list.item(0).getFirstChild();

            return node.getNodeValue();
        } catch (Exception e) {
            return new String();
        }
    }

}

