package baliadapter.generic.system.httpxmlbaligeneric;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXParseException;

import baliadapter.util.InputStreamUtils;

import framework.codecadapter.erlangtypes.Element;
import framework.codecadapter.erlangtypes.ElementType;
import framework.codecadapter.erlangtypes.ListElement;
import framework.codecadapter.erlangtypes.SimpleElement;
import framework.codecadapter.erlangtypes.TupleElement;

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

        ListElement assets = null;
        try {
            assets = getAssets(bais);
            isXML = true;
            bais.reset();
        } catch(SAXParseException spe) {
            isXML = false;
        }
        return new HTTPXMLResponse(responseCode, assets, isXML);
    }

    private static ListElement getAssets(InputStream inputStream)
    throws Exception {
        ListElement assets = processDocument(createDocument(inputStream));
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

    private static ListElement processDocument(Document document) {

        List<Element> assets = new ArrayList<Element>();
        NodeList nodes = document.getChildNodes();

        if(nodes != null) {
            for (int i = 0; i < nodes.getLength(); i++) {
                Node node = nodes.item(i);

                if (node.getNodeType() != Node.ELEMENT_NODE)
                    continue;

                org.w3c.dom.Element element = (org.w3c.dom.Element)node;
                String tagname = element.getTagName();
                if ("Assets".equalsIgnoreCase(tagname)) {
                    assets.addAll(processAssets(element));
                }
            }
        }

        return new ListElement(assets);
    }

    private static List<Element> processAssets(org.w3c.dom.Element root) {
        List<Element> assets = new ArrayList<Element>();

        NodeList nodes = root.getChildNodes();

        if (nodes != null) {
            for (int i = 0; i < nodes.getLength(); i++) {
                Node node = nodes.item(i);

                if (node.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }

                org.w3c.dom.Element element = (org.w3c.dom.Element)node;
                String tagname = element.getTagName();

                if ("Asset".equalsIgnoreCase(tagname)) {
                    assets.add(processAsset(element));
                }
            }
        }
        return assets;
    }

    private static TupleElement toErlangTupleElement(String id, Element e) {
        List<Element> elements = new ArrayList<Element>();
        elements.add(new SimpleElement(id, ElementType.ATOM));
        elements.add(e);
        return new TupleElement(elements);
    }
    
    private static ListElement processAsset(org.w3c.dom.Element root) {

        List<Element> elements = new ArrayList<Element>();
        elements.add(toErlangTupleElement("id",
                new SimpleElement(root.getAttribute("Asset_ID"))));
        elements.add(toErlangTupleElement("creation_date",
                new SimpleElement(root.getAttribute("Creation_Date"))));
        elements.add(toErlangTupleElement("title",
                new SimpleElement(getElementValue(root, "Title"))));
        elements.add(toErlangTupleElement("date",
                new SimpleElement(getElementValue(root, "Date"))));
        elements.add(toErlangTupleElement("summary",
                new SimpleElement(getElementValue(root, "Summary"))));
        elements.add(toErlangTupleElement("rating",
                new SimpleElement(getElementValue(root, "Rating"))));        
        elements.add(toErlangTupleElement("genre",
                new SimpleElement(getElementValue(root, "Genre"))));
        elements.add(toErlangTupleElement("run_time",
                new SimpleElement(getElementValue(root, "Run_Time"))));
        elements.add(toErlangTupleElement("content",
                new ListElement(new ArrayList<Element>())));
        elements.add(toErlangTupleElement("still_image",
                new SimpleElement(new byte[]{})));
        elements.add(toErlangTupleElement("collections",
                new ListElement(new ArrayList<Element>())));

        return new ListElement(elements);
    }

    private static String getElementValue(org.w3c.dom.Element root, String name) {
        try {
            NodeList list = root.getElementsByTagName(name);
            Node node = list.item(0).getFirstChild();

            return node.getNodeValue();
        } catch (Exception e) {
            return new String();
        }
    }

}

