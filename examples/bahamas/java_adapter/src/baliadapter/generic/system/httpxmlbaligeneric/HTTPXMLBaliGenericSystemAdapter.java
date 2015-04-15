package baliadapter.generic.system.httpxmlbaligeneric;

import java.io.InputStream;
import java.io.StringWriter;
import java.net.ConnectException;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import baliadapter.util.BaliHTTPComm;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.lambdastream.bali.exception.ServerException;

import framework.codecadapter.erlangtypes.ElementType;
import framework.codecadapter.erlangtypes.ListElement;
import framework.codecadapter.erlangtypes.SimpleElement;
import framework.codecadapter.requests.AdapterRequest;
import framework.codecadapter.requests.GenericAdapterRequest;
import framework.codecadapter.responses.GenericDataResponse;
import framework.codecadapter.responses.GenericErrorResponse;
import framework.systemadapter.SystemAdapterCommunication;
import framework.systemadapter.adapters.StandardSystemAdapter;
import framework.testcommunication.TestCommunication;

public class HTTPXMLBaliGenericSystemAdapter extends StandardSystemAdapter {

    private BaliHTTPComm baliHTTPComm;

    public SystemAdapterCommunication send(OtpErlangPid from,
            AdapterRequest adapterRequest) throws Exception {

        GenericAdapterRequest genericAdapterRequest =
            (GenericAdapterRequest) adapterRequest;

        try {
            TestCommunication testCommunication = TestCommunication.getInstance();

            try {

                /*
                 * InitAction
                 */
                if("init".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm == null) {
                        String url = (String)genericAdapterRequest.getParams().get(0);
                        String userName = (String)genericAdapterRequest.getParams().get(1);
                        String password = (String)genericAdapterRequest.getParams().get(2);
                        baliHTTPComm = new BaliHTTPComm(url, userName, password);
                        testCommunication.enqueueMsg(from,
                                new GenericDataResponse(
                                        new SimpleElement("ok", ElementType.ATOM)));
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("already_started", null));
                    }

                /*
                 * CreateAction
                 */
                } else if("create".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm != null) {
                        InputStream inputStream = baliHTTPComm.doPostInputStream(
                                "/adi/create_assets.yaws", toObject(
                                        genericAdapterRequest.getParams().get(0)));
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, false);
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * FindByIdAction
                 */
                } else if("find_by_id".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm != null) {
                        String assetId = (String)genericAdapterRequest.getParams().get(0);
                        InputStream inputStream = baliHTTPComm.doGetInputStream(
                                "/adi/asset.yaws?id=" + BaliHTTPComm.encodeParam(
                                        assetId));
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, true);                    
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * FindAllAction
                 */
                } else if("find_all".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm != null) {
                        InputStream inputStream = baliHTTPComm.doGetInputStream(
                        "/adi/assets.yaws");
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, false);
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * UpdateAction
                 */
                } else if("update".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm != null) {

                        /*
                         * TODO BUG: updateAsset does not throw a 
                         * InstanceNotFoundException if the asset does not exist,
                         * so we find the asset before to force the exception.
                         */
                        InputStream inputStreamTmp = baliHTTPComm.doGetInputStream(
                                "/adi/asset.yaws?id=" + BaliHTTPComm.encodeParam(
                                        (String)((List<Object>)
                                                (((List<Object>)
                                                        ((List<Object>)
                                                                genericAdapterRequest.getParams().get(0))).get(0))).get(1))

                        );
                        if(HTTPXMLResponse.NOT_FOUND_RESPONSE.equals(
                                HTTPXMLBaliSystemCodec.getResponse(
                                        inputStreamTmp).getResponseCode())) {
                            testCommunication.enqueueMsg(from,
                                    new GenericErrorResponse("not_found", null));
                        } else {
                            InputStream inputStream = baliHTTPComm.doPostInputStream(
                                    "/adi/update_assets.yaws", toObject(
                                            genericAdapterRequest.getParams().get(0)));
                            HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                    inputStream);
                            enqueueMsg(from, response, false);
                        }
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * DeleteAction
                 */
                } else if("delete".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm != null) {
                        String assetId = (String)genericAdapterRequest.getParams().get(0);
                        InputStream inputStream = baliHTTPComm.doGetInputStream(
                                "/adi/delete_asset.yaws?id=" +
                                BaliHTTPComm.encodeParam(assetId));
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, false);
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * ResetAction
                 */
                } else if("reset".equals(genericAdapterRequest.getName())) {

                    if(baliHTTPComm != null) {
                        baliHTTPComm = null;
                        testCommunication.enqueueMsg(from,
                                new GenericDataResponse(
                                        new SimpleElement("ok", ElementType.ATOM)));
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }
                }

                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);
            } catch(ConnectException ce) {
                testCommunication.enqueueMsg(from,
                        new GenericErrorResponse("connection_error", null));
                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);
            } catch(ServerException se) {
                se.printStackTrace();
                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_ERROR);
            } catch(Exception e) {
                testCommunication.enqueueMsg(from,
                        new GenericErrorResponse("authentication_error", null));
                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);
            }

        } catch(Exception e) {
            e.printStackTrace();
            return new SystemAdapterCommunication(
                    SystemAdapterCommunication.STATUS_ERROR);
        }

    }

    private void enqueueMsg(OtpErlangPid from, HTTPXMLResponse response,
            boolean expectsOneAsset)
    throws Exception {

        TestCommunication testCommunication = TestCommunication.getInstance();

        if(!response.isXML()) {
            if(HTTPXMLResponse.OK_RESPONSE.equals(
                    response.getResponseCode())) {
                testCommunication.enqueueMsg(from,
                        new GenericDataResponse(
                                new SimpleElement("ok", ElementType.ATOM)));
            } else if(HTTPXMLResponse.NOT_FOUND_RESPONSE.equals(
                    response.getResponseCode())) {
                testCommunication.enqueueMsg(from,
                        new GenericErrorResponse("not_found", null));
            } else if(HTTPXMLResponse.DUPLICATE_ID_RESPONSE.equals(
                    response.getResponseCode())) {
                testCommunication.enqueueMsg(from,
                        new GenericErrorResponse("duplicated_asset", null));
            }
        } else {
            ListElement assets = response.getAssets();
            if(assets.getElements().size() == 1 && expectsOneAsset) {
                testCommunication.enqueueMsg(from, new GenericDataResponse(
                        assets.getElements().get(0)));
            } else {
                testCommunication.enqueueMsg(from, new GenericDataResponse(
                        assets));
            }
        }
    }

    private String toObject(Object o) throws Exception {

        if(o instanceof List) {
            List<List<String>> fields = (List<List<String>>) o;

            String assetId = null;
            String creationDate = null;
            String assetTitle = null;
            String assetDate = null;
            String assetSummary = null;
            String assetRating = null;
            String assetGenre = null;
            String assetRuntime = null;

            for(List<String> f : fields) {

                String key = f.get(0);
                if("id".equals(key)) {
                    assetId = f.get(1);
                }
                if("creation_date".equals(key)) {
                    creationDate = f.get(1);
                }
                if("title".equals(key)) {
                    assetTitle = f.get(1);
                }
                if("date".equals(key)) {
                    assetDate = f.get(1);
                }
                if("summary".equals(key)) {
                    assetSummary = f.get(1);
                }
                if("rating".equals(key)) {
                    assetRating = f.get(1);
                }
                if("genre".equals(key)) {
                    assetGenre = f.get(1);
                }
                if("run_time".equals(key)) {
                    assetRuntime = f.get(1);
                }

            }

            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            DOMImplementation impl = builder.getDOMImplementation();
            Document xmldoc = impl.createDocument(null, "Assets", null);
            Element root = xmldoc.getDocumentElement();

            Element assetElement = xmldoc.createElement("Asset");
            root.appendChild(assetElement);
            assetElement.setAttribute("Asset_ID", assetId);
            assetElement.setAttribute("Creation_Date", creationDate);

            Element title = xmldoc.createElement("Title");
            title.appendChild(xmldoc.createTextNode(assetTitle));
            assetElement.appendChild(title);

            Element date = xmldoc.createElement("Date");
            date.appendChild(xmldoc.createTextNode(assetDate));
            assetElement.appendChild(date);

            Element summary = xmldoc.createElement("Summary");
            summary.appendChild(xmldoc.createTextNode(assetSummary));
            assetElement.appendChild(summary);

            Element rating = xmldoc.createElement("Rating");
            rating.appendChild(xmldoc.createTextNode(assetRating));
            assetElement.appendChild(rating);

            Element genre = xmldoc.createElement("Genre");
            genre.appendChild(xmldoc.createTextNode(assetGenre));
            assetElement.appendChild(genre);

            Element runTime = xmldoc.createElement("Run_Time");
            runTime.appendChild(xmldoc.createTextNode(assetRuntime));
            assetElement.appendChild(runTime);

            DOMSource domSource = new DOMSource(xmldoc);
            TransformerFactory tfactory = TransformerFactory.newInstance();
            Transformer transformer = tfactory.newTransformer();

            StringWriter writer = new StringWriter();
            StreamResult streamResult = new StreamResult(writer);
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");

            transformer.transform(domSource, streamResult);
            return writer.toString();

        }

        throw new Exception("Not supported");

    }

}
