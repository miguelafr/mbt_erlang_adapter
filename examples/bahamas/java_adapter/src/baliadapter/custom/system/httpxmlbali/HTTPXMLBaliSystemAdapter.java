package baliadapter.custom.system.httpxmlbali;

import java.io.InputStream;
import java.io.StringWriter;
import java.net.ConnectException;
import java.util.Collection;

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

import baliadapter.custom.codec.AssetTransfer;
import baliadapter.custom.requests.Action;
import baliadapter.custom.requests.CreateAction;
import baliadapter.custom.requests.DeleteAction;
import baliadapter.custom.requests.FindAllAction;
import baliadapter.custom.requests.FindByIdAction;
import baliadapter.custom.requests.InitAction;
import baliadapter.custom.requests.ResetAction;
import baliadapter.custom.requests.UpdateAction;
import baliadapter.custom.responses.AlreadyStartedResponse;
import baliadapter.custom.responses.AssetNotFoundResponse;
import baliadapter.custom.responses.AssetResponse;
import baliadapter.custom.responses.AssetsResponse;
import baliadapter.custom.responses.ConnectionErrorResponse;
import baliadapter.custom.responses.DuplicatedAssetResponse;
import baliadapter.custom.responses.NotStartedResponse;
import baliadapter.custom.responses.OkResponse;
import baliadapter.custom.responses.UserNotAuthenticatedResponse;
import baliadapter.util.BaliHTTPComm;

import com.ericsson.otp.erlang.OtpErlangPid;

import framework.codecadapter.requests.AdapterRequest;
import framework.systemadapter.SystemAdapterCommunication;
import framework.systemadapter.adapters.StandardSystemAdapter;
import framework.testcommunication.TestCommunication;

public class HTTPXMLBaliSystemAdapter extends StandardSystemAdapter {

    private BaliHTTPComm baliHTTPComm;

    /*
     * TODO: Delete this variable. It is only necessary for a hack.
     */
    private InitAction lastInitAction;


    public SystemAdapterCommunication send(OtpErlangPid from,
            AdapterRequest adapterRequest) {

        try {

            TestCommunication testCommunication = TestCommunication.getInstance();

            try {
                Action  action = (Action) adapterRequest;

                /*
                 * InitAction
                 */
                if(action instanceof InitAction) {
                    if(baliHTTPComm == null) {
                        InitAction initAction = (InitAction) action;
                        baliHTTPComm = new BaliHTTPComm(initAction.getUrl(),
                                initAction.getUserName(), initAction.getPassword());
                        testCommunication.enqueueMsg(from, new OkResponse());

                        /*
                         * TODO: Delete this line. It is only necessary for a hack.
                         */
                        lastInitAction = initAction;

                    } else {
                        testCommunication.enqueueMsg(from, new AlreadyStartedResponse());
                    }

                    /*
                     * CreateAction
                     */
                } else if(action instanceof CreateAction) {
                    CreateAction createAction = (CreateAction) action;
                    if(baliHTTPComm != null) {
                        InputStream inputStream = baliHTTPComm.doPostInputStream(
                                "/adi/create_assets.yaws", toObject(
                                        createAction.getAsset()));
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, false);
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                    /*
                     * FindByIdAction
                     */
                } else if(action instanceof FindByIdAction) {
                    FindByIdAction findByIdAction = (FindByIdAction) action;
                    if(baliHTTPComm != null) {
                        InputStream inputStream = baliHTTPComm.doGetInputStream(
                                "/adi/asset.yaws?id=" + BaliHTTPComm.encodeParam(
                                        findByIdAction.getAssetId()));
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, true);                    
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                    /*
                     * FindAllAction
                     */
                } else if(action instanceof FindAllAction) {
                    if(baliHTTPComm != null) {
                        InputStream inputStream = baliHTTPComm.doGetInputStream(
                        "/adi/assets.yaws");
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, false);
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                    /*
                     * UpdateAction
                     */
                } else if(action instanceof UpdateAction) {
                    UpdateAction updateAction = (UpdateAction) action;
                    if(baliHTTPComm != null) {
                        /*
                         * TODO BUG: updateAsset does not throw a 
                         * InstanceNotFoundException if the asset does not exist,
                         * so we find the asset before to force the exception.
                         */
                        InputStream inputStreamTmp = baliHTTPComm.doGetInputStream(
                                "/adi/asset.yaws?id=" + BaliHTTPComm.encodeParam(
                                        updateAction.getAsset().getAssetId()));
                        if(HTTPXMLResponse.NOT_FOUND_RESPONSE.equals(
                                HTTPXMLBaliSystemCodec.getResponse(
                                        inputStreamTmp).getResponseCode())) {
                            testCommunication.enqueueMsg(from, new AssetNotFoundResponse());
                        } else {
                            InputStream inputStream = baliHTTPComm.doPostInputStream(
                                    "/adi/update_assets.yaws", toObject(
                                            updateAction.getAsset()));
                            HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                    inputStream);
                            enqueueMsg(from, response, false);
                        }
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                    /*
                     * DeleteAction
                     */
                } else if(action instanceof DeleteAction) {
                    DeleteAction deleteAction = (DeleteAction) action;
                    if(baliHTTPComm != null) {
                        InputStream inputStream = baliHTTPComm.doGetInputStream(
                                "/adi/delete_asset.yaws?id=" +
                                BaliHTTPComm.encodeParam(deleteAction.getAssetId()));
                        HTTPXMLResponse response = HTTPXMLBaliSystemCodec.getResponse(
                                inputStream);
                        enqueueMsg(from, response, false);
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                    /*
                     * ResetAction
                     */
                } else if(action instanceof ResetAction) {
                    if(baliHTTPComm != null) {
                        baliHTTPComm = null;
                        testCommunication.enqueueMsg(from, new OkResponse());
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }
                }

                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);
            } catch(ConnectException ce) {
                testCommunication.enqueueMsg(from,
                        new ConnectionErrorResponse());
                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);
            } catch(Exception e) {
                testCommunication.enqueueMsg(from,
                        new UserNotAuthenticatedResponse());
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
                testCommunication.enqueueMsg(from, new OkResponse());
            } else if(HTTPXMLResponse.NOT_FOUND_RESPONSE.equals(
                    response.getResponseCode())) {
                testCommunication.enqueueMsg(from, new AssetNotFoundResponse());
            } else if(HTTPXMLResponse.DUPLICATE_ID_RESPONSE.equals(
                    response.getResponseCode())) {
                testCommunication.enqueueMsg(from, new DuplicatedAssetResponse());
            }
        } else {
            Collection<AssetTransfer> assets = response.getAssets();
            if(assets.size() == 1 && expectsOneAsset) {
                testCommunication.enqueueMsg(from, new AssetResponse(
                        assets.iterator().next()));
            } else {
                testCommunication.enqueueMsg(from, new AssetsResponse(assets));
            }
        }
    }

    private String toObject(AssetTransfer asset) throws Exception {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            DOMImplementation impl = builder.getDOMImplementation();
            Document xmldoc = impl.createDocument(null, "Assets", null);
            Element root = xmldoc.getDocumentElement();

            Element assetElement = xmldoc.createElement("Asset");
            root.appendChild(assetElement);
            assetElement.setAttribute("Asset_ID", asset.getAssetId());
            assetElement.setAttribute("Creation_Date", new Long(
                    asset.getCreationDate()).toString());

            Element title = xmldoc.createElement("Title");
            title.appendChild(xmldoc.createTextNode(asset.getTitle()));
            assetElement.appendChild(title);

            Element date = xmldoc.createElement("Date");
            date.appendChild(xmldoc.createTextNode(asset.getDate()));
            assetElement.appendChild(date);

            Element summary = xmldoc.createElement("Summary");
            summary.appendChild(xmldoc.createTextNode(asset.getSummary()));
            assetElement.appendChild(summary);

            Element genre = xmldoc.createElement("Genre");
            genre.appendChild(xmldoc.createTextNode(asset.getGenre()));
            assetElement.appendChild(genre);

            Element rating = xmldoc.createElement("Rating");
            rating.appendChild(xmldoc.createTextNode(asset.getRating()));
            assetElement.appendChild(rating);

            Element runTime = xmldoc.createElement("Run_Time");
            runTime.appendChild(xmldoc.createTextNode(asset.getRunTime()));
            assetElement.appendChild(runTime);

            DOMSource domSource = new DOMSource(xmldoc);
            TransformerFactory tfactory = TransformerFactory.newInstance();
            Transformer transformer = tfactory.newTransformer();

            StringWriter writer = new StringWriter();
            StreamResult streamResult = new StreamResult(writer);
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");

            transformer.transform(domSource, streamResult);
            return writer.toString();

        } catch (Exception e) {
            return null;
        }

    }

}
