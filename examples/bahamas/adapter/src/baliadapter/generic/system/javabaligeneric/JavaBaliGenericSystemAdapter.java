package baliadapter.generic.system.javabaligeneric;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import baliadapter.util.RuntimeEncoder;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.lambdastream.bali.asset.adi.Asset;
import com.lambdastream.bali.asset.adi.TitleAsset;
import com.lambdastream.bali.asset.facade.AssetFacade;
import com.lambdastream.bali.exception.DuplicateInstanceException;
import com.lambdastream.bali.exception.InstanceNotFoundException;
import com.lambdastream.bali.exception.ServerException;
import com.lambdastream.bali.exception.UserNotAuthenticatedException;
import com.lambdastream.bali.util.configuration.BaliConfiguration;
import com.lambdastream.bali.util.configuration.DefaultLoginRequestor;

import framework.codecadapter.erlangtypes.Element;
import framework.codecadapter.erlangtypes.ElementType;
import framework.codecadapter.erlangtypes.ListElement;
import framework.codecadapter.erlangtypes.SimpleElement;
import framework.codecadapter.erlangtypes.TupleElement;
import framework.codecadapter.requests.AdapterRequest;
import framework.codecadapter.requests.GenericAdapterRequest;
import framework.codecadapter.responses.GenericDataResponse;
import framework.codecadapter.responses.GenericErrorResponse;
import framework.systemadapter.SystemAdapterCommunication;
import framework.systemadapter.adapters.StandardSystemAdapter;
import framework.testcommunication.TestCommunication;

public class JavaBaliGenericSystemAdapter extends StandardSystemAdapter {

    private AssetFacade assetFacade;

    /*
     * TODO: Delete this variable. It is only necessary for a hack.
     */
    private GenericAdapterRequest lastInitAction;
    
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

                    if(assetFacade == null) {
                        String url = (String)genericAdapterRequest.getParams().get(0);
                        String userName = (String)genericAdapterRequest.getParams().get(1);
                        String password = (String)genericAdapterRequest.getParams().get(2);
                        assetFacade = AssetFacade.createInstance(
                                new BaliConfiguration(url, new DefaultLoginRequestor(
                                        userName, password)));
                        testCommunication.enqueueMsg(from,
                                new GenericDataResponse(
                                        new SimpleElement("ok", ElementType.ATOM)));
                        
                        /*
                         * TODO: Delete this line. It is only necessary for a hack.
                         */
                        lastInitAction = genericAdapterRequest;
                        
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("already_started", null));
                    }

                /*
                 * CreateAction
                 */
                } else if("create".equals(genericAdapterRequest.getName())) {
                    if(assetFacade != null) {
                        try {
                            Asset asset = toObject(
                                    genericAdapterRequest.getParams().get(0));

                            assetFacade.addAsset(asset);
                            testCommunication.enqueueMsg(from,
                                    new GenericDataResponse(
                                            new SimpleElement("ok", ElementType.ATOM)));
                        } catch(DuplicateInstanceException die) {
                            testCommunication.enqueueMsg(from,
                                    new GenericErrorResponse("duplicated_asset", null));
                        }
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * FindByIdAction
                 */
                } else if("find_by_id".equals(genericAdapterRequest.getName())) {
                    if(assetFacade != null) {
                        try {
                            String assetId = (String)genericAdapterRequest.getParams().get(0);
                            ListElement asset = toErlang(assetFacade.findAsset(
                                    assetId));
                            testCommunication.enqueueMsg(from, new GenericDataResponse(
                                    asset));
                        } catch(InstanceNotFoundException infe) {
                            testCommunication.enqueueMsg(from,
                                    new GenericErrorResponse("not_found", null));
                        }
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * FindAllAction
                 */
                } else if("find_all".equals(genericAdapterRequest.getName())) {
                    if(assetFacade != null) {
                        ListElement assets = toErlang(assetFacade.findAssets());
                        testCommunication.enqueueMsg(from,
                                new GenericDataResponse(assets));
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * UpdateAction
                 */
                } else if("update".equals(genericAdapterRequest.getName())) {
                    if(assetFacade != null) {
                        try {
                            Asset asset = toObject(
                                    genericAdapterRequest.getParams().get(0));
//                            /*
//                             * TODO BUG: updateAsset does not throw a 
//                             * InstanceNotFoundException if the asset does not exist,
//                             * so we find the asset before to force the exception.
//                             */
//                            assetFacade.findAsset(asset.getAssetID());
                            assetFacade.updateAsset(asset);
                            testCommunication.enqueueMsg(from,
                                    new GenericDataResponse(
                                            new SimpleElement("ok", ElementType.ATOM)));
                        } catch(InstanceNotFoundException infe) {
                            testCommunication.enqueueMsg(from,
                                    new GenericErrorResponse("not_found", null));
                        }
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * DeleteAction
                 */
                } else if("delete".equals(genericAdapterRequest.getName())) {
                    if(assetFacade != null) {
                        try {
                            String assetId = (String)genericAdapterRequest.getParams().get(0);
                            assetFacade.removeAsset(assetId);
                            testCommunication.enqueueMsg(from,
                                    new GenericDataResponse(
                                            new SimpleElement("ok", ElementType.ATOM)));
                        } catch(InstanceNotFoundException infe) {
                            testCommunication.enqueueMsg(from,
                                    new GenericErrorResponse("not_found", null));
                        }
                    } else {
                        testCommunication.enqueueMsg(from,
                                new GenericErrorResponse("not_started", null));
                    }

                /*
                 * ResetAction
                 */
                } else if("reset".equals(genericAdapterRequest.getName())) {
                    if(assetFacade != null) {
                        assetFacade = null;
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

            } catch(UserNotAuthenticatedException unae) {
                /*
                 * TODO: Hack done to differentiate between an incorrect
                 * user/password and an incorrect URL.
                 */
                if(!((String)lastInitAction.getParams().get(0)).equals(
                        "http://localhost:8888")) {
                    testCommunication.enqueueMsg(from,
                            new GenericErrorResponse("connection_error", null));
                } else {
                    testCommunication.enqueueMsg(from,
                            new GenericErrorResponse("authentication_error", null));

                }
                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);
            } catch(ServerException se) {
                se.printStackTrace();
                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_ERROR);
            }

        } catch(Exception e) {
            e.printStackTrace();
            return new SystemAdapterCommunication(
                    SystemAdapterCommunication.STATUS_ERROR);
        }

    }

    private TupleElement toErlangTupleElement(String id, Element e) {
        List<Element> elements = new ArrayList<Element>();
        elements.add(new SimpleElement(id, ElementType.ATOM));
        elements.add(e);
        return new TupleElement(elements);
    }

    private ListElement toErlang(Asset asset) {
        List<Element> elements = new ArrayList<Element>();
        elements.add(toErlangTupleElement("id",
                new SimpleElement(asset.getAssetID())));
        elements.add(toErlangTupleElement("creation_date",
                new SimpleElement(asset.getCreationDate().toString())));
        elements.add(toErlangTupleElement("title",
                new SimpleElement(asset.getTitleAsset().getTitle())));
        elements.add(toErlangTupleElement("date",
                new SimpleElement(asset.getTitleAsset().getDate())));
        elements.add(toErlangTupleElement("summary",
                new SimpleElement(asset.getTitleAsset().getSummary())));
        elements.add(toErlangTupleElement("rating",
                new SimpleElement(asset.getTitleAsset().getRating())));
        elements.add(toErlangTupleElement("genre",
                new SimpleElement(asset.getTitleAsset().getGenre())));
        elements.add(toErlangTupleElement("run_time",
                new SimpleElement(RuntimeEncoder.decodeRuntime(
                        asset.getTitleAsset().getRunTime()))));
        elements.add(toErlangTupleElement("content",
                new ListElement(new ArrayList<Element>())));
        elements.add(toErlangTupleElement("still_image",
                new SimpleElement(new byte[]{})));
        elements.add(toErlangTupleElement("collections",
                new ListElement(new ArrayList<Element>())));
        return new ListElement(elements);
    }

    private ListElement toErlang(Collection<Asset> assets) {
        List<Element> result = new ArrayList<Element>();
        for(Asset asset : assets) {
            result.add(toErlang(asset));
        }
        return new ListElement(result);
    }

    private Asset toObject(Object o) throws Exception {

        Asset result = new Asset();
        TitleAsset titleAsset = new TitleAsset();

        List<List<String>> fields = (List<List<String>>) o;

        for(List<String> f : fields) {
            String key = f.get(0);
            if("id".equals(key)) {
                result.setAssetID(f.get(1));
            }
            if("creation_date".equals(key)) {
                result.setCreationDate(new Long(f.get(1)));
            }
            if("title".equals(key)) {
                titleAsset.setTitle(f.get(1));
            }
            if("date".equals(key)) {
                titleAsset.setDate(f.get(1));
            }
            if("summary".equals(key)) {
                titleAsset.setSummary(f.get(1));
            }
            if("rating".equals(key)) {
                titleAsset.setRating(f.get(1));
            }
            if("genre".equals(key)) {
                titleAsset.setGenre(f.get(1));
            }
            if("run_time".equals(key)) {
                titleAsset.setRunTime(RuntimeEncoder.encodeRuntime(f.get(1)));
            }
        }

        result.setTitleAsset(titleAsset);

        return result;
    }

}
