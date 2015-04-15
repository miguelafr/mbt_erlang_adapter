package baliadapter.custom.system.javabali;

import java.util.Collection;

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

import com.ericsson.otp.erlang.OtpErlangPid;
import com.lambdastream.bali.asset.facade.AssetFacade;
import com.lambdastream.bali.exception.DuplicateInstanceException;
import com.lambdastream.bali.exception.InstanceNotFoundException;
import com.lambdastream.bali.exception.ServerException;
import com.lambdastream.bali.exception.UserNotAuthenticatedException;
import com.lambdastream.bali.util.configuration.BaliConfiguration;
import com.lambdastream.bali.util.configuration.DefaultLoginRequestor;

import framework.codecadapter.requests.AdapterRequest;
import framework.systemadapter.SystemAdapterCommunication;
import framework.systemadapter.adapters.StandardSystemAdapter;
import framework.testcommunication.TestCommunication;

public class JavaBaliSystemAdapter extends StandardSystemAdapter {

    private AssetFacade assetFacade;

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
                    InitAction initAction = (InitAction) action;
                    if(assetFacade == null) {
                        assetFacade = AssetFacade.createInstance(
                                new BaliConfiguration(initAction.getUrl(),
                                        new DefaultLoginRequestor(
                                                initAction.getUserName(),
                                                initAction.getPassword())));
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
                    if(assetFacade != null) {
                        try {
                            assetFacade.addAsset(JavaBaliSystemCodec.getAsset(
                                    createAction.getAsset()));
                            testCommunication.enqueueMsg(from, new OkResponse());
                        } catch(DuplicateInstanceException die) {
                            testCommunication.enqueueMsg(from,
                                    new DuplicatedAssetResponse());
                        }
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                /*
                 * FindByIdAction
                 */
                } else if(action instanceof FindByIdAction) {
                    FindByIdAction findByIdAction = (FindByIdAction) action;
                    if(assetFacade != null) {
                        try {
                            AssetTransfer asset = JavaBaliSystemCodec.getAssetTransfer(
                                    assetFacade.findAsset(
                                            findByIdAction.getAssetId()));
                            testCommunication.enqueueMsg(from, new AssetResponse(
                                    asset));
                        } catch(InstanceNotFoundException infe) {
                            testCommunication.enqueueMsg(from,
                                    new AssetNotFoundResponse());
                        }
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                /*
                 * FindAllAction
                 */
                } else if(action instanceof FindAllAction) {
                    if(assetFacade != null) {
                        Collection<AssetTransfer> assets =
                            JavaBaliSystemCodec.getAssetTransfers(assetFacade.findAssets());
                        testCommunication.enqueueMsg(from, new AssetsResponse(assets));
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                /*
                 * UpdateAction
                 */
                } else if(action instanceof UpdateAction) {
                    UpdateAction updateAction = (UpdateAction) action;
                    if(assetFacade != null) {
                        try {
                            /*
                             * TODO BUG: updateAsset does not throw a 
                             * InstanceNotFoundException if the asset does not exist,
                             * so we find the asset before to force the exception.
                             */
                            assetFacade.findAsset(updateAction.getAsset().getAssetId());
                            assetFacade.updateAsset(JavaBaliSystemCodec.getAsset(
                                    updateAction.getAsset()));
                            testCommunication.enqueueMsg(from, new OkResponse());
                        } catch(InstanceNotFoundException infe) {
                            testCommunication.enqueueMsg(from,
                                    new AssetNotFoundResponse());
                        }
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                /*
                 * DeleteAction
                 */
                } else if(action instanceof DeleteAction) {
                    DeleteAction deleteAction = (DeleteAction) action;
                    if(assetFacade != null) {
                        try {
                            assetFacade.removeAsset(deleteAction.getAssetId());
                            testCommunication.enqueueMsg(from, new OkResponse());
                        } catch(InstanceNotFoundException infe) {
                            testCommunication.enqueueMsg(from,
                                    new AssetNotFoundResponse());
                        }
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }

                /*
                 * ResetAction
                 */
                } else if(action instanceof ResetAction) {
                    if(assetFacade != null) {
                        assetFacade = null;
                        testCommunication.enqueueMsg(from, new OkResponse());
                    } else {
                        testCommunication.enqueueMsg(from, new NotStartedResponse());
                    }
                }

                return new SystemAdapterCommunication(
                        SystemAdapterCommunication.STATUS_OK);

            } catch(UserNotAuthenticatedException unae) {
                /*
                 * TODO: Hack done to differentiate between an incorrect
                 * user/password and an incorrect URL.
                 */
                if(!lastInitAction.getUrl().equals("http://localhost:8888")) {
                    testCommunication.enqueueMsg(from,
                            new ConnectionErrorResponse());
                } else {
                    testCommunication.enqueueMsg(from,
                            new UserNotAuthenticatedResponse());

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

}
