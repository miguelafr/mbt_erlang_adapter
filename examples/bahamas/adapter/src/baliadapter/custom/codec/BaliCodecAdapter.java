package baliadapter.custom.codec;

import java.util.Collection;

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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import framework.codecadapter.codecs.StandardCodecAdapter;
import framework.codecadapter.requests.AdapterRequest;
import framework.codecadapter.responses.AdapterResponse;

public class BaliCodecAdapter extends StandardCodecAdapter {

    public AdapterRequest encode(OtpErlangObject oeo) throws Exception {

        OtpErlangTuple oet = (OtpErlangTuple) oeo;

        OtpErlangAtom otpFunName = (OtpErlangAtom)oet.elementAt(0);
        OtpErlangList otpParams = (OtpErlangList)oet.elementAt(1);

        String funName = otpFunName.toString();

        /*
         * init
         */
        if("init".equals(funName)) {
            String url = ((OtpErlangString)getValue(otpParams.elementAt(0))).stringValue();
            String userName = 
                ((OtpErlangString)getValue(otpParams.elementAt(1))).stringValue();
            String password =
                ((OtpErlangString)getValue(otpParams.elementAt(2))).stringValue();
            return new InitAction(url, userName, password);
            
        /*
         * create
         */
        } else if("create".equals(funName)) {
            AssetTransfer asset = encodeAsset(
                    (OtpErlangList)getValue(otpParams.elementAt(0)));
            return new CreateAction(asset);
            
        /*
         * find_by_id
         */
        } else if("find_by_id".equals(funName)) {
            String assetId =
                ((OtpErlangString)getValue(otpParams.elementAt(0))).stringValue();
            return new FindByIdAction(assetId);
            
        /*
         * find_all
         */
        } else if("find_all".equals(funName)) {
            return new FindAllAction();
            
        /*
         * update
         */
        } else if("update".equals(funName)) {
            AssetTransfer asset = encodeAsset(
                    (OtpErlangList)getValue(otpParams.elementAt(0)));
            return new UpdateAction(asset);
            
        /*
         * delete
         */
        } else if("delete".equals(funName)) {
            String assetId =
                ((OtpErlangString)getValue(otpParams.elementAt(0))).stringValue();
            return new DeleteAction(assetId);
            
        /*
         * reset
         */
        } else if("reset".equals(funName)) {
            return new ResetAction();
        }
        
        return null;
    }

    public OtpErlangObject decode(AdapterResponse ar) throws Exception {

        /*
         * Collection
         */
        if(ar instanceof AssetsResponse) {
            AssetsResponse assetsResponse = (AssetsResponse) ar;
            return decodeAssets(assetsResponse.getAssets());
            
        /*
         * Asset
         */
        } else if(ar instanceof AssetResponse) {
            AssetResponse assetResponse = (AssetResponse) ar;
            return decodeAsset(assetResponse.getAsset());
            
        /*
         * OkResponse
         */
        } else if(ar instanceof OkResponse) {
            return new OtpErlangAtom("ok");
            
        /*
         * NotStartedResponse
         */
        } else if(ar instanceof NotStartedResponse) {
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom("not_started")
                    });
            
        /*
         * AlreadyStartedResponse
         */
        } else if(ar instanceof AlreadyStartedResponse) {
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom("already_started")
                    });
            
        /*
         * UserNotAuthenticatedResponse
         */
        } else if(ar instanceof UserNotAuthenticatedResponse) {
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom("authentication_error")
                    });

        /*
         * ConnectionErrorResponse
         */
        } else if(ar instanceof ConnectionErrorResponse) {
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom("connection_error")
                    });
                
        /*
         * AssetNotFoundResponse
         */
        } else if(ar instanceof AssetNotFoundResponse) {
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom("not_found")
                    });
            
        /*
         * DuplicatedAssetResponse
         */
        } else if(ar instanceof DuplicatedAssetResponse) {
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom("duplicated_asset")
                    });            
        } else {
            
            throw new Exception();
        }

    }

    private OtpErlangList decodeAssets(Collection<AssetTransfer> assets) {
        OtpErlangList[] result = new OtpErlangList[assets.size()];

        int i = 0;
        for(AssetTransfer asset: assets) {
            result[i] = decodeAsset(asset);
            i++;
        }

        return new OtpErlangList(result);
    }

    
    private AssetTransfer encodeAsset(OtpErlangList assetFields) {
        
        String assetId = null;
        String creationDate = null;
        String title = null;
        String date = null;
        String summary = null;
        String rating = null;
        String genre = null;
        String runTime = null;
        
        for(OtpErlangObject e : assetFields.elements()) {
            OtpErlangTuple assetField = (OtpErlangTuple)e;
            String key = ((OtpErlangAtom)assetField.elementAt(0)).atomValue();
            if("id".equals(key)) {
                assetId = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("creation_date".equals(key)) {
                creationDate = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("title".equals(key)) {
                title = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("date".equals(key)) {
                date = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("summary".equals(key)) {
                summary = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("rating".equals(key)) {
                rating = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("genre".equals(key)) {
                genre = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
            if("run_time".equals(key)) {
                runTime = ((OtpErlangString)assetField.elementAt(1)).stringValue();
            }
        }

        return new AssetTransfer(assetId, creationDate, title, date, summary,
                rating, genre, runTime);
    }
    
    private OtpErlangList decodeAsset(AssetTransfer asset) {
        OtpErlangObject[] fields = new OtpErlangObject[]{
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("id"),
                        new OtpErlangString(asset.getAssetId())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("creation_date"),
                        new OtpErlangString(asset.getCreationDate())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("title"),
                        new OtpErlangString(asset.getTitle())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("date"),
                        new OtpErlangString(asset.getDate())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("summary"),
                        new OtpErlangString(asset.getSummary())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("rating"),
                        new OtpErlangString(asset.getRating())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("genre"),
                        new OtpErlangString(asset.getGenre())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("run_time"),
                        new OtpErlangString(asset.getRunTime())
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("content"),
                        new OtpErlangList()
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("still_image"),
                        new OtpErlangBinary(new byte[]{})
                }),
                new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangAtom("collections"),
                        new OtpErlangList()
                })
        };
        return new OtpErlangList(fields);
    }
    
    private OtpErlangObject getValue(OtpErlangObject param) {
        OtpErlangTuple paramValue = (OtpErlangTuple)((OtpErlangTuple)param).elementAt(1);
        return paramValue.elementAt(1);
    }

}
