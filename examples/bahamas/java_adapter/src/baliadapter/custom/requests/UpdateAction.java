package baliadapter.custom.requests;

import baliadapter.custom.codec.AssetTransfer;

public class UpdateAction extends Action {

    private AssetTransfer asset;
    
    public UpdateAction(AssetTransfer asset) {
        this.asset = asset;
    }
    
    public AssetTransfer getAsset() {
        return asset;
    }
}
