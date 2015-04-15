package baliadapter.custom.requests;

import baliadapter.custom.codec.AssetTransfer;

public class CreateAction extends Action {

    private AssetTransfer asset;
    
    public CreateAction(AssetTransfer asset) {
        this.asset = asset;
    }
    
    public AssetTransfer getAsset() {
        return asset;
    }
}
