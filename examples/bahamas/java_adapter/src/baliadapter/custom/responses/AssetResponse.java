package baliadapter.custom.responses;

import baliadapter.custom.codec.AssetTransfer;

public class AssetResponse extends Response {

    private AssetTransfer asset;
    
    public AssetResponse(AssetTransfer asset) {
        this.asset = asset;
    }
    
    public AssetTransfer getAsset() {
        return asset;
    }
    
}
