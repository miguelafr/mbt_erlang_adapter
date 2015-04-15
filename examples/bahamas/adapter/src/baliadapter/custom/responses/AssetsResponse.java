package baliadapter.custom.responses;

import java.util.Collection;

import baliadapter.custom.codec.AssetTransfer;

public class AssetsResponse extends Response {

    private Collection<AssetTransfer> assets;
    
    public AssetsResponse(Collection<AssetTransfer> assets) {
        this.assets = assets;
    }
    
    public Collection<AssetTransfer> getAssets() {
        return assets;
    }
}
