package baliadapter.custom.requests;

public class FindByIdAction extends Action {

    private String assetId;
    
    public FindByIdAction(String assetId) {
        this.assetId = assetId;
    }
    
    public String getAssetId() {
        return assetId;
    }
}
