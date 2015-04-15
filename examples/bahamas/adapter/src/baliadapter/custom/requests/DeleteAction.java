package baliadapter.custom.requests;

public class DeleteAction extends Action {

    private String assetId;
    
    public DeleteAction(String assetId) {
        this.assetId = assetId;
    }
    
    public String getAssetId() {
        return assetId;
    }
}
