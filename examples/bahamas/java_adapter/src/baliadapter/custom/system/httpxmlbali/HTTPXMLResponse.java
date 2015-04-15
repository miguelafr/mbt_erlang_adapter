package baliadapter.custom.system.httpxmlbali;

import java.util.Collection;

import baliadapter.custom.codec.AssetTransfer;

public class HTTPXMLResponse {

    public final static String OK_RESPONSE = "ok";

    public final static String NOT_FOUND_RESPONSE = "not_found";

    public final static String DUPLICATE_ID_RESPONSE = "duplicate_id";
    
    private String responseCode;
    
    private Collection<AssetTransfer> assets;
    
    private boolean isXML;
    
    public HTTPXMLResponse(String responseCode,
            Collection<AssetTransfer> assets, boolean isXML) {
        this.responseCode = responseCode;
        this.assets = assets;
        this.isXML = isXML;
    }
    
    public String getResponseCode() {
        return responseCode;
    }
    
    public Collection<AssetTransfer> getAssets() {
        return assets;
    }
    
    public boolean isXML() {
        return isXML;
    }
}
