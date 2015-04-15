package baliadapter.generic.system.httpxmlbaligeneric;

import framework.codecadapter.erlangtypes.ListElement;

public class HTTPXMLResponse {

    public final static String OK_RESPONSE = "ok";

    public final static String NOT_FOUND_RESPONSE = "not_found";

    public final static String DUPLICATE_ID_RESPONSE = "duplicate_id";
    
    private String responseCode;
    
    private ListElement assets;
    
    private boolean isXML;
    
    public HTTPXMLResponse(String responseCode, ListElement assets,
            boolean isXML) {
        this.responseCode = responseCode;
        this.assets = assets;
        this.isXML = isXML;
    }
    
    public String getResponseCode() {
        return responseCode;
    }
    
    public ListElement getAssets() {
        return assets;
    }
    
    public boolean isXML() {
        return isXML;
    }
}
