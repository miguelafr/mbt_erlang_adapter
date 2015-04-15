package framework.codecadapter.responses;

import framework.codecadapter.erlangtypes.Element;

public class GenericDataResponse implements AdapterResponse {

    private Element data;
    
    public GenericDataResponse(Element data) {
        this.data = data;
    }
    
    public Element getData() {
        return data;
    }
}
