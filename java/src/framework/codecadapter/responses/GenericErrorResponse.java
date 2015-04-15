package framework.codecadapter.responses;

import framework.codecadapter.erlangtypes.SimpleElement;

public class GenericErrorResponse implements AdapterResponse {

    private String name;
    
    private SimpleElement data;
    
    public GenericErrorResponse(String name, SimpleElement data) {
        this.name = name;
        this.data = data;
    }
    
    public String getName() {
        return name;
    }
    
    public SimpleElement getData() {
        return data;
    }
}
