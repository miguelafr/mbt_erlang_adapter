package framework.codecadapter.requests;

import java.util.List;


public class GenericAdapterRequest implements AdapterRequest {

    private String name;
    
    private List<Object> params;
    
    public GenericAdapterRequest(String name, List<Object> params) {
        this.name = name;
        this.params = params;
    }
    
    public String getName() {
        return name;
    }
    
    public List<Object> getParams() {
        return params;
    }
    
}
