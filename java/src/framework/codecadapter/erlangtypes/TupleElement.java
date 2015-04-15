package framework.codecadapter.erlangtypes;

import java.util.List;

public class TupleElement implements Element {

    private List<Element> elements;
    
    public TupleElement(List<Element> elements) {
        this.elements = elements;
    }
    
    public List<Element> getElements() {
        return elements;
    }
}
