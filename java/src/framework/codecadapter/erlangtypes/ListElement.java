package framework.codecadapter.erlangtypes;

import java.util.List;

public class ListElement implements Element {

    private List<Element> elements;
    
    public ListElement(List<Element> elements) {
        this.elements = elements;
    }
    
    public List<Element> getElements() {
        return elements;
    }
    
}
