package framework.codecadapter.erlangtypes;


public class SimpleElement implements Element {

    private ElementType type;
    
    private Object value;

    public SimpleElement(byte[] value) {
        this.value = value;
        this.type = ElementType.BINARY;
    }
    
    public SimpleElement(String value) {
        this.value = value;
        this.type = ElementType.STRING;
    }
    
    public SimpleElement(Object value, ElementType type) {
        this.value = value;
        this.type = type;
    }
    
    public ElementType getType() {
        return type;
    }
    
    public Object getValue() {
        return value;
    }
}
