package framework.systemadapter;

public class SystemAdapterCommunication {

    public static int STATUS_OK = 1;
    public static int STATUS_ERROR = -1;
    
    private int statusCode;
    
    public SystemAdapterCommunication(int statusCode) {
        this.statusCode = statusCode;
    }
    
    public int getStatusCode() {
        return statusCode;
    }
    
    public String toString() {
        return new String("statusCode = " + statusCode);
    }
}
