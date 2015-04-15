package framework.systemadapter;


public final class SystemAdapterFactory {

    private static SystemAdapter instance;
    
    public static void setSystemAdapter(SystemAdapter systemAdapter) {
        instance = systemAdapter;
    }
    
    public static SystemAdapter getSystemAdapter() {
        return instance;
    }
}
