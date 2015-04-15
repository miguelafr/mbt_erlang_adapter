package baliadapter;
import baliadapter.generic.system.httpxmlbaligeneric.HTTPXMLBaliGenericSystemAdapter;
import framework.systemadapter.SystemAdapterFactory;
import framework.testcommunication.TestCommunication;


public class MainGenericHTTPXMLBali {

    public static void main(String[] args) {
        try {
            
            System.out.println("Starting adapter...");
            
            /*
             * Initializes system adapter for communicating with the SUT
             */
            SystemAdapterFactory.setSystemAdapter(new HTTPXMLBaliGenericSystemAdapter());
            
            /*
             * Start receiving messages
             */
            TestCommunication.getInstance().start();
            
        } catch(Exception e) {
            e.printStackTrace();
        }
    }

}
