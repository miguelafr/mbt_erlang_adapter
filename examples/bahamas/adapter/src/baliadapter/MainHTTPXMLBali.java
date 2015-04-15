package baliadapter;
import baliadapter.custom.codec.BaliCodecAdapter;
import baliadapter.custom.system.httpxmlbali.HTTPXMLBaliSystemAdapter;
import framework.codecadapter.CodecAdapterFactory;
import framework.systemadapter.SystemAdapterFactory;
import framework.testcommunication.TestCommunication;


public class MainHTTPXMLBali {

    public static void main(String[] args) {
        try {
            
            System.out.println("Starting adapter...");
            
            /*
             * Initialize codec for messages
             */
            CodecAdapterFactory.setCodecAdapter(new BaliCodecAdapter());
            
            /*
             * Initializes system adapter for communicating with the SUT
             */
            SystemAdapterFactory.setSystemAdapter(new HTTPXMLBaliSystemAdapter());
            
            /*
             * Start receiving messages
             */
            TestCommunication.getInstance().start();
            
        } catch(Exception e) {
            e.printStackTrace();
        }
    }

}
