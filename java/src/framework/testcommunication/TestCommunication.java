package framework.testcommunication;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import framework.codecadapter.CodecAdapter;
import framework.codecadapter.CodecAdapterFactory;
import framework.codecadapter.requests.AdapterRequest;
import framework.codecadapter.responses.AdapterResponse;
import framework.systemadapter.SystemAdapter;
import framework.systemadapter.SystemAdapterCommunication;
import framework.systemadapter.SystemAdapterFactory;

public class TestCommunication {

    private static TestCommunication instance;

    private OtpNode self;
    private OtpMbox mbox;

    private TestCommunication() {
        ;
    }

    public static TestCommunication getInstance() {
        if(instance == null) {
            instance = new TestCommunication();
        }

        return instance;
    }

    public void start() throws Exception {
        self = new OtpNode("testnode@localhost");
        mbox = self.createMbox("testservice");

        CodecAdapter codecAdapter = CodecAdapterFactory.getCodecAdapter();
        SystemAdapter systemAdapter = SystemAdapterFactory.getSystemAdapter();

        while (true) {
            /*
             * Receive a new message
             */
            OtpErlangObject o = mbox.receive();

            /*
             * Get the pid of the process which sent the message
             */
            OtpErlangPid from = (OtpErlangPid)(((OtpErlangTuple)o).elementAt(0));
            
            /*
             * Encode message
             */
            Object encodedMsg = codecAdapter.encode(
                    ((OtpErlangTuple)o).elementAt(1));
            
            /*
             * Send message to the SUT
             */
            SystemAdapterCommunication systemAdapterCommunication =
                systemAdapter.send(from, encodedMsg);

            /*
             * Check the status code
             */
            if(systemAdapterCommunication.getStatusCode() ==
                SystemAdapterCommunication.STATUS_ERROR) {
                throw new Exception();
            }
        }
    }

    public void enqueueMsg(OtpErlangPid from, AdapterResponse o) throws Exception {
        CodecAdapter codecAdapter = CodecAdapterFactory.getCodecAdapter();

        OtpErlangObject oeo = codecAdapter.decode(o);
        mbox.send(from, oeo);
    }
}
