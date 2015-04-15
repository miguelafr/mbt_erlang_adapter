package framework.codecadapter.codecs;

import com.ericsson.otp.erlang.OtpErlangObject;

import framework.codecadapter.CodecAdapter;
import framework.codecadapter.requests.AdapterRequest;
import framework.codecadapter.responses.AdapterResponse;

public abstract class StandardCodecAdapter implements CodecAdapter {

    public final OtpErlangObject decode(Object o) throws Exception {
        return decode((AdapterResponse)o);
    }

    public abstract AdapterRequest encode(OtpErlangObject o) throws Exception;
    
    public abstract OtpErlangObject decode(AdapterResponse ar) throws Exception; 

}
