package framework.codecadapter.codecs;

import com.ericsson.otp.erlang.OtpErlangObject;

import framework.codecadapter.CodecAdapter;

public class SimpleCodecAdapter implements CodecAdapter {

    public Object encode(OtpErlangObject o) throws Exception {
        return o;
    }
    
    public OtpErlangObject decode(Object o) throws Exception {
        return (OtpErlangObject)o;
    }

}
