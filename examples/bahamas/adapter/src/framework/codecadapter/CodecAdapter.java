package framework.codecadapter;

import com.ericsson.otp.erlang.OtpErlangObject;


public interface CodecAdapter {

    public Object encode(OtpErlangObject o) throws Exception;
    
    public OtpErlangObject decode(Object o) throws Exception;
}
