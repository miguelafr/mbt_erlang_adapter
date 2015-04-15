package framework.systemadapter;

import com.ericsson.otp.erlang.OtpErlangPid;


public interface SystemAdapter {

    public SystemAdapterCommunication send(OtpErlangPid from, Object o)
    throws Exception;
    
}
