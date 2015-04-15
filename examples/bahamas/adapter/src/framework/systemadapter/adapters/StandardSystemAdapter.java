package framework.systemadapter.adapters;

import com.ericsson.otp.erlang.OtpErlangPid;

import framework.codecadapter.requests.AdapterRequest;
import framework.systemadapter.SystemAdapter;
import framework.systemadapter.SystemAdapterCommunication;

public abstract class StandardSystemAdapter implements SystemAdapter {

    public final SystemAdapterCommunication send(OtpErlangPid from,
            Object o) throws Exception {
        return send(from, (AdapterRequest) o);
    }
    
    public abstract SystemAdapterCommunication send(OtpErlangPid from,
            AdapterRequest adapterRequest) throws Exception;

}
