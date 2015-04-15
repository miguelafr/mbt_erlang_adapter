package framework.codecadapter.codecs;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import framework.codecadapter.CodecAdapter;
import framework.codecadapter.erlangtypes.ElementType;
import framework.codecadapter.erlangtypes.ListElement;
import framework.codecadapter.erlangtypes.SimpleElement;
import framework.codecadapter.erlangtypes.TupleElement;
import framework.codecadapter.requests.GenericAdapterRequest;
import framework.codecadapter.responses.GenericDataResponse;
import framework.codecadapter.responses.GenericErrorResponse;

public class GenericCodecAdapter implements CodecAdapter {

    public Object encode(OtpErlangObject o) throws Exception {

        OtpErlangTuple oet = (OtpErlangTuple) o;

        OtpErlangAtom otpFunName = (OtpErlangAtom)oet.elementAt(0);
        OtpErlangList otpParams = (OtpErlangList)oet.elementAt(1);

        String funName = otpFunName.toString();

        List<Object> params = new ArrayList<Object>();
        for(OtpErlangObject oeo : otpParams.elements()) {
            params.add(toParam((OtpErlangTuple)oeo));
        }

        return new GenericAdapterRequest(funName, params);

    }

    public OtpErlangObject decode(Object o) throws Exception {
        if(o instanceof GenericDataResponse) {
            GenericDataResponse genericDataResponse =
                (GenericDataResponse) o;
            return toErlangObject(genericDataResponse.getData());

        } else if(o instanceof GenericErrorResponse) {
            GenericErrorResponse genericErrorResponse =
                (GenericErrorResponse) o;
            return new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom("error"),
                            new OtpErlangAtom(genericErrorResponse.getName())
                    });
        }

        return null;
    }

    private Object toParam(OtpErlangTuple o) throws Exception{

        OtpErlangTuple typeTuple = ((OtpErlangTuple)o.elementAt(0));
        OtpErlangTuple valueTuple = ((OtpErlangTuple)o.elementAt(1));

        OtpErlangObject type = typeTuple.elementAt(1);

        if(type instanceof OtpErlangAtom) {

            return toObject(valueTuple.elementAt(1));

        } else if(type instanceof OtpErlangTuple) {
            OtpErlangTuple subtypeTuple = (OtpErlangTuple) type;

            OtpErlangAtom subtype = (OtpErlangAtom)subtypeTuple.elementAt(0);

            if("tuple".equals(subtype.atomValue())) {

                List<Object> result = new ArrayList<Object>();
                OtpErlangTuple tuple =
                    ((OtpErlangTuple)valueTuple.elementAt(1));
                OtpErlangTuple tupleTypes =
                    (OtpErlangTuple)subtypeTuple.elementAt(1);

                for(int i = 0; i < tuple.elements().length; i++) {

                    result.add(toParam(
                            new OtpErlangTuple(new OtpErlangObject[] {
                                    new OtpErlangTuple(new OtpErlangObject[] {
                                            new OtpErlangAtom("type"),
                                            tupleTypes.elementAt(i)
                                    }),
                                    new OtpErlangTuple(new OtpErlangObject[] {
                                            new OtpErlangAtom("value"),
                                            tuple.elementAt(i)
                                    })
                            })
                    ));
                }

                return result;

            } else if("list".equals(subtype.atomValue())) {
                List<Object> result = new ArrayList<Object>();
                OtpErlangList tuple =
                    ((OtpErlangList)valueTuple.elementAt(1));
                OtpErlangTuple tupleTypes =
                    (OtpErlangTuple)subtypeTuple.elementAt(1);

                for(int i = 0; i < tuple.elements().length; i++) {
                    result.add(toParam(
                            new OtpErlangTuple(new OtpErlangObject[] {
                                    new OtpErlangTuple(new OtpErlangObject[] {
                                            new OtpErlangAtom("type"),
                                            tupleTypes.elementAt(i)
                                    }),
                                    new OtpErlangTuple(new OtpErlangObject[] {
                                            new OtpErlangAtom("value"),
                                            tuple.elementAt(i)
                                    })
                            })
                    ));
                }

                return result;

            } else {
                System.out.println(o.getClass().getName());
                throw new Exception("Not supported");
            }

        } else {
            System.out.println(o.getClass().getName());
            throw new Exception("Not supported");
        }

    }

    private Object toObject(OtpErlangObject o) throws Exception{
        if(o instanceof OtpErlangString){
            return ((OtpErlangString)o).stringValue();

        } else if(o instanceof OtpErlangAtom) {
            return ((OtpErlangAtom)o).atomValue();

        } else if(o instanceof OtpErlangBinary) {
            return ((OtpErlangBinary)o).binaryValue();

        } else if(o instanceof OtpErlangList) {
            List<Object> result = new ArrayList<Object>();
            for(OtpErlangObject e : ((OtpErlangList)o).elements()) {
                result.add(toObject(e));
            }
            return result;

        } else {
            System.out.println(o.getClass().getName());
            throw new Exception("Not supported");
        }
    }
    
    private OtpErlangObject toErlangObject(Object o) throws Exception {

        if(o instanceof SimpleElement) {

            SimpleElement e = (SimpleElement)o;

            if(ElementType.ATOM.equals(e.getType())) {
                return new OtpErlangAtom((String)e.getValue());
            } else if(ElementType.STRING.equals(e.getType())) {
                return new OtpErlangString((String)e.getValue());
            } else if(ElementType.BINARY.equals(e.getType())) {
                return new OtpErlangBinary((byte[])e.getValue());
            }

        } else if(o instanceof TupleElement) {

            TupleElement t = (TupleElement) o;
            OtpErlangObject[] fields =
                new OtpErlangObject[t.getElements().size()];
            int i = 0;
            for(i = 0; i < t.getElements().size(); i++) {
                fields[i] = toErlangObject(t.getElements().get(i));
            }

            return new OtpErlangTuple(fields);

        } else if(o instanceof ListElement) {

            ListElement l = (ListElement) o;
            OtpErlangObject[] elements =
                new OtpErlangObject[l.getElements().size()];
            int i = 0;
            for(i = 0; i < l.getElements().size(); i++) {
                elements[i] = toErlangObject(l.getElements().get(i));
            }

            return new OtpErlangList(elements);

        }

        System.out.println(o.getClass().getName());
        throw new Exception("Not supported");

    }

}
