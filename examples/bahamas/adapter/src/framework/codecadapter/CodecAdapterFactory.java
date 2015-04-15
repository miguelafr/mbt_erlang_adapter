package framework.codecadapter;

import framework.codecadapter.codecs.GenericCodecAdapter;

public final class CodecAdapterFactory {
    
    private static CodecAdapter instance = new GenericCodecAdapter();
    
    public static void setCodecAdapter(CodecAdapter codecAdapter) {
        instance = codecAdapter;
    }
    
    public static CodecAdapter getCodecAdapter() {
        return instance;
    }
}
