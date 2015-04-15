package baliadapter.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class InputStreamUtils {

    public static ByteArrayInputStream getByteArrayInputStream(
            InputStream inputStream) throws IOException {
        
        InputStreamReader isr = new InputStreamReader(inputStream);

        List<Byte> bytes = new ArrayList<Byte>();
        byte b = -1;
        while((b = (byte) isr.read()) != -1) {
            bytes.add(new Byte(b));
        }

        byte[] buf = new byte[bytes.size()];
        int i = 0;
        for(Byte bt : bytes) {
            buf[i] = bt.byteValue();
            i++;
        }

        ByteArrayInputStream bais = new ByteArrayInputStream(buf);

        return bais;

    }
}
