package baliadapter.util;

import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

public class BaliHTTPComm {

    private String baseUrl;

    private String userName;

    private String password;

    private String cookie;

    public BaliHTTPComm(String baseUrl, String userName, String password) {
        this.baseUrl = baseUrl;
        this.userName = userName;
        this.password = password;
        this.cookie = "";
    }

    public InputStream doGetInputStream(String path) throws Exception {

        URL url = new URL(baseUrl + path);
        HttpURLConnection hpcon = (HttpURLConnection)url.openConnection();
        hpcon.setRequestMethod("GET");
        hpcon.setRequestProperty("Cookie", cookie);
        hpcon.setRequestProperty("Connection", "close");
        hpcon.setInstanceFollowRedirects(false);
        hpcon.getResponseCode();

        if (expiredCookie(hpcon)) {
            login(baseUrl, userName, password);
            if (cookie != null) {
                return doGetInputStream(path);
            } else {
                throw new Exception();
            }
        }

        return hpcon.getInputStream();
    }

    public InputStream doPostInputStream(String path, String data)
    throws Exception {

        URL url = new URL(baseUrl + path);
        String encoded = encodeParam(data);
        HttpURLConnection hpcon = (HttpURLConnection)url.openConnection();
        hpcon.setRequestMethod("POST");
        hpcon.setRequestProperty("Content-Length",
                Integer.toString(encoded.getBytes().length));
        hpcon.setRequestProperty("Content-Type", "text/xml");
        hpcon.setRequestProperty("Cookie", cookie);
        hpcon.setRequestProperty("Connection", "close");
        hpcon.setInstanceFollowRedirects(false);
        hpcon.setDoInput(true);
        hpcon.setDoOutput(true);
        hpcon.setUseCaches(false);

        DataOutputStream printout = new DataOutputStream(
                hpcon.getOutputStream());
        printout.writeBytes(encoded);
        printout.flush();
        printout.close();

        if (expiredCookie(hpcon)) {
            login(baseUrl, userName, password);
            if (cookie != null) {
                return doPostInputStream(path, data);
            } else {
                throw new Exception();
            }
        }

        return hpcon.getInputStream();
    }

    public static String encodeParam(String param) throws Exception {
        try {
            return URLEncoder.encode(param, "UTF-8");
        } catch (UnsupportedEncodingException uee) {
            throw new Exception(uee);
        }
    }
    
    private boolean expiredCookie(HttpURLConnection hpcon) {
        return (hpcon.getHeaderField("Location") != null);
    }

    private boolean login(String baseUrl, String user, String password) {
        HttpURLConnection hpcon = null;

        try {
            URL url = new URL(baseUrl + "/login/login_post.yaws");
            String data = "user=" + encodeParam(user) +
            "&passwd=" + encodeParam(password);

            hpcon = (HttpURLConnection)url.openConnection();
            hpcon.setRequestMethod("POST");
            hpcon.setRequestProperty("Content-Type", "text/html");
            hpcon.setDoInput(true);
            hpcon.setDoOutput(true);
            hpcon.setInstanceFollowRedirects(false);

            DataOutputStream printout = new DataOutputStream(hpcon.getOutputStream());
            printout.writeBytes(data);
            printout.flush();
            printout.close();

            cookie = hpcon.getHeaderField("Set-Cookie");

            if (cookie != null) {
                return true;
            }

            return false;

        } catch (Exception e){
            return false;
        } finally {
            if(hpcon != null) {
                hpcon.disconnect();
            }
        }
    }

}
