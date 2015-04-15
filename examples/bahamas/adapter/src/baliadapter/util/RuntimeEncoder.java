package baliadapter.util;

public class RuntimeEncoder {

    public static Long encodeRuntime(String runTime) {
        Long result = new Long(0);

        try {
            int index = runTime.indexOf(':');
            String hoursString = runTime.substring(0, index);
            String remaind = runTime.substring(index + 1);

            index = remaind.indexOf(':');
            String minutesString = remaind.substring(0, index);
            String secondsString  = remaind.substring(index +1);

            int hours = Integer.valueOf(hoursString).intValue();
            int minutes = Integer.valueOf(minutesString).intValue();
            int seconds = Integer.valueOf(secondsString).intValue();

            result = new Long(seconds * 1000 + minutes * 60000 + hours * 3600000);
        } catch (Exception e) {}

        return result;
    }
    
    public static String decodeRuntime(Long runTime) {
        long milliseconds = runTime.longValue();

        int seconds = (int)(milliseconds/1000) % 60;
        int minutes = (int)(milliseconds/60000) % 60;
        int hours = (int)(milliseconds/3600000);

        return hours +
                (minutes < 10 ? ":0" + minutes : ":" + minutes) +
                (seconds < 10 ? ":0" + seconds : ":" + seconds);
    }
    
}
