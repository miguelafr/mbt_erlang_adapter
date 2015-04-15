package baliadapter.custom.codec;


public class AssetTransfer {

    private String assetId;
    
    private String creationDate;
    
    private String title;
    
    private String date;
    
    private String summary;
    
    private String rating;
    
    private String genre;
    
    private String runTime;

    public AssetTransfer(String assetId, String creationDate, String title,
            String date, String summary, String rating, String genre,
            String runTime) {
        this.assetId = assetId;
        this.creationDate = creationDate;
        this.title = title;
        this.date = date;
        this.summary = summary;
        this.rating = rating;
        this.genre = genre;
        this.runTime = runTime;
    }

    public String getAssetId() {
        return assetId;
    }

    public String getCreationDate() {
        return creationDate;
    }

    public String getTitle() {
        return title;
    }

    public String getDate() {
        return date;
    }

    public String getSummary() {
        return summary;
    }

    public String getRating() {
        return rating;
    }

    public String getGenre() {
        return genre;
    }

    public String getRunTime() {
        return runTime;
    }
    
}
