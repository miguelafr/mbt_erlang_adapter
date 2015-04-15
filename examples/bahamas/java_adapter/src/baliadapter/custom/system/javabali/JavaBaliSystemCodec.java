package baliadapter.custom.system.javabali;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import baliadapter.custom.codec.AssetTransfer;
import baliadapter.util.RuntimeEncoder;

import com.lambdastream.bali.asset.adi.Asset;
import com.lambdastream.bali.asset.adi.TitleAsset;

public class JavaBaliSystemCodec {

    public JavaBaliSystemCodec(InputStream inputStream) {
        ;
    }

    public static AssetTransfer getAssetTransfer(Asset asset)
    throws Exception {
        return new AssetTransfer(asset.getAssetID(),
                    asset.getCreationDate().toString(),
                    asset.getTitleAsset().getTitle(),
                    asset.getTitleAsset().getDate(),
                    asset.getTitleAsset().getSummary(),
                    asset.getTitleAsset().getRating(),
                    asset.getTitleAsset().getGenre(),
                    RuntimeEncoder.decodeRuntime(asset.getTitleAsset().getRunTime()));
    }
    
    public static Collection<AssetTransfer> getAssetTransfers(
            Collection<Asset> assets) throws Exception {
        Collection<AssetTransfer> result = new ArrayList<AssetTransfer>();
        for(Asset asset : assets) {
            result.add(getAssetTransfer(asset));
        }
        return result;
    }

    public static Asset getAsset(AssetTransfer asset)
    throws Exception {
        Asset result = new Asset();
        result.setAssetID(asset.getAssetId());
        result.setCreationDate(new Long(asset.getCreationDate()));
        
        TitleAsset titleAsset = new TitleAsset();
        titleAsset.setTitle(asset.getTitle());
        titleAsset.setDate(asset.getDate());
        titleAsset.setSummary(asset.getSummary());
        titleAsset.setRating(asset.getRating());
        titleAsset.setGenre(asset.getGenre());
        titleAsset.setRunTime(RuntimeEncoder.encodeRuntime(asset.getRunTime()));
        
        result.setTitleAsset(titleAsset);
        
        return result;
    }
    
    public static Collection<Asset> getAssets(
            Collection<AssetTransfer> assets) throws Exception {
        Collection<Asset> result = new ArrayList<Asset>();
        for(AssetTransfer asset : assets) {
            result.add(getAsset(asset));
        }
        return result;
    }

}

