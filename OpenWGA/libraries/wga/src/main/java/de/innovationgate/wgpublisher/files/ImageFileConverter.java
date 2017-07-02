package de.innovationgate.wgpublisher.files;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.imgscalr.Scalr.Rotation;

import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Directory;
import com.drew.metadata.Metadata;
import com.drew.metadata.exif.ExifIFD0Directory;

import de.innovationgate.utils.SimpleImageInfo;
import de.innovationgate.utils.imgscalr.ImgScalrScaler;
import de.innovationgate.webgate.api.WGFileConverter;

/**
 * rotates images based on EXIF tag 'orientation'
 * 
 * @author ws
 *
 */

public class ImageFileConverter implements WGFileConverter {
	
    private String getMimeType(File file) throws IOException{
    	InputStream in = new FileInputStream(file);
    	try{
	        SimpleImageInfo imageInfo = new SimpleImageInfo(in);
	        return imageInfo.getMimeType();
    	}
    	catch(Exception e){
    		return null;
    	}
    	finally {
    		in.close();
    	}
    }
    
	@Override
	public void convert(File file) throws IOException {
		String mimeType = getMimeType(file); 
		if(mimeType==null || !mimeType.startsWith("image/"))
			return;	// only Images are supported
		try {
			Metadata metadata = ImageMetadataReader.readMetadata(file);
		    Directory directory = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
		    if(directory==null || !directory.containsTag(ExifIFD0Directory.TAG_ORIENTATION))
		    	return;
		    int orientation = directory.getInt(ExifIFD0Directory.TAG_ORIENTATION);
		    if(orientation>1){
			    ImgScalrScaler scaler = new ImgScalrScaler();
			    InputStream in = new FileInputStream(file);
	    		scaler.load(in, mimeType);
	    		in.close();
		    	switch (orientation) {
			    	case 3:
			    		scaler.rotate(Rotation.CW_180);
			    		break;
			    	case 6:
			    		scaler.rotate(Rotation.CW_90);
			    		break;
			    	case 8:
			    		scaler.rotate(Rotation.CW_270);
			    		break;
		    	}
	    		scaler.writeImage(file);
		    }
		} catch (Exception e) {
			e.printStackTrace();
		}
	} 
    

}
