package de.innovationgate.utils.imgscalr;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import org.imgscalr.Scalr;
import org.imgscalr.Scalr.Method;
import org.imgscalr.Scalr.Mode;
import org.imgscalr.Scalr.Rotation;

import de.innovationgate.utils.CountBytesOutputStream;
import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.MimeTypeSpecificImageScaler;

public class ImgScalrScaler implements ImageScaler, MimeTypeSpecificImageScaler {
    
    class Scale {
        
        public Scale() {
            
        }
        
        public Scale(int h, int v) {
            _horizontal = h;
            _vertical = v;
        }
        
        private int _horizontal = 100;
        private int _vertical = 100;
        /**
         * @return Returns the horizontal.
         */
        public int getHorizontal() {
            return _horizontal;
        }
        /**
         * @param horizontal The horizontal to set.
         */
        public void setHorizontal(int horizontal) {
            _horizontal = horizontal;
        }
        /**
         * @return Returns the vertical.
         */
        public int getVertical() {
            return _vertical;
        }
        /**
         * @param vertical The vertical to set.
         */
        public void setVertical(int vertical) {
            _vertical = vertical;
        }
        
        public void setHorizontal(float horizontal) {
            _horizontal = (int) (horizontal * 100);
        }
        
        public void setVertical(float vertical) {
            _vertical = (int) (vertical * 100);
        }
        
    }
    
    public static final String SYSPROPERTY_METHOD = "de.innovationgate.wga.imgscalr.method";

    public static final List<String> SUPPORTED_MIMETYPES = Arrays.asList(new String[] {"image/jpg", "image/jpeg", "image/png", "image/bmp", "image/wbmp"}); 

    private Method _method = Method.QUALITY;
    private BufferedImage _img;
    private ImageIOOutputEncoder _encoder;
    private String _sourceFileName = null;
    
    public ImgScalrScaler() {
        String method = System.getProperty(SYSPROPERTY_METHOD);
        if (method != null) {
            _method = Method.valueOf(method);
        }
    }

    public void load(InputStream is) throws IOException {

        ImageInputStream imageIn = ImageIO.createImageInputStream(is);
        ImageReader imageReader = null;
        try {
            Iterator<ImageReader> imageReaders = ImageIO.getImageReaders(imageIn);
            if (!imageReaders.hasNext()) {
                throw new IOException("No ImageIO readers available to read given image data");
            }
            
            imageReader = imageReaders.next();
            imageReader.setInput(imageIn);
            _img = (BufferedImage) imageReader.readAsRenderedImage(imageReader.getMinIndex(), null);
        }
        finally {
            if (imageReader != null) {
                imageReader.dispose();
            }
            if (imageIn != null) {
                imageIn.close();
            }
        }

    }

    public void useJPEGForOutput() {
        _encoder = ImageIOOutputEncoder.getForMimeType("image/jpeg");
    }

    public void usePNGForOutput() {
        _encoder = ImageIOOutputEncoder.getForMimeType("image/png");
    }

    public void scaleToSize(int width, int height) {

        BufferedImage image = _img;
        if (width == -1) {
            _img = Scalr.resize(image, _method, Mode.FIT_TO_HEIGHT, height);
        }
        else if (height == -1) {
            _img = Scalr.resize(image, _method, Mode.FIT_TO_WIDTH, width);
        }
        else {
            _img = Scalr.resize(image, _method, Mode.FIT_EXACT, width, height);
        }
        
        image.flush();
     
    }

    public void rotate(Rotation r){
    	_img = Scalr.rotate(_img, r);
    }
    
    public void scaleToSize(int width, int height, boolean keepRatio) {
        
        if (keepRatio && width != -1 && height != -1) {
            if (getAspectRatio() > 1) {
                // Landscape -- scale to width
                scaleToSize(width, -1);
            } else {
                // Portrait -- scale to height
                scaleToSize(-1, height);    
            }
        } else {
            scaleToSize(width, height);
        }

    }
    
    /**
     * Returns the aspect ratio (width/height) of the image
     */ 
    public float getAspectRatio() {
            return (float)getWidth() / (float)getHeight();
    }

    public void scaleByFactor(int hfactor, int vfactor) {
        scaleToSize(getWidth() * hfactor, getHeight() * vfactor);

    }

    public void shrinkToSize(int width, int height) {

        if (width >= _img.getWidth() && height >= _img.getHeight()) {
            return;
        }
        
        // Determine ratios for width and height
        float widthRatio = (float) width / (float) _img.getWidth();
        float heightRatio = (float) height / (float) _img.getHeight();
        
        Scale scale = new Scale();
        
        // The smallest ratio determines the real scaling. The other value is then shrinked with the same ratio
        if (widthRatio < heightRatio) {
            scale.setHorizontal(width);
            scale.setVertical(calculateRatio(_img.getHeight(), _img.getWidth(), width));
        }
        else {
            scale.setVertical(height);
            scale.setHorizontal(calculateRatio(_img.getWidth(), _img.getHeight(), height));
        }
        
        BufferedImage image = _img;
        _img = Scalr.resize(image, _method, Mode.AUTOMATIC, scale.getHorizontal(), scale.getVertical());
        image.flush();

    }

    public void growToSize(int width, int height) {

        if (width <= _img.getWidth() && height <= _img.getHeight()) {
            return;
        }
        
        BufferedImage image = _img;
        _img = Scalr.resize(image, _method, Mode.AUTOMATIC, width, height);
        image.flush();
        

    }

    public void writeImage(OutputStream out) {
        try {
            _encoder.encode(_img, out);
        }
        catch (Exception e) {
            throw new RuntimeException("Exception encoding image", e);
        }
    }

    public void writeImage(File outFile) throws IOException {
        FileOutputStream out = new FileOutputStream(outFile);
        writeImage(out);
        out.flush();
        out.close();
    }

    public String getFormat() {
        // TODO Auto-generated method stub
        return null;
    }

    public void setFormat(String format) {
        // TODO Auto-generated method stub

    }

    public Object getImageEncodeParam() {
        return _encoder.getImageEncodeParam();
    }

    public void setImageEncodeParam(Object imageEncodeParam) {
        _encoder.setImageEncodeParam(imageEncodeParam);
    }

    public float getQuality() {
        return _encoder.getCompressionQuality();
    }

    public void setQuality(float q) {
        _encoder.setCompressionQuality(q);

    }

    public int getWidth() {
        return _img.getWidth();
    }

    public int getHeight() {
        return _img.getHeight();
    }

    public long determineRenderedSize() {
        CountBytesOutputStream out = new CountBytesOutputStream();
        writeImage(out);
        return out.getSize();
    }

    public boolean isTransparencySupportedForOutput() {
        return _encoder.isTransparencySupported();
    }

    public String getFormatSuffix() {
        if (_encoder.getFormat().equals("JPEG")) {
            return ".jpg";
        }
        else if (_encoder.getFormat().equals("PNG")) {
            return ".png";
        }
        else {
            return "." + _encoder.getFormat().toLowerCase();
        }
    }

    public String getSourceFileName() {
        return _sourceFileName;
    }

    public void setSourceFileName(String name) {
        _sourceFileName = name;
    }

    public void load(InputStream is, String mimeType) throws IllegalArgumentException, IOException {
        
        load(is);
        
        ImageIOOutputEncoder encoder = ImageIOOutputEncoder.getForMimeType(mimeType);
        if (encoder != null) {
            _encoder = encoder;
        }
        else {
            throw new IllegalArgumentException("Mimetype not supported: " + mimeType);
        }
        
    }

    public void crop(int x, int y, int width, int height) throws UnsupportedOperationException, IOException {

        BufferedImage image = _img;
        _img = Scalr.crop(image, x, y, width, height);
        image.flush();
        
    }

    private int calculateRatio(float sourceA, float sourceB, float targetB) {
        
        int ratio = Math.round(sourceA * (targetB / sourceB));
        if (ratio == 0) {
            ratio = 1;
        }
        return ratio;
        
    }

}
