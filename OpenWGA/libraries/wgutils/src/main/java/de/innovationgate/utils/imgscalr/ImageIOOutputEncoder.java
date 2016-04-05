/*******************************************************************************
 * Copyright 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.utils.imgscalr;

import java.awt.image.RenderedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.Iterator;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.MemoryCacheImageOutputStream;


public class ImageIOOutputEncoder {
    
    private ImageWriteParam _imageWriteParam;
    private String _mimeType;
    private ImageWriter _writer;
    
    public static ImageIOOutputEncoder getForMimeType(String mimeType) {
        try {
            return new ImageIOOutputEncoder(mimeType);
        }
        catch (IOException e) {
            return null;
        }
    }

    private ImageIOOutputEncoder(String mimeType) throws IOException {
        _mimeType = mimeType;
        fetchWriter();
        _imageWriteParam = _writer.getDefaultWriteParam();
    }

    public void encode(RenderedImage img, OutputStream out) throws IOException {

        fetchWriter();
        try {
            _writer.setOutput(new MemoryCacheImageOutputStream(out));
            _writer.write(null, new IIOImage(img, null, null), _imageWriteParam);
        }
        finally {
            _writer.dispose();
            _writer = null;
        }
    }

    private void fetchWriter() throws IOException {
        Iterator<ImageWriter> writers = ImageIO.getImageWritersByMIMEType(_mimeType);
        if (!writers.hasNext()) {
            throw new IOException("No image write availailable for MIME Type" + _mimeType);
        }
        
        _writer = writers.next();
    }

    public Object getImageEncodeParam() {
        return _imageWriteParam;
    }

    public void setImageEncodeParam(Object param) {
        if (param instanceof ImageWriteParam) {
            _imageWriteParam = (ImageWriteParam) param;
        }
        else {
            throw new InvalidParameterException("Image encode param must be of type " + ImageWriteParam.class.getName());
        }
        
    }
    
    public String getFormat() {
        return _mimeType.substring(_mimeType.indexOf("/") +1).toUpperCase();
    }

    public float getCompressionQuality() {
        return _imageWriteParam.getCompressionQuality();
    }

    public void setCompressionQuality(float quality) {
        _imageWriteParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
        _imageWriteParam.setCompressionQuality(quality);
    }
    
    public boolean isCompressionSupported() {
        
        try {
            _imageWriteParam.getCompressionMode();
            return true;
        }
        catch (UnsupportedOperationException e) {
            return false;
        }
        
    }

    public boolean isTransparencySupported() {
        return true;
    }
    
}
