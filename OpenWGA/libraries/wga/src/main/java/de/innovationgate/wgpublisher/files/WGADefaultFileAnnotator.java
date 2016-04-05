/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wgpublisher.files;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import javax.activation.DataSource;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import org.apache.log4j.Logger;

import de.innovationgate.utils.SimpleImageInfo;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileAnnotator;

public class WGADefaultFileAnnotator implements WGFileAnnotator {

    @Override
    public void annotateFile(DataSource originalFileData, WGFileAnnotations fileAnnotations) throws WGAPIException {
        
        String mimeType = null;
        int displayWidth = -1;
        int displayHeight = -1;
        
        // Image data: First try with SimpleImageInfo
        InputStream in = null;
        try {
            in = originalFileData.getInputStream();
            SimpleImageInfo imageInfo = new SimpleImageInfo(in);
            displayWidth = imageInfo.getWidth();
            displayHeight = imageInfo.getHeight();
            mimeType = imageInfo.getMimeType();
        }
        catch (IOException e) {
            // Fail silently if the image type cannot be used with SimpleImageInfo
        }
        finally {
            if (in != null) {
                try {
                    in.close();
                }
                catch (Throwable e) {}
            }
        }

        // Image data: Second try with ImageIO
        if (mimeType == null || displayWidth == -1 || displayHeight == -1) {
            in = null;
            try {
                in = originalFileData.getInputStream();
                ImageInputStream imageIn = ImageIO.createImageInputStream(in);
                if (imageIn != null) {
                    Iterator<ImageReader> readers = ImageIO.getImageReaders(imageIn);
                    if (readers.hasNext()) {
                        ImageReader reader = readers.next();
                        try {
                            reader.setInput(imageIn);
                            displayWidth = reader.getWidth(reader.getMinIndex());
                            displayHeight = reader.getHeight(reader.getMinIndex());
                            
                            if (mimeType == null) {
                                // Tricky ImageIO mime type determination:
                                // If the format name of the reader equals the first format name of the SPI => default implementation => the SPI only serves one MIME type
                                // => we can use the first best mime type returned by the SPI
                                String formatName = reader.getFormatName();
                                if (formatName.equals(reader.getOriginatingProvider().getFormatNames()[0])) {
                                    mimeType = reader.getOriginatingProvider().getMIMETypes()[0];
                                }
                            }
                        }
                        finally {
                            reader.dispose();
                        }
                    }
                }
            }
            catch (IOException e) {
                Logger.getLogger("wga.annotator").error("Exception annotating file in WGA default file annotator", e);
            }
            finally {
                if (in != null) {
                    try {
                        in.close();
                    }
                    catch (Throwable e) {}
                }
            }
        }
        
        // MIME Type - If not yet determined by image processing we try other ways
        if (mimeType == null) {
            String name = originalFileData.getName();
            if (name != null) {
                mimeType = WGFactory.getMimetypeDeterminationService().determineByFilename(name);
            }
        }
        
        // Transfer filled data to the annotations
        if (mimeType != null) {
            fileAnnotations.setMimeType(mimeType);
        }
        if (displayHeight != -1) {
            fileAnnotations.setDisplayHeight(displayHeight);
        }
        if (displayWidth != -1) {
            fileAnnotations.setDisplayWidth(displayWidth);
        }
        

    }

    @Override
    public int getOrderNr() {
        return 0;
    } 
    

    

}
