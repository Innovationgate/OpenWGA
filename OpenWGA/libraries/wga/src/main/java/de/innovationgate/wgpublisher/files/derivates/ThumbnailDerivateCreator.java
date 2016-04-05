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

package de.innovationgate.wgpublisher.files.derivates;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.MimeTypeSpecificImageScaler;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.webtml.utils.ImageScalerFactory;

public class ThumbnailDerivateCreator implements FileDerivateCreator {
    
    public static final String DBATTRIB_THUMBNAIL_SIZES = "FileDerivates.ThumbnailDerivateCreator.Sizes";
    
    public static class ThumbnailSize extends DerivateInfo {
        private int _width = -1;
        private int _height = -1;
        
        public ThumbnailSize(int widthParam, int heightParam) {
            super("thumbnail" + String.valueOf(widthParam) + "x" + String.valueOf(heightParam), WGFileAnnotations.USAGE_POSTER);
            _width = widthParam;
            _height = heightParam;
        }

        protected int getWidth() {
            return _width;
        }

        protected int getHeight() {
            return _height;
        }
    }
    //100, 320, 768 und 1024
    public static final String DEFAULT_THUMBNAIL_SIZES = "2048x-1,1024x-1,768x-1,320x-1,100x-1";
    
    public static final Set<String> SUPPORTED_MIMETYPES = Collections.unmodifiableSet(new HashSet<String>(Arrays.asList(new String[] {"image/jpeg", "image/jpg", "image/png"})));
   
    @Override
    public Set<DerivateInfo> getDerivateInfos(WGA wga, WGContent content, WGFileMetaData md) throws WGException {
        
        if (!SUPPORTED_MIMETYPES.contains(md.getMimeType())) {
            return null;
        }
        
        Set<ThumbnailSize> thumbnailSizes = new HashSet<ThumbnailDerivateCreator.ThumbnailSize>();
        List<String> configuredSizes = (List<String>) wga.app(content.getDatabase()).getPublisherOption(DBATTRIB_THUMBNAIL_SIZES);
        for (String configuredSizeStr : configuredSizes) {
            try {
                int xPos = configuredSizeStr.indexOf("x");
                if (xPos == -1) {
                    throw new IllegalArgumentException("Thumbnail size string must be of format <width>x<height>");
                }
                int width = Integer.parseInt(configuredSizeStr.substring(0, xPos));
                int height = Integer.parseInt(configuredSizeStr.substring(xPos + 1));
                thumbnailSizes.add(new ThumbnailSize(width, height));
                
            }
            catch (Throwable e) {
                FileDerivateManager.LOG.error("Exception parsing configured thumbnail size '" + configuredSizeStr + "'", e);
            }
            
        }
        
        Set<DerivateInfo> infos = new HashSet<DerivateInfo>();
        for (ThumbnailSize size : thumbnailSizes) {
            if ((size.getHeight() != -1 && size.getHeight() < md.getDisplayHeight()) || (size.getWidth() != -1 && size.getWidth() < md.getDisplayWidth())) {
                infos.add(size);
            }
        }
        
        return infos;
    }

    @Override
    public void createDerivate(WGA wga, WGContent content, WGFileMetaData md, DerivateInfo derivateInfo, OutputStream out) throws WGException {

        ThumbnailSize thumbnailSize = (ThumbnailSize) derivateInfo;        
        FileDerivateManager.LOG.debug("Creating thumbnail derivate " + thumbnailSize.getWidth() + "x" + thumbnailSize.getHeight() + " for file '" + md.getName() + "' on document '" + content.getDocumentKey() + "' (" + content.getDatabase().getDbReference() + ")");
        
        try {
            MimeTypeSpecificImageScaler scaler = ImageScalerFactory.createMimeTypeSpecificImageScaler(wga.getCore(), md.getMimeType());
            InputStream in = content.getFileData(md.getName());
            scaler.load(in, md.getMimeType());
            in.close();
            scaler.scaleToSize(thumbnailSize.getWidth(), thumbnailSize.getHeight(), true);
            scaler.writeImage(out);
            out.flush();
        }
        catch (Exception e) {
            throw new WGException("Exception creating thumbnail derivate", e);
        }
        
    }

}
