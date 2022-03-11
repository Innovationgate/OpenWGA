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

package de.innovationgate.wgpublisher.webtml.utils;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WGAAwareService;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQueryTerm;

/**
 * Service to generate the contents of the "srcset" attribute for HTML tags image and picture.
 * This generates srcset for providing different images for different device pixel ratios. The necessary prerequisite for this is that the image served normally is a derivate.
 */
public class SrcSetCreator implements WGAAwareService {
    
    public static final List<Float> RATIOS = Arrays.asList(new Float[] {2f, 3f, 4f});

    private WGA _wga;
    
    /**
     * Creates a dimension object from the given file metadata, representing the maximum size that a visual representation of this file is available.
     * May be the dimensions of the original file if it is an image.
     * @param content The document holding the file
     * @param md The file metadata of the original file
     * @param Usage of the derivate requested
     * @return The size of the largest visual representation of the given usage or null if no representation was found.
     * @deprecated Use {@link #getMaxAvailableSize(WGFileMetaData, String)} instead: Parameter WGContent has never been used!
     * @throws WGException 
     */
    public Dimension getMaxAvailableSize(WGContent content, WGFileMetaData fileMeta, String usage) throws WGException {
    	return getMaxAvailableSize(fileMeta, usage);
    }

    /**
     * Creates a dimension object from the given file metadata, representing the maximum size that a visual representation of this file is available.
     * May be the dimensions of the original file if it is an image.
     * @param content The document holding the file
     * @param md The file metadata of the original file
     * @param Usage of the derivate requested
     * @return The size of the largest visual representation of the given usage or null if no representation was found.
     * @throws WGException 
     */
    public Dimension getMaxAvailableSize(WGFileMetaData fileMeta, String usage) throws WGException {

        if (fileMeta == null) {
            return null;
        }
        
        // For poster derivates of images: Just use the original
        if (fileMeta.getMimeType() != null && fileMeta.getMimeType().startsWith("image/") && WGFileAnnotations.USAGE_POSTER.equals(usage)) {
            int width = fileMeta.getDisplayWidth();
            int height = fileMeta.getDisplayHeight();
            if (width != -1 && height != -1) {
                return new Dimension(width, height);
            }
            else {
                return null;
            }
        }
        
        // For everthing else: The largest poster derivate is to use (derivate queries without size restriction always return the largest matching file)
        WGFileAnnotations derivateMd = _wga.selectDerivate(fileMeta.getName(), "usage=" + usage);
        if (derivateMd != null) {
            int width = derivateMd.getDisplayWidth();
            int height = derivateMd.getDisplayHeight();
            if (width != -1 && height != -1) {
                return new Dimension(width, height);
            }
        }
        
        // No poster? Hopeless case.
        return null;
            
    }
    
    /**
     * Creates a dimension object from the given file metadata, representing the maximum size that a poster representation of this file is available.
     * May be the dimensions of the original file if it is an image.
     * @param content The document holding the file
     * @param md The file metadata of the original file
     * @deprecated Use {@link #getMaxAvailableSize(WGFileMetaData)} instead: Parameter WGContent has never been used!
     * @throws WGException 
     */
    public Dimension getMaxAvailablePosterSize(WGContent content, WGFileMetaData fileMeta) throws WGException {
        return getMaxAvailableSize(content, fileMeta, WGFileAnnotations.USAGE_POSTER);
    }

    /**
     * Creates a dimension object from the given file metadata, representing the maximum size that a poster representation of this file is available.
     * May be the dimensions of the original file if it is an image.
     * @param content The document holding the file
     * @param md The file metadata of the original file
     * @throws WGException 
     */
    public Dimension getMaxAvailablePosterSize(WGFileMetaData fileMeta) throws WGException {
        return getMaxAvailableSize(fileMeta, WGFileAnnotations.USAGE_POSTER);
    }
    
    /**
     * Create srcset content for the given image URL. A static non-service version of the functionality.
     * @param wga A WGA context object
     * @param fileUrl The file URL
     * @param absoluteUrls Indicates if the generated URLs in the srcset should be absolute
     * @deprecated Use {@link #createSrcSet(WGA, URLBuilder, boolean, Dimension)} instead, so DPRs beyond the original size can be omitted
     * @throws WGException
     */
    public static String createSrcSet(WGA wga, URLBuilder fileUrl, boolean absoluteUrls) throws WGException {        
        return wga.service(SrcSetCreator.class).createSrcSet(fileUrl, absoluteUrls, null);
    }

    public static String createSrcSet(WGA wga, URLBuilder fileUrl, boolean absoluteUrls, Dimension dim) throws WGException {        
        return wga.service(SrcSetCreator.class).createSrcSet(fileUrl, absoluteUrls, dim);
    }

    
    /**
     * Create srcset content for the given image URL.
     * @param fileUrl The file URL
     * @param absoluteUrls Indicates if the generated URLs in the srcset should be absolute
     * @param maxAvailableSize The size of the original image file or the largest poster derivate
     * @throws WGException
     */
    public String createSrcSet(URLBuilder fileUrl, boolean absoluteUrls, Dimension maxAvailableSize) throws WGException {
    	return createSrcSet(fileUrl, absoluteUrls, maxAvailableSize, false);
    }
    public String createSrcSet(URLBuilder fileUrl, boolean absoluteUrls, Dimension maxAvailableSize, boolean imgSet) throws WGException {
        
        try {
            // Pre-parse derivate query
            DerivateQuery derivateQuery = null;
            float widthTermValue = -1;
            float heightTermValue = -1;
            String derivateQueryStr = fileUrl.getParameter(WGPDispatcher.URLPARAM_DERIVATE);
            if (derivateQueryStr != null) {
                derivateQuery = _wga.getCore().getFileDerivateManager().parseDerivateQuery(derivateQueryStr, false);
                
                DerivateQueryTerm widthTerm = derivateQuery.get(DerivateQuery.QUERYTERM_WIDTH);
                if (widthTerm != null) {
                    widthTermValue = Integer.parseInt(widthTerm.getValue());
                }
                
                DerivateQueryTerm heightTerm = derivateQuery.get(DerivateQuery.QUERYTERM_HEIGHT);
                if (heightTerm != null) {
                    heightTermValue = Integer.parseInt(heightTerm.getValue());
                }
            }
            
            // Pre-parse online scaling values
            int maxHeight = -1;
            String maxHeightStr = fileUrl.getParameter(WGPDispatcher.URLPARAM_MAXHEIGHT);
            if (maxHeightStr != null) {
                maxHeight = Integer.parseInt(maxHeightStr);
            }
            
            int maxWidth = -1;
            String maxWidthStr = fileUrl.getParameter(WGPDispatcher.URLPARAM_MAXWIDTH);
            if (maxWidthStr != null) {
                maxWidth = Integer.parseInt(maxWidthStr);
            }

            if (widthTermValue == -1 && heightTermValue == -1 && maxHeight == -1 && maxWidth == -1) {
                // No scaling specification? No srcset: #00005288.
                return "";
            }

            
            List<String> sources = new ArrayList<String>();
            
            // Create the srcset based on predefined ratios
            for (Float ratio : RATIOS) {
                
                URLBuilder srcUrl = (URLBuilder) fileUrl.clone();
                
                if (derivateQuery != null) { 
                    
                    // Omit all ratios where the requested dimensions exceed those of the original (#00003663)
                    if (maxAvailableSize != null) {
                        if (widthTermValue != -1 && (widthTermValue * ratio) > maxAvailableSize.getWidth()) {
                            break;
                        }
                        
                        if (heightTermValue != -1 && (heightTermValue * ratio) > maxAvailableSize.getHeight()) {
                            break;
                        }
                    }
                    
                    derivateQuery.addTerm(DerivateQuery.QUERYTERM_DEVICEPIXELRATIO, "=", String.valueOf(ratio));
                    srcUrl.setParameter(WGPDispatcher.URLPARAM_DERIVATE, derivateQuery.toString());
                }
                
                if (maxHeight != -1) {
                    float ratioHeight = maxHeight * ratio;
                    if (maxAvailableSize != null && ratioHeight > maxAvailableSize.getHeight()) { // Omit all ratios where the requested dimensions exceed those of the original (#00003663)
                        break;
                    }
                    srcUrl.setParameter(WGPDispatcher.URLPARAM_MAXHEIGHT, WGUtils.DECIMALFORMAT_SYSTEM.format(ratioHeight));                
                }
                
                if (maxWidth != -1) {
                    float ratioWidth = maxWidth * ratio;
                    if (maxAvailableSize != null && ratioWidth > maxAvailableSize.getWidth()) { // Omit all ratios where the requested dimensions exceed those of the original (#00003663)
                        break;
                    }
                    srcUrl.setParameter(WGPDispatcher.URLPARAM_MAXWIDTH, WGUtils.DECIMALFORMAT_SYSTEM.format(ratioWidth));                
                }
                
                String url = absoluteUrls ? srcUrl.build(true) : srcUrl.buildLikeGiven();
                if(imgSet)
                	url = "url('" + url + "')";
                sources.add(url + " " + ratio + "x");
            }
            
            return WGUtils.serializeCollection(sources, ", ");
        }
        catch (Exception e) {
            throw new WGException("Exception creating srcset attribute", e);
        }
        
        
    }

    
    @Override
    public void injectWGA(WGA wga) {
        _wga = wga;
    }

    
    /**
     * Create srcset content for the given image URL
     * @param fileUrl A URLBuilder containing the file URL
     * @param absoluteUrls Indicates if the generated URLs in the srcset should be absolute
     * @deprecated Use {@link #createSrcSet(URLBuilder, boolean, Dimension)} instead, so DPRs beyond the original size can be omitted
     * @throws WGException
     */
    public String createSrcSet(URLBuilder fileUrl, boolean absoluteUrls) throws WGException {
        return createSrcSet(fileUrl, absoluteUrls);
    }
    
    /**
     * Create srcset content for the given image URL
     * @param fileUrl The file URL
     * @param absoluteUrls Indicates if the generated URLs in the srcset should be absolute
     * @deprecated Use {@link #createSrcSet(String, boolean, Dimension)} instead, so DPRs beyond the original size can be omitted
     * @throws WGException
     */
    public String createSrcSet(String fileUrl, boolean absoluteUrls) throws WGException {
        return createSrcSet(_wga.urlBuilder(fileUrl), absoluteUrls);
    }
    
    /**
     * Create srcset content for the given image URL
     * @param fileUrl The file URL
     * @param absoluteUrls Indicates if the generated URLs in the srcset should be absolute
     * @param maxAvailableSize The size of the original image file or the largest poster derivate
     * @throws WGException
     */
    public String createSrcSet(String fileUrl, boolean absoluteUrls, Dimension maxAvailableSize) throws WGException {
        return createSrcSet(_wga.urlBuilder(fileUrl), absoluteUrls, maxAvailableSize);
    }
    
    /**
     * Create srcset content for the given image URL
     * @param fileUrl The file URL
     * @deprecated Use {@link #createSrcSet(String, Dimension)} instead, so DPRs beyond the original size can be omitted
     * @throws WGException
     */
    public String createSrcSet(String fileUrl) throws WGException {
        return createSrcSet(_wga.urlBuilder(fileUrl), false);
    }
    
    /**
     * Create srcset content for the given image URL
     * @param fileUrl The file URL
     * @param maxAvailableSize The size of the original image file or the largest poster derivate
     * @throws WGException
     */
    public String createSrcSet(String fileUrl, Dimension maxAvailableSize) throws WGException {
        return createSrcSet(_wga.urlBuilder(fileUrl), false, maxAvailableSize);
    }
    
    /**
     * Static private method to create img-set or src-set for the given image
     * @param ctx the TMLContext
     * @param filename The file name
     * @param derivateQuery the derivate query to be used
     * @throws WGException
     */
    private static String createSrcSet(TMLContext ctx, String filename, String derivateQuery, boolean imgSet) throws WGException {
    	WGA wga = WGA.get(ctx);
    	SrcSetCreator ssc = wga.service(SrcSetCreator.class);
    	WGFileMetaData fileMeta = ctx.content().getFileMetaData(filename);
    	Dimension maxAvailableSize = ssc.getMaxAvailablePosterSize(fileMeta);
    	URLBuilder builder = wga.urlBuilder(ctx.fileurl(filename));
    	builder.setParameter(WGPDispatcher.URLPARAM_DERIVATE, derivateQuery);
        return ssc.createSrcSet(builder, false, maxAvailableSize, imgSet);
    }

    /**
     * Static method to create src-set for the given image
     * @param ctx the TMLContext
     * @param filename The file name
     * @param derivateQuery the derivate query to be used
     * @throws WGException
     */
    public static String createSrcSet(TMLContext ctx, String filename, String derivateQuery) throws WGException {
    	return createSrcSet(ctx, filename, derivateQuery, false);
    }

    /**
     * Static method to create img-set for the given image
     * @param ctx the TMLContext
     * @param filename The file name
     * @param derivateQuery the derivate query to be used
     * @throws WGException
     */
    public static String createImgSet(TMLContext ctx, String filename, String derivateQuery) throws WGException {
    	return createSrcSet(ctx, filename, derivateQuery, true);
    }
    
}
