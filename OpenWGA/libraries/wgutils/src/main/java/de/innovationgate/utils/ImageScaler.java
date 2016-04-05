/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
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

package de.innovationgate.utils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import de.innovationgate.wga.common.CodeCompletion;

/**
 * Interface for classes in WGA that provide format neutral image scaling functions.
 */
@CodeCompletion(beanMode=CodeCompletion.BEAN_MODE_ALL)
public interface ImageScaler {
    
    /**
     * Loads image data.
     * @param is image data input stream.
     * @throws IOException
     */
    public void load(InputStream is) throws IOException;

    /**
     * Sets the scaler to use JPEG images for output with 90% quality.
     */
    public abstract void useJPEGForOutput();

    /**
     * Sets the scaler to use PNG images (with color palette) for output,  
     */
    public abstract void usePNGForOutput();

    /**
     * Scales the image to a determined target size
     *  
     * This method uses the parameter width and height as target sizes for the scaled image.
     * Images may get larger if target sizes exceed the source sizes. If either width or height is specified as -1, it is
     * automatically calculated to keep the ratio of the picture. If both sizes are -1 the image will keep its size.
     * 
     * @param width The target width. Use -1 to automatically calculate target width for keeping the image ratio
     * @param height The target height. Use -1 to automatically calculate target height for keeping the image ratio
     * 
     */
    public abstract void scaleToSize(int width, int height);

    /**
     * Scales the image to the determined target size
     * if keepRation==true
     *  image orientation is determined (landscape or portrait) 
     *  on landscape the image is scaled with the same ratio to the given width
     *  on portrait the image is scaled with the same ratio to the given height
     *    
     * @param width The target width
     * @param height The target height
     * @param keepRatio true/ false
     */
    public abstract void scaleToSize(int width, int height, boolean keepRatio);

    /**
     * Scales the image based on the given scaling factors. A scaling factor of 1 keeps the size. Scaling factors larger than
     * 1 make the image larger, factors smaller than 1 make it smaller. The amount of scaling is represented by the comparison 
     * between the factor and 1. A factor of 2 will double the image size. A factor of 0.5 will make it half size.
     * 
     * @param hfactor The horizontal scaling factor
     * @param vfactor The vertical scaling factor
     */
    public abstract void scaleByFactor(int hfactor, int vfactor);

    /**
     * Scales the images down to fit into given maximum sizes.
     * 
     * This method uses the parameters width and height as "maximum sizes" for the picture. It will determine if any
     * dimension of the source image exceeds the given maximum size and scale it down if so. The ratio of the picture is 
     * always preserved as the smaller of the neccessary scale factors (for width and height) is used for both dimensions.
     * If the image does not exceed any of the given maximum sizes, it will not be scaled.
     * 
     * @param width The maximum width
     * @param height The maximum height
     */
    public abstract void shrinkToSize(int width, int height);

    /**
     * Scales the images up to fit given minimum sizes.
     * 
     * This method uses the parameters width and height as "minimum sizes" for the picture. It will determine if any
     * dimension of the source image is lower than the given minimum size and scale it up if so. The ratio of the picture is 
     * always preserved as the larger of the neccessary scale factors (for width and height) is used for both dimensions.
     * If the image is already as large or larget than the given minimum sizes, it will not be scaled.
     * 
     * @param width The maximum width
     * @param height The maximum height
     */
    public abstract void growToSize(int width, int height);

    /**
     * Writes the rendered and scaled image to an outputstream
     * @param out
     */
    public abstract void writeImage(OutputStream out);

    /**
     * Writes the rendered and scaled image to a file.
     * @param outFile
     * @throws IOException if the file cannot be written
     */
    public abstract void writeImage(File outFile) throws IOException;

    /**
     * Returns the output format.
     */
    public abstract String getFormat();

    /**
     * Sets the output format
     * @param format The format to set.
     */
    public abstract void setFormat(String format);

    /**
     * Returns the image encoding params.
     */
    public abstract Object getImageEncodeParam();

    /**
     * Sets the image encoding params
     * @param imageEncodeParam The imageEncodeParam to set.
     */
    public abstract void setImageEncodeParam(Object imageEncodeParam);





    /**
     * Returns the quality of the output format.
     * This is a value between 0 and 1 for JPEG encoding (since this is a lossy format)
     * and 1 for all other formats who are lossless.
     */
    public abstract float getQuality();

    /**
     * Sets the quality of the output format.
     * This only applies to JPEG output and can be set to a value between something above 0 
     * and 1, where higher means better quality.
     * @param q
     */
    public abstract void setQuality(float q);

    /**
     * Returns the current image width
     */
    public abstract int getWidth();

    /**
     * Returns the current image height
     */
    public abstract int getHeight();

    /**
     * Determines the bytes size of the image if it would be written to an output stream.
     */
    public abstract long determineRenderedSize();

    /**
     * Returns if transparency is supported by the current output format
     */
    public abstract boolean isTransparencySupportedForOutput();
    
    /**
     * Returns the normale file suffix for the current output format, including leading "."
     */
    public String getFormatSuffix();
    
    /**
     * Returns the name of the sourcefile
     */
    public String getSourceFileName();
    
    /**
     * Sets the name of the sourcefile
     */
    public void setSourceFileName(String name);

}
