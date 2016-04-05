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
 * An image scaler for a specific mime type. Produces the same output mimetype as for input.
 */
@CodeCompletion(beanMode=CodeCompletion.BEAN_MODE_ALL)
public interface MimeTypeSpecificImageScaler {
    
    /**
     * Loads image data.
     * @param is image data input stream.
     * @param mimeType of the loaded image
     * @throws IllegalArgumentException if the mimetype is not supported
     * @throws IOException if something goes wrong on loading
     */
    public void load(InputStream is, String mimeType) throws IllegalArgumentException, IOException;

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
    public void scaleToSize(int width, int height, boolean keepRatio) throws UnsupportedOperationException, IOException;

    /**
     * Scales the image based on the given scaling factors. A scaling factor of 1 keeps the size. Scaling factors larger than
     * 1 make the image larger, factors smaller than 1 make it smaller. The amount of scaling is represented by the comparison 
     * between the factor and 1. A factor of 2 will double the image size. A factor of 0.5 will make it half size.
     * 
     * @param hfactor The horizontal scaling factor
     * @param vfactor The vertical scaling factor
     */
    public void scaleByFactor(int hfactor, int vfactor) throws UnsupportedOperationException, IOException;

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
    public void shrinkToSize(int width, int height) throws UnsupportedOperationException, IOException;

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
    public void growToSize(int width, int height) throws UnsupportedOperationException, IOException;

    /**
     * Writes the rendered and scaled image to an outputstream
     * @param out
     */
    public void writeImage(OutputStream out) throws IOException;

    /**
     * Returns the image encoding params.
     */
    public Object getImageEncodeParam();

    /**
     * Sets the image encoding params
     * @param imageEncodeParam The imageEncodeParam to set.
     */
    public void setImageEncodeParam(Object imageEncodeParam);

    /**
     * Returns the current image width
     */
    public int getWidth();

    /**
     * Returns the current image height
     */
    public int getHeight();

    /**
     * Determines the bytes size of the image if it would be written to an output stream.
     */
    public long determineRenderedSize() throws UnsupportedOperationException, IOException;

    /**
     * Returns if transparency is supported by the used mime type
     */
    public boolean isTransparencySupportedForOutput();
    
    /**
     * This method crops out the selected area from an image
     * 
     * The parameters x and y defines the start coordinate.
     * The parameters width and height defines the dimension to crop.
     *  
     * @param x
     * @param y
     * @param width
     * @param height
     * @throws UnsupportedOperationException If this scaler does not support cropping
     * 
     */
    public void crop(int x, int y, int width, int height) throws UnsupportedOperationException, IOException;

}
