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

import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;

import org.simpleframework.xml.core.Persister;

import de.innovationgate.utils.WGUtils;

/**
 * Service for reading and writing image links, like stored into items by <tml:image item="itemname"/>
 * Image Links are normally stored as a serialized XML version of the {@link ImageLink} object. However there are numerous legacy formats of image links which can also be read by this reader.
 */
public class ImageLinkReader {
    
    private static final Persister PERSISTER = new Persister();
    
    /**
     * Creates a new image link pointing to a file on the current document
     * @param file The name of the file
     */
    public ImageLink create(String file) {
        ImageLink ili = new ImageLink();
        ili.setFile(file);
        return ili;
    }
    
    /**
     * Creates a new image link pointing to a file an any document
     * @param doc The name/key of the document containing the file, either name of a file container or content key of a content document
     * @param file The name of the file
     */
    public ImageLink create(String doc, String file) {
        ImageLink ili = new ImageLink();
        ili.setDoc(doc);
        ili.setFile(file);
        return ili;
    }
    
    /**
     * Reads an image link from an item value list. This method is able to read image links in XML format (in a single string) just as in any legacy format (which involve lists of values)
     * @param itemValues The item value list. This should be the content of any item, read as list.
     */
    public ImageLink read(List<String> itemValues) {
        
     // Modern storage as serialized XML
        if (itemValues.size() == 1) {
            try {
                ImageLink ili = PERSISTER.read(ImageLink.class, new StringReader(itemValues.get(0)));
                return ili;
            }
            catch (Exception e) {
            }
        }
        
        // Legacy storage
        ImageLink ili = new ImageLink();
        if (itemValues.size() == 0) { // Should not happen, but was coded as fallback in the legacy code, so to be sure...
            return ili;
        }
        
        // First two elements either contain file, file and empty string, or doc and file
        if (itemValues.size() >= 2 && !WGUtils.isEmpty(itemValues.get(1))) {
            ili.setDoc(itemValues.get(0).toString());
            ili.setFile(itemValues.get(1).toString());
        }
        else {
            ili.setFile(itemValues.get(0).toString());
        }

        if (itemValues.size() >= 3 && !WGUtils.isEmpty(itemValues.get(2))) {
            ili.setTitle(itemValues.get(2).toString());
        }
        
        if( itemValues.size() >= 4 && !WGUtils.isEmpty(itemValues.get(3))){
            ili.setBorder(itemValues.get(3).toString());
        }
        
        if( itemValues.size() >= 5 && !WGUtils.isEmpty(itemValues.get(4))){
            ili.setBorderColor(itemValues.get(4).toString());
        }
        
        if( itemValues.size() >= 6 && !WGUtils.isEmpty(itemValues.get(5))){
            ili.setAlign(itemValues.get(5).toString());
        }
        
        return ili;
        
    }
    
    /**
     * Writes an image link, always in XML format
     * @param ili The image link
     * @return The serialized image link
     */
    public String write(ImageLink ili) {
        try {
            StringWriter writer = new StringWriter();
            PERSISTER.write(ili, writer);
            return writer.getBuffer().toString();
        }
        catch (Exception e) {
            throw new IllegalStateException("Exception serializing ImageLink Item", e);
        }
    }

}
