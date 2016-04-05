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

package de.innovationgate.wgpublisher.encoders;

/**
 * A text chunk for encoding
 */
public class TextChunk {
    
    /**
     * An enum denoting the origin of a text chunk, which often decides if an encoder will transform or ignore it
     */
    public enum Origin {
        
        /**
         * Generic input string, fully encodeable. Maybe transformed by an earlier encoder.
         */
        INPUT,  
        
        /**
         * Added by a WGA encoder. Most encoders add chunks of this origin so they won't be modified by later encoders.
         */
        ADDED

    }
    
    /**
     * Returns the content type of the text chunk
     */
    protected String getContentType() {
        return _contentType;
    }

    private Origin _origin;
    private String _contentType;
    private String _text;
    
    public TextChunk(Origin chunkType, String contentType, String text) {
        super();
        _origin = chunkType;
        _contentType = contentType;
        _text = text;
    }
    
    /**
     * Returns the origin of the text chunk
     */
    public Origin getOrigin() {
        return _origin;
    }
    
    /**
     * Returns the text content of the chunk
     */
    public String getText() {
        return _text;
    }
    
    @Override
    public String toString() {
        return getText();
    }

}
