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

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Root;

/**
 * An image link, pointing to any image file in any OpenWGA database. It is normally stored in the context of an OpenWGA content document (like an item), which is used to complete missing data on an incomplete image link.
 */
@Root(name="ImageLink")
public class ImageLink {
    
    @Attribute(name="db",required=false)
    private String _db = null;
    
    @Attribute(name="doc",required=false)
    private String _doc = null;
    
    @Attribute(name="file")
    private String _file = null;
    
    @Attribute(name="title",required=false)
    private String _title = null;

    @Attribute(name="alt",required=false)
    private String _alt = null;

    @Attribute(name="border",required=false)
    private String _border = null;
    
    @Attribute(name="borderColor",required=false)
    private String _borderColor = null;
    
    @Attribute(name="align",required=false)
    private String _align = null;
    
    @Attribute(name="cssClass",required=false)
    private String _cssClass = null;
    
    @Attribute(name="cssStyle",required=false)
    private String _cssStyle = null;

    /**
     * Returns the key of the database containing the image file. Contains null for a file in the same database as the context document
     */
    public String getDb() {
        return _db;
    }

    /**
     * Sets the key of the database containing the image file. Set null for a file in the same database as the context document
     */
    public void setDb(String db) {
        _db = db;
    }

    /**
     * Returns the key of the document containing the image file, either the name of a file container or the content key of a content document. Contains null for a file on the context document itself.
     */
    public String getDoc() {
        return _doc;
    }

    /**
     * Sets the key of the document containing the image file, either the name of a file container or the content key of a content document. Set null for a file on the context document itself.
     */
    public void setDoc(String doc) {
        _doc = doc;
    }

    /**
     * Returns the name of the file
     */
    public String getFile() {
        return _file;
    }

    /**
     * Sets the name of the file
     */
    public void setFile(String file) {
        _file = file;
    }

    /**
     * Returns the title of the image, which will be used as content for attributes "title" and "alt" on HTML output
     */
    public String getTitle() {
        return _title;
    }

    /**
     * Sets the title of the image, which will be used as content for attributes "title" and "alt" on HTML output
     */
    public void setTitle(String alt) {
        _title = alt;
    }

    public void setAlt(String alt) {
        _alt = alt;
    }
    public String getAlt() {
        return _alt;
    }

    /**
     * Returns the thickness of the border around the image as CSS expression. Contains null for none.
     * @deprecated Use {@link #getCssClass()} and {@link #getCssStyle()} instead
     */
    public String getBorder() {
        return _border;
    }

    /**
     * Sets the thickness of the border around the image as CSS expression. Set null for none.
     * @deprecated Use {@link #setCssClass(String)} and {@link #setCssStyle(String)} instead
     */
    public void setBorder(String border) {
        _border = border;
    }

    /**
     * Returns the color of the border around the image as CSS color expression. Contains null for none.
     * @deprecated Use {@link #getCssClass()} and {@link #getCssStyle()} instead
     */
    public String getBorderColor() {
        return _borderColor;
    }

    /**
     * Sets the color of the border around the image as CSS color expression. Set null for none.
     * @deprecated Use {@link #setCssClass(String)} and {@link #setCssStyle(String)} instead
     */
    public void setBorderColor(String borderColor) {
        _borderColor = borderColor;
    }

    /**
     * Returns the alignment of the image on HTML output. Contains null for none.
     * @deprecated Use {@link #getCssClass()} and {@link #getCssStyle()} instead
     */
    public String getAlign() {
        return _align;
    }

    /**
     * Sets the alignment of the image on HTML output. Set null for none.
     * @deprecated Use {@link #setCssClass(String)} and {@link #setCssStyle(String)} instead
     */
    public void setAlign(String align) {
        _align = align;
    }

    /**
     * Returns a CSS class to apply to the HTML image tag on HTML output. Contains null for none.
     */
    public String getCssClass() {
        return _cssClass;
    }


    /**
     * Sets a CSS class to apply to the HTML image tag on HTML output. Contains null for none.
     */
    public void setCssClass(String cssClass) {
        _cssClass = cssClass;
    }


    /**
     * Returns CSS style instructions to apply to the HTML image tag on HTML output. Contains null for none.
     */
    public String getCssStyle() {
        return _cssStyle;
    }

    /**
     * Sets CSS style instructions to apply to the HTML image tag on HTML output. Set null for none.
     */
    public void setCssStyle(String cssStyle) {
        _cssStyle = cssStyle;
    }
    
    /**
     * Uses an {@link ImageLinkReader} to write the data of this image link into a string
     */
    @Override
    public String toString() {
        ImageLinkReader reader = new ImageLinkReader();
        return reader.write(this);
    }

}
