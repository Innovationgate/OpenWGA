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

package de.innovationgate.wgpublisher;

import java.util.Enumeration;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Returns data about the brand of OpenWGA product
 */
public abstract class WGABrand {
    
    public static final String BRAND_LABELFILE = "brand";
    
    private static ResourceBundle BUNDLE = ResourceBundle.getBundle(WGACore.SYSTEMLABEL_BASE_PATH + BRAND_LABELFILE, Locale.ENGLISH, WGAVersion.class.getClassLoader());

    public static String getName() {
        return BUNDLE.getString("brand.name") + BUNDLE.getString("brand.trademark.sign");
    }
    
    public static String getProduct() {
        return getName() + " " + BUNDLE.getString("product.name");
    }
    
    public static String getPlatform() {
        try {
            return BUNDLE.getString("platform.name");
        }
        catch(MissingResourceException e) {
            return null;
        }
    }
    
    public static String get(String key) {
        return BUNDLE.getString(key);
    }


}
