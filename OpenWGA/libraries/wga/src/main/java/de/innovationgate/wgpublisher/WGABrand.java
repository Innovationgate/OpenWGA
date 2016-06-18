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

import java.io.IOException;
import java.io.InputStream;
import java.util.MissingResourceException;
import java.util.Properties;

/**
 * Returns data about the brand of OpenWGA product
 */
public abstract class WGABrand {

    public static final Properties BRAND = new Properties();
    static {
        InputStream propsIn = null;
        try {
            if (WGACore.INSTANCE.getServletContext() != null) {
                propsIn = WGACore.INSTANCE.getServletContext().getResourceAsStream("/WEB-INF/brand.properties");
                if (propsIn == null) {
                    throw new Error("/WEB-INF/brand.properties does not exist");
                }
                
                BRAND.load(propsIn);
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        finally {
            if (propsIn != null) {
                try {
                    propsIn.close();
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public static String getName() {
        return BRAND.getProperty("brand.name") + BRAND.getProperty("brand.trademark.sign");
    }
    
    public static String getProduct() {
        return getName() + " " + BRAND.getProperty("product.name");
    }
    
    public static String getPlatform() {
        try {
            return BRAND.getProperty("platform.name");
        }
        catch(MissingResourceException e) {
            return null;
        }
    }
    
    public static String get(String key) {
        return BRAND.getProperty(key);
    }

}
