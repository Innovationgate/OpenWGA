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
import java.util.Properties;

import de.innovationgate.wga.common.beans.csconfig.v1.Version;

/**
 * Contains all version constants for WGA, read from wgabuild.properties.
 */
public abstract class WGAVersion {
    
    public static final Properties BUILD_PROPERTIES = new Properties();
    static {
        InputStream propsIn = null;
        try {
            if (WGACore.INSTANCE.getServletContext() != null) {
                propsIn = WGACore.INSTANCE.getServletContext().getResourceAsStream("/WEB-INF/wgabuild.properties");
                if (propsIn == null) {
                    throw new Error("/WEB-INF/wgabuild.properties does not exist");
                }
                
                BUILD_PROPERTIES.load(propsIn);
            }
            
            // Defaults for other environments without running WGACore, just for the sake of preventing crashes
            else {
                BUILD_PROPERTIES.setProperty("build", "639");
                BUILD_PROPERTIES.setProperty("majorVersion", "7");
                BUILD_PROPERTIES.setProperty("minorVersion", "2");
                BUILD_PROPERTIES.setProperty("maintenanceVersion", "0");
                BUILD_PROPERTIES.setProperty("patchVersion", "0");
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

    public static final int WGAPUBLISHER_BUILD_VERSION = Integer.parseInt(BUILD_PROPERTIES.getProperty("build"));
    public static final int WGAPUBLISHER_MAJOR_VERSION = Integer.parseInt(BUILD_PROPERTIES.getProperty("majorVersion"));;
    public static final int WGAPUBLISHER_MINOR_VERSION = Integer.parseInt(BUILD_PROPERTIES.getProperty("minorVersion"));
    public static final int WGAPUBLISHER_MAINTENANCE_VERSION = Integer.parseInt(BUILD_PROPERTIES.getProperty("maintenanceVersion"));;
    public static final int WGAPUBLISHER_PATCH_VERSION = Integer.parseInt(BUILD_PROPERTIES.getProperty("patchVersion"));;
    
    public static final String WGAPUBLISHER_PRODUCT_NAME = WGABrand.getProduct();
    public static final String WGAPUBLISHER_PLATFORM_NAME = WGABrand.getPlatform();
    
    // set to current WGAVersion on changes in the cluster protocol (for e.g. new or changed ClusterTasks)
    public static final String WGAPUBLISHER_CLUSTERPROTOCOL_VERSION = "7.2.0.0";
    
    public static final Version VERSION = new Version(WGAPUBLISHER_MAJOR_VERSION,
            WGAPUBLISHER_MINOR_VERSION,
            WGAPUBLISHER_MAINTENANCE_VERSION,
            WGAPUBLISHER_PATCH_VERSION,
            WGAPUBLISHER_BUILD_VERSION);
    
    public static Version toCsConfigVersion() {
        return VERSION;
    }

}
