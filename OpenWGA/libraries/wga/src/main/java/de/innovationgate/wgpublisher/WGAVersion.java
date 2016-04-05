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

import de.innovationgate.wga.common.beans.csconfig.v1.Version;

/**
 * Contains all version constants for WGA.
 */
public abstract class WGAVersion {

    public static final int WGAPUBLISHER_BUILD_VERSION = 639;
    public static final int WGAPUBLISHER_MAJOR_VERSION = 7;
    public static final int WGAPUBLISHER_MINOR_VERSION = 2;
    public static final int WGAPUBLISHER_MAINTENANCE_VERSION = 0;
    public static final int WGAPUBLISHER_PATCH_VERSION = 0;
    
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
