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

package de.innovationgate.csmaintenance;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;

public class CSMaintenanceCoreEventListener implements WGACoreEventListener {
    
    public static final CSMaintenanceCoreEventListener INSTANCE = new CSMaintenanceCoreEventListener();
    
    private CSMaintenanceCoreEventListener() {
    }
    
    
    
    public static final String SERVEROPTION_APPLIED_PATCH_LEVEL = CS5Patcher.PLUGIN_ID + ".AppliedPatchLevel";

    public void contentStoreConnected(WGACoreEvent event) {
    }

    public void contentStoreDisconnected(WGACoreEvent event) {
    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {

// Obsolete patching functionality at startup. Disabled for now (#00003109)
//        try {
//            WGAConfiguration config = event.getCore().getWgaConfiguration().clone();
//            OptionReader reader = OptionReader.create(config.getServerOptions(), new MaintenanceOptionsModuleDefinition());
//            
//            Integer appliedPatchLevel = (Integer) reader.readOptionValueOrDefault(CSMaintenanceCoreEventListener.SERVEROPTION_APPLIED_PATCH_LEVEL);
//            if (appliedPatchLevel >= CS5Patcher.CS5_PATCH_LEVEL) {
//                return;
//            }
//            
//            event.getCore().logCategoryInfo("OpenWGA content store maintenance", 1);
//            CS5Patcher patcher = new CS5Patcher(event.getCore(), event.getCore().getLog(), true);
//            patcher.execute();
//            
//            config.getServerOptions().put(CSMaintenanceCoreEventListener.SERVEROPTION_APPLIED_PATCH_LEVEL, String.valueOf(CS5Patcher.CS5_PATCH_LEVEL));
//            event.getCore().saveWgaConfiguration(config);
//        }
//        catch (Exception e) {
//            event.getCore().getLog().error("Exception running content store maintenance", e);
//        }
        
        try {
            WGAConfiguration config = event.getCore().getWgaConfiguration().clone();
            if (config.getServerOptions().get(CSMaintenanceCoreEventListener.SERVEROPTION_APPLIED_PATCH_LEVEL) != null) {
                event.getCore().getLog().info("Removing obsolete server option regarding applied patch level");
                config.getServerOptions().remove(CSMaintenanceCoreEventListener.SERVEROPTION_APPLIED_PATCH_LEVEL);
                event.getCore().saveWgaConfiguration(config);
            }
        }
        catch (Exception e) {
            event.getCore().getLog().error("Exception running content store maintenance", e);
        }
      

    }

    public void startupPreConnect(WGACoreEvent event) {
    }

}
