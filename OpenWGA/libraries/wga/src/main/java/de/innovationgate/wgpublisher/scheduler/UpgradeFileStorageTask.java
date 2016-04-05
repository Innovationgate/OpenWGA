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

package de.innovationgate.wgpublisher.scheduler;

import java.util.Locale;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;

public class UpgradeFileStorageTask implements TaskImplementation, TitledTaskImplementation {
    
    public static final String OPTION_DBKEY = "dbkey";
    
    public static volatile boolean _running = false;
    
    @Override
    public void execute(JobContext jobContext) throws JobFailedException {
        
        try {
            String dbkey = (String) jobContext.getOption(OPTION_DBKEY);
            WGDatabase db = WGA.get(jobContext).db(dbkey);
            if (db == null) {
                throw new JobFailedException("App '" + dbkey + "' is unknown or not connected");
            }
            performUpgrade(jobContext, db);
        }
        catch (JobFailedException e) {
            throw e;
        }       
        catch (WGException e) {
            throw new JobFailedException("Exception running file storage upgrade task", e);
        }


    }

    public static Long performUpgrade(JobContext jobContext, WGDatabase db) throws WGAPIException, JobFailedException {
        
        _running = true;
        try {
            
            if (!db.isBackendServiceSupported(WGDatabaseImpl.BACKENDSERVICE_UPGRADE_FILE_STORAGE)) {
                throw new JobFailedException("App '" + db.getDbReference() + "' does not support upgrading file storage. This is only possible since OpenWGA content store version 5 of patch level 4.");
            }
            
            Long freedMemory = (Long) db.callBackendService(WGDatabaseImpl.BACKENDSERVICE_UPGRADE_FILE_STORAGE, new Object[] { jobContext.getLog() });
            
            
            if (jobContext.getWgaCore().getFileDerivateManager().isProcessServer()) {
                jobContext.getLog().info("Removing status of file derivate creation to allow (re-)creation for upgraded files");
                jobContext.getWgaCore().getFileDerivateManager().updateAllDerivates(db);
            }
            else {
                jobContext.getLog().warn("You may need to manually trigger file derivate re-creation for your apps for them to become available.");
            }
            
            return freedMemory;
            
        }
        finally {
            _running  = false;
        }
    }

    @Override
    public String getTitle(Locale locale, Map<String, String> options) {
        return "Upgrading file storage on app '" + options.get(OPTION_DBKEY) + "'";
    }

    public static boolean isRunning() {
        return _running;
    }

}
