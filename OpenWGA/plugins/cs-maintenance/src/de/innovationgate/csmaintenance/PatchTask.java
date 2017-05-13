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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.scheduler.TaskImplementation;
import de.innovationgate.wgpublisher.scheduler.TitledTaskImplementation;
import de.innovationgate.wgpublisher.scheduler.UpgradeFileStorageTask;

public class PatchTask implements TaskImplementation, TitledTaskImplementation {

    public void execute(JobContext jobContext) throws JobFailedException {
        
        try {
            CS5Patcher patcher = new CS5Patcher(jobContext.getWgaCore(), jobContext.getLog());
            
            // Collect databases to patch
            List<String> dbKeys = new ArrayList<String>();
            String dbKey = jobContext.getOption("dbkey");
            String dbSelection = jobContext.getOption("dbselection");
            if (dbKey != null) {
                jobContext.getLog().info("Patching content store '" + dbKey + "'");
                dbKeys.add(dbKey);
            }
            else if ("plugins".equals(dbSelection)) {
                jobContext.getLog().info("Patching all OpenWGA plugins");
                for (WGDatabase db : jobContext.getWgaCore().getContentdbs().values()) {
                    if (db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                        dbKeys.add(db.getDbReference());
                    }
                }
            }
            else {
                jobContext.getLog().info("Patching all databases");
                for (WGDatabase db : jobContext.getWgaCore().getContentdbs().values()) {
                    dbKeys.add(db.getDbReference());
                }
            }
            
            // Collect old patch levels, so we know where a file storage upgrade will be neccessary
            Map<String,Integer> oldPatchLevels = new HashMap<String, Integer>();
            try {
                for (String patchDbKey : dbKeys) {
                    WGDatabase db = jobContext.getWgaCore().getContentdbs().get(patchDbKey);
                    if (db.getContentStoreVersion() >= 5) {
                        db.openSession();
                        Number patchLevel = (Number) db.getExtensionData(WGDatabase.EXTDATA_PATCH_LEVEL);
                        if (patchLevel == null) {
                            patchLevel = 0;
                        }
                        oldPatchLevels.put(patchDbKey, patchLevel.intValue());
                    }
                }
            }
            finally {
                WGFactory.getInstance().closeSessions();
            }
            
            List<String> patchedDbs = patcher.execute(dbKeys);
            
            String upgradeFiles = jobContext.getOption("upgradefiles");
            if ("true".equals(upgradeFiles)) {
                jobContext.getLog().info("Upgrading files storage on patched content stores");
                Map<String,Long> freedMemory = new HashMap<String, Long>();
                for (String key : patchedDbs) {
                    WGDatabase db = jobContext.getWgaCore().getContentdbs().get(key);
                    if (db == null) {
                        jobContext.getLog().error("App '" + dbKey + "' is not connected after patching. Cannot upgrade file storage");
                        continue;
                    }
                    if (db.getContentStoreVersion() >= 5 && db.getContentStorePatchLevel() >= 4) {
                        
                        // Do no storage upgrade on content stores that were on patch level 4 before 
                        Integer oldPatchLevel = oldPatchLevels.get(key);
                        if (oldPatchLevel == null || oldPatchLevel >= 4) { 
                            continue;
                        }
                        
                        jobContext.getLog().info("Upgrading files storage of content store '" + db.getDbReference() + "'");
                        db.openSession();
                        Long freed = UpgradeFileStorageTask.performUpgrade(jobContext, db);
                        freedMemory.put(db.getDbReference(), freed);
                    }
                }
                
                jobContext.getLog().info("Upgrade File Storage: Summary of freed memory");
                long sum = 0;
                for (Map.Entry<String,Long> entry : freedMemory.entrySet()) {
                    jobContext.getLog().info(entry.getKey() + ": " + WGUtils.DECIMALFORMAT_STANDARD.format(entry.getValue() / 1024 / 1024) + " MB");
                    sum+=entry.getValue();
                }
                jobContext.getLog().info("Total freed memory: " + WGUtils.DECIMALFORMAT_STANDARD.format(sum / 1024 / 1024) + " MB");
                
            }
            
        }
        catch (Exception e) {
            throw new JobFailedException("Exception running patcher task", e);
        }
    }

    @Override
    public String getTitle(Locale locale, Map<String, String> options) {
        return "Upgrading content stores";
    }



}
