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

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.jdbc.Work;

import de.innovationgate.csmaintenance.patchlevel.Level1;
import de.innovationgate.csmaintenance.patchlevel.Level2;
import de.innovationgate.csmaintenance.patchlevel.Level3;
import de.innovationgate.csmaintenance.patchlevel.Level4;
import de.innovationgate.csmaintenance.patchlevel.Level5;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class CS5Patcher {
    
    public static final String PLUGIN_ID = "de.innovationgate.csmaintenance";
    
    public static final int CS5_PATCH_LEVEL = 5;
    
    public static final String DBMETA_PATCH_LEVEL = WGDatabase.EXTDATA_PATCH_LEVEL;
    
    public static final String SYSPROP_DISABLE_BATCH = "de.innovationgate.wga.cs5patcher.disable_batch";

    private WGACore _core;

    private Logger _log;

    private boolean _batchDisabled = Boolean.parseBoolean(System.getProperty(SYSPROP_DISABLE_BATCH));
    
    private Map<Integer,CS5PatchLevel> _patches = new HashMap<Integer,CS5PatchLevel>();
    
    public CS5Patcher(WGACore core, Logger log) throws WGAPIException, IOException {
        _core = core;
        _log = log;
        
        WGAPlugin patchPlugin = _core.getPluginSet().getPluginByUniqueName("de.innovationgate.csmaintenance");
        WGDatabase patchDB = _core.getContentdbs().get(patchPlugin.buildDatabaseKey());
        if (!patchDB.isSessionOpen()) {
            patchDB.openSession();
        }
        
        _patches.put(1, new Level1(patchDB));
        _patches.put(2, new Level2(patchDB));
        _patches.put(3, new Level3(patchDB));
        _patches.put(4, new Level4(patchDB));
        _patches.put(5, new Level5(patchDB));
        
    }
    
    private boolean applyPatches(WGDatabase db, int patchLevel, Map<Integer, CS5PatchLevel> patches) {

        
        for (int patchIdx=1; patchIdx <= CS5_PATCH_LEVEL; patchIdx++) {
            
            if (patchLevel >= patchIdx) {
                continue;
            }
            
            try {
                CS5PatchLevel dbPatches = patches.get(patchIdx);
                final String patchCode = dbPatches.getPatch(db).getPatchCode(db);
                if (patchCode == null) {
                    _log.info("No patch " + patchIdx + " available for db of type " + db.getTypeName());
                    return false;
                }
        
                _log.info("Applying patch " + patchIdx + " to db '" + db.getDbReference() + "' (" + db.getTypeName() + ")");
                
            
                ((Session) db.getNativeObject()).doWork(new Work() {
                    
                    public void execute(Connection con) throws SQLException {
                        Boolean autoCommitMode = null;
                        autoCommitMode = con.getAutoCommit();
                        con.setAutoCommit(false);
                        
                        try {
                            Iterator<String> statements = WGUtils.deserializeCollection(patchCode, ";", false, new Character('\'')).iterator();
                            if (!_batchDisabled) {
                                patchViaBatch(con, statements);
                            }
                            else {
                                patchWithoutBatch(con, statements);
                            }
                            con.commit();
                        }
                        finally {
                            if (autoCommitMode != null) {
                                try {
                                    con.setAutoCommit(autoCommitMode);
                                }
                                catch (SQLException e) {
                                }
                            }
                        }
                    }

                });
                
            
                _log.info("Finished. Writing patch level information.");
                db.writeExtensionData(DBMETA_PATCH_LEVEL, patchIdx);
            }
            catch (Exception e) {
                _log.info("Exception applying patch", e);
                return false;
            }
            
            
        }
        
        return true;
        
        
    }
    
    private void patchViaBatch(Connection con, Iterator<String> statements) throws SQLException {
        Statement st = con.createStatement();
        while (statements.hasNext()) {
           String code = ((String) statements.next()).trim();
           if (!code.equals("")) {
               st.addBatch(code);
           }
        }
        st.executeBatch();
    }
    
    private void patchWithoutBatch(Connection con, Iterator<String> statements) throws SQLException {
        Statement st = con.createStatement();
        while (statements.hasNext()) {
           String code = ((String) statements.next()).trim();
           if (!code.equals("")) {
               st.executeUpdate(code);
           }
        }
    }
    
    public void executeOnAll()  throws WGAPIException, IOException {
        execute(new ArrayList<String>(_core.getContentdbs().keySet()));
    }
    
    public List<String> execute(List<String> dbKeys) throws WGAPIException, IOException {
        
        List<String> successfullyPatched = new ArrayList<String>();
        _log.info("OpenWGA Content Store 5 Patcher starting");
        _log.info("Patch level to apply: " + CS5_PATCH_LEVEL);
        
        List<String> dbsToReconnect = new ArrayList<String>();
        
        _log.info("Applying patches to content stores");
        for (String dbKey : dbKeys) {
            
            WGDatabase db = _core.getContentdbs().get(dbKey);
            if (db == null) {
                _log.error("Unknown database " + dbKey);
                continue;
            }
            
            try {
            
                if (db.getContentStoreVersion() == WGDatabase.CSVERSION_WGA5) {
                    
                    if (db.hasFeature(WGDatabase.FEATURE_UNPATCHEABLE)) {
                        continue;
                    }
                    
                    if (!db.isSessionOpen()) {
                        db.openSession();
                    }
                    
                    Number patchLevel = (Number) db.getExtensionData(DBMETA_PATCH_LEVEL);
                    if (patchLevel == null) {
                        patchLevel = 0;
                    }
                    
                    if (patchLevel.intValue() < CS5_PATCH_LEVEL) {
                        if (applyPatches(db, patchLevel.intValue(), _patches)) {
                            if (!db.getBooleanAttribute(WGACore.DBATTRIB_ADMIN_APP, false)) {
                                dbsToReconnect.add(db.getDbReference());
                            }
                            else {
                                _log.info("Database '" + db.getDbReference() + "' will not be reconnected as it belongs to an administrative application.");
                            }
                        }
                    }
                    else {
                        _log.info("Skipping db '" + db.getDbReference() + "' as the patch level is already applied.");
                    }
                }
            
            }
            catch (Throwable e) {
                _log.error("Exception in testing/applying patch for db " + db.getDbReference(), e);
            }
        }
        
        if (dbsToReconnect.size() > 0) {
            
            // Reconnect database. Disabled bc. this may disturb the job execution UI. Better to ask the admin to restart the server 
            _log.info("Finished appying patches. Now reconnecting patched applications");
            for (String dbKey : dbsToReconnect) {
                _core.removeContentDB(dbKey);
            }
            _core.updatePlugins();
            _core.updateContentDBs();
            
            _log.info("Checking availability of reconnected applications");
            for (String dbKey : dbsToReconnect) {
                if (_core.getContentdbs().containsKey(dbKey)) {
                    successfullyPatched.add(dbKey);
                    _log.info("Application '" + dbKey + "' is available");
                }
                else {
                    _log.warn("Application '" + dbKey + "' is NOT available! Please check appplication _log for details.");
                }
            }
        }
        else {
            _log.info("Finished appying patches. No content stores were patched.");
        }
        
        _log.info("OpenWGA Content Store 5 Patch Task finished successfully");
        return successfullyPatched;
    }
    
    public CS5PatchLevel getPatch(int level) {
        return _patches.get(level);
    }
    
    public String getManualPatchCode(WGDatabase db) throws WGAPIException {
        
        // Read current patch level
        boolean patchLevelInfoExists = false;
        Number currentPatchLevelExtData = (Number) db.getExtensionData(DBMETA_PATCH_LEVEL);
        int currentPatchLevel= 0;
        if (currentPatchLevelExtData != null) {
            patchLevelInfoExists = true;
            currentPatchLevel = currentPatchLevelExtData.intValue();
        }
        
        // Collect the codes of each patch level yet to apply on this db
        StringBuffer code = new StringBuffer();
        currentPatchLevel++;
        while (currentPatchLevel <= CS5_PATCH_LEVEL) {
            CS5PatchLevel patchLevel = _patches.get(currentPatchLevel);
            if (patchLevel == null) {
                throw new IllegalArgumentException("No patch of level " + currentPatchLevel);
            }
            
            CS5Patch patch = patchLevel.getPatch(db);
            if (patch == null) {
                throw new IllegalArgumentException("No patch of level " + currentPatchLevel + " for database type " + db.getTypeName());
            }
            
            
            code.append(patch.getPatchCode(db));
            currentPatchLevel++;
        }
        
        // Create statement to update patch level info
        if (patchLevelInfoExists) {
            code.append("UPDATE extensiondata SET numbervalue=" + CS5_PATCH_LEVEL + " WHERE name='" + DBMETA_PATCH_LEVEL + "';\n");
        }
        else {
            code.append("INSERT INTO extensiondata (name, datatype, numbervalue) VALUES ('" + DBMETA_PATCH_LEVEL + "', " + WGDocumentImpl.ITEMTYPE_NUMBER + ", " + CS5_PATCH_LEVEL + ");\n");
        }
        
        return code.toString();
        
    }

}
