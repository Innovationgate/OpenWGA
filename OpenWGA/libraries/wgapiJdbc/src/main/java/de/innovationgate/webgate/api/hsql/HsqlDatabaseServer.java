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

package de.innovationgate.webgate.api.hsql;

import java.io.File;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.servers.ServerConnectionException;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;

public class HsqlDatabaseServer extends WGDatabaseServer {
    
    public static final String V1_PATH_PREFIX = "";
    public static final String V2_PATH_PREFIX = "/hsql2";
    
    @Override
    public boolean isConnectionTestable() {
        return true;
    }

    public File getDatabaseDirectory() throws WGBackendException {
        return getDatabaseDirectory(V1_PATH_PREFIX);
    }
    
    public File getDatabaseDirectory(String prefix) throws WGBackendException {
        
        String hsqlRoot = System.getProperty(WGDatabaseImpl.SYSPROPERTY_HSQL_ROOT) + prefix;
        String dirPath = getOptions().get(Database.OPTION_PATH);
        
        File dir;
        if (dirPath != null && !dirPath.trim().equals("")) {
            dir = new File(hsqlRoot, dirPath);
        }
        else {
            dir = new File(hsqlRoot);
        }
        
        return dir;
    }

    @Override
    public void testConnection() throws ServerConnectionException {
    }

    @Override
    public void init(String uid, String title, Map<String, String> options) throws WGAPIException {
        super.init(uid, title, options);
        
        // Migrate v1 databases if v2 folder not yet present
        File dbDir = getDatabaseDirectory(V1_PATH_PREFIX);
        if (!dbDir.exists()) {
            if (!dbDir.mkdirs()) {
                throw new WGBackendException("Cannot create hsql database directory " + dbDir.getPath());
            }
            
            /*
            File v1dbDir = getDatabaseDirectory(V1_PATH_PREFIX);
            WGFactory.getLogger().info("Migrating HSQL1 databases to version 2");
            for (File file : v1dbDir.listFiles()) {
                if (!file.isDirectory()) {
                    try {
                        WGUtils.copyFile(file, dbDir);
                    }
                    catch (IOException e) {
                        throw new WGBackendException("Exception migrating hsql1 databases from '" + v1dbDir.getAbsolutePath() + "' to '" + dbDir.getAbsolutePath() + "'", e);
                    }
                }
            }*/
        }
    }
}
