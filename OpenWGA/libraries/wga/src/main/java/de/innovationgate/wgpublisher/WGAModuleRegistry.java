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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.servers.ServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;

public class WGAModuleRegistry extends ModuleRegistry {
    
    public List<ModuleDefinition> getContentDatabaseTypesForServer(WGDatabaseServer server) {
        
        List<ModuleDefinition> dbTypes = new ArrayList<ModuleDefinition>();
        Iterator<ModuleDefinition> allDbTypes = getModulesForType(ContentDatabaseModuleType.class).values().iterator();
        while (allDbTypes.hasNext()) {
            ModuleDefinition moduleDefinition = (ModuleDefinition) allDbTypes.next();
            DatabaseProperties props = (DatabaseProperties) moduleDefinition.getProperties();
            Iterator<ServerDatabaseRetriever> retrievers = props.getServerDatabaseRetrievers().iterator();
            while (retrievers.hasNext()) {
                ServerDatabaseRetriever serverDatabaseRetriever = (ServerDatabaseRetriever) retrievers.next();
                if (server.getClass().isAssignableFrom(serverDatabaseRetriever.getDatabaseServerType())) {
                    dbTypes.add(moduleDefinition);
                    break;
                }
            }
        }
        
        return dbTypes;
        
    }

}
