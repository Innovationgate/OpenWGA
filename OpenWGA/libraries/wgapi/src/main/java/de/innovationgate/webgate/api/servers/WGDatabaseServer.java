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
package de.innovationgate.webgate.api.servers;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;
import de.innovationgate.wga.modules.types.ContentStoreModuleType;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;
import de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType;

/**
 * Object representing a WGAPI database server, offering all available functionalities.
 * Most of the functionality available her is redirected to the {@link ServerDatabaseRetriever} for the given database/server type.
 */
public abstract class WGDatabaseServer {

    private String _uid;

    private String _title;

    private Map<String, String> _options;
    
    public String getTitle(Locale locale) {
        if (_title != null) {
            return _title;
        }
        else {
            ModuleDefinition serverDef = getModuleDefinition(getClass());
            if (serverDef != null) {
                DatabaseServerProperties props = (DatabaseServerProperties) serverDef.getProperties();
                if (props.isSingleton()) {
                    return props.getSingletonTitle(locale);
                }
            }
            return "(No name)";
        }
    }

    public static ModuleDefinition getModuleDefinition(Class serverClass) {
        return WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, serverClass);
    }
    
    public boolean isSingleton() {

            ModuleDefinition serverDef = getModuleDefinition(getClass());
            if (serverDef != null) {
                DatabaseServerProperties props = (DatabaseServerProperties) serverDef.getProperties();
                return props.isSingleton();
            }
            else {
                return false;
            }
    }
        
    

    public void init(String uid, String title, Map<String, String> options) throws WGAPIException {
        _uid = uid;
        _title = title;
        _options = options;
    }

    public String getUid() {
        return _uid;
    }

    public Map<String, String> getOptions() {
        return _options;
    }
    
    public WGDatabase openDatabase(Class<? extends WGDatabaseCore> implClass, Map<String,String> options) throws WGAPIException, ModuleDependencyException {
        
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not suitable for database server type '" + getClass().getName() + "'");
        }
        return retriever.openDatabase(implClass, this, options, false);
        
    }
    
    public WGDatabase prepareDatabase(Class<? extends WGDatabaseCore> implClass, Map<String,String> options) throws WGAPIException, ModuleDependencyException {
        
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not suitable for database server type '" + getClass().getName() + "'");
        }
        return retriever.openDatabase(implClass, this, options, true);
        
    }
    
    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, Map<String,String> options) throws WGAPIException, ModuleDependencyException {
        
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not suitable for database server type '" + getClass().getName() + "'");
        }
        
        if (!retriever.isCreatable()) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not creatable on database server type '" + getClass().getName() + "'");
        }
        
        return retriever.createDatabase(implClass, this, options);
        
    }
    
    public List<ModuleDefinition> getContentDatabaseTypes() {
        
        List<ModuleDefinition> types = new ArrayList<ModuleDefinition>();
                
        Iterator<ModuleDefinition> cdTypes = WGFactory.getModuleRegistry().getModulesForType(ContentDatabaseModuleType.class).values().iterator();
        while (cdTypes.hasNext()) {
            ModuleDefinition moduleDefinition = (ModuleDefinition) cdTypes.next();
            try {
                ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(moduleDefinition);
                if (retriever != null) {
                    types.add(moduleDefinition);
                }
            }
            catch (ModuleDependencyException e) {
                // Ignored here
            }
        }
        
        return types;
        
    }
    

    
    public List<ModuleDefinition> getContentStoreTypes() {
        
        List<ModuleDefinition> types = new ArrayList<ModuleDefinition>();
                
        Iterator<ModuleDefinition> cdTypes = WGFactory.getModuleRegistry().getModulesForType(ContentStoreModuleType.class).values().iterator();
        while (cdTypes.hasNext()) {
            ModuleDefinition moduleDefinition = (ModuleDefinition) cdTypes.next();
            try {
                ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(moduleDefinition);
                if (retriever != null) {
                    types.add(moduleDefinition);
                }
            }
            catch (ModuleDependencyException e) {
                // Ignored here
            }
        }
        
        return types;
        
    }
    
    public List<ModuleDefinition> getPersonalisationDatabaseTypes() {
        
        List<ModuleDefinition> types = new ArrayList<ModuleDefinition>();
        Iterator<ModuleDefinition> csTypes = WGFactory.getModuleRegistry().getModulesForType(PersonalisationDatabaseModuleType.class).values().iterator();
        while (csTypes.hasNext()) {
            ModuleDefinition moduleDefinition = (ModuleDefinition) csTypes.next();
            try {
                ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(moduleDefinition);
                if (retriever != null) {
                    types.add(moduleDefinition);
                }
            }
            catch (ModuleDependencyException e) {
                // Ignored here
            }
        }
        
        return types;
        
    }

    protected ServerDatabaseRetriever fetchServerDatabaseRetriever(Class<? extends WGDatabaseCore> implClass) throws WGInvalidDatabaseException, ModuleDependencyException {
        // Find the module definition
        ModuleDefinition dbModuleDef = fetchDatabaseModuleDefinition(implClass);
        if (dbModuleDef == null) {
            throw new WGInvalidDatabaseException("Module definition of database implementation class '" + implClass.getName() + "' cannot be found");
        }
        
        return fetchServerDatabaseRetriever(dbModuleDef);
    }

    private ServerDatabaseRetriever fetchServerDatabaseRetriever(ModuleDefinition dbModuleDef) throws ModuleDependencyException {
        dbModuleDef.testDependencies();
        return fetchServerDatabaseRetriever(dbModuleDef, getClass());
    }
    
    public static ServerDatabaseRetriever fetchServerDatabaseRetriever(ModuleDefinition dbModuleDef, Class serverClass) throws ModuleDependencyException {
        
        // Find the database retriever for this server
        DatabaseProperties props = (DatabaseProperties) dbModuleDef.getProperties();
        Iterator<ServerDatabaseRetriever> retrievers = props.getServerDatabaseRetrievers().iterator();
        ServerDatabaseRetriever retriever = null;
        while (retrievers.hasNext()) {
            ServerDatabaseRetriever serverDatabaseRetriever = (ServerDatabaseRetriever) retrievers.next();
            if (serverDatabaseRetriever.getDatabaseServerType().isAssignableFrom(serverClass)) {
                retriever = serverDatabaseRetriever;
                break;
            }
        }
        return retriever;
    }

    public ModuleDefinition fetchDatabaseModuleDefinition(Class<? extends WGDatabaseCore> implClass) {
        ModuleDefinition dbModuleDef = WGFactory.getModuleRegistry().getModuleDefinition(ContentStoreModuleType.class, implClass);
        if (dbModuleDef == null) {
            dbModuleDef = WGFactory.getModuleRegistry().getModuleDefinition(ContentDatabaseModuleType.class, implClass);
        }
        if (dbModuleDef == null) {
            dbModuleDef = WGFactory.getModuleRegistry().getModuleDefinition(PersonalisationDatabaseModuleType.class, implClass);
        }
        return dbModuleDef;
    }
    
    public List<DatabaseInformation> getAvailableDatabases(Class<? extends WGDatabaseCore> implClass) throws WGAPIException, ModuleDependencyException {
        
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not suitable for database server type '" + getClass().getName() + "'");
        }
        
        if (!retriever.isAvailableDatabasesRetrievable()) {
            throw new WGInvalidDatabaseException("Available databases for implementation class '" + implClass.getName() + "' are not retrievable database server type '" + getClass().getName() + "'");
        }
        
        return retriever.getAvailableDatabases(this);
        
    }
    
    public boolean isAvailableDatabasesRetrievable(Class<? extends WGDatabaseCore> implClass) throws WGAPIException, ModuleDependencyException {
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not suitable for database server type '" + getClass().getName() + "'");
        }
        return retriever.isAvailableDatabasesRetrievable();
    }
    
    public boolean isDatabaseTypeCreatable(Class<? extends WGDatabaseCore> implClass) throws WGAPIException, ModuleDependencyException {
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            return false;
        }
        return retriever.isCreatable();
    }
    
    public void testConnection() throws ServerConnectionException {
        throw new ServerConnectionException("Connection test is not possible on this server type");
    }
    
    public boolean isConnectionTestable() {
        return false;
    }
    
    public List<String> getDatabasePathOptions(Class<? extends WGDatabaseCore> implClass) throws WGAPIException, ModuleDependencyException {
        ServerDatabaseRetriever retriever = fetchServerDatabaseRetriever(implClass);
        if (retriever == null) {
            throw new WGInvalidDatabaseException("The database implementation class '" + implClass.getName() + "' is not suitable for database server type '" + getClass().getName() + "'");
        }
        return retriever.getDatabasePathOptions();        
    }
    
    public void importState(WGDatabaseServer oldServer) {
    }
}
