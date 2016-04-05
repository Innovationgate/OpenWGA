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
package de.innovationgate.webgate.api.modules.dbs;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;

public abstract class GenericContentDatabaseModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {

    private LocalisationBundleLoader _genericLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(GenericContentDatabaseModuleDefinition.class) + "/genericdb", getClass().getClassLoader());
    protected ModuleRegistry _registry;  

    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition allowCacheMaintenance = new LocalizedOptionDefinition(WGDatabase.COPTION_ALLOWCACHEMAINTENANCE, BooleanOptionType.INSTANCE, _genericLoader);
        allowCacheMaintenance.setOptional(true);
        allowCacheMaintenance.setDefaultValue(Boolean.TRUE.toString());
        allowCacheMaintenance.setExpert(true);
        options.addOption(allowCacheMaintenance);
        
        LocalizedOptionDefinition deletionCheck = new LocalizedOptionDefinition(WGDatabase.COPTION_DELETIONCHECK, BooleanOptionType.INSTANCE, _genericLoader);
        deletionCheck.setOptional(true);
        deletionCheck.setDefaultValue(Boolean.FALSE.toString());
        deletionCheck.setExpert(true);
        options.addOption(deletionCheck);
        
        LocalizedOptionDefinition documentCacheSize = new LocalizedOptionDefinition(WGDatabase.COPTION_DOCUMENTCACHESIZE, IntegerOptionType.INSTANCE, _genericLoader);
        documentCacheSize.setOptional(true);
        documentCacheSize.setDefaultValue(String.valueOf(WGDatabase.DOCUMENTCACHE_SIZE_DEFAULT));
        options.addOption(documentCacheSize);
        
        LocalizedOptionDefinition nameCacheSize = new LocalizedOptionDefinition(WGDatabase.COPTION_NAMECACHESIZE, IntegerOptionType.INSTANCE, _genericLoader);
        nameCacheSize.setOptional(true);
        nameCacheSize.setDefaultValue(String.valueOf(WGDatabase.NAMECACHE_SIZE_DEFAULT));
        options.addOption(nameCacheSize);
        
        LocalizedOptionDefinition queryCacheSize = new LocalizedOptionDefinition(WGDatabase.COPTION_QUERYCACHESIZE, IntegerOptionType.INSTANCE, _genericLoader);
        queryCacheSize.setOptional(true);
        queryCacheSize.setDefaultValue(String.valueOf(WGDatabase.QUERYCACHE_SIZE_DEFAULT));
        options.addOption(queryCacheSize);

        LocalizedOptionDefinition listCacheRebuildThreshold = new LocalizedOptionDefinition(WGDatabase.COPTION_LIST_CACHE_REBUILD_THRESHOLD, IntegerOptionType.INSTANCE, _genericLoader);
        listCacheRebuildThreshold.setOptional(true);
        listCacheRebuildThreshold.setExpert(true);
        listCacheRebuildThreshold.setDefaultValue(Integer.toString(10));
        options.addOption(listCacheRebuildThreshold);

        LocalizedOptionDefinition updateTimeout = new LocalizedOptionDefinition(WGDatabase.COPTION_UPDATETIMEOUT, IntegerOptionType.INSTANCE, _genericLoader);
        updateTimeout.setOptional(true);
        updateTimeout.setDefaultValue(Integer.toString(5));
        updateTimeout.setExpert(true);
        options.addOption(updateTimeout);
        
        LocalizedOptionDefinition caching = new LocalizedOptionDefinition(WGDatabase.COPTION_CACHING_ENABLED, BooleanOptionType.INSTANCE, _genericLoader);
        caching.setOptional(true);
        caching.setDefaultValue(Boolean.TRUE.toString());
        caching.setExpert(true);
        options.addOption(caching);
        
        LocalizedOptionDefinition maxCores = new LocalizedOptionDefinition(WGDatabase.COPTION_MAXCORES, IntegerOptionType.INSTANCE, _genericLoader);
        maxCores.setOptional(true);
        maxCores.setDefaultValue(Integer.toString(1000));
        maxCores.setExpert(true);
        options.addOption(maxCores);
        
        LocalizedOptionDefinition monitorLastChange = new LocalizedOptionDefinition(WGDatabase.COPTION_MONITORLASTCHANGE, BooleanOptionType.INSTANCE, _genericLoader);
        monitorLastChange.setOptional(true);
        monitorLastChange.setDefaultValue(Boolean.TRUE.toString());
        monitorLastChange.setExpert(true);
        options.addOption(monitorLastChange);
        
        PredefinedValuesOptionType noItemBehaviourProvider = new PredefinedValuesOptionType(_genericLoader, WGDatabase.COPTION_NOITEMBEHAVIOUR);
        noItemBehaviourProvider.addValue("default");
        noItemBehaviourProvider.addValue("compatible");
        LocalizedOptionDefinition noItemBehaviour = new LocalizedOptionDefinition(WGDatabase.COPTION_NOITEMBEHAVIOUR, noItemBehaviourProvider, _genericLoader);
        noItemBehaviour.setOptional(true);
        noItemBehaviour.setExpert(true);
        noItemBehaviour.setDefaultValue("default");
        options.addOption(noItemBehaviour);
        
        LocalizedOptionDefinition userCachesEnabled = new LocalizedOptionDefinition(WGDatabase.COPTION_USERCACHES_ENABLED, BooleanOptionType.INSTANCE, _genericLoader);
        userCachesEnabled.setOptional(true);
        userCachesEnabled.setExpert(true);
        userCachesEnabled.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(userCachesEnabled);
        
        LocalizedOptionDefinition userCacheLatency = new LocalizedOptionDefinition(WGDatabase.COPTION_USERCACHELATENCY, IntegerOptionType.INSTANCE, _genericLoader);
        userCacheLatency.setOptional(true);
        userCacheLatency.setExpert(true);
        userCacheLatency.setDefaultValue("30");
        options.addOption(userCacheLatency);
        
        LocalizedOptionDefinition cacheLatency = new LocalizedOptionDefinition(WGDatabase.COPTION_CACHELATENCY, IntegerOptionType.INSTANCE, _genericLoader);
        cacheLatency.setOptional(true);
        cacheLatency.setExpert(true);
        cacheLatency.setDefaultValue("0");
        options.addOption(cacheLatency);
        
        LocalizedOptionDefinition useSharedPool = new LocalizedOptionDefinition(WGDatabase.COPTION_SHAREDPOOL, BooleanOptionType.INSTANCE, _genericLoader);
        useSharedPool.setOptional(true);
        useSharedPool.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(useSharedPool);
        
        LocalizedOptionDefinition clustered = new LocalizedOptionDefinition(WGDatabase.COPTION_CLUSTERED, BooleanOptionType.INSTANCE, _genericLoader);
        clustered.setOptional(true);
        clustered.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(clustered);
        
        
        /* Currently unusable, since mixed usage of certauth and regular logins not possible in the same domain (DBLoginInfo conflict of credential types)
        LocalizedOptionDefinition certAuth = new LocalizedOptionDefinition(WGDatabase.COPTION_DISABLE_CERTAUTH, BooleanOptionType.INSTANCE, _genericLoader);
        certAuth.setOptional(true);
        certAuth.setDefaultValue(Boolean.FALSE.toString());
        certAuth.setExpert(true);
        options.addOption(certAuth);
        */
        
        return options;
    }
    
    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

    protected ModuleRegistry getRegistry() {
        return _registry;
    }

    protected LocalisationBundleLoader getGenericLoader() {
        return _genericLoader;
    }


}
