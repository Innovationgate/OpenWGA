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

package de.innovationgate.wgpublisher.modules.auth;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.modules.AvailabilityTestableModuleDefinition;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleAvailabilityException;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.CommaSeparatedListOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.ServerFilePathOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.AuthenticationSourceModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.auth.CSAuthModule;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType;
import de.innovationgate.wgpublisher.modules.TMLScriptCodeOptionType;

public class CSAuthModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition, AvailabilityTestableModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/cs", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("auth.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return CSAuthModule.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return AuthenticationSourceModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition collectCondition = new LocalizedOptionDefinition(CSAuthModule.COPTION_COLLECT_CONDITION, TMLScriptCodeOptionType.INSTANCE, _bundleLoader);
        collectCondition.setOptional(true);
        options.addOption(collectCondition);

        LocalizedOptionDefinition dbKey = new LocalizedOptionDefinition(CSAuthModule.COPTION_DBKEY, new DatabasesOptionType(_registry, true), _bundleLoader);
        options.addOption(dbKey);

        LocalizedOptionDefinition itemAliases = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_ALIASES, StringOptionType.INSTANCE, _bundleLoader);
        itemAliases.setOptional(true);
        itemAliases.setDefaultValue(CSAuthModule.DEFAULTITEM_USERALIASES);
        options.addOption(itemAliases);
        
        LocalizedOptionDefinition itemEmail = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_EMAIL, StringOptionType.INSTANCE, _bundleLoader);
        itemEmail.setOptional(true);
        itemEmail.setDefaultValue(CSAuthModule.DEFAULTITEM_EMAIL);
        options.addOption(itemEmail);
        
        LocalizedOptionDefinition itemEnabled = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_ENABLED, StringOptionType.INSTANCE, _bundleLoader);
        itemEnabled.setOptional(true);
        itemEnabled.setDefaultValue(CSAuthModule.DEFAULTITEM_ENABLED);
        options.addOption(itemEnabled);
        
        LocalizedOptionDefinition itemGroupmembers = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_GROUPMEMBERS, StringOptionType.INSTANCE, _bundleLoader);
        itemGroupmembers.setOptional(true);
        itemGroupmembers.setDefaultValue(CSAuthModule.DEFAULTITEM_MEMBERS);
        options.addOption(itemGroupmembers);
        
        LocalizedOptionDefinition itemGroupname = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_GROUPNAME, StringOptionType.INSTANCE, _bundleLoader);
        itemGroupname.setOptional(true);
        itemGroupname.setDefaultValue(CSAuthModule.DEFAULTITEM_GROUPNAME);
        options.addOption(itemGroupname);
        
        LocalizedOptionDefinition itemPassword = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_PASSWORD, StringOptionType.INSTANCE, _bundleLoader);
        itemPassword.setOptional(true);
        itemPassword.setDefaultValue(CSAuthModule.DEFAULTITEM_PASSWORD);
        options.addOption(itemPassword);
        
        LocalizedOptionDefinition itemUsername = new LocalizedOptionDefinition(CSAuthModule.COPTION_ITEM_USERNAME, StringOptionType.INSTANCE, _bundleLoader);
        itemUsername.setOptional(true);
        itemUsername.setDefaultValue(CSAuthModule.DEFAULTITEM_USERNAME);
        options.addOption(itemUsername);
        
        LocalizedOptionDefinition aliasItems = new LocalizedOptionDefinition(CSAuthModule.COPTION_ALIAS_ITEMS, CommaSeparatedListOptionType.INSTANCE, _bundleLoader);
        aliasItems.setOptional(true);
        options.addOption(aliasItems);
        
        LocalizedOptionDefinition usersRoot = new LocalizedOptionDefinition(CSAuthModule.COPTION_ROOTDOC_USERS, StringOptionType.INSTANCE, _bundleLoader);
        usersRoot.setOptional(true);
        usersRoot.setDefaultValue(CSAuthModule.DEFAULT_USERSROOT);
        options.addOption(usersRoot);

        LocalizedOptionDefinition groupsRoot = new LocalizedOptionDefinition(CSAuthModule.COPTION_ROOTDOC_GROUPS, StringOptionType.INSTANCE, _bundleLoader);
        groupsRoot.setOptional(true);
        groupsRoot.setDefaultValue(CSAuthModule.DEFAULT_GROUPSROOT);
        options.addOption(groupsRoot);
        
        LocalizedOptionDefinition userContentClass = new LocalizedOptionDefinition(CSAuthModule.COPTION_LOGIN_CONTENTCLASS, StringOptionType.INSTANCE, _bundleLoader);
        userContentClass.setOptional(true);
        options.addOption(userContentClass);

        LocalizedOptionDefinition groupContentClass = new LocalizedOptionDefinition(CSAuthModule.COPTION_GROUP_CONTENTCLASS, StringOptionType.INSTANCE, _bundleLoader);
        groupContentClass.setOptional(true);
        options.addOption(groupContentClass);

        
        PredefinedValuesOptionType collectModesType = new PredefinedValuesOptionType(_bundleLoader, CSAuthModule.COPTION_COLLECT_MODE);
        collectModesType.addValue(CSAuthModule.COLLECTMODE_PRELOAD);
        collectModesType.addValue(CSAuthModule.COLLECTMODE_LAZY);
        LocalizedOptionDefinition collectMode = new LocalizedOptionDefinition(CSAuthModule.COPTION_COLLECT_MODE, collectModesType, _bundleLoader);
        collectMode.setOptional(true);
        collectMode.setDefaultValue(CSAuthModule.COLLECTMODE_PRELOAD);
        options.addOption(collectMode);
        
        
        LocalizedOptionDefinition collectScript = new LocalizedOptionDefinition(CSAuthModule.COPTION_SCRIPT_COLLECT, TMLScriptCodeOptionType.INSTANCE, _bundleLoader);
        collectScript.setOptional(true);
        collectScript.setExpert(true);
        options.addOption(collectScript);
        
        LocalizedOptionDefinition collectLazyCachesize = new LocalizedOptionDefinition(CSAuthModule.COPTION_COLLECT_LAZY_CACHESIZE, IntegerOptionType.INSTANCE, _bundleLoader);
        collectLazyCachesize.setDefaultValue(String.valueOf(CSAuthModule.COLLECT_LAZY_CACHESIZE_DEFAULT));
        collectLazyCachesize.setOptional(true);
        collectLazyCachesize.addDependentOption(CSAuthModule.COPTION_COLLECT_MODE, CSAuthModule.COLLECTMODE_LAZY);
        options.addOption(collectLazyCachesize);
        
        LocalizedOptionDefinition labeledNames = new LocalizedOptionDefinition(CSAuthModule.COPTION_LABELED_NAMES, CommaSeparatedListOptionType.INSTANCE, _bundleLoader);
        labeledNames.setOptional(true);
        options.addOption(labeledNames);
        
        LocalizedOptionDefinition commonNameExpr = new LocalizedOptionDefinition(CSAuthModule.COPTION_COMMONNAME_EXPRESSION, StringOptionType.INSTANCE, _bundleLoader);
        commonNameExpr.setOptional(true);
        options.addOption(commonNameExpr);
        
        LocalizedOptionDefinition  certAuth = new LocalizedOptionDefinition(CSAuthModule.COPTION_CERTAUTH, BooleanOptionType.INSTANCE, _bundleLoader);
        certAuth.setOptional(true);
        certAuth.setExpert(true);
        options.addOption(certAuth);

        LocalizedOptionDefinition  ca = new LocalizedOptionDefinition(CSAuthModule.COPTION_CA, ServerFilePathOptionType.INSTANCE, _bundleLoader);
        ca.setExpert(true);
        ca.addDependentOption(CSAuthModule.COPTION_CERTAUTH);
        options.addOption(ca);
        
        LocalizedOptionDefinition  crl = new LocalizedOptionDefinition(CSAuthModule.COPTION_CRL, ServerFilePathOptionType.INSTANCE, _bundleLoader);
        crl.addDependentOption(CSAuthModule.COPTION_CA);
        crl.setExpert(true);
        crl.addDependentOption(CSAuthModule.COPTION_CERTAUTH);
        options.addOption(crl);

        
        return options;
        
        
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("auth.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

    public void testAvailability(ModuleRegistry reg) throws ModuleAvailabilityException {
        
        WGACore core = (WGACore) reg.getContextObjects().get(WGACore.class);
        if (core == null) {
            throw new ModuleAvailabilityException("Cannot retrieve WGACore to test availability");
        }
        
        boolean csFound = false;
        for (WGDatabase db : core.getContentdbs().values()) {
            if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && !db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                csFound = true;
                break;
            }
        }
        if (!csFound) {
            throw new ModuleAvailabilityException("No WGA Content Store is connected that could be used as authentication source");
        }
        
    }

}
