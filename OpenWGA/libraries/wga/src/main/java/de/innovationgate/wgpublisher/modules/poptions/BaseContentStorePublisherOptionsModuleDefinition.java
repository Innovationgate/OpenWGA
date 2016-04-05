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

package de.innovationgate.wgpublisher.modules.poptions;

import java.nio.charset.Charset;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.model.BrowsingSecurity;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
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
import de.innovationgate.wga.modules.options.TextEncodingOptionType;
import de.innovationgate.wga.modules.types.ContentDatabasePublisherOptionsModuleType;
import de.innovationgate.wga.modules.types.FileAnnotatorModuleType;
import de.innovationgate.wga.modules.types.FileDerivateCreatorModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;
import de.innovationgate.wgpublisher.files.derivates.ThumbnailDerivateCreator;
import de.innovationgate.wgpublisher.lang.DynamicLanguageBehaviour;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType;
import de.innovationgate.wgpublisher.modules.ExpressionLanguageOptionType;
import de.innovationgate.wgpublisher.modules.FileAnnotatorsOptionType;
import de.innovationgate.wgpublisher.modules.FileDerivateCreatorsOptionType;
import de.innovationgate.wgpublisher.modules.LanguageBehavioursOptionType;
import de.innovationgate.wgpublisher.modules.MediaKeyOptionType;
import de.innovationgate.wgpublisher.modules.PersonalisationModesOptionType;
import de.innovationgate.wgpublisher.modules.PersonalisationStatisticModesOptionType;

public class BaseContentStorePublisherOptionsModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/poptions_cs", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return "Publisher options defined by WGA Core";
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentStorePublisherOptionsCollector.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition incidentApp = new LocalizedOptionDefinition(WGACore.DBATTRIB_INCIDENTS_APP, new DatabasesOptionType(_registry, DatabasesOptionType.DatabaseType.APP, DatabasesOptionType.DatabaseType.PLUGIN), _bundleLoader);
        incidentApp.setOptional(true);
        options.addOption(incidentApp);
        
        LocalizedOptionDefinition defMediaKey = new LocalizedOptionDefinition(WGACore.DBATTRIB_DEFAULT_MEDIAKEY, new MediaKeyOptionType(_registry), _bundleLoader);
        defMediaKey.setOptional(true);
        defMediaKey.setDefaultValue(WGDatabase.DEFAULT_MEDIAKEY);
        options.addOption(defMediaKey);

        LocalizedOptionDefinition allowPublishing = new LocalizedOptionDefinition(WGACore.DBATTRIB_ALLOW_PUBLISHING, BooleanOptionType.INSTANCE, _bundleLoader);
        allowPublishing.setOptional(true);
        allowPublishing.setExpert(true);
        allowPublishing.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(allowPublishing);
        
        LocalizedOptionDefinition allowBrowsing = new LocalizedOptionDefinition(WGACore.DBATTRIB_ALLOW_BROWSING, BooleanOptionType.INSTANCE, _bundleLoader);
        allowBrowsing.setOptional(true);
        allowBrowsing.setExpert(true);
        allowBrowsing.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(allowBrowsing);
        
        PredefinedValuesOptionType browsingSecurityOptionType = new PredefinedValuesOptionType(_bundleLoader, WGACore.DBATTRIB_BROWSING_SECURITY);
        browsingSecurityOptionType.addValue(String.valueOf(BrowsingSecurity.FULL_ACCESS));
        browsingSecurityOptionType.addValue(String.valueOf(BrowsingSecurity.NO_AUTHORING));
        browsingSecurityOptionType.addValue(String.valueOf(BrowsingSecurity.NO_BROWSING));
        LocalizedOptionDefinition browsingSecurity = new LocalizedOptionDefinition(WGACore.DBATTRIB_BROWSING_SECURITY, browsingSecurityOptionType, _bundleLoader);
        browsingSecurity.setOptional(true);
        browsingSecurity.setDefaultValue(String.valueOf(BrowsingSecurity.FULL_ACCESS));
        options.addOption(browsingSecurity);
        
        LocalizedOptionDefinition designEncoding = new LocalizedOptionDefinition(WGACore.DBATTRIB_DESIGN_ENCODING, TextEncodingOptionType.INSTANCE, _bundleLoader);
        designEncoding.setOptional(true);
        designEncoding.setExpert(true);
        designEncoding.setDefaultValue(Charset.defaultCharset().name());
        options.addOption(designEncoding);
        
        LocalizedOptionDefinition directAccessDefault = new LocalizedOptionDefinition(WGACore.DBATTRIB_DIRECTACCESSDEFAULT, BooleanOptionType.INSTANCE, _bundleLoader);
        directAccessDefault.setOptional(true);
        directAccessDefault.setExpert(true);
        directAccessDefault.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(directAccessDefault);
        
        LocalizedOptionDefinition expressionDefault = new LocalizedOptionDefinition(WGACore.DBATTRIB_EXPRESSION_DEFAULT, ExpressionLanguageOptionType.INSTANCE, _bundleLoader);
        expressionDefault.setOptional(true);
        expressionDefault.setExpert(true);
        expressionDefault.setDefaultValue(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
        options.addOption(expressionDefault);
        
        LocalizedOptionDefinition homePage = new LocalizedOptionDefinition(WGACore.DBATTRIB_HOME_PAGE, StringOptionType.INSTANCE, _bundleLoader);
        homePage.setOptional(true);
        options.addOption(homePage);

        LocalizedOptionDefinition homeDocName = new LocalizedOptionDefinition(WGACore.DBATTRIB_HOME_PAGE_NAME, StringOptionType.INSTANCE, _bundleLoader);
        homeDocName.setOptional(true);
        options.addOption(homeDocName);
        

        LocalizedOptionDefinition httpLogin = new LocalizedOptionDefinition(WGACore.DBATTRIB_HTTPLOGIN, BooleanOptionType.INSTANCE, _bundleLoader);
        httpLogin.setOptional(true);
        options.addOption(httpLogin);

        LocalizedOptionDefinition isRemoteCS = new LocalizedOptionDefinition(WGACore.DBATTRIB_ISREMOTECS, BooleanOptionType.INSTANCE, _bundleLoader);
        isRemoteCS.setOptional(true);
        isRemoteCS.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(isRemoteCS);
        
        LocalizedOptionDefinition langBehaviour = new LocalizedOptionDefinition(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR, new LanguageBehavioursOptionType(_registry), _bundleLoader);
        langBehaviour.setOptional(true);
        options.addOption(langBehaviour);
        
        LocalizedOptionDefinition dynamicLBConfigFile = new LocalizedOptionDefinition(DynamicLanguageBehaviour.PUBOPTION_CONFIG_FILE, ServerFilePathOptionType.INSTANCE, _bundleLoader);
        dynamicLBConfigFile.addDependentOption(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR, DynamicLanguageBehaviour.class.getName());
        dynamicLBConfigFile.setOptional(true);
        options.addOption(dynamicLBConfigFile);
        
        LocalizedOptionDefinition loginPage = new LocalizedOptionDefinition(WGACore.DBATTRIB_LOGIN_PAGE, StringOptionType.INSTANCE, _bundleLoader);
        loginPage.setOptional(true);
        options.addOption(loginPage);
        
        LocalizedOptionDefinition multiLangContent = new LocalizedOptionDefinition(WGACore.DBATTRIB_MULTILANGUAGE_CONTENT, BooleanOptionType.INSTANCE, _bundleLoader);
        multiLangContent.setOptional(true);
        multiLangContent.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(multiLangContent);
        
        LocalizedOptionDefinition persMode = new LocalizedOptionDefinition(WGACore.DBATTRIB_PERSMODE, PersonalisationModesOptionType.INSTANCE, _bundleLoader);
        persMode.setDefaultValue(String.valueOf(Constants.PERSMODE_SESSION));
        options.addOption(persMode);
        
        LocalizedOptionDefinition persModeOptIn = new LocalizedOptionDefinition(WGACore.DBATTRIB_PERSMODE_OPT_IN, BooleanOptionType.INSTANCE, _bundleLoader);
        persModeOptIn.setDefaultValue(String.valueOf(Boolean.FALSE.toString()));
        persModeOptIn.addDependentOption(WGACore.DBATTRIB_PERSMODE, String.valueOf(Constants.PERSMODE_AUTO));
        //persModeOptIn.addDependentOption(WGACore.DBATTRIB_PERSMODE, String.valueOf(Constants.PERSMODE_LOGIN));
        options.addOption(persModeOptIn);
        
        PredefinedValuesOptionType regType = new PredefinedValuesOptionType(_bundleLoader, WGACore.DBATTRIB_PORTLETREGISTRYMODE);
        regType.addValue(WGDatabase.PORTLETREGISTRYMODE_PERSISTENT);
        regType.addValue(WGDatabase.PORTLETREGISTRYMODE_TRANSIENT);
        LocalizedOptionDefinition regMode = new LocalizedOptionDefinition(WGACore.DBATTRIB_PORTLETREGISTRYMODE, regType, _bundleLoader);
        regMode.setOptional(true);
        options.addOption(regMode);
        
        LocalizedOptionDefinition serializableProfile = new LocalizedOptionDefinition(WGACore.DBATTRIB_SERIALIZABLE_USERPROFILE, BooleanOptionType.INSTANCE, _bundleLoader);
        serializableProfile.setExpert(true);
        serializableProfile.setOptional(true);
        serializableProfile.setDefaultValue(Boolean.FALSE.toString());
        serializableProfile.addDependentOption(WGACore.DBATTRIB_PERSMODE, String.valueOf(Constants.PERSMODE_SESSION));
        options.addOption(serializableProfile);
        
        LocalizedOptionDefinition persStatMode = new LocalizedOptionDefinition(WGACore.DBATTRIB_PERSSTATMODE, PersonalisationStatisticModesOptionType.INSTANCE, _bundleLoader);
        persStatMode.setOptional(true);
        persStatMode.setDefaultValue(String.valueOf(Constants.PERSSTATMODE_SESSION));
        options.addOption(persStatMode);
        
        LocalizedOptionDefinition sessionCookie = new LocalizedOptionDefinition(WGACore.DBATTRIB_SESSIONCOOKIE, StringOptionType.INSTANCE, _bundleLoader);
        sessionCookie.setOptional(true);
        sessionCookie.setExpert(true);
        sessionCookie.setDefaultValue("LtpaToken");
        options.addOption(sessionCookie);
        
        LocalizedOptionDefinition sessionCookieDomain = new LocalizedOptionDefinition(WGACore.DBATTRIB_SESSIONCOOKIEDOMAIN, StringOptionType.INSTANCE, _bundleLoader);
        sessionCookieDomain.setOptional(true);
        sessionCookieDomain.setExpert(true);
        sessionCookieDomain.addDependentOption(WGACore.DBATTRIB_SESSIONCOOKIE);
        options.addOption(sessionCookieDomain);
        
        LocalizedOptionDefinition useRemoteCS = new LocalizedOptionDefinition(WGACore.DBATTRIB_USEREMOTECS, BooleanOptionType.INSTANCE, _bundleLoader);
        useRemoteCS.setOptional(true);
        useRemoteCS.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(useRemoteCS);
        
        LocalizedOptionDefinition redirectHost = new LocalizedOptionDefinition(WGACore.DBATTRIB_REDIRECTHOST, StringOptionType.INSTANCE, _bundleLoader);
        redirectHost.setOptional(true);
        options.addOption(redirectHost);
        
        LocalizedOptionDefinition redirectPort = new LocalizedOptionDefinition(WGACore.DBATTRIB_REDIRECTPORT, IntegerOptionType.INSTANCE, _bundleLoader);
        redirectPort.setOptional(true);
        options.addOption(redirectPort);
        
        LocalizedOptionDefinition redirectProtocol = new LocalizedOptionDefinition(WGACore.DBATTRIB_REDIRECTPROTOCOL, StringOptionType.INSTANCE, _bundleLoader);
        redirectProtocol.setOptional(true);
        options.addOption(redirectProtocol);
        
        LocalizedOptionDefinition hdbUseVersioning = new LocalizedOptionDefinition(WGACore.DBATTRIB_HDB_USE_VERSIONING, BooleanOptionType.INSTANCE, _bundleLoader);
        hdbUseVersioning.setOptional(true);
        hdbUseVersioning.setExpert(true);
        hdbUseVersioning.setDefaultValue(Boolean.FALSE.toString());
        //hdbUseVersioning.addDependentOption(WGACore.DBATTRIB_ISHDB, Boolean.TRUE.toString());
        options.addOption(hdbUseVersioning);
        
        LocalizedOptionDefinition varProvisioning = new LocalizedOptionDefinition(WGACore.DBATTRIB_VARPROVISIONING, BooleanOptionType.INSTANCE, _bundleLoader);
        varProvisioning.setOptional(true);
        varProvisioning.setExpert(true);
        varProvisioning.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(varProvisioning);
        
        LocalizedOptionDefinition titlePathURL = new LocalizedOptionDefinition(WGACore.DBATTRIB_TITLEPATHURL, BooleanOptionType.INSTANCE, _bundleLoader);
        titlePathURL.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(titlePathURL);
        
        LocalizedOptionDefinition tpuContentIndexing = new LocalizedOptionDefinition(WGACore.DBATTRIB_TITLEPATHURL_CONTENTINDEXING, BooleanOptionType.INSTANCE, _bundleLoader);
        tpuContentIndexing.setOptional(true);
        tpuContentIndexing.setExpert(true);
        tpuContentIndexing.setDefaultValue(Boolean.FALSE.toString());
        tpuContentIndexing.addDependentOption(WGACore.DBATTRIB_TITLEPATHURL, Boolean.TRUE.toString());
        options.addOption(tpuContentIndexing);
        
        LocalizedOptionDefinition tpuShortcutArea = new LocalizedOptionDefinition(WGACore.DBATTRIB_TITLEPATHURL_SHORTCUTAREA, StringOptionType.INSTANCE, _bundleLoader);
        tpuShortcutArea.setOptional(true);
        tpuShortcutArea.addDependentOption(WGACore.DBATTRIB_TITLEPATHURL, Boolean.TRUE.toString());
        options.addOption(tpuShortcutArea);
        
        LocalizedOptionDefinition tpuMixedLangs = new LocalizedOptionDefinition(WGACore.DBATTRIB_TITLEPATHURL_MIXEDLANGUAGES, BooleanOptionType.INSTANCE, _bundleLoader);
        tpuMixedLangs.setOptional(true);
        tpuMixedLangs.setDefaultValue(Boolean.FALSE.toString());
        tpuMixedLangs.addDependentOption(WGACore.DBATTRIB_TITLEPATHURL, Boolean.TRUE.toString());
        options.addOption(tpuMixedLangs);
        
        LocalizedOptionDefinition tpuIncludeKeys = new LocalizedOptionDefinition(WGACore.DBATTRIB_TITLEPATHURL_INCLUDEKEYS, BooleanOptionType.INSTANCE, _bundleLoader);
        tpuIncludeKeys.setDefaultValue(Boolean.TRUE.toString());
        tpuIncludeKeys.addDependentOption(WGACore.DBATTRIB_TITLEPATHURL, Boolean.TRUE.toString());
        options.addOption(tpuIncludeKeys);
        
        LocalizedOptionDefinition tmlCacheServeStale = new LocalizedOptionDefinition(WGACore.DBATTRIB_WEBTMLCACHE_SERVESTALEDATA, BooleanOptionType.INSTANCE, _bundleLoader);
        tmlCacheServeStale.setOptional(true);
        tmlCacheServeStale.setExpert(true);
        tmlCacheServeStale.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(tmlCacheServeStale);
        
        LocalizedOptionDefinition webdavAllCharacters = new LocalizedOptionDefinition(WGACore.DBATTRIB_SHARE_ALLOWALLCHARS, BooleanOptionType.INSTANCE, _bundleLoader);
        webdavAllCharacters.setOptional(true);
        webdavAllCharacters.setExpert(true);
        webdavAllCharacters.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(webdavAllCharacters);
        
        LocalizedOptionDefinition nameURLs = new LocalizedOptionDefinition(WGACore.DBATTRIB_CREATE_NAME_URLS, BooleanOptionType.INSTANCE, _bundleLoader);
        nameURLs.setOptional(true);
        nameURLs.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(nameURLs);
        
        LocalizedOptionDefinition enableAccessLogging = new LocalizedOptionDefinition(WGACore.DBATTRIB_ENABLE_ACCESSLOGGING, BooleanOptionType.INSTANCE, _bundleLoader);
        enableAccessLogging.setOptional(true);
        enableAccessLogging.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(enableAccessLogging);
        
        LocalizedOptionDefinition secureApp = new LocalizedOptionDefinition(WGACore.DBATTRIB_SECURE_APP, BooleanOptionType.INSTANCE, _bundleLoader);
        secureApp.setOptional(true);
        secureApp.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(secureApp);
        
        LocalizedOptionDefinition defaultPortHTTPS = new LocalizedOptionDefinition(WGACore.DBATTRIB_DEFAULTPORT_HTTPS, IntegerOptionType.INSTANCE, _bundleLoader);
        defaultPortHTTPS.setOptional(true);
        defaultPortHTTPS.setDefaultValue("443");
        options.addOption(defaultPortHTTPS);
        
        LocalizedOptionDefinition defaultPortHTTP = new LocalizedOptionDefinition(WGACore.DBATTRIB_DEFAULTPORT_HTTP, IntegerOptionType.INSTANCE, _bundleLoader);
        defaultPortHTTP.setOptional(true);
        defaultPortHTTP.setDefaultValue("80");
        options.addOption(defaultPortHTTP);
        
        LocalizedOptionDefinition forceLabelLanguage = new LocalizedOptionDefinition(WGACore.DBATTRIB_FORCE_LABEL_LANGUAGE, StringOptionType.INSTANCE, _bundleLoader);
        forceLabelLanguage.setOptional(true);
        options.addOption(forceLabelLanguage);
        
        LocalizedOptionDefinition fallbackLabelLanguage = new LocalizedOptionDefinition(WGACore.DBATTRIB_FALLBACK_LABEL_LANGUAGE, StringOptionType.INSTANCE, _bundleLoader);
        fallbackLabelLanguage.setOptional(true);
        options.addOption(fallbackLabelLanguage);
        
        PredefinedValuesOptionType keepParamsOnAJAXType = new PredefinedValuesOptionType(_bundleLoader, WGACore.DBATTRIB_AJAX_KEEP_URL_PARAMS);
        keepParamsOnAJAXType.addValue("default");
        keepParamsOnAJAXType.addValue(Boolean.TRUE.toString());
        keepParamsOnAJAXType.addValue(Boolean.FALSE.toString());
        LocalizedOptionDefinition keepParamsOnAJAX = new LocalizedOptionDefinition(WGACore.DBATTRIB_AJAX_KEEP_URL_PARAMS, keepParamsOnAJAXType, _bundleLoader);
        keepParamsOnAJAX.setOptional(true);
        keepParamsOnAJAX.setExpert(true);
        keepParamsOnAJAX.setDefaultValue("default");
        options.addOption(keepParamsOnAJAX);
        
        LocalizedOptionDefinition showSessionExpiredMessage = new LocalizedOptionDefinition(WGACore.DBATTRIB_SHOW_SESSION_EXPIRED_MESSAGE, BooleanOptionType.INSTANCE, _bundleLoader);
        showSessionExpiredMessage.setOptional(true);
        showSessionExpiredMessage.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(showSessionExpiredMessage);
        
        LocalizedOptionDefinition useLoginParameterOnContentURLs = new LocalizedOptionDefinition(WGACore.DBATTRIB_USE_LOGINPARAMETER_ON_CONTENTURLS, BooleanOptionType.INSTANCE, _bundleLoader);
        useLoginParameterOnContentURLs.setOptional(true);
        useLoginParameterOnContentURLs.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(useLoginParameterOnContentURLs);
        
        PredefinedValuesOptionType derivatesEnabledType = new PredefinedValuesOptionType(_bundleLoader, WGACore.DBATTRIB_FILE_DERIVATES_ENABLED);
        derivatesEnabledType.addValue(FileDerivateManager.DERIVATEMODE_OFF);
        derivatesEnabledType.addValue(FileDerivateManager.DERIVATEMODE_SPECIAL_CREATORS);
        derivatesEnabledType.addValue(FileDerivateManager.DERIVATEMODE_ALL_CREATORS);
        LocalizedOptionDefinition derivatesEnabled = new LocalizedOptionDefinition(WGACore.DBATTRIB_FILE_DERIVATES_ENABLED, derivatesEnabledType, _bundleLoader);
        derivatesEnabled.setDefaultValue(FileDerivateManager.DERIVATEMODE_ALL_CREATORS);
        derivatesEnabled.setOptional(true);
        options.addOption(derivatesEnabled);
        
        LocalizedOptionDefinition derivateCreators = new LocalizedOptionDefinition(WGACore.DBATTRIB_FILE_DERIVATES_CREATORS, new FileDerivateCreatorsOptionType(_registry), _bundleLoader);
        derivateCreators.addDependentOption(WGACore.DBATTRIB_FILE_DERIVATES_ENABLED, "specialCreators");
        options.addOption(derivateCreators);
        
        LocalizedOptionDefinition thumbnailSizes = new LocalizedOptionDefinition(ThumbnailDerivateCreator.DBATTRIB_THUMBNAIL_SIZES, CommaSeparatedListOptionType.INSTANCE, _bundleLoader);
        thumbnailSizes.setOptional(true);
        thumbnailSizes.setDefaultValue(ThumbnailDerivateCreator.DEFAULT_THUMBNAIL_SIZES);
        options.addOption(thumbnailSizes);
        
        LocalizedOptionDefinition fileAnnotators = new LocalizedOptionDefinition(WGACore.DBATTRIB_FILE_ANNOTATORS, new FileAnnotatorsOptionType(_registry), _bundleLoader);
        fileAnnotators.setOptional(true);
        fileAnnotators.setExpert(true);
        options.addOption(fileAnnotators);
        
        LocalizedOptionDefinition useNonFinalHTFeatures = new LocalizedOptionDefinition(WGACore.DBATTRIB_USE_NONFINAL_HT_FEATURES, BooleanOptionType.INSTANCE, _bundleLoader);
        useNonFinalHTFeatures.setDefaultValue(Boolean.TRUE.toString());
        useNonFinalHTFeatures.setOptional(true);
        options.addOption(useNonFinalHTFeatures);
        
        LocalizedOptionDefinition enableEnhancedItemExpressions = new LocalizedOptionDefinition(WGACore.DBATTRIB_ENHANCED_ITEMEXPRESSIONS, BooleanOptionType.INSTANCE, _bundleLoader);
        enableEnhancedItemExpressions.setDefaultValue(Boolean.TRUE.toString());
        enableEnhancedItemExpressions.setOptional(true);
        options.addOption(enableEnhancedItemExpressions);
        
        return options;
        
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return "Base publisher options";
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

}
