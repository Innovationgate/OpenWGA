/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.model;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.GeneralSecurityException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import net.java.dev.genesis.annotation.DataProvider;
import net.java.dev.genesis.annotation.NotBound;

import org.apache.commons.beanutils.BeanUtils;

import de.innovationgate.utils.DESEncrypter.PersistentKeyException;
import de.innovationgate.utils.DirZipper;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.beans.DesignDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.ACLRole;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.ElementMapping;
import de.innovationgate.wga.common.beans.csconfig.v1.EncoderMapping;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.JobDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.common.beans.csconfig.v1.RemoteAction;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.common.beans.csconfig.v2.Shortcut;

public class WGADesignConfigurationModel extends AbstractModel {
	
	public static final String BUILD_PROP_SIGNATURE = "signature";
	public static final String BUILD_PROP_COMMENT = "comment";

    public static final int INTEGER_NOT_SET = -1;

    public static final String STRING_NOT_SET = "$notset";

    public static final AuthPlugin EMPTY_AUTH_PLUGIN = new AuthPlugin(STRING_NOT_SET, "None");

    public static final Map<Integer, AccessLevel> ACCESSLEVELS = new HashMap<Integer, AccessLevel>();

    static {
        ACCESSLEVELS.put(AccessLevel.LEVEL_NO_ACCESS, new AccessLevel(AccessLevel.LEVEL_NO_ACCESS, "NO ACCESS"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_READER, new AccessLevel(AccessLevel.LEVEL_READER, "READER"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_READER_DESIGNER, new AccessLevel(AccessLevel.LEVEL_READER_DESIGNER, "READER/DESIGNER"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_AUTHOR, new AccessLevel(AccessLevel.LEVEL_AUTHOR, "AUTHOR"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_AUTHOR_DESIGNER, new AccessLevel(AccessLevel.LEVEL_AUTHOR_DESIGNER, "AUTHOR/DESIGNER"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_EDITOR, new AccessLevel(AccessLevel.LEVEL_EDITOR, "EDITOR"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_EDITOR_DESIGNER, new AccessLevel(AccessLevel.LEVEL_EDITOR_DESIGNER, "EDITOR/DESIGNER"));
        ACCESSLEVELS.put(AccessLevel.LEVEL_MANAGER, new AccessLevel(AccessLevel.LEVEL_MANAGER, "MANAGER"));
        ACCESSLEVELS.put(INTEGER_NOT_SET, new AccessLevel(INTEGER_NOT_SET, "Do not set"));
    }

    public static final Map<String, ExpressionLanguage> EXPRESSION_LANGUAGES = new HashMap<String, ExpressionLanguage>();

    static {
        EXPRESSION_LANGUAGES.put(STRING_NOT_SET, new ExpressionLanguage(STRING_NOT_SET, "Not set"));
        EXPRESSION_LANGUAGES.put(ExpressionLanguage.LANGUAGE_TMLSCRIPT, new ExpressionLanguage(ExpressionLanguage.LANGUAGE_TMLSCRIPT, "TMLScript"));
        EXPRESSION_LANGUAGES.put(ExpressionLanguage.LANGUAGE_NATIVE, new ExpressionLanguage(ExpressionLanguage.LANGUAGE_NATIVE, "Native database language"));
    }

    public static final Map<String, Encoding> ITEMENCODINGS = new HashMap<String, Encoding>();

    static {
        ITEMENCODINGS.put(Encoding.ENCODING_HTML, new Encoding(Encoding.ENCODING_HTML, "html"));
        ITEMENCODINGS.put(Encoding.ENCODING_NONE, new Encoding(Encoding.ENCODING_NONE, "none"));
        ITEMENCODINGS.put(STRING_NOT_SET, new Encoding(STRING_NOT_SET, "Not set"));
    }

    public static final Map<String, Encoding> DESIGNENCODINGS = new HashMap<String, Encoding>();

    static {
        DESIGNENCODINGS.put(Encoding.ENCODING_UTF_8, new Encoding(Encoding.ENCODING_UTF_8, "Unicode (UTF-8)"));
        DESIGNENCODINGS.put(Encoding.ENCODING_ISO_8859_1, new Encoding(Encoding.ENCODING_ISO_8859_1, "Latin-1 (ISO-8859-1)"));
        DESIGNENCODINGS.put(STRING_NOT_SET, new Encoding(STRING_NOT_SET, "Not set (Platform encoding)"));
    }

    public static final Map<Integer, PersonalisationMode> PERSONALISATION_MODES = new HashMap<Integer, PersonalisationMode>();
    static {
        PERSONALISATION_MODES.put(Constants.PERSMODE_SESSION, new PersonalisationMode(Constants.PERSMODE_SESSION, "Session based (no persistence)"));
        PERSONALISATION_MODES.put(Constants.PERSMODE_AUTO, new PersonalisationMode(Constants.PERSMODE_AUTO, "Automatic (per browser / cookie-based)"));
        PERSONALISATION_MODES.put(Constants.PERSMODE_LOGIN, new PersonalisationMode(Constants.PERSMODE_LOGIN, "Login (per database login)"));
        //PERSONALISATION_MODES.put(Constants.PERSMODE_CUSTOM, new PersonalisationMode(Constants.PERSMODE_CUSTOM, "Custom (assignment via TMLScript)"));
    }

    public static final Map<Integer, AccessLevel> ACCESSLEVELS_REMOTE_ACTIONS = new HashMap<Integer, AccessLevel>();

    static {
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_READER, new AccessLevel(AccessLevel.LEVEL_READER, "READER"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_READER_DESIGNER, new AccessLevel(AccessLevel.LEVEL_READER_DESIGNER, "READER/DESIGNER"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_AUTHOR, new AccessLevel(AccessLevel.LEVEL_AUTHOR, "AUTHOR"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_AUTHOR_DESIGNER, new AccessLevel(AccessLevel.LEVEL_AUTHOR_DESIGNER, "AUTHOR/DESIGNER"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_EDITOR, new AccessLevel(AccessLevel.LEVEL_EDITOR, "EDITOR"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_EDITOR_DESIGNER, new AccessLevel(AccessLevel.LEVEL_EDITOR_DESIGNER, "EDITOR/DESIGNER"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(AccessLevel.LEVEL_MANAGER, new AccessLevel(AccessLevel.LEVEL_MANAGER, "MANAGER"));
        ACCESSLEVELS_REMOTE_ACTIONS.put(10000, new AccessLevel(10000, "Specified callers only"));
    }

    public static final Map<Integer, JobType> JOBTYPES = new HashMap<Integer, JobType>();

    static {
        JOBTYPES.put(JobType.TYPE_JAVA, new JobType(JobType.TYPE_JAVA, "Java Task Implementation"));
        JOBTYPES.put(JobType.TYPE_TMLSCRIPTMODULE, new JobType(JobType.TYPE_TMLSCRIPTMODULE, "TMLScript Module"));
    }

    public static final String FEATURE_SHORTCUTS = "SHORTCUTS";

    public static final Map<Integer, ShortcutType> SHORTCUTTYPES = new HashMap<Integer, ShortcutType>();

    static {
        SHORTCUTTYPES.put(new Integer(ShortcutType.TYPE_PLUGIN), new ShortcutType(ShortcutType.TYPE_PLUGIN, "WGA plugin shortcut"));
        SHORTCUTTYPES.put(new Integer(ShortcutType.TYPE_TMLSCRIPT_GLOBAL), new ShortcutType(ShortcutType.TYPE_TMLSCRIPT_GLOBAL, "TMLScript global"));
        SHORTCUTTYPES.put(new Integer(ShortcutType.TYPE_ITEM_MAPPING), new ShortcutType(ShortcutType.TYPE_ITEM_MAPPING, "Item mapping"));
        SHORTCUTTYPES.put(new Integer(ShortcutType.TYPE_META_MAPPING), new ShortcutType(ShortcutType.TYPE_META_MAPPING, "Meta mapping"));
    }
    
    public static final Map<Integer, BrowsingSecurity> BROWSINGSECURITY_VALUES = new LinkedHashMap<Integer, BrowsingSecurity>();

    static {
        BROWSINGSECURITY_VALUES.put(BrowsingSecurity.FULL_ACCESS, new BrowsingSecurity(BrowsingSecurity.FULL_ACCESS, "Normal access"));
        BROWSINGSECURITY_VALUES.put(BrowsingSecurity.NO_AUTHORING, new BrowsingSecurity(BrowsingSecurity.NO_AUTHORING, "No authoring"));
        BROWSINGSECURITY_VALUES.put(BrowsingSecurity.NO_BROWSING, new BrowsingSecurity(BrowsingSecurity.NO_BROWSING, "No authoring and content addressing")); 
    }
    
    public static final String FEATURE_MEDIAKEYMAPPINGS = "MEDIAKEYMAPPINGS";
    public static final String FEATURE_ELEMENTMAPPINGS = "ELEMENTMAPPINGS";
    public static final String FEATURE_ENCODERMAPPINGS = "ENCODERMAPPINGS";
    public static final String FEATURE_JOBDEFINITIONS = "JOBDEFINITIONS";
    public static final String FEATURE_REMOTEACTIONS = "REMOTEACTIONS";
    
    
    protected File _syncInfoFile;

    protected DesignDefinition _syncInfo;

    protected File _designDirectory;

    protected File _csConfigFile;

    protected CSConfig _csConfig;

    // a in memory state of the last shortcuts set on this model
    // necessary to restore shortcuts when version compliance is changed
    // multiple times without save
    private List<Shortcut> _cachedShortcuts;

    // special props for versions to avoid parsing errors in setter methods
    private String _pluginVersion;

    private int _pluginBuild;

    private String _pluginWGAVersion;

    private String _pluginJavaVersion;
    private File _csConfigFileBase;
    private CSConfig _csConfigBase;
    
    private String _minimumWGAVersion;
    private String _minimumWGAVersionSourceInfo;
    private Version _minimumWGAVersionAsVersion;

    @NotBound
    public Version getMinimumWGAVersionAsVersion() {
        return _minimumWGAVersionAsVersion;
    }

    /**
     * contains options which are direct modifable and should be filtered from custom publisher options list
     */
    public static final List<String> DIRECT_MODIFIABLE_PUBLISHER_OPTIONS = new ArrayList<String>();
    static {
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_HOME_PAGE);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_LOGIN_PAGE);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_MULTI_LANGUAGE_CONTENT);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_USES_HDB);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_DEFAULT_ITEM_ENCODING);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_DESIGN_ENCODING);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_ADMIN_APP);
    	DIRECT_MODIFIABLE_PUBLISHER_OPTIONS.add(PublisherOption.OPTION_BROWSING_SECURITY);
    }


    public WGADesignConfigurationModel(File syncInfoFile) throws IOException {
        _syncInfoFile = syncInfoFile;
        reload();
    }

    @Override
    public void saveChanges() throws IOException, IllegalAccessException, InvocationTargetException {
        normalizePublisherOptions();
        ensureCorrectCSConfigVersion();
        List<ValidationError> validationErrors = validate();
        if (validationErrors.isEmpty()) {            
            _csConfig.write(_csConfigFile);
            writeDesignDefinition();
            _cachedShortcuts = null;
        }
        else {
            throw new IOException("Configuration contains invalid values.", validationErrors.get(0));
        }
    }

    private void writeDesignDefinition() throws IOException {
        _syncInfo.setFileEncoding("csconfig");
        
        // Ensure correct file name based on the version compliance
        String correctName = _csConfig.getDesignDefinitionFileName();
        if (!_syncInfoFile.getName().equals(correctName)) {
            File newSyncInfoFile  = new File(_syncInfoFile.getParentFile(), correctName);
            if (_syncInfoFile.exists()) {
                _syncInfoFile.delete();
            }
            _syncInfoFile = newSyncInfoFile;
        }
        
        _syncInfo.write(_syncInfoFile);
    }
    
    private void normalizePublisherOptions() {
        Iterator<PublisherOption> options = getPublisherOptions().iterator();
        while (options.hasNext()) {
            PublisherOption option = options.next();
            if (PublisherOption.OPTION_HOME_PAGE.equals(option.getName()) || PublisherOption.OPTION_LOGIN_PAGE.equals(option.getName())) {
                if (option.getValue() != null && option.getValue().trim().equals("")) {
                    options.remove();
                }
            } else if (option.getValue() != null && option.getValue().equals(STRING_NOT_SET)) {
                options.remove();
            }
        }        
    }

    public void setPublisherOption(String name, String value, boolean fireModelChanged) {
        if (value != null && value.equals(STRING_NOT_SET)) {
            removePublisherOption(name, fireModelChanged);
        } else {
            PublisherOption option = getPublisherOption(name);
            if (option != null) {
                option.setValue(value);
            }
            else {
                _csConfig.getPublisherOptions().add(new PublisherOption(name, value));
            }
            if (fireModelChanged) {
                fireModelChanged();
            }
        }
    }

    public void setPublisherOption(String name, String value) {
        setPublisherOption(name, value, true);
    }


    public void setPublisherOption(String name, boolean value) {
        setPublisherOption(name, String.valueOf(value));
    }
    
    public void setPublisherOption(String name, int value) {
        setPublisherOption(name, String.valueOf(value));
    }

    public PublisherOption getPublisherOption(String name) {
        Iterator options = _csConfig.getPublisherOptions().iterator();
        while (options.hasNext()) {
            PublisherOption option = (PublisherOption) options.next();
            if (option.getName().equals(name)) {
                return option;
            }
        }
        return null;
    }
    
    @SuppressWarnings("unchecked")
    @NotBound
    public List<PublisherOption> getPublisherOptions() {
    	return _csConfig.getPublisherOptions();
    }

    private void removePublisherOption(String name, boolean fireModelChanged) {
        Iterator options = _csConfig.getPublisherOptions().iterator();
        boolean removed = false;
        while (options.hasNext()) {
            PublisherOption option = (PublisherOption) options.next();
            if (option.getName().equals(name)) {
                options.remove();
                removed = true;
            }
        }
        if (fireModelChanged && removed) {
            fireModelChanged();
        }
    }
    
    private void removePublisherOption(String name) {
        removePublisherOption(name, true);
    }

    private String getPublisherOptionValue(String name) {
        PublisherOption option = findOption(name, false);
        if (option != null) {
            return option.getValue();
        }
        else {
            return getDefaultValue(name);
        }
    }

    /**
     * checks value != null && value != ''
     */
    protected boolean isEmpty(String value) {
        if (value != null && !value.trim().equalsIgnoreCase("")) {
            return false;
        }
        else {
            return true;
        }
    }

    public String getInitScript() {
        return _csConfig.getInitScript();
    }

    public String getConnectionScript() {
        return _csConfig.getConnectionScript();
    }

    public void setInitScript(String script) {
        _csConfig.setInitScript(script);
        fireModelChanged();
    }

    public void setConnectionScript(String script) {
        _csConfig.setConnectionScript(script);
        fireModelChanged();
    }
    public void setDisconnectionScript(String script) {
    	//_disconnectionScript = script;
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
            ((de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _csConfig).setDisconnectionScript(script);            
        }
        fireModelChanged();
    }
    
    public void setPluginClearDatabaseOnUpdate(boolean value) {
        if (_csConfig.getPluginConfig() != null && _csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
            ((de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) _csConfig.getPluginConfig()).setClearDatabaseOnUpdate(value);
        }
        fireModelChanged();
    }
    
    public void setPluginNoDatabase(boolean value) {
        if (_csConfig.getPluginConfig() != null && _csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) {
            ((de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) _csConfig.getPluginConfig()).getOptions().put(de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig.OPTION_NO_DATABASE, String.valueOf(value));
        }
        fireModelChanged();
    }
    
    public void setPluginDisableInit(boolean value) {
        if (_csConfig.getPluginConfig() != null && _csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
            ((de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) _csConfig.getPluginConfig()).setDisablePluginInit(value);            
        }
        fireModelChanged();
    }

    @DataProvider(objectField = "anonymousAccessLevel")
    public List<AccessLevel> provideAnonymousAccessLevels() {
        Map<Integer, AccessLevel> levels = new HashMap<Integer, AccessLevel>();
        levels.putAll(ACCESSLEVELS);        
        if (getVersionCompliance().toWGAVersion().isAtLeast(6, 0)) {
            levels.remove(AccessLevel.LEVEL_READER_DESIGNER);
            levels.remove(AccessLevel.LEVEL_AUTHOR_DESIGNER);
            levels.remove(AccessLevel.LEVEL_EDITOR_DESIGNER);            
        }
        List<AccessLevel> list = new ArrayList<AccessLevel>(levels.values());
        return WGUtils.sortByProperty(list, "key");
    }

    @DataProvider(objectField = "defaultAccessLevel")
    public List<AccessLevel> provideDefaultAccessLevels() {
        Map<Integer, AccessLevel> levels = new HashMap<Integer, AccessLevel>();
        levels.putAll(ACCESSLEVELS);        
        if (getVersionCompliance().toWGAVersion().isAtLeast(6, 0)) {
            levels.remove(AccessLevel.LEVEL_READER_DESIGNER);
            levels.remove(AccessLevel.LEVEL_AUTHOR_DESIGNER);
            levels.remove(AccessLevel.LEVEL_EDITOR_DESIGNER);            
        }
        List<AccessLevel> list = new ArrayList<AccessLevel>(levels.values());
        return WGUtils.sortByProperty(list, "key");
    }
    
    @SuppressWarnings("unchecked")
    public List<AccessLevel> provideRemoteActionsAccessLevels() {
        Map<Integer, AccessLevel> levels = new HashMap<Integer, AccessLevel>();
        levels.putAll(ACCESSLEVELS_REMOTE_ACTIONS);        
        if (getVersionCompliance().toWGAVersion().isAtLeast(6, 0)) {
            levels.remove(AccessLevel.LEVEL_READER_DESIGNER);
            levels.remove(AccessLevel.LEVEL_AUTHOR_DESIGNER);
            levels.remove(AccessLevel.LEVEL_EDITOR_DESIGNER);            
        }
        List<AccessLevel> list = new ArrayList<AccessLevel>(levels.values());
        return WGUtils.sortByProperty(list, "key");
    }

    @DataProvider(objectField = "versionCompliance")
    public List<VersionCompliance> provideVersionCompliances() {
        return new ArrayList<VersionCompliance>(VersionCompliance.VERSIONCOMPLIANCES.values());  
    }

 public AccessLevel getAnonymousAccessLevel() {
        return ACCESSLEVELS.get(_csConfig.getAnonymousAccessLevel());
    }

    public void setAnonymousAccessLevel(AccessLevel level) {
        _csConfig.setAnonymousAccessLevel(level.getKey());
        fireModelChanged();
    }

    public AccessLevel getDefaultAccessLevel() {
        AccessLevel level = ACCESSLEVELS.get(_csConfig.getDefaultAccessLevel());
        return level;
    }

    public VersionCompliance getVersionCompliance() {
        if (isOverlay() && _csConfigBase != null) {
            return VersionCompliance.get(_csConfigBase.getVersionCompliance());
        } else {
            return VersionCompliance.get(_csConfig.getVersionCompliance());
        }
    }

    public void setDefaultAccessLevel(AccessLevel level) {
        _csConfig.setDefaultAccessLevel(level.getKey());
        fireModelChanged();
    }

    
    public void setVersionCompliance(VersionCompliance vc) throws IllegalAccessException, InvocationTargetException {
        _csConfig.setVersionCompliance(vc.getKey());        
        ensureCorrectCSConfigVersion();
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            Version v = ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig)_csConfig).getMinimumWGAVersion();
            if (v != null && !v.isAtLeast(vc.toWGAVersion())) {
                ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) _csConfig).setMinimumWGAVersion(vc.toWGAVersion());
            }
        }
        loadVersionProperties();
        fireModelChanged();
    }

    @NotBound
    public File getTMLScriptDirectory() {
        return new File(_designDirectory, "scripts/tmlscript");
    }

    public String getDesignKey() {
        return _syncInfo.getDesignKey();
    }

    public String getDesignDirectory() {
        return _designDirectory.getAbsolutePath();
    }

    @NotBound
    public String[] getLibraryNames() {
        File systemDir = new File(_designDirectory, "files/system");
        String[] names = systemDir.list(new FilenameFilter() {

            public boolean accept(File dir, String name) {
                return name.endsWith(".jar");
            }

        });
        return names;
    }

    public void addLibrary(File file) throws IOException {
        File systemDir = new File(_designDirectory, "files/system");
        File target = new File(systemDir, file.getName());
        FileInputStream in = new FileInputStream(file);
        FileOutputStream out = new FileOutputStream(target);
        WGUtils.inToOut(in, out, 2048);
        in.close();
        out.close();
    }

    public boolean hasLibrary(String name) {
        return Arrays.asList(getLibraryNames()).contains(name);
    }

    public boolean deleteLibrary(String filename) {
        File systemDir = new File(_designDirectory, "files/system");
        return new File(systemDir, filename).delete();
    }

    public String getHomePage() {
        return getPublisherOptionValue(PublisherOption.OPTION_HOME_PAGE);
    }

    public void setHomePage(String page) {
        setPublisherOption(PublisherOption.OPTION_HOME_PAGE, page);
    }

    public String getLoginPage() {
        return getPublisherOptionValue(PublisherOption.OPTION_LOGIN_PAGE);
    }

    public void setLoginPage(String page) {
        setPublisherOption(PublisherOption.OPTION_LOGIN_PAGE, page);
    }

    public boolean isMultiLanguageContent() {
        return Boolean.parseBoolean(getPublisherOptionValue(PublisherOption.OPTION_MULTI_LANGUAGE_CONTENT));
    }

    public void setMultiLanguageContent(boolean enabled) {
        setPublisherOption(PublisherOption.OPTION_MULTI_LANGUAGE_CONTENT, enabled);
    }

    @SuppressWarnings("unchecked")
    @DataProvider(objectField = "defaultItemEncoding")
    public List<Encoding> populateItemEncodings() {

        List<Encoding> encodings = new ArrayList<Encoding>(ITEMENCODINGS.values());

        Iterator<EncoderMapping> customEncoders = _csConfig.getEncoderMappings().iterator();
        while (customEncoders.hasNext()) {
            EncoderMapping element = customEncoders.next();
            encodings.add(new Encoding(element.getName(), element.getName()));
        }

        return encodings;
    }

    public Encoding getDefaultItemEncoding() {
        String encValue = getPublisherOptionValue(PublisherOption.OPTION_DEFAULT_ITEM_ENCODING);
        Iterator<Encoding> encodings = populateItemEncodings().iterator();
        while (encodings.hasNext()) {
            Encoding encoding = encodings.next();
            if (encoding.getKey().equals(encValue)) {
                return encoding;
            }
        }
        return null;
    }

    public void setDefaultItemEncoding(Encoding encoding) {
        setPublisherOption(PublisherOption.OPTION_DEFAULT_ITEM_ENCODING, encoding.getKey());
    }

    @DataProvider(objectField = "designEncoding")
    public List<Encoding> populateDesignEncodings() {
        List<Encoding> encodings = new ArrayList<Encoding>(DESIGNENCODINGS.values());

        String currentEncodingValue = retrieveDesignEncodingValue();
        if (currentEncodingValue != null) {
            Encoding currentEncoding = new Encoding(currentEncodingValue, currentEncodingValue);
            if (!DESIGNENCODINGS.containsKey(currentEncodingValue)) {
                encodings.add(currentEncoding);
            }
        }

        return encodings;
    }

    public Encoding getDesignEncoding() {
        String encValue = retrieveDesignEncodingValue();

        Iterator<Encoding> encodings = populateDesignEncodings().iterator();
        while (encodings.hasNext()) {
            Encoding encoding = encodings.next();
            if (encoding.getKey().equals(encValue)) {
                return encoding;
            }
        }
        return null;
    }

    @NotBound
    private String retrieveDesignEncodingValue() {
        String encValue = getPublisherOptionValue(PublisherOption.OPTION_DESIGN_ENCODING);
        if (encValue == null || encValue.trim().equals("")) {
            encValue = STRING_NOT_SET;
        }
        return encValue;
    }

    public void setDesignEncoding(Encoding encoding) {
        if (encoding != null) {
            setPublisherOption(PublisherOption.OPTION_DESIGN_ENCODING, encoding.getKey());
        }
    }

//    @DataProvider(objectField = "expressionDefault")
//    public List<ExpressionLanguage> populateExpressionLanguages() {
//        return new ArrayList<ExpressionLanguage>(EXPRESSION_LANGUAGES.values());
//    }

//    public ExpressionLanguage getExpressionDefault() {
//        String xpCode = getPublisherOptionValue(PublisherOption.OPTION_EXPRESSION_DEFAULT);
//        return (ExpressionLanguage) findBeanByKey(xpCode, populateExpressionLanguages());
//    }

//    public void setExpressionDefault(ExpressionLanguage language) {
//        setPublisherOption(PublisherOption.OPTION_EXPRESSION_DEFAULT, language.getKey());
//    }

    private String getDefaultValue(String name) {

        if (name.equals(PublisherOption.OPTION_DEFAULT_ITEM_ENCODING)) {
            return STRING_NOT_SET;
        }
        else if (name.equals(PublisherOption.OPTION_DEFAULT_MEDIA_KEY)) {
            return Encoding.ENCODING_HTML;
        }
        else if (name.equals(PublisherOption.OPTION_EXPRESSION_DEFAULT)) {
            return STRING_NOT_SET;
        }
        else if (name.equals(PublisherOption.OPTION_MULTI_LANGUAGE_CONTENT)) {
            return "true";
        }
        else if (name.equals(PublisherOption.OPTION_DESIGN_ENCODING)) {
            return STRING_NOT_SET;
        }
        else if (name.equals(PublisherOption.OPTION_ADMIN_APP)) {
            return "false";
        }
        else if (name.equals(PublisherOption.OPTION_BROWSING_SECURITY)) {
            return "90";
        }
        else {
            return "";
        }
    }

    private PublisherOption findOption(String name, boolean create) {
        Iterator<PublisherOption> optionIt = _csConfig.getPublisherOptions().iterator();
        while (optionIt.hasNext()) {
            PublisherOption option = optionIt.next();
            if (option.getName().equals(name)) {
                return option;
            }
        }

        if (create) {
            PublisherOption option = new PublisherOption();
            option.setName(name);
            _csConfig.getPublisherOptions().add(option);
            return option;
        }
        else {
            return null;
        }

    }

    private Object findBeanByKey(String key, List list) {
        Iterator beans = list.iterator();
        while (beans.hasNext()) {
            KeyValueBean<String, Object> bean = (KeyValueBean<String, Object>) beans.next();
            if (bean.getKey().equals(key)) {
                return bean;
            }
        }
        return null;
    }

//    public String getDefaultMediaKey() {
//        return getPublisherOptionValue(PublisherOption.OPTION_DEFAULT_MEDIA_KEY);
//    }
//
//    public void setDefaultMediaKey(String key) {
//        setPublisherOption(PublisherOption.OPTION_DEFAULT_MEDIA_KEY, key);
//    }

    public boolean isDirectAccessDefault() {
        return Boolean.parseBoolean(getPublisherOptionValue(PublisherOption.OPTION_DIRECT_ACCESS_DEFAULT));
    }
//
//    public void setDirectAccessDefault(boolean value) {
//        setPublisherOption(PublisherOption.OPTION_DIRECT_ACCESS_DEFAULT, value);
//    }

    public boolean isUsesHDB() {
    	if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
    		return true;
    	} else if (_csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA3)) {
    		return false;
    	}
        return Boolean.parseBoolean(getPublisherOptionValue(PublisherOption.OPTION_USES_HDB));
    }

    public void setUsesHDB(boolean value) {    	
        setPublisherOption(PublisherOption.OPTION_USES_HDB, value);
    }

    @NotBound
    public boolean hasPluginConfig() {
        return _csConfig.getPluginConfig() != null;
    }

    public void createPluginConfig() {
        PluginConfig config =  PluginConfig.instantiatePluginConfigForCompliance(_csConfig);
        config.getId().getVersion().setBuildVersion(1);
        
        // default pers mode for WGA versions < 6.0
        config.setPersonalisationMode(Constants.PERSMODE_AUTO);
        
        if (getVersionCompliance() != null) {
            Version minWGAVersion = getVersionCompliance().toWGAVersion();
            if (minWGAVersion != null) {
                config.setMinimumWGAVersion(minWGAVersion);
            }
                        
            
            // OpenWGA 5.0 requires java 1.5
            if (minWGAVersion.isAtLeast(5, 0)) {
                config.setMinimumJavaVersion(new Version("1.5.0"));
            }  
            
            // OpenWGA 6.0 requires Java 6
            if (minWGAVersion.isAtLeast(6, 0)) {
                config.setMinimumJavaVersion(new Version("1.6.0"));
                config.setPersonalisationMode(Constants.PERSMODE_SESSION);
            }
            
            // OpenWGA 7.2 requires Java 7
            if (minWGAVersion.isAtLeast(7, 2)) {
                config.setMinimumJavaVersion(new Version("1.7.0"));
            }
        }                      
        
        _csConfig.setPluginConfig(config);

        loadPluginVersionProperties();

        fireModelChanged();
    }

    private void loadPluginVersionProperties() {       
        if (_csConfig.getPluginConfig() != null) {
            _pluginVersion = _csConfig.getPluginConfig().getId().getVersion().getMainVersionString();
            _pluginBuild = _csConfig.getPluginConfig().getId().getVersion().getBuildVersion();
            _pluginWGAVersion = _csConfig.getPluginConfig().getMinimumWGAVersion().getMainVersionString();
            _pluginJavaVersion = _csConfig.getPluginConfig().getMinimumJavaVersion().toString();
        }
    }
    
    private void loadVersionProperties() {
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            Version v = ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig)_csConfig).getMinimumWGAVersion();
            if (v != null) {
                _minimumWGAVersionAsVersion = v;                
            } else {
                _minimumWGAVersionAsVersion = getVersionCompliance().toWGAVersion();
            }
            _minimumWGAVersionSourceInfo = "";
        } else if (hasPluginConfig()) {
            _minimumWGAVersionAsVersion = _csConfig.getPluginConfig().getMinimumWGAVersion();
            _minimumWGAVersionSourceInfo = "* determined by plugin configuration";
        } else if (isOverlay() && _csConfigBase != null && _csConfigBase.getPluginConfig() != null) {
            _minimumWGAVersionAsVersion = _csConfigBase.getPluginConfig().getMinimumWGAVersion();
            _minimumWGAVersionSourceInfo = "* determined by plugin configuration from base design";
        } else {
            _minimumWGAVersionAsVersion = getVersionCompliance().toWGAVersion();
            if (isOverlay() && _csConfigBase != null) {
                _minimumWGAVersionSourceInfo = "* determined by version compliance of base design";
            } else {
                _minimumWGAVersionSourceInfo = "* determined by version compliance";
            }
        }
        _minimumWGAVersion = _minimumWGAVersionAsVersion.getMainVersionString();
    }
    

    public String getMinimumWGAVersionSourceInfo() {
        return _minimumWGAVersionSourceInfo;
    }

    public void setMinimumWGAVersionSourceInfo(String minimumWGAVersionSourceInfo) {
        _minimumWGAVersionSourceInfo = minimumWGAVersionSourceInfo;
    }

    public void removePluginConfig() {
        _csConfig.setPluginConfig(null);
        _pluginBuild = 0;
        _pluginJavaVersion = null;
        _pluginVersion = null;
        _pluginWGAVersion = null;
        fireModelChanged();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<PluginID> getPluginDependencies() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getDependencies();
        }
        else {
            return Collections.EMPTY_LIST;
        }
    }

    public String getPluginUniqueName() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getId().getUniqueName();
        }
        else {
            return null;
        }
    }

    public void setPluginUniqueName(String uname) {
        if (uname != null) {
            uname = uname.trim();
        }
        _csConfig.getPluginConfig().getId().setUniqueName(uname);
        fireModelChanged();
    }

    public String getPluginJavaVersion() {
        if (hasPluginConfig()) {
            return _pluginJavaVersion;
        }
        else {
            return null;
        }
    }

    public void setPluginJavaVersion(String version) {
        _pluginJavaVersion = version;
        fireModelChanged();
    }

    public String getPluginVersion() {
        if (hasPluginConfig()) {
            return _pluginVersion;
        }
        else {
            return null;
        }
    }

    public void setPluginVersion(String version) {
        _pluginVersion = version;
        fireModelChanged();
    }

    public int getPluginBuild() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getId().getVersion().getBuildVersion();
        }
        else {
            return -1;
        }
    }

    public void setPluginBuild(int build) {
        _pluginBuild = build;
        fireModelChanged();
    }

    public String getPluginWGAVersion() {
        if (hasPluginConfig()) {
            return _pluginWGAVersion;
        }
        else {
            return null;
        }
    }

    public void setPluginWGAVersion(String version) {
        _pluginWGAVersion = version;
        fireModelChanged();
    }
    
    public String getMinimumWGAVersion() {        
        return _minimumWGAVersion;
    }
    
    public void setMinimumWGAVersion(String version) throws IllegalAccessException, InvocationTargetException {
        _minimumWGAVersion = version;
        
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            _pluginWGAVersion = version;
        }
        fireModelChanged();
    }

    public String getPluginTitle() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getTitle();
        }
        else {
            return null;
        }
    }

    public void setPluginTitle(String title) {
        _csConfig.getPluginConfig().setTitle(title);
        fireModelChanged();
    }

    public String getPluginVendor() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getVendor();
        }
        else {
            return null;
        }
    }

    public void setPluginVendor(String vendor) {
        _csConfig.getPluginConfig().setVendor(vendor);
        fireModelChanged();
    }

    public String getPluginVendorHomepage() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getWebHomepage();
        }
        else {
            return null;
        }
    }

    public void setPluginVendorHomepage(String homepage) {
        _csConfig.getPluginConfig().setWebHomepage(homepage);
        fireModelChanged();
    }

    public String getPluginDescription() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getDescription();
        }
        else {
            return null;
        }
    }

    public void setPluginDescription(String description) {
        _csConfig.getPluginConfig().setDescription(description);
        fireModelChanged();
    }

    public String getPluginHomepage() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().getPluginHomepage();
        }
        else {
            return null;
        }
    }

    public void setPluginHomepage(String page) {
        _csConfig.getPluginConfig().setPluginHomepage(page);
        fireModelChanged();
    }

    @DataProvider(objectField = "pluginAuthenticationSource")
    public List<AuthPlugin> populatePluginAuthenticationSources() {

        List<AuthPlugin> list = new ArrayList<AuthPlugin>();
        list.add(EMPTY_AUTH_PLUGIN);
        list.add(new AuthPlugin(PluginConfig.AUTHSOURCE_DEFAULT_DOMAIN, "Default Domain"));

        if (isPluginUsageAsAuthenticationSource()) {
            list.add(new AuthPlugin(_csConfig.getPluginConfig().getId().getUniqueName(), "Myself"));
        }

        Iterator<PluginID> dependencies = getPluginDependencies().iterator();
        while (dependencies.hasNext()) {
            PluginID id = dependencies.next();
            list.add(new AuthPlugin(id.getUniqueName(), id.getUniqueName()));
        }
        return list;
    }

    public AuthPlugin getPluginAuthenticationSource() {
        if (hasPluginConfig()) {
            String authString = _csConfig.getPluginConfig().getAuthentication();
            if (authString == null || authString.trim().equalsIgnoreCase("")) {
                authString = STRING_NOT_SET;
            }
            Iterator<AuthPlugin> authPlugins = populatePluginAuthenticationSources().iterator();
            while (authPlugins.hasNext()) {
                AuthPlugin plugin = authPlugins.next();
                if (plugin.getKey().equals(authString)) {
                    return plugin;
                }
            }
            return EMPTY_AUTH_PLUGIN;
        }
        else {
            return null;
        }
    }

    public void setPluginAuthenticationSource(AuthPlugin auth) {
        if (auth != null && !EMPTY_AUTH_PLUGIN.equals(auth)) {
            _csConfig.getPluginConfig().setAuthentication(auth.getKey());
            fireModelChanged();
        }
        else if (EMPTY_AUTH_PLUGIN.equals(auth)) {
            _csConfig.getPluginConfig().setAuthentication(null);
            fireModelChanged();
        }
    }

    public boolean isPluginUsageAsAuthenticationSource() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().isUsageAsAuthSource();
        }
        else {
            return false;
        }
    }
    
    public boolean isStaticClasspath() {
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
            return ((de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _csConfig).isStaticClasspath();
        } else {
        	return false;
        }
        
    }
    
    public void setStaticClasspath(boolean value) {
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
            ((de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _csConfig).setStaticClasspath(value);
        }
        fireModelChanged();
    }

    public void setPluginUsageAsAuthenticationSource(boolean enabled) {
        _csConfig.getPluginConfig().setUsageAsAuthSource(enabled);
        fireModelChanged();
    }

    public boolean isPluginUsageAsContentStore() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().isUsageAsContentStore();
        }
        else {
            return false;
        }
    }

    public void setPluginUsageAsContentStore(boolean enabled) {
        _csConfig.getPluginConfig().setUsageAsContentStore(enabled);
        fireModelChanged();
    }

    public boolean isPluginUsageAsDesignProvider() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().isUsageAsDesignProvider();
        }
        else {
            return false;
        }
    }

    public void setPluginUsageAsDesignProvider(boolean enabled) {
        _csConfig.getPluginConfig().setUsageAsDesignProvider(enabled);
        fireModelChanged();
    }

    @DataProvider(objectField = "pluginPersonalisationMode")
    public List<PersonalisationMode> populatePluginPersonalisationModes() {
        List<PersonalisationMode> modes = new ArrayList<PersonalisationMode>(PERSONALISATION_MODES.values());
        return modes;
    }

    public PersonalisationMode getPluginPersonalisationMode() {
        if (hasPluginConfig()) {
            int mode = _csConfig.getPluginConfig().getPersonalisationMode();
            Iterator<PersonalisationMode> encodings = populatePluginPersonalisationModes().iterator();
            while (encodings.hasNext()) {
                PersonalisationMode persMode = encodings.next();
                if (persMode.getKey().equals(mode)) {
                    return persMode;
                }
            }
            return null;
        }
        else {
            return null;
        }
    }

    public void setPluginPersonalisationMode(PersonalisationMode mode) {
        if (mode != null) {
            _csConfig.getPluginConfig().setPersonalisationMode(mode.getKey());
            fireModelChanged();
        }
    }

    public boolean isAllowContentAuthoring() {
        if (hasPluginConfig()) {
            return _csConfig.getPluginConfig().isShowOnStartPage();
        }
        else {
            return false;
        }
    }

    public void setAllowContentAuthoring(boolean enabled) {
        _csConfig.getPluginConfig().setShowOnStartPage(enabled);
        fireModelChanged();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<EncoderMapping> getEncoderMappings() {
        return _csConfig.getEncoderMappings();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<ElementMapping> getElementMappings() {
        return _csConfig.getElementMappings();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<MediaKey> getMediaKeys() {
        return _csConfig.getMediaKeys();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<RemoteAction> getRemoteActions() {
        return _csConfig.getRemoteActions();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<ACLRole> getACLRoles() {
        return _csConfig.getRoles();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<JobDefinition> getJobDefinitions() {
        return _csConfig.getJobDefinitions();
    }

    @SuppressWarnings("unchecked")
    @NotBound
    public List<Shortcut> getShortcuts() {
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) {
            return ((de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) _csConfig).getShortcuts();
        }
        return null;
    }
    
    public String getDisconnectionScript() {
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
            return ((de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _csConfig).getDisconnectionScript();
        }
        return null;
    }
    
    public boolean isPluginClearDatabaseOnUpdate() {    	
        if (_csConfig.getPluginConfig() != null && _csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
        	return ((de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) _csConfig.getPluginConfig()).isClearDatabaseOnUpdate();
        }
        return false;
    }
    
    public boolean isPluginNoDatabase() {        
        if (_csConfig.getPluginConfig() != null && _csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) {
            return "true".equals(((de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) _csConfig.getPluginConfig()).getOptions().get(de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig.OPTION_NO_DATABASE));
        }
        return false;
    }
    
    public boolean isPluginDisableInit() {
        if (_csConfig.getPluginConfig() != null && _csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
            return ((de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) _csConfig.getPluginConfig()).isDisablePluginInit();
        }
        return false;
    }

    public boolean isFeatureSupported(String featureID) {
        if (FEATURE_SHORTCUTS.equals(featureID)) {
            return (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig);
        } else if (FEATURE_MEDIAKEYMAPPINGS.equals(featureID)) {
            return true;
        } else if (FEATURE_ELEMENTMAPPINGS.equals(featureID)) {
            if (isOverlay()) {
                return _minimumWGAVersionAsVersion.isAtLeast(7, 0);
            } else {
                return true;
            }
        } else if (FEATURE_ENCODERMAPPINGS.equals(featureID)) {
            if (isOverlay()) {
                return _minimumWGAVersionAsVersion.isAtLeast(7, 0);
            } else {
                return true;
            }
        } else if (FEATURE_JOBDEFINITIONS.equals(featureID)) {
            if (isOverlay()) {
                return _minimumWGAVersionAsVersion.isAtLeast(7, 0);
            } else {
                return true;
            }
        } else if (FEATURE_REMOTEACTIONS.equals(featureID)) {
            return !isOverlay();
        }
        else {
            return false;
        }
    }

    public File getPluginExportFile(File targetDir, boolean shortFileName) {
        String fileName;
        if (shortFileName) {
            fileName = _csConfig.getPluginConfig().getId().buildShortFileName();
        }
        else {
            fileName = _csConfig.getPluginConfig().getId().buildQualifiedFileName();
        }
        return new File(targetDir, fileName);
    }

    public void exportPlugin(File pluginFile, int buildnumber, boolean obfuscate, boolean increaseBuild, String javaSourceFolder) throws IOException, PersistentKeyException, GeneralSecurityException, IllegalAccessException, InvocationTargetException {                                
            // Update build number
            if (_pluginBuild != buildnumber) {
                setPluginBuild(buildnumber);
                saveChanges();
            }

            ZipOutputStream stream = createZIP(pluginFile, obfuscate, javaSourceFolder);
            stream.close();
            
            // Increment build version
            if (increaseBuild) {
            	setPluginBuild(getPluginBuild() + 1);
            	saveChanges();   
            }
                         	
    }
    
    public void exportPlugin(File pluginFile, int buildnumber, boolean obfuscate, boolean increaseBuild) throws IOException, PersistentKeyException, GeneralSecurityException, IllegalAccessException, InvocationTargetException {
        exportPlugin(pluginFile, buildnumber, obfuscate, increaseBuild, null);
    }
    
    public File getDesignExportFile(File targetDir) {
    	return new File(targetDir, _syncInfo.getDesignKey() + ".wgadesign");
    }
    
    public void exportDesign(File file, Properties buildProperties) throws FileNotFoundException, PersistentKeyException, GeneralSecurityException, IOException {
    	ZipOutputStream stream = createZIP(file, false, null);
    	ZipEntry buildPropertiesEntry = new ZipEntry("files/system/build.properties");
    	stream.putNextEntry(buildPropertiesEntry);
    	ByteArrayOutputStream propertiesContent = new ByteArrayOutputStream();
    	buildProperties.store(propertiesContent, null);
    	stream.write(propertiesContent.toByteArray());
    	stream.close();
    }
    
    public Properties createBuildProperties() {
    	Properties properties = new Properties();
    	properties.setProperty(BUILD_PROP_SIGNATURE,  System.getProperty("user.name") + "_" + new SimpleDateFormat("yyyyMMdd_HHmm").format(new Date()));
    	return properties;
    }

	private ZipOutputStream createZIP(File zipFile, boolean obfuscate, String javaSourceFolder) throws FileNotFoundException, PersistentKeyException, GeneralSecurityException, IOException {
    	if (!zipFile.exists()) {
    		zipFile.createNewFile();
    	}

		ZipOutputStream zipOutputStream = new ZipOutputStream(new FileOutputStream(zipFile));
		zipOutputStream.setLevel(0);
		
		DirZipper zipper = new DirZipper();
		zipper.setInputStreamProvider(new PluginISProvider(_designDirectory, obfuscate));
		zipper.addFilePatternToIgnore("^\\..*$");
		
		// Zip all folders and files that belong into an exported plugin
		List<File> filesToZip = new ArrayList<File>();
		filesToZip.add(new File(_designDirectory, DesignDirectory.FOLDERNAME_TML));
		filesToZip.add(new File(_designDirectory, DesignDirectory.FOLDERNAME_SCRIPT));
		filesToZip.add(new File(_designDirectory, DesignDirectory.FOLDERNAME_FILES));
		filesToZip.add(new File(_designDirectory, DesignDirectory.SYNCINFO_FILE));
		filesToZip.add(new File(_designDirectory, DesignDirectory.DESIGN_DEFINITION_FILE));
		Iterator<File> files = filesToZip.iterator();
		while (files.hasNext()) {
		    File file = (File) files.next();
		    if (file.exists()) {
		        if (file.isDirectory()) {
		            zipper.zipDirectory(file, zipOutputStream, file.getName() + "/");
		        }
		        else {
		            zipper.zipFile(file, zipOutputStream);
		        }
		        
		    }
		}
		
		// If the design directory has a java classes folder we put those into a jar and zip them to "files/system"
		File javaClassesDir = new File(_designDirectory, DesignDirectory.FOLDERNAME_JAVA);
		if (javaClassesDir.exists() && javaClassesDir.isDirectory()) {
		    TemporaryFile jar = new TemporaryFile("plugin-classes.jar", null, null);
		    ZipOutputStream jarStream = new ZipOutputStream(new FileOutputStream(jar.getFile()));
		    jarStream.setLevel(0);
		    DirZipper jarZipper = new DirZipper();
		    jarZipper.addFilePatternToIgnore("^\\..*$");
		    jarZipper.zipDirectory(javaClassesDir, jarStream);
		    
		    jarStream.close();
		    
		    zipper.zipNormalFile(jar.getFile(), zipOutputStream, "files/system/");
		    
		    jar.delete();
		}
		
		// If we were given a java source folder we put its contents into plugin-sources.zip and put that into "files/system" (for OSS plugins)
        if (javaSourceFolder != null) {
            File javaSourceDir = new File(javaSourceFolder);
            TemporaryFile zip = new TemporaryFile("plugin-sources.zip", null, null);
            ZipOutputStream zipStream = new ZipOutputStream(new FileOutputStream(zip.getFile()));
            zipStream.setLevel(0);
            DirZipper jarZipper = new DirZipper();
            jarZipper.addFilePatternToIgnore("^\\..*$");
            jarZipper.zipDirectory(javaSourceDir, zipStream);
            
            zipStream.close();
            
            zipper.zipNormalFile(zip.getFile(), zipOutputStream, "files/system/");
            
            zip.delete();
        }
		

		

		if (obfuscate) {
		    ZipEntry anEntry = new ZipEntry(DesignDirectory.OBFUSCATE_FLAGFILE);
		    zipOutputStream.putNextEntry(anEntry);
		    zipOutputStream.write("Obfuscation flagfile".getBytes());
		}
		
		return zipOutputStream;
	}

    @Override
    public void reload() throws IOException {
        _syncInfo = DesignDefinition.load(_syncInfoFile);
        _designDirectory = _syncInfoFile.getParentFile();
        _csConfigFile = new File(_designDirectory, "files/system/csconfig.xml");
        if (!_csConfigFile.exists()) {
            File files = new File(_designDirectory, "files");
            if (!files.exists()) {
                files.mkdir();
            }
            File system = new File(files, "system");
            if (!system.exists()) {
                system.mkdir();
            }
            _csConfig = new CSConfig();
            _csConfig.write(_csConfigFile);
        }
        else {
            try {
                _csConfig = CSConfig.load(_csConfigFile);
            }
            catch (InvalidCSConfigVersionException e) {
                IOException io = new IOException("Invalid cs config version: " + e.getMessage());
                io.setStackTrace(e.getStackTrace());
                throw io;
            }
        }

        _csConfigFileBase = new File(_designDirectory, "files/system/base-csconfig.xml");
        if (_csConfigFileBase.exists()) {
            try {
                _csConfigBase = CSConfig.load(_csConfigFileBase);
            }
            catch (InvalidCSConfigVersionException e) {
                IOException io = new IOException("Invalid base cs config version: " + e.getMessage());
                io.setStackTrace(e.getStackTrace());
                throw io;
            }
        }                

        try {
            ensureCorrectCSConfigVersion();
        }
        catch (Exception e) {
            IOException ioe = new IOException(e.getMessage());
            ioe.setStackTrace(e.getStackTrace());
            throw ioe;
        }
        
        loadVersionProperties();        
        loadPluginVersionProperties();
        
        // migrate design encoding to csConfig
        String encValue = _syncInfo.getFileEncoding();
        if (encValue != null && !encValue.trim().equalsIgnoreCase("csconfig")) {
            _syncInfo.setFileEncoding(null);
            if (encValue.trim().equals("")) {
                removePublisherOption(PublisherOption.OPTION_DESIGN_ENCODING);
            }
            else {
                setPublisherOption(PublisherOption.OPTION_DESIGN_ENCODING, encValue, false);
            }
        } else if (encValue == null) {
        	// design encoding is not defined in _syncinfo -> syncinfo has priority so change model to reflect correct encoding
        	setPublisherOption(PublisherOption.OPTION_DESIGN_ENCODING, STRING_NOT_SET, false);
        }
        
        
    }
    
    @Override
    public List<ValidationError> validate() {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        
        // validate publisher options only valid with defined values
        PublisherOption pOption = getPublisherOption(PublisherOption.OPTION_BROWSING_SECURITY);
        if (pOption != null) {
            try {
                int browsingSecurity = Integer.parseInt(pOption.getValue());
                if (BROWSINGSECURITY_VALUES.get(browsingSecurity) == null) {
                    errors.add(new ValidationError("Unknown value '" + browsingSecurity + "' for option 'BrowsingSecurity'.", new String[] { "publisherOptions" }));    
                }
            }
            catch (NumberFormatException e) { 
                errors.add(new ValidationError("Cannot interpret 'BrowsingSecurity' as int.", new String[] { "publisherOptions" }));
            }
        }
        if (!errors.isEmpty()) {
            return errors;
        }
        
        
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            try {
                Version newV = new Version(_minimumWGAVersion);
                ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig)_csConfig).setMinimumWGAVersion(newV);
            }
            catch (RuntimeException e) {
                // unparsable version
                errors.add(new ValidationError("Invalid version format.", new String[] { "minimumWGAVersion" }));
            }            
        }
        
        if (hasPluginConfig()) {
            try {
                Version newV = new Version(_pluginVersion);
                newV.setBuildVersion(_pluginBuild);
                _csConfig.getPluginConfig().getId().setVersion(newV);
            }
            catch (RuntimeException e) {
                // unparsable version
                errors.add(new ValidationError("Invalid version format.", new String[] { "pluginVersion" }));
            }

            
            try {
                Version newV = new Version(_pluginWGAVersion);
                _csConfig.getPluginConfig().setMinimumWGAVersion(newV);
                loadVersionProperties();
            }
            catch (RuntimeException e) {
                if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig)) {
                    // unparsable version
                    errors.add(new ValidationError("Invalid version format.", new String[] { "pluginWGAVersion" }));
                }
            }
            

            try {
                Version newV = new Version(_pluginJavaVersion);
                _csConfig.getPluginConfig().setMinimumJavaVersion(newV);
            }
            catch (RuntimeException e) {
                // unparsable version
                errors.add(new ValidationError("Invalid version format.", new String[] { "pluginJavaVersion" }));
            }
            
            if (isPluginDisableInit()) {
                if (!(_csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig)) {
                    errors.add(new ValidationError("Plugin initialisation functionalities can only be disabled with WGA5 compatibility or higher.", new String[] { "versionCompliance", "pluginDisableInit"}));
                }
            }
            
            if (isPluginClearDatabaseOnUpdate()) {
                if (!(_csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig)) {
                    errors.add(new ValidationError("Clear plugin database on plugin update can only be enabled with WGA5 compatibility or higher.", new String[] { "versionCompliance", "pluginClearDatabaseOnUpdate"}));
                }
            }
            
            if (getPluginPersonalisationMode() != null && getPluginPersonalisationMode().getKey().equals(Constants.PERSMODE_SESSION)) {
                if (!getVersionCompliance().toWGAVersion().isAtLeast(6, 0)) {
                    errors.add(new ValidationError("Session based personalisation requires WGA 6.0 or higher.", new String[] { "versionCompliance", "pluginPersonalisationMode"}));
                }
            }
            
            if (isOverlay() && _csConfigBase != null) {                
                // check if base plugin is in dependency list
                boolean baseDependencyDefined = false;
                PluginID baseID = _csConfigBase.getPluginConfig().getId();                
                for (PluginID dependency : getPluginDependencies()) {
                    if (dependency.getUniqueName().equals(baseID.getUniqueName()) && dependency.getVersion().isAtLeast(baseID.getVersion())) {
                        baseDependencyDefined = true;
                        break;
                    }
                }
                
                if (!baseDependencyDefined) {
                    errors.add(new ValidationError("Dependency to base plugin '" + baseID.getUniqueName() + " (>=" + baseID.getVersion().getMainVersionString() + ")' is not defined.", new String[] {"pluginDependencies"}));
                }
            }
        }

        // Prevent HDB enabling with Non-WGA4-Compatibility
        PublisherOption option = _csConfig.findPublisherOption("isHDB");
        boolean hdbEnabled = (option != null && option.getValue().equals("true"));
        if (hdbEnabled) {
            if (_csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA3)) {
                errors.add(new ValidationError("HDB interface can only be enabled with WGA4 compatibility or higher.", new String[] { "versionCompliance", "usesHDB" }));
            }
        }
        
        // Prevent Disconnection Script with Non-WGA5-Compatibility
        if (getDisconnectionScript() != null && getDisconnectionScript().trim().length() > 0) {
            if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig)) {
                errors.add(new ValidationError("Disconnection script can only be enabled with WGA5 compatibility or higher.", new String[] { "versionCompliance", "disconnectionScript"}));
            }
        }

        if (isStaticClasspath()) {
            if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig)) {
                errors.add(new ValidationError("Prevent reloading of java libraries can only be enabled with WGA5 compatibility or higher.", new String[] { "versionCompliance", "staticClasspath"}));
            }
        }
        
        // Prevent Admin App enabling with Non-WGA5-Compat.
        if (isAdminApp()) {
            if (_csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA3) ||
                    _csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA4) ||
                    _csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA41)) {
                errors.add(new ValidationError("Admin application can only be enabled for WGA5 compatibility or higher.", new String[] { "versionCompliance", "adminApp" }));
            }
        }
        
        // Validate role names
        Iterator<ACLRole> roles = getACLRoles().iterator();
        while (roles.hasNext()) {
            ACLRole role = (ACLRole) roles.next();
            if (!Constants.PATTERN_ROLENAMES.matcher(role.getName()).matches()) {
                errors.add(new ValidationError("Role name '" + role.getName() + "' is invalid. Use only international alphanumeric characters and the symbols -, _, $, #, [ and ]",
                        new String[] { "aclRoles" }));
            }
        }

        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) {
            de.innovationgate.wga.common.beans.csconfig.v2.CSConfig v2 = (de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) _csConfig;
            Iterator<Shortcut> shortcuts = v2.getShortcuts().iterator();
            while (shortcuts.hasNext()) {
                Shortcut shortcut = shortcuts.next();
                if (WGUtils.isEmpty(shortcut.getShortcut())) {
                    errors.add(new ValidationError("A shortcut has no name", new String[] { "shortcuts" }));
                }
                else if (WGUtils.isEmpty(shortcut.getReference())) {
                    errors.add(new ValidationError("The shortcut '" + shortcut.getShortcut() + "' has no reference", new String[] { "shortcuts" }));
                }
                else if (shortcut.getType() == Shortcut.TYPE_TMLSCRIPT_GLOBAL) {
                    if (!Character.isUpperCase(shortcut.getShortcut().charAt(0))) {
                        errors.add(new ValidationError("The TMLScript global shortcut '" + shortcut.getShortcut() + "' must start with an uppercase letter.", new String[] { "shortcuts" }));
                    }
                }

            }
        }

        if (hasPluginConfig()) {
            // Validate plugin id
            PluginConfig pc = _csConfig.getPluginConfig();
            PluginID id = pc.getId();

            errors.addAll(validatePluginUniqueName(getPluginUniqueName()));

            // Validate plugin dependencies
            Iterator<PluginID> dependencies = pc.getDependencies().iterator();
            while (dependencies.hasNext()) {
                PluginID did = (PluginID) dependencies.next();
                if (did.getUniqueName() == null || did.getUniqueName().trim().equals("")) {
                    errors.add(new ValidationError("Unique name of plugin dependency should not be empty.", new String[] { "pluginDependencies" }));
                }
                else if (did.getUniqueName().contains(" ")) {
                    errors.add(new ValidationError("Unique name of plugin dependency should not contain white spaces.", new String[] { "pluginDependencies" }));
                }
            }

            // Validate relation between minimum WGA version and WGA compatibility version
            Version minWGAVersion = _csConfig.getPluginConfig().getMinimumWGAVersion();
            String affectedVersionField = "pluginWGAVersion";
            if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
                minWGAVersion = ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig)_csConfig).getMinimumWGAVersion();
                affectedVersionField = "minimumWGAVersion";
            }            
            float fMinimumWGAVersion = Float.valueOf(minWGAVersion.getMajorVersion() + "." + minWGAVersion.getMinorVersion());
            float compatibilityWGAVersion = Float.valueOf(_csConfig.getVersionCompliance().substring(3));
            if (fMinimumWGAVersion < compatibilityWGAVersion) {
                errors.add(new ValidationError("The minimum WGA version is lower than the version chosen in 'Version compliance'", new String[] { affectedVersionField,
                        "versionCompliance" }));
            }
            if (minWGAVersion.isAtLeast(6, 0)&& !_csConfig.getPluginConfig().getMinimumJavaVersion().isAtLeast(1, 6)) {
                errors.add(new ValidationError("Invalid Java version. OpenWGA >= 6.0 requires Java 1.6.0 or higher.", new String[] { "pluginJavaVersion" }));
            } else if (minWGAVersion.isAtLeast(5, 0)&& !_csConfig.getPluginConfig().getMinimumJavaVersion().isAtLeast(1, 5)) {
                errors.add(new ValidationError("Invalid Java version. OpenWGA >= 5.0 requires Java 1.5.0 or higher.", new String[] { "pluginJavaVersion" }));
            }            
        }
        
        if (getBrowsingSecurity() == null) {
            errors.add(new ValidationError("Unknown value for 'BrowsingSecurity'.", new String[] { "publisherOptions" }));
        } else {        
            // Prevent BrowsingSecurity other than default for WGA < 5.3.
            if (!getBrowsingSecurity().getKey().equals(BrowsingSecurity.FULL_ACCESS)) {
                if (!getVersionCompliance().toWGAVersion().isAtLeast(5, 3)) {
                    errors.add(new ValidationError("Browsing security can only be enabled for WGA 5.3 compatibility or higher.", new String[] { "versionCompliance", "browsingSecurity" }));
                }
            }
        }
        
        if (getVersionCompliance().toWGAVersion().isAtLeast(6, 0)) {
            // validate accesslevels
            if (getAnonymousAccessLevel() != null) {
                if (getAnonymousAccessLevel().getKey() == AccessLevel.LEVEL_READER_DESIGNER) {
                    errors.add(new ValidationError("Access level 'Reader/Designer' has been removed in OpenWGA 6.0 and later. Please change to 'Reader'.", new String[] { "anonymousAccessLevel" }));
                } else if (getAnonymousAccessLevel().getKey() == AccessLevel.LEVEL_AUTHOR_DESIGNER) {
                    errors.add(new ValidationError("Access level 'Author/Designer' has been removed in OpenWGA 6.0 and later. Please change to 'Author'.", new String[] { "anonymousAccessLevel" }));
                } else if (getAnonymousAccessLevel().getKey() == AccessLevel.LEVEL_EDITOR_DESIGNER) {
                    errors.add(new ValidationError("Access level 'Editor/Designer' has been removed in OpenWGA 6.0 and later. Please change to 'Editor'.", new String[] { "anonymousAccessLevel" }));
                }
            }
            if (getDefaultAccessLevel() != null) {
                if (getDefaultAccessLevel().getKey() == AccessLevel.LEVEL_READER_DESIGNER) {
                    errors.add(new ValidationError("Access level 'Reader/Designer' has been removed in OpenWGA 6.0 and later. Please change to 'Reader'.", new String[] { "defaultAccessLevel" }));
                } else if (getDefaultAccessLevel().getKey() == AccessLevel.LEVEL_AUTHOR_DESIGNER) {
                    errors.add(new ValidationError("Access level 'Author/Designer' has been removed in OpenWGA 6.0 and later. Please change to 'Author'.", new String[] { "defaultAccessLevel" }));
                } else if (getDefaultAccessLevel().getKey() == AccessLevel.LEVEL_EDITOR_DESIGNER) {
                    errors.add(new ValidationError("Access level 'Editor/Designer' has been removed in OpenWGA 6.0 and later. Please change to 'Editor'.", new String[] { "defaultAccessLevel" }));
                }
            }
            if (getRemoteActions() != null) {
                for (RemoteAction remoteAction : getRemoteActions()) {
                    if (remoteAction.getCallerLevel() == AccessLevel.LEVEL_READER_DESIGNER) {
                        errors.add(new ValidationError("Remote action specifies invalid access level 'Reader/Designer'. Please change to 'Reader'.", new String[] { "remoteActions" }));
                        break;
                    } else if (remoteAction.getCallerLevel() == AccessLevel.LEVEL_AUTHOR_DESIGNER) {
                        errors.add(new ValidationError("Remote action specifies invalid access level 'Author/Designer'. Please change to 'Author'.", new String[] { "remoteActions" }));
                        break;
                    } else if (remoteAction.getCallerLevel() == AccessLevel.LEVEL_EDITOR_DESIGNER) {
                        errors.add(new ValidationError("Remote action specifies invalid access level 'Editor/Designer'. Please change to 'Editor'.", new String[] { "remoteActions" }));
                        break;
                    }
                }
            }
        }
        
        return errors;
    }
    
    
    public static List<ValidationError> validatePluginUniqueName(String uname) {
    	List<ValidationError> errors = new ArrayList<ValidationError>();
    	if (uname == null || uname.trim().equals("")) {
            errors.add(new ValidationError("Plugin unique name should not be empty.", new String[] { "pluginUniqueName" }));
        }
        else if (uname.contains(" ")) {
            errors.add(new ValidationError("Plugin unique name should not contain white spaces.", new String[] { "pluginUniqueName" }));
        } else if (uname.endsWith(".")) {
        	errors.add(new ValidationError("Plugin unique name should not end with an '.'", new String[] { "pluginUniqueName" }));
        }
    	return errors;
    }

    private void ensureCorrectCSConfigVersion() throws IllegalAccessException, InvocationTargetException {    	
        // cache current shortcuts in memory - necessary to restore them if
        // vesion compliance is changed multiple times without save
        if (getShortcuts() != null) {
            _cachedShortcuts = getShortcuts();
        }
        
        boolean doFireModelChanged = false;

        // Convert CsConfig and PluginConfig object to proper version for WGA version compliance/minimum version
        Class<? extends CSConfig> csConfigClassForCompliance = CSConfig.getCSConfigClassForCompliance(_csConfig.getVersionCompliance());
        if (!csConfigClassForCompliance.isAssignableFrom(_csConfig.getClass())) {  // CSConfig storage version is lower than the storage version for the compliance? 
            changeCSConfigStorageVersion(csConfigClassForCompliance);
            doFireModelChanged = true;
        }
        else if (!(de.innovationgate.wga.common.beans.csconfig.v4.CSConfig.class.isAssignableFrom(csConfigClassForCompliance)) && !(csConfigClassForCompliance.equals(_csConfig.getClass()))) { // For compliance storage version below 4 we enforce the exact storage version here 
            changeCSConfigStorageVersion(csConfigClassForCompliance);
            doFireModelChanged = true;
        }
        
        // Convert CsConfig and PluginConfig object to proper version for WGA minimum version (from csconfig storage version 4 on up this is the relevant field)
        if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            de.innovationgate.wga.common.beans.csconfig.v4.CSConfig v4config = (de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) _csConfig; 
            Class<? extends CSConfig> csConfigClassForMinimumWGAVersion = CSConfig.getCSConfigClassForMinimumWGAVersion(v4config.getMinimumWGAVersion());
            if (!csConfigClassForMinimumWGAVersion.equals(_csConfig.getClass())) {
                changeCSConfigStorageVersion(csConfigClassForMinimumWGAVersion);
                doFireModelChanged = true;
            }
        }
        
        if (doFireModelChanged) {
            fireModelChanged();
        }
        
    }

    protected void changeCSConfigStorageVersion(Class<? extends CSConfig> csConfigClassForCompliance) throws IllegalAccessException, InvocationTargetException {
        Class<? extends CSConfig> oldCSConfigClass = _csConfig.getClass();
        CSConfig correctVersionCSConfig = CSConfig.instantiateCSConfig(csConfigClassForCompliance);
        BeanUtils.copyProperties(correctVersionCSConfig, _csConfig); 
        
        if (_csConfig.getPluginConfig() != null) {
            PluginConfig correctVersionPluginConfig = PluginConfig.instantiatePluginConfigForCompliance(correctVersionCSConfig);
            BeanUtils.copyProperties(correctVersionPluginConfig, _csConfig.getPluginConfig());
            correctVersionCSConfig.setPluginConfig(correctVersionPluginConfig);
        }
                    
        _csConfig = correctVersionCSConfig;
        
        loadVersionProperties();

        // reload cached design shortcuts
        if (isFeatureSupported(FEATURE_SHORTCUTS) && _cachedShortcuts != null && _cachedShortcuts.size() > 0 && getShortcuts().size() == 0) {
            getShortcuts().addAll(_cachedShortcuts);
        }
    }

	@Override
	public boolean isEditable(String propertyName) {
		if (propertyName != null) {
			if (propertyName.equals("usesHDB")) {
				if (_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
					return false;
				} else if (_csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA3)) {
					return false;
				}
					
			} else if (propertyName.equals("staticClasspath")) {
				if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig)) {
					return false;
				}
			} else if (propertyName.equals("pluginDisableInit")) {
				if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig)) {
					return false;
				}
			} else if (propertyName.equals("pluginNoDatabase")) {
		        if (!(_csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig)) {
		            return false;
		        }
			}
			else if (propertyName.equals("pluginClearDatabaseOnUpdate")) {
		    
			    
			    if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig)) {
					return false;
				}
			    else if (_csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) {
			        de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig v5PluginConfig = (de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) _csConfig.getPluginConfig();
			        if ("true".equals(v5PluginConfig.getOptions().get(de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig.OPTION_NO_DATABASE))) {
			            return false;
			        }
			    }
				
			} else if (propertyName.equals("disconnectionScript")) {
				if (!(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig)) {
					return false;
				}
			} else if (propertyName.equals("adminApp")) {
			    if (_csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA3) ||
			            _csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA4) ||
			            _csConfig.getVersionCompliance().equals(CSConfig.VERSIONCOMPLIANCE_WGA41)) {
			        return false;
			    } else {
			        return true;
			    }
			} else if (propertyName.equals("minimumWGAVersion")) {
			    return _csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig;
			} else if (propertyName.equals("versionCompliance")) {
			    return !isOverlay();
			} else if (propertyName.equals("pluginWGAVersion")) {
			    return !(_csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig);
			}
//			} else if (propertyName.equals("browsingSecurity")) {
//			    if (getVersionCompliance().toWGAVersion().isAtLeast(5, 3)) {
//			        return true;
//			    } else {
//			        return false;
//			    }
//            }
		} 
		return true;
	}

    public boolean isAdminApp() {
        return Boolean.parseBoolean(getPublisherOptionValue(PublisherOption.OPTION_ADMIN_APP));
    }

    public void setAdminApp(boolean enabled) {
        setPublisherOption(PublisherOption.OPTION_ADMIN_APP, enabled);
    }
    
    public void setBrowsingSecurity(BrowsingSecurity level) {
        setPublisherOption(PublisherOption.OPTION_BROWSING_SECURITY, level.getKey());
    }
	
    public BrowsingSecurity getBrowsingSecurity() {
        return BROWSINGSECURITY_VALUES.get(Integer.parseInt(getPublisherOptionValue(PublisherOption.OPTION_BROWSING_SECURITY)));
    }
    
    @DataProvider(objectField = "browsingSecurity")
    public List<BrowsingSecurity> provideBrowsingSecurityValues() {
        List<BrowsingSecurity> list = new ArrayList<BrowsingSecurity>(BROWSINGSECURITY_VALUES.values());
        return list;
    }
    
    public boolean isOverlay() {
        File overlayConfig = new File(_designDirectory, "files/system/overlay.xml");
        return overlayConfig.exists();
    }
    
    @NotBound
    @SuppressWarnings("unchecked")
    public List<MediaKey> getBaseMediaKeys() {
        if (_csConfigBase != null) {
            return _csConfigBase.getMediaKeys();
        } else {
            return Collections.emptyList();
        }
    }

    public void mergeWithBaseCSConfig() throws IOException, IllegalAccessException, InvocationTargetException {
        if (_csConfigBase != null) {
            // merge version compliance
            VersionCompliance versionComplianceBase = getBaseVersionCompliance();
            if (versionComplianceBase != null) {
                VersionCompliance myVersionCompliance = VersionCompliance.get(_csConfig.getVersionCompliance());
                if (myVersionCompliance != null && myVersionCompliance.toWGAVersion().compareTo(versionComplianceBase.toWGAVersion()) < 0) {
                    setVersionCompliance(versionComplianceBase);
                    
                    saveChanges();
                    fireModelChanged();
                }
            }                       
        }
    }
    
    @NotBound
    public List<Shortcut> getBaseShortcuts() {
        if (_csConfigBase != null &&  _csConfigBase instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) {
            return ((de.innovationgate.wga.common.beans.csconfig.v2.CSConfig)_csConfigBase).getShortcuts();
        } else {
            return Collections.emptyList();
        }
    }
    
    @NotBound
    public VersionCompliance getBaseVersionCompliance() {
        if (_csConfigBase != null) {
            return VersionCompliance.VERSIONCOMPLIANCES.get(_csConfigBase.getVersionCompliance());
        }
        return null;
    }
    
    public List<String> getDirectModifiablePublisherOptions() {
        List<String> options= new ArrayList<String>();
        options.add(PublisherOption.OPTION_HOME_PAGE);
        options.add(PublisherOption.OPTION_LOGIN_PAGE);
        options.add(PublisherOption.OPTION_DESIGN_ENCODING);
        options.add(PublisherOption.OPTION_USES_HDB);
        
        if (!isOverlay()) {
            options.add(PublisherOption.OPTION_MULTI_LANGUAGE_CONTENT);            
            options.add(PublisherOption.OPTION_DEFAULT_ITEM_ENCODING);        
            options.add(PublisherOption.OPTION_ADMIN_APP);
            options.add(PublisherOption.OPTION_BROWSING_SECURITY);
        }
        
        return Collections.unmodifiableList(options);
    }
    
}
