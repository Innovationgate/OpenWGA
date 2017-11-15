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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.zip.ZipInputStream;

import org.apache.log4j.Logger;

import de.innovationgate.utils.DynamicClassLoadingChain;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGACL;
import de.innovationgate.webgate.api.WGACLEntry;
import de.innovationgate.webgate.api.WGACLEntryFlags;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabase.ConnectAction;
import de.innovationgate.webgate.api.WGDatabaseConnectListener;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.jdbc.FileContainer;
import de.innovationgate.webgate.api.schemadef.WGSchemaDefinition;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.wga.common.beans.csconfig.v1.ACLRole;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.common.beans.csconfig.v1.RemoteAction;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.design.OverlayDesignProvider;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.db.DBDesignProvider;
import de.innovationgate.wgpublisher.design.db.PluginDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.plugins.WGAPluginSet;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class SystemContainerManager implements WGDatabaseEventListener, WGDesignChangeListener, WGDatabaseConnectListener {

    
    public static final String INITDUMP_CS = "init.wgacs";
    public static final String INITDUMP_PLUGIN = "plugin-init.wgacs";
    public static final String CSCONFIG_FILE = "csconfig.xml";
    public static final String SCHEMA_FILE = "schema.xml";
    public static final String CSCONFIG_PATH = "files/system/" + CSCONFIG_FILE;
    public static final String SCHEMA_PATH = "files/system/" + SCHEMA_FILE;
    public static final String OVERLAY_DATA_PATH = "files/system/" + OverlayDesignProvider.OVERLAY_DATA_FILE;
    public static final String LICENSE_PATH = "files/system/license.txt";
    
    public class LibraryReloadOccasion implements ProblemOccasion {

        private String _dbKey;

        public LibraryReloadOccasion(String dbkey) {
            _dbKey = dbkey;
        }

        @Override
        public ProblemScope getDefaultScope() {
            return new DatabaseScope(_dbKey);
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return SystemContainerManager.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return false;
        }
        
    }
    
    class SystemContainerContext {
        
        private ContainerInfo _info;
        private boolean _initACL;
        private WGDatabase _db;
        private boolean _initDisabled = false;
        private boolean _emptyContentInitDisabled = false;
        private WGAPlugin _plugin;

        public SystemContainerContext(WGDatabase db, ContainerInfo info, WGAPlugin plugin, boolean initACL) {
            _db = db;
            _info = info;
            _plugin = plugin;
            _initACL = initACL;
            
            _initDisabled = false;
            if (_db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                if (_info != null && _info.getCsConfig() != null && _info.getCsConfig().getPluginConfig() != null) {
                    PluginConfig pc = _info.getCsConfig().getPluginConfig();
                    if (pc instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
                        _initDisabled = ((de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) pc).isDisablePluginInit();
                    }
                    if (pc instanceof de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) {
                        Map<String,String> pluginOptions = ((de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) pc).getOptions();
                        if ("true".equals(pluginOptions.get(de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig.OPTION_NO_DATABASE))) {
                            _emptyContentInitDisabled = true;
                        }
                    }
                }
            }
        }
        
        public void performInitialisation() {
        	performInitialisation(null);
        }
        
        public void performInitialisation(Boolean dbIsEmptyContent) {
            try {
                WGFileContainer fc = _db.getFileContainer("system");
                WGFileContainer overlayFc = _db.getFileContainer("overlay:system");
                
                // Build shortcuts so they are available in setup scripts
                _core.buildDesignShortcuts(_db);
                
                // Actions to perform on an db with empty acl (this will not be disabled with _initDisabled since even design provider plugins need general access)
                CSConfig csConfig = null;
                CSConfig csConfigOverlay = null;
                if (_info != null) {
                    csConfig = _info.getCsConfig();
                    csConfigOverlay = _info.getOverlayCsConfig();
                    if (csConfig != null && _initACL) {
                        doEmptyACLActions(_db, csConfig);
                    }
                }
                
                // Actions to bypass if initialisation is disabled
                if (!_initDisabled) {
                    

                    // Actions to bypass if initialisation for empty content stores is disabled
                    if (!_emptyContentInitDisabled) {
                        // Actions to perform on a content-empty db
                        boolean isEmptyContent = _db.isContentEmpty();
                        if (dbIsEmptyContent != null) {
                            isEmptyContent = dbIsEmptyContent.booleanValue();
                        }
    
                        // Process init dump
                        boolean disableBaseContentInitialisations = false;
                        if (fc != null && isEmptyContent) {
    
                            String initDump;
                            if (_db.hasAttribute(WGACore.DBATTRIB_PLUGIN_VERSION) && fc.hasFile(INITDUMP_PLUGIN)) {
                                initDump = INITDUMP_PLUGIN;
                            }
                            else {
                                initDump = INITDUMP_CS;
                            }
                            
                            if (overlayFc != null && overlayFc.getFileNames().contains(initDump)) {
                                _log.info("Importing initial data for empty database '" + _db.getDbReference() + "'");
                                _core.importContentStoreDump(new ZipInputStream(overlayFc.getFileData(initDump)), _db);
                                disableBaseContentInitialisations = true;
                            }
                            else if (fc.getFileNames().contains(initDump)) {
                                _log.info("Importing initial data for empty database '" + _db.getDbReference() + "'");
                                _core.importContentStoreDump(new ZipInputStream(fc.getFileData(initDump)), _db);
                            }
                        }
                        
                        
                        // We directly init HDB for this database if the system file container has a hdb model descriptor file
                        // So following scripts can use implicit model containers
                        if (fc != null && fc.hasFile(HDBModel.MODEL_FILE)) {
                            WGHierarchicalDatabase.getOrCreateInstance(_db);
                        }
                        
                        // Process schema
                        if (_info != null && _info.getSchema() != null) {
                            _db.onConnect(
                                new WGDatabase.DatabaseAction() {
                                    @Override
                                    public void run(WGDatabase db) throws Exception {
                                        _db.enforceSchema(_info.getSchema());
                                        
                                        // The event manager needs to register the event code scripts, which are now active from schema (#00001696)
                                        // We need to re-register here
                                        _core.getEventManager().updateDatabaseEvents(_db);
                                    }
                                });
                        }
                        
                        // Process roles
                        if (csConfig != null) {
                            processRoles(csConfig);   
                        }
                        
                        if (csConfigOverlay != null) {
                            processRoles(csConfigOverlay);
                        }
    
                        // Process init script
                        if (isEmptyContent) {
                            
                            if (csConfig != null && !disableBaseContentInitialisations) {
                                execInitialisationScript(csConfig, false);
                            }
                            
                            if (csConfigOverlay != null) {
                                execInitialisationScript(csConfigOverlay, true);
                            }
                            
                            // Re-evaluate the default language
                            _db.onConnect(_core.new ValidateDefaultLanguageAction());
                        }
                    }

                    
                    // Connection script
                    if (csConfig != null) {
                        execConnectionScript(csConfig, false);
                    }
                    
                    if (csConfigOverlay != null) {
                        execConnectionScript(csConfigOverlay, true);
                    }
                    
                }
            
                
                // Add as database event listener to get updates for csconfig.xml
                if (_db.getDesignProvider() != null) {
                    _db.getDesignProvider().addDesignChangeListener(SystemContainerManager.this);
                }
                else {
                    _db.addDatabaseEventListener(SystemContainerManager.this);
                }
            }
            catch (Exception e) {
                _log.error("Error adding database for system container processing", e);
            }
        }

        public void processRoles(CSConfig csConfig) throws WGBackendException, WGAuthorisationException, WGAPIException {
            WGACL acl = _db.getACL();
            Iterator roles = csConfig.getRoles().iterator();
            while (roles.hasNext()) {
                ACLRole role = (ACLRole) roles.next();
                WGACLEntry existingEntry = acl.getEntry(role.getName());
                if (existingEntry == null) {
                    _log.info("Adding role '" + role.getName() + "' to ACL");
                    acl.createRoleEntry(role.getName());
                    if (role.isDefaultManagerRole()) {
                        assingRoleToDefaultManager(_db, acl, role);
                    }
                }
            }
        }

        private void execInitialisationScript(CSConfig csConfig, boolean overlay) {
            if (csConfig != null && !WGUtils.isEmpty(csConfig.getInitScript())) {
                try {
                    String prefix = (overlay ? "overlay:" : "");
                    WGCSSJSModule mod = _db.getScriptModule(prefix + csConfig.getInitScript(), WGScriptModule.CODETYPE_TMLSCRIPT);
                    if (mod != null) {
                        _log.info("Running " + (overlay ? "overlay " : "") + "initialisation script of database '" + _db.getDbReference() + "'");
                        TMLContext context = new TMLContext(_db.getDummyContent(null), _core, null, null);
                        
                        Map params = new HashMap();
                        params.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, new DesignResourceReference(mod));
                        params.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, (overlay ? "Overlay " : "") + "Initialisation script of app '" + _db.getDbReference() + "'");
                        ExpressionResult result = ExpressionEngineFactory.getTMLScriptEngine().evaluateExpression(mod.getCode(), context, ExpressionEngine.TYPE_SCRIPT, params);
                        if (result.isError()) {
                            _log.error("Error running initialisation script '" + csConfig.getInitScript() + "' of db '" + _db.getDbReference() + "'", result.getException());
                        }
                    }
                    else {
                        _log.error("Initialisiation script '" + csConfig.getInitScript() + "' not found in database '" + _db.getDbReference() + "'");
                    }
                }
                catch (WGAPIException e) {
                    _log.error("Error initialising database '" + _db.getDbReference() + "'", e);
                }
            }
        }
        
        public void performDisconnection() {
            
            try {
                if (_db.isSessionOpen()) {
                    _db.reopenSession(null, null);
                }
                else {
                    _db.openSession();
                }
                
                // Disconnection script
                if (_info != null) {
                    
                    if (_info.getOverlayCsConfig() != null && _info.getOverlayCsConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig && !_initDisabled) {
                        de.innovationgate.wga.common.beans.csconfig.v3.CSConfig v3Config = (de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _info.getOverlayCsConfig();
                        execDisconnectionScript(v3Config, true);
                    }

                    
                    if (_info.getCsConfig() != null && _info.getCsConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig && !_initDisabled) {
                        de.innovationgate.wga.common.beans.csconfig.v3.CSConfig v3Config = (de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _info.getCsConfig();
                        execDisconnectionScript(v3Config, false);
                    }
                    
                }
            }
            catch (Exception e) {
                _log.error("Error removing database from system container processing", e);
            }
            
        }
        
        public void putPublisherOptions(Map<String, String> optionsTarget) {
            
            if (_info == null || _info.getCsConfig() == null) {
                return;
    }
    
            List publisherOptions = _info.getCsConfig().getPublisherOptions();
            putPublisherOptionsList(publisherOptions, optionsTarget);
            
            if (_info.getOverlayCsConfig() != null) {
                putPublisherOptionsList(_info.getOverlayCsConfig().getPublisherOptions(), optionsTarget);
            }

        }

        private void putPublisherOptionsList(List publisherOptions, Map<String, String> optionsTarget) {
            Iterator options = publisherOptions.iterator();
            while (options.hasNext()) {
                PublisherOption option = (PublisherOption) options.next();
                optionsTarget.put(option.getName(), option.getValue());
            }
        }

        private void execConnectionScript(CSConfig csConfig, boolean overlay) {
        
            String script = csConfig.getConnectionScript();
            
            if (WGUtils.isEmpty(script)) {
                return;
            }
            
            try {
                
                String prefix = (overlay ? "overlay:" : "");
                
                WGCSSJSModule mod = _db.getCSSJSModule(prefix + script, WGScriptModule.CODETYPE_TMLSCRIPT);
                if (mod != null) {
                    _log.info("Running " + (overlay ? "overlay " : "") + "connection script of app '" + _db.getDbReference() + "'");
                    TMLContext context = new TMLContext(_db.getDummyContent(null), _core, null, null);
                    
                    Map params = new HashMap();
                    params.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, new DesignResourceReference(mod));
                    params.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, (overlay ? "Overlay " : "") + "Connection script of app '" + _db.getDbReference() + "'");
                    ExpressionResult result = ExpressionEngineFactory.getTMLScriptEngine().evaluateExpression(mod.getCode(), context, ExpressionEngine.TYPE_SCRIPT, params);
                    if (result.isError()) {
                        _log.error("Error running connection script '" + csConfig.getConnectionScript() + "' of '" + _db.getDbReference() + "'", result.getException());
                    }
                }
                else {
                    _log.error("Connection script '" + csConfig.getConnectionScript() + "' not found in design of '" + _db.getDbReference() + "'");
                }
            }
            catch (WGAPIException e) {
                _log.error("Exception processing connection script of '" + _db.getDbReference() + "'", e);
            }
            
        }

        private void execDisconnectionScript(de.innovationgate.wga.common.beans.csconfig.v3.CSConfig csConfig, boolean overlay) {
        
            String script = csConfig.getDisconnectionScript();
            if (WGUtils.isEmpty(script)) {
                return;
            }
            
            try {
                
                String prefix = (overlay ? "overlay:" : "");
                WGCSSJSModule mod = _db.getCSSJSModule(prefix + script, WGScriptModule.CODETYPE_TMLSCRIPT);
                if (mod != null) {
                    
                    _log.info("Running " + (overlay ? "overlay " : "") + "disconnection script of '" + _db.getDbReference() + "'");
                    TMLContext context = new TMLContext(_db.getDummyContent(null), _core, null, null);
                    
                    Map params = new HashMap();
                    params.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, new DesignResourceReference(mod));
                    params.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, (overlay ? "Overlay " : "") + "Disonnection script of app '" + _db.getDbReference() + "'");
                    ExpressionResult result = ExpressionEngineFactory.getTMLScriptEngine().evaluateExpression(mod.getCode(), context, ExpressionEngine.TYPE_SCRIPT, params);
                    
                    if (result.isError()) {
                        _log.error("Error running disconnection script '" + script + "' of '" + _db.getDbReference() + "'", result.getException());
                    }
                }
                else {
                    _log.error("Disconnection script '" + script + "' not found in design of '" + _db.getDbReference() + "'");
                }
            }
            catch (WGAPIException e) {
                _log.error("Exception processing disconnection script of '" + _db.getDbReference() + "'", e);
            }
            
        }
        
    }
    

    class ContainerInfo {
        
        private Date _lastModified;
        private File _deploymentDir;
        private File _overlayDeploymentDir;
        private boolean _deleteDeploymentDirOnFinalize = true;
        private List<File> _javaClassesDirs = new ArrayList<File>();
        private boolean _fromProviderDB = false;
        private boolean _enforcedLibraryUpdate = false;
        private CSConfig _csConfig = null;
        private Set _enforcedMediaMappings = new HashSet();
        private Set _enforcedElementMappings = new HashSet();
        private Set _enforcedJobDefinitions = new HashSet();
        private Set _enforcedEncoderMappings = new HashSet();
        private Map<String,Date> _deployedJARDates = new HashMap<String,Date>();
        private String _dbkey;
        private WGSchemaDefinition _schema = null;
        private Date _overlayLastModified;
        private CSConfig _overlayCsConfig;
        private File _deploymentJarsDir;
        private File _overlayDeploymentJarsDir;
        public ContainerInfo(WGDatabase db, ContainerInfo oldInfo) throws WGException, IOException, InvalidCSConfigVersionException {
            
            // Handle system file container
            WGFileContainer con = db.getFileContainer("system");
            WGFileContainer conOverlay = db.getFileContainer("overlay:system");
            
            // Determine if this system file container originates from another DB
            // which means we can bypass many operations for this DB, since the provider DB already enforced them 
            _dbkey = db.getDbReference();
            WGDesignProvider designProvider = db.getDesignProvider();
            if (isFromProviderDatabase(designProvider)) {
                _fromProviderDB = true;
            }
            
            // Create the deployment dir where to put JARs that are to be loaded to classpath
            if (!_fromProviderDB) {
                _deploymentDir = createDeploymentDir(con, oldInfo);
                _overlayDeploymentDir = createDeploymentDir(conOverlay, oldInfo);
                
                // Dedicated "jar" directory
                WGFileContainer jars = db.getFileContainer("system:jars");
                if (jars != null) {
                    _deploymentJarsDir = createDeploymentDir(jars, oldInfo);
                }
                
                WGFileContainer overlayJars = db.getFileContainer("overlay:system:jars");
                if (overlayJars != null) {
                    _overlayDeploymentJarsDir = createDeploymentDir(overlayJars, oldInfo);
                }

            }
            
            // Look for a Java directory in the design provider
            if (designProvider instanceof OverlayDesignProvider) {
                OverlayDesignProvider overlayProvider = (OverlayDesignProvider) designProvider;
                addJavaClassesDir(overlayProvider.getOriginal());
                addJavaClassesDir(overlayProvider.getOverlay());
            }
            else {
                addJavaClassesDir(designProvider);
            }
            
            _lastModified = (con != null ? con.getLastModified() : null);
            _overlayLastModified = (conOverlay != null ? conOverlay.getLastModified() : null);
            
            if (con != null && con.getFileNames().contains(CSCONFIG_FILE)) {
                InputStream in = con.getFileData(CSCONFIG_FILE);
                _csConfig = CSConfig.load(in, true);
            }
            
            if (conOverlay != null && conOverlay.getFileNames().contains(CSCONFIG_FILE)) {
                InputStream in = conOverlay.getFileData(CSCONFIG_FILE);
                _overlayCsConfig = CSConfig.load(in, true);
                if (_csConfig != null) {
                    _csConfig.importOverlayConfig(_overlayCsConfig);
                }
            }
            
            // Check mininum OpenWGA version
            if (_csConfig != null) {
                Version version = getRealMinimumWGAVersion(_csConfig);
                Version currentVersion = WGAVersion.toCsConfigVersion();
                if (!currentVersion.isAtLeast(version)) {
                    throw Problem.create(new WGACore.ConnectDatabaseProblemOccasion(db.getDbReference()), "databaseConnectionFailed.incompatibleDesign", ProblemSeverity.HIGH, Problem.var("targetversion", version.getMainVersionString()));
                }
                if (_overlayCsConfig != null) {
                    version = getRealMinimumWGAVersion(_overlayCsConfig);
                    if (!currentVersion.isAtLeast(version)) {
                        throw Problem.create(new WGACore.ConnectDatabaseProblemOccasion(db.getDbReference()), "databaseConnectionFailed.incompatibleOverlayDesign", ProblemSeverity.HIGH, Problem.var("targetversion", version.getMainVersionString()));
                    }
                }
            }
            
            if (con != null && con.getFileNames().contains(SCHEMA_FILE)) {
                InputStream in = con.getFileData(SCHEMA_FILE);
                try {
                    _schema  = WGSchemaDefinition.read(in);
                }
                catch (Exception e) {
                    _core.getProblemRegistry().addProblem(Problem.create(new WGACore.ConnectDatabaseProblemOccasion(db.getDbReference()), "databaseConnectionProblem.invalidSchema", ProblemSeverity.LOW, e));
                    _core.getLog().error("Exception reading schema definition of datababase " + _dbkey, e);
                }
                in.close();
            }
            
            if (conOverlay != null && conOverlay.getFileNames().contains(SCHEMA_FILE)) {
                InputStream in = conOverlay.getFileData(SCHEMA_FILE);
                try {
                    WGSchemaDefinition schema  = WGSchemaDefinition.read(in);
                    if (_schema != null) {
                        _schema.importSchema(schema);
                    }
                    else {
                        _schema = schema;
                    }
                }
                catch (Exception e) {
                    _core.getProblemRegistry().addProblem(Problem.create(new WGACore.ConnectDatabaseProblemOccasion(db.getDbReference()), "databaseConnectionProblem.invalidOverlaySchema", ProblemSeverity.LOW, e));
                    _core.getLog().error("Exception reading schema definition of datababase " + _dbkey, e);
                }
                in.close();
            }
        }
        private boolean isFromProviderDatabase(WGDesignProvider designProvider) {
            
            if (designProvider instanceof DBDesignProvider) {
                return true;
            }
            
            if (designProvider instanceof OverlayDesignProvider) {
                OverlayDesignProvider overlayProvider = (OverlayDesignProvider) designProvider;
                if (overlayProvider.getOverlay() instanceof DBDesignProvider) {
                    return true;
                }
            }
            
            return false;
            
        }
        private Version getRealMinimumWGAVersion(CSConfig csConfig) {

            if (csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
                return ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) csConfig).getMinimumWGAVersion();
            }
            else {
                return csConfig.getComplianceVersion();
            }
            
        }
        private void addJavaClassesDir(WGDesignProvider designProvider) {
            
            if (designProvider instanceof PluginDesignProvider) {
                PluginDesignProvider pluginProvider = (PluginDesignProvider) designProvider;
                designProvider = pluginProvider.getSourceDesignProvider();
            }
            
            if (designProvider instanceof FileSystemDesignProvider) {
                FileSystemDesignProvider fsProvider = (FileSystemDesignProvider) designProvider;
                if (fsProvider.getJavaClassesPath() != null) {
                    _javaClassesDirs.add(new File(fsProvider.getJavaClassesPath()));
                }
            }
        }
        public File getDeploymentDir() {
            return _deploymentDir;
        }
        public Date getLastModified() {
            return _lastModified;
        }
        protected void finalize() {
            
            if (!isStaticClasspath() && _deleteDeploymentDirOnFinalize) {
                if (_deploymentDir != null && _deploymentDir.exists()) {
                    WGUtils.delTree(_deploymentDir);
                    _deploymentDir = null;
                }
                if (_overlayDeploymentDir != null && _overlayDeploymentDir.exists()) {
                    WGUtils.delTree(_overlayDeploymentDir);
                    _overlayDeploymentDir = null;
                }
                if (_deploymentJarsDir != null && _deploymentJarsDir.exists()) {
                    WGUtils.delTree(_deploymentJarsDir);
                    _deploymentJarsDir = null;
                }
                if (_overlayDeploymentJarsDir != null && _overlayDeploymentJarsDir.exists()) {
                    WGUtils.delTree(_overlayDeploymentJarsDir);
                    _overlayDeploymentJarsDir = null;
                }
            }
        }
        public List<URL> getJARURLs() {
        	List<URL> jars = new ArrayList<URL>();
            if (_deploymentDir != null) {            
	            collectJARs(_deploymentDir, jars);
            }
            if (_overlayDeploymentDir != null) {
                collectJARs(_overlayDeploymentDir, jars);
            }
            if (_deploymentJarsDir != null) {            
                collectJARs(_deploymentJarsDir, jars);
            }
            if (_overlayDeploymentJarsDir != null) {
                collectJARs(_overlayDeploymentJarsDir, jars);
            }
            
            // Sort the URLs so this version can be compared to previous version
            Collections.sort(jars, URLStringComparator.INSTANCE);
            
            return jars;            
        }
        private void collectJARs(File dir, List<URL> jars) {
            File[] files = dir.listFiles();
            if (files != null) {	            
                for (int i = 0; i < files.length; i++) {
                    File file = files[i];
                    if (file.getName().endsWith(".jar")) {
                        try {
                            jars.add(file.toURI().toURL());
                        }
                        catch (MalformedURLException e) {
                            _log.error("Error creating URL for custom jar", e);
                        }
                    }
                }
            }
        }
        public CSConfig getCsConfig() {
            return _csConfig;
        }
        public Set getEnforcedElementMappings() {
            return _enforcedElementMappings;
        }
        public Set getEnforcedEncoderMappings() {
            return _enforcedEncoderMappings;
        }
        public Set getEnforcedJobDefinitions() {
            return _enforcedJobDefinitions;
        }
        public Set getEnforcedMediaMappings() {
            return _enforcedMediaMappings;
        }
        public void setCsConfig(CSConfig csConfig) {
            _csConfig = csConfig;
        }
        public void setDeploymentDir(File deploymentDir) {
            _deploymentDir = deploymentDir;
        }
        public void setLastModified(Date lastModified) {
            _lastModified = lastModified;
        }
        public boolean isFromProviderDB() {
            return _fromProviderDB;
        }
        public Collection<String> getJARDescriptions() {
            
            List<String> jars = new ArrayList<String>();
            
            getJARDescriptions(jars, _deploymentDir);
            getJARDescriptions(jars, _overlayDeploymentDir);
            getJARDescriptions(jars, _deploymentJarsDir);
            getJARDescriptions(jars, _overlayDeploymentJarsDir);

            
            for (File classesDir : getJavaClassesDirs()) {
                jars.add(_dbkey + " / java classes directory in design: " + classesDir.getPath());
            }
            
            return jars;
            
        }
        private void getJARDescriptions(List<String> jars, File dir) {
            if (dir == null) {
                return;
            }
            
            File[] files = dir.listFiles();
            if (files == null) {
                return;
            }
            
            for (int i = 0; i < files.length; i++) {
                File file = files[i];
                if (file.getName().endsWith(".jar")) {
                    jars.add(_dbkey + " / " + file.getName());
                }
            }
            
        }
        public boolean isEnforcedLibraryUpdate() {
            return _enforcedLibraryUpdate;
        }
        public void setEnforcedLibraryUpdate(boolean enfordedLibraryUpdate) {
            _enforcedLibraryUpdate = enfordedLibraryUpdate;
        }
        public List<File> getJavaClassesDirs() {
            return _javaClassesDirs;
        }
        private File createDeploymentDir(WGFileContainer con, ContainerInfo oldInfo) throws IOException, WGAPIException {
            
            if  (con == null) {
                return null;
            }
            
            // First collect jar files
            List<String> jars = new ArrayList<String>();
            Iterator fileNames = con.getFileNames().iterator();
            while (fileNames.hasNext()) {
                String fileName = (String) fileNames.next();
                
                // Filter out jars
                if (fileName.endsWith(".jar")) {
                    jars.add(fileName);
                }
            }
            
            // If we have no jars we dont need a deployment dir
            if (jars.size() == 0) {
                return null;
            }

            // If we have on old ContainerInfo we can determine if anything has changed since last JAR deployment

            if (oldInfo != null && jars.size() ==  oldInfo._deployedJARDates.size()) {
                boolean keepDeployment = true;
                
                // We only may recreate the deployment if the classpath is dynamic - If it is static we always keep it.
                if (!isStaticClasspath()) {
                    Iterator<Map.Entry<String,Date>> oldJARs = oldInfo._deployedJARDates.entrySet().iterator();
                    while (oldJARs.hasNext()) {
                        Map.Entry<java.lang.String, java.util.Date> entry = (Map.Entry<java.lang.String, java.util.Date>) oldJARs.next();
                        if (!jars.contains(entry.getKey())) {
                            keepDeployment = false;
                            break;
                        }
                        if (!entry.getValue().equals(con.getFileLastModified(entry.getKey()))) {
                            keepDeployment = false;
                            break;
                        }
                    }
                }
                
                // If nothing changed we can take the old deployment dir and continue to use it (this will keep us from updating the lib loader)                
                if (keepDeployment) {
                    oldInfo._deleteDeploymentDirOnFinalize = false;
                    return oldInfo._deploymentDir;
                }
                
            }
            
            // Create deployment dir for this container
            File deploymentDir = File.createTempFile("fcd", ".tmp", _deploymentBase);
            deploymentDir.delete();
            deploymentDir.mkdir();
            
            // Copy jars into it
            Iterator<String> jarsIt = jars.iterator();
            while (jarsIt.hasNext()) {
                String jarName = jarsIt.next();
                
                // Copy jar data to deployment dir
                InputStream dataIn = con.getFileData(jarName);
                OutputStream dataOut = new FileOutputStream(new File(deploymentDir, jarName));
                WGUtils.inToOut(dataIn, dataOut, 2048);
                dataIn.close();
                dataOut.close();
                _deployedJARDates.put(jarName, con.getFileLastModified(jarName));
            }
            
            return deploymentDir;
            
        }
        public String getDbkey() {
            return _dbkey;
        }
        public boolean isStaticClasspath() {
            if (_csConfig != null && _csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) {
                return ((de.innovationgate.wga.common.beans.csconfig.v3.CSConfig) _csConfig).isStaticClasspath();
            }
            else {
                return false;
            }
        }
        public WGSchemaDefinition getSchema() {
            return _schema;
        }
        public CSConfig getOverlayCsConfig() {
            return _overlayCsConfig;
        }
        public Date getOverlayLastModified() {
            return _overlayLastModified;
        }
        public void setOverlayLastModified(Date overlayLastModified) {
            _overlayLastModified = overlayLastModified;
        }

    }
    
    private WGACore _core;
    private Map<String, ContainerInfo> _containerInfos = new ConcurrentHashMap<String, ContainerInfo>();
    private File _deploymentBase;
    private Logger _log = Logger.getLogger("wga.fcsystem");

    public SystemContainerManager(WGACore core) {
        _core = core;
        
        _deploymentBase = new File(WGFactory.getTempDir(), "scdeployment");
        _deploymentBase.mkdir();
        
    }

    public SystemContainerContext addDatabase(WGDatabase db, WGAPlugin plugin, boolean initACL) throws Exception {
        
        if (!db.isDesignRole()) {
            return null;
        }
        
        if (!db.isConnected()) {
            db.addDatabaseConnectListener(this);
            return null;
        }
                
        // retrieve and set container info
        ContainerInfo info = checkForUpdates(db);
        
        // Validate design and database version
        if (info != null && info.getCsConfig() != null) {
            Version designVersion = info.getCsConfig().getComplianceVersion();
            if (designVersion.isAtLeast(5, 0) && db.getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
                Problem.Vars vars = Problem
                        .var("csversion", String.valueOf(db.getContentStoreVersion()))
                        .var("designversion", designVersion.toString());
                Problem problem = Problem.create(new WGACore.ConnectDatabaseProblemOccasion(db.getDbReference()), "databaseConnectionProblem.csVersionTooLow", ProblemSeverity.LOW, vars);
                _core.getProblemRegistry().addProblem(problem);
                _log.error("Database " + db.getDbReference() + " is a content store of version " + db.getContentStoreVersion() + " while it's design needs a higher content store of version 5. The design may not work correctly.");
            }
        }
        
        
        return new SystemContainerContext(db, info, plugin, initACL);
        
        
    }
    
    public SystemContainerContext addDatabase(WGDatabase db, boolean initACL) throws Exception {
        return addDatabase(db, null, initACL);
    }
    


    private void doEmptyACLActions(WGDatabase db, CSConfig csConfig) throws WGAuthorisationException, WGBackendException, WGAPIException {
        
        WGACL acl = db.getACL();
        if (acl == null) {
            _log.warn("Unable to retrieve ACL of database '" + db.getDbReference() + "' although database has ACL management feature");
            return;
        }
        
        if (csConfig.getAnonymousAccessLevel() != -1) {
            _log.info("Adding anonymous access level to ACL");
            acl.createUserEntry(WGDatabase.ANONYMOUS_USER, csConfig.getAnonymousAccessLevel());
        }
        
        
        if (csConfig.getDefaultAccessLevel() != -1) {
            _log.info("Adding default access level to ACL");
            acl.createUserEntry("*", csConfig.getDefaultAccessLevel());
        }
        
        // Enforce default access level "reader" for plugins that are design providers, and where the default level is not set
        else if (db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX) && csConfig.getPluginConfig() != null && csConfig.getPluginConfig().isUsageAsDesignProvider()) {
            _log.info("Enforcing default access level 'READER' for design provider plugin");
            acl.createUserEntry("*", WGDatabase.ACCESSLEVEL_READER);
        }
        
    }

    private void assingRoleToDefaultManager(WGDatabase db, WGACL acl, ACLRole role) {

        try {
            // Retrieve default manager
            WGADomain domain = _core.getDomainForDatabase(db);
            if (domain == null || domain.getConfig().getDefaultManager() == null) {
                return;
            }
            
            // Retrieve ACL entry for default manager
            WGACLEntry aclEntry = acl.getEntry(domain.getConfig().getDefaultManager());
            if (aclEntry == null) {
                aclEntry = acl.createUserEntry(domain.getConfig().getDefaultManager(), WGDatabase.ACCESSLEVEL_MANAGER);
            }
            
            // Modify flags to include role and store
            WGACLEntryFlags flags = acl.parseFlags(aclEntry);
            if (!flags.getRoles().contains(role.getName())) {
                flags.getRoles().add(role.getName());
            }
            aclEntry.setFlags(flags.toString());
            acl.save(aclEntry);
            
        }
        catch (WGAPIException e) {
           _log.error("Error assigning role '" + role.getName() + "' to default manager for database '" + db.getDbReference() + "'", e);
        }
        
    }


    
    
    public void removeDatabase(WGDatabase db) {
        if (!db.isDesignRole()) { 
            return;
        }
        
        if (db.getDesignProvider() != null) {
            db.getDesignProvider().removeDesignChangeListener(this);
        }
        else {
            db.removeDatabaseEventListener(this);
        }
        
        ContainerInfo info = _containerInfos.remove(db.getDbReference());
        if (info != null) {
            WGAPlugin plugin = null;
            PluginID pluginID = (PluginID) db.getAttribute(WGACore.DBATTRIB_PLUGIN_ID);
            if (pluginID != null) {
                plugin = _core.getPluginSet().getPluginByID(pluginID);
            }
            
            SystemContainerContext scContext = new SystemContainerContext(db, info, plugin, false);
            scContext.performDisconnection();
            _core.removeCSConfig(db, info, true);
        }
    }

    public void databaseUpdate(WGDatabaseEvent event) {
        try {
            WGDocument doc = event.getEditedDocument();
            if (doc != null) {
                if (doc instanceof WGFileContainer) {
                    WGFileContainer con = (WGFileContainer) doc;
                    if (con.getName().equals("system")) {
                        checkForUpdates(event.getDatabase());
                    }
                }
            }
            else {
                checkForUpdates(event.getDatabase());

            }
        }
        catch (Exception e) {
            _log.error("Error checking for system container update", e);
        }
    }

    protected ContainerInfo checkForUpdates(WGDatabase database) throws WGException, IOException, InvalidCSConfigVersionException {

        // Load old container info
        ContainerInfo info = _containerInfos.get(database.getDbReference());

        
        // Cancel if design/db is not ready
        if (!database.isReady()) {
            return info;
        }
        
        WGDesignProvider designProvider = database.getDesignProvider();
        if (designProvider instanceof WGADesignProvider) {
            WGADesignProvider wgaDesignProvider = (WGADesignProvider) designProvider;
            if (!wgaDesignProvider.isReady()) {
                return info;
            }
        }
        
        // Double checked locking check for update need
        DynamicClassLoadingChain.SubLoader loaderToSearchModules = null;
        if (needsUpdate(database, info)) {
                
                synchronized (this) {
            
                    // Update container info when container exists
                    if (needsUpdate(database, info)) {
                        
                        // Remove old info
                        if (info !=null) {
                           _core.removeCSConfig(database, info, false);
                           _containerInfos.remove(database.getDbReference());
                        }
                        
                        // Create new info and store
                        info = new ContainerInfo(database, info);
                        _containerInfos.put(database.getDbReference(), info);
            
                        if (info.getCsConfig() != null) {
                            database.setAttribute(WGACore.DBATTRIB_CSCONFIG, info.getCsConfig());
                        }
                        else {
                            database.removeAttribute(WGACore.DBATTRIB_CSCONFIG);
                        }
                        
                        // Process new info for complete WGA runtime
                        _core.enforceCSConfig(database, info);
                        if (info.isEnforcedLibraryUpdate()) {
                            
                            // We must load module registrations manually here because removeCSConfig() removed them and they are not picked up elsewhere, as nothing gets reconnected
                            DynamicClassLoadingChain.SubLoader loader = WGACore.getLibraryClassLoadingChain().getSubLoader(database.getDbReference());
                            if (loader != null) {
                                loaderToSearchModules = loader;
                            }
                        }
                        
                        // Process new info for specific database. We can only do this after the database has been fully connected
                        if (database.getAttribute(WGACore.DBATTRIB_FULLY_CONNECTED) != null) {
                            _core.updateFieldMappings(database, null);
                        }
                        
                        // Trigger HDBModel definition reload
                        HDBModel model = HDBModel.getModel(database);
                        if (model != null) {
                            model.reloadDefinition();
                        }
                    }
                }
                
                
                if (loaderToSearchModules != null) {
                    synchronized (_core) {
                        List<URLClassLoader> loaders = new ArrayList<URLClassLoader>();
                        loaders.add(loaderToSearchModules);
                        _core.getModuleRegistry().searchModuleDefinitions(_core, loaders);
                    }
                }
            
        }
        
        
        return info;
        
    }

    private boolean needsUpdate(WGDatabase database, ContainerInfo info) throws WGAPIException {
        
        WGFileContainer con = database.getFileContainer("system");        
        if (needsUpdate(con, info, (info != null ? info.getLastModified(): null))) {
            return true;
        }
        
        WGFileContainer conOverlay = database.getFileContainer("overlay:system");
        if (needsUpdate(conOverlay, info, (info != null ? info.getOverlayLastModified() : null))) {
            return true;
        }
        
        return false;
    }

    private boolean needsUpdate(WGFileContainer con, ContainerInfo info, Date conLastModified) throws WGAPIException {
        
        // If not yet registered
        if (info == null) {
            return true;
        }
        
        // Both dont exist
        if (con == null && conLastModified == null) {
            return false;
        }
        // No container but info exists: We must update to remove the info
        else if (con == null && conLastModified != null) {
            return true;
        }
        // Container exists but no info yet
        else if (con != null && conLastModified == null) {
            return true;
        }
        // Info and container exist: Compare change dates, return false if equal
        else  {
            if (conLastModified.equals(con.getLastModified())) {
                return false;
            }
            else {
                return true;
            }
        }
    }

    public boolean isTemporary() {
        return false;
    }

    public synchronized URL[] getJARURLs() {
        
        List<URL> urls = new ArrayList<URL>();
        
        Iterator<ContainerInfo> infos = _containerInfos.values().iterator();
        while (infos.hasNext()) {
            ContainerInfo info = infos.next();
            urls.addAll(info.getJARURLs());
            
            for (File classesDir : info.getJavaClassesDirs()) {
                try {
                    URL javaClassesURL = classesDir.toURL();
                    if (!urls.contains(javaClassesURL)) {
                        urls.add(javaClassesURL);
                    }
                }
                catch (MalformedURLException e) {
                    _log.error("Exception registering java classes directory " + classesDir.getPath(), e);
                }
            }
        }

        // Sort the URLs so this version can be compared to previous version
        Collections.sort(urls, URLStringComparator.INSTANCE);
        
        return urls.toArray(new URL[urls.size()]);
        
    }
    
    public List<String> getJARDescriptions() {
        
        List<String> urls = new ArrayList<String>();
        
        Iterator<ContainerInfo> infos = _containerInfos.values().iterator();
        while (infos.hasNext()) {
            ContainerInfo info = infos.next();
            urls.addAll(info.getJARDescriptions());
        }
                
        return urls;
    }

    public void designChanged(WGDesignChangeEvent event) {
        
        // In case of a design changed event by a design provider we must spawn
        // the processing to a separate thread, bc. we cannot be sure that the
        // consumer db that uses the provider was opened, yet we cannot open it
        // in the same thread bc. this might be a security leak
        
        MasterSessionTask task = new MasterSessionTask(event.getDatabase()) {
            protected void exec(WGDatabase db) throws Throwable {
                try {
                    checkForUpdates(db);
                }
                catch (Exception e) {
                    _log.error("Error checking for system container update", e);
                }
            }
        };
        task.run();
                
    }

    public void databaseConnected(WGDatabaseEvent event) {
        try {
            SystemContainerContext scc = addDatabase(event.getDatabase(), false);
            if (scc != null) {
                
                // Put publisher options from design now. Remove those that are reset as first level options
                Map<String, String> publisherOptions = new HashMap<String, String>();
                scc.putPublisherOptions(publisherOptions);
                Set firstLevelPublisherOptions = (Set) event.getDatabase().getAttribute(WGACore.DBATTRIB_FIRSTLEVELPUBLISHEROPTIONS);
                if (firstLevelPublisherOptions != null) {
                    Iterator flOptions = firstLevelPublisherOptions.iterator();
                    while (flOptions.hasNext()) {
                        String option = (String) flOptions.next();
                        publisherOptions.remove(option);
                    }
                }
                
                // Put publisher options. Convert to appropriate type if necessary
                Iterator<String> optionKeys = publisherOptions.keySet().iterator();
                while (optionKeys.hasNext()) {
                    String optionName = optionKeys.next();
                    String optionValue = publisherOptions.get(optionName);
                    event.getDatabase().setAttribute(optionName, optionValue);
                }
                
                scc.performInitialisation();
            }
        }
        catch (InvalidCSConfigVersionException e) {
            _log.error("The design of application '" + event.getDatabase().getDbReference() + "' was developed for a higher WGA version: " + e.getTargetVersion());
        }
        catch (Exception e) {
            _log.error("Exception processing system file container", e);
            _core.getProblemRegistry().addProblem(Problem.create(new WGACore.ConnectDatabaseProblemOccasion(event.getDatabase().getDbReference()), "databaseConnectionFailed.invalidDesign", ProblemSeverity.HIGH, e));
        }
    }

    public void databaseConnectionError(WGDatabaseEvent event) {
    }

    protected void updateLibraryLoader(DynamicClassLoadingChain libraryLoader) {
        
        Iterator<ContainerInfo> infos = _containerInfos.values().iterator();
        while (infos.hasNext()) {
            ContainerInfo info = infos.next();
            if (info.isFromProviderDB()) {
                continue;
            }
            
            // Collect urls for this design
            List<URL> urlList = info.getJARURLs();
            for (File classesDir : info.getJavaClassesDirs()) {
                try {
                    URL javaClassesURL = classesDir.toURL();
                    if (!urlList.contains(javaClassesURL)) {
                        urlList.add(javaClassesURL);
                    }
                }
                catch (MalformedURLException e) {
                    _log.error("Exception registering java classes directory " + classesDir.getPath(), e);
                }
            }
            
            if (urlList.size() == 0) {
                continue;
            }
            
            // Look if it changed, if so, update the library loader with it
            URL[] urls = urlList.toArray(new URL[urlList.size()]);
            URL[] oldUrls = libraryLoader.getSubLoaderURLs(info.getDbkey());
            if (oldUrls == null || !Arrays.equals(urls, oldUrls)) {
                
                if (oldUrls == null || !info.isStaticClasspath()) {
                    try {
                        libraryLoader.updateSubLoader(info.getDbkey(), urls, info.isStaticClasspath());
                        _core.getLog().info("Updating WGA java library loader for design of application '" + info.getDbkey() + "' containing " + urls.length + " java libraries" + (info.isStaticClasspath() ? " (static classpath)" : ""));
                    }
                    catch (IllegalStateException e) {
                        _core.getLog().warn("Java libraries for application '" + info.getDbkey() + "' have changed but WGA java library loader will not be updated because the db design is marked static");
                        _core.getProblemRegistry().addProblem(Problem.create(new LibraryReloadOccasion(info.getDbkey()), "libraryReloadProblem.staticClasspath", ProblemSeverity.LOW, Problem.var("dbkey", info.getDbkey()).var("libs", WGUtils.serializeCollection(info.getJARDescriptions(), "\n"))));
                    }
                }
                else {
                    _core.getLog().warn("Java libraries for application '" + info.getDbkey() + "' have changed but WGA java library loader will not be updated because the db design is marked static");
                    _core.getProblemRegistry().addProblem(Problem.create(new LibraryReloadOccasion(info.getDbkey()), "libraryReloadProblem.staticClasspath", ProblemSeverity.LOW, Problem.var("dbkey", info.getDbkey()).var("libs", WGUtils.serializeCollection(info.getJARDescriptions(), "\n"))));
                }

            }
            
        }
        
    }
    
    public RemoteAction getRemoteAction(WGDatabase db, String actionID) {
        
        ContainerInfo info = _containerInfos.get(db.getDbReference());
        if (info == null) {
            return null;
        }
        
        if (info.getOverlayCsConfig() != null) {
            RemoteAction action = info.getOverlayCsConfig().findRemoteAction(actionID);
            if (action != null) {
                action = (RemoteAction) XStreamUtils.clone(action);
                action.setModuleName("overlay:" + action.getModuleName());
                return action;
            }
        }
        
        if (info.getCsConfig() != null) {
            RemoteAction action = info.getCsConfig().findRemoteAction(actionID);
            if (action != null) {
                return action;
            }
        }
        
        return null;
        
        
        
    }
    
}
