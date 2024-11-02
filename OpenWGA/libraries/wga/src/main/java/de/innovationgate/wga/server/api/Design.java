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

package de.innovationgate.wga.server.api;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.model.OverlaySupport;
import de.innovationgate.wga.modules.BundleLoader;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.FormInfo;
import de.innovationgate.wgpublisher.MissingDesignResourceException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.design.OverlayDesignProvider;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.db.PluginDesignProvider;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.lang.LanguageBehaviour;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemKeyQualificator;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.problems.UseSpecialTextLoader;
import de.innovationgate.wgpublisher.url.WGASpecificFileURLBuilder;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

/**
 * The Design object represents a design context and offers various related functionalities, for example to fetch additional design resources relative to this context.
 * The design context retrieved by WGA.design() (without parameter) points to the OpenWGA design of the current WebTML/TMLScript environment. Calling WGA.design() with different parameters creates design objects that instead may point to specific designs of other applications.
 * Besides addressing the design of a special OpenWGA application the design context may hold additional design reference information which is called its base reference. This points to a specific resource name as a "starting point". The base reference is used by the design object to locate other resources in a relative way (manually via Design.resolve()) or to retrieve design resources at the base reference position (for example Design.getFileContainer()).
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Design {
    
    /**
     * Parameter for {@link #addProblem(String, String, Map)} containing an Error object
     */
    public static final String ADDPROBLEM_PARAM_ERROR = "error";
    /**
     * Parameter for {@link #addProblem(String, String, Map)} containing the problem severity
     */
    public static final String ADDPROBLEM_PARAM_SEVERITY = "severity";
    private static final String LABELPARAM_CLOSER = "}";
    private static final String LABELPARAM_OPENER = "{";    
    
    /**
     * The problem occasion type used by {@link Design#startProblemOccasion(String)} and {@link Design#addProblem(String, String, Map)}
     */
    public static class DesignOccasion implements ProblemOccasion, UseSpecialTextLoader, ProblemKeyQualificator {
        
        private String _dbKey;
        private boolean _clearedAutomatically;
        private String _occasionKey;
        private WGACore _core;
        private Design _design;

        public DesignOccasion(Design design, String occKey, boolean cleared) throws WGException {
            _core = design._wga.getCore();
            _dbKey = design.db().getDbReference();
            _occasionKey = occKey;
            _clearedAutomatically = cleared;
            _design = design;
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
            return null;
        }

        @Override
        public boolean isClearedAutomatically() {
            return _clearedAutomatically;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_dbKey == null) ? 0 : _dbKey.hashCode());
            result = prime * result + ((_occasionKey == null) ? 0 : _occasionKey.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            DesignOccasion other = (DesignOccasion) obj;
            if (_dbKey == null) {
                if (other._dbKey != null)
                    return false;
            }
            else if (!_dbKey.equals(other._dbKey))
                return false;
            if (_occasionKey == null) {
                if (other._occasionKey != null)
                    return false;
            }
            else if (!_occasionKey.equals(other._occasionKey))
                return false;
            return true;
        }

        @Override
        public BundleLoader createTextLoader() {

            return new BundleLoader() {
                
                @Override
                public ResourceBundle getBundle(Locale locale) {
                    try {
                        
                        // Requested language
                        ResourceBundle bundle = _design.getLabelBundle(null, "problems", locale.toString());
                        if (bundle != null) {
                            return bundle;
                        }
                        
                        // DB default language
                        bundle = _design.getLabelBundle(null, "problems", _design.db().getDefaultLanguage());
                        if (bundle != null) {
                            return bundle;
                        }
                        
                        // Generic problem label fallback language "en"
                        return _design.getLabelBundle(null, "problems", "en");
                        
                    }
                    catch (Exception e) {
                        throw new RuntimeException("Exception retrieving problem text for app " + _dbKey + ", occasion " + _occasionKey, e);
                    }
                }
            };
            
            
        }

        @Override
        public String getBaseKey() {
            return _occasionKey;
        }
        
        
        
    }
    
    private WGA _wga;
    private TMLDesignContext _designContext;
    private Boolean _customizable;
    
    protected Design(WGA wga, String dbKey) throws WGException {
        this(wga, wga.db(dbKey));
    }
    
    protected Design(WGA wga, WGDatabase db) throws WGException {
        _wga =wga;
        if (db == null) {
            throw new IllegalArgumentException("Cannot determine design because database could not be retrieved.");
        }
        
        if (!db.isSessionOpen()) {
            throw new IllegalArgumentException("Cannot determine design because no database session is open: " + db.getDbReference());
        }
        
        if (_wga.isTMLContextAvailable()) {
            _designContext = ((TMLContext) _wga.tmlcontext()).getDesignContext().createContextDelegate(db, null);
        }
        else {
            _designContext = new DesignContext(db, "");
        }
    }
    
    protected Design(WGA wga, TMLContext context, WGDatabase db) throws WGException {
        _wga = wga;
        if (context == null) {
            context = (TMLContext) _wga.createTMLContext(db);
        }
        _designContext = context.getDesignContext().createContextDelegate(db, null);
    }

    protected Design(WGA wga, TMLDesignContext designContext) throws WGException {
        _wga = wga;
        _designContext = designContext;
        if (_designContext == null) {
            throw new WGAServerException("Cannot determine design");
        }

    }
    
    /**
     * Returns a TMLScript global that is available for the current design
     * This method allows to retrieve TMLScript globals with their name as method parameter. It can retrieve normal globals just like globals of design scope.
     * This might contain native TMLScript objects which are not usable from Java.
     * @param context Obsolete parameter. May be null.
     * @param name
     * @throws WGAPIException
     * @deprecated Bogus context parameter. Use {@link #getGlobal(String)}
     */
    public Object getGlobal(Context context, String name) throws WGException {
        return _wga.getCore().getTmlscriptGlobalRegistry().getGlobal(name, _designContext.getDesignDB()).getRef();
    }
    
    /**
     * Returns a TMLScript global that is available for the current design
     * This method allows to retrieve TMLScript globals with their name as method parameter. It can retrieve normal globals just like globals of design scope.
     * This might contain native TMLScript objects which are not usable from Java.
     * @param name Name the global
     * @throws WGAPIException
     */
    public Object getGlobal(String name) throws WGException {
        TMLScriptGlobal global = _wga.getCore().getTmlscriptGlobalRegistry().getGlobal(name, _designContext.getDesignDB());
        if (global != null) {
            return global.getRef();
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns the HDBModel object for the current designs application
     * This property is null if the application of the current design does not use HDBModel.
     */
    @CodeCompletion
    public HDBModel getHdbModel() throws WGException {
        return HDBModel.getModel(_designContext.getDesignDB());
    }
    
    /**
     * Returns a WebTML label from the current design, using default label container and file
     * @param key Label key
     * @return Label text
     * @throws WGException
     */
    public String label(String key) throws WGException {
        return label(null, null, key, null, true); 
    }
    
    /**
     * Returns a WebTML label from the current design, using default label container
     * @param file Label file, specify null for default.
     * @param key Label key
     * @return Label text
     * @throws WGException
     */
    public String label(String file, String key) throws WGException {
        return label(null, file, key, null, true); 
    }
    
    /**
     * Returns a WebTML label from the current design
     * @param container Label container, specify null for default.
     * @param file Label file, specify null for default.
     * @param key Label key
     * @return Label text
     * @throws WGException
     */
    public String label(String container, String file, String key) throws WGException {
        return label(container, file, key, null, true); 
    }
    
    /**
     * Returns a WebTML label from the current design and accepts label parameters
     * @param container label container, specify null for default.
     * @param file label file, specify null for default.
     * @param key label key
     * @param params Label parameters. List element 0 is used as param 1, element 1 as param 2 and so on
     * @return Label text
     * @throws WGException
     */
    public String label(String container, String file, String key, List<String> params) throws WGException {
        return label(container, file, key, params, true); 
    }

    /**
     * Returns a WebTML label from the current design and accepts label parameters
     * @param containerName label container, specify null for default.
     * @param fileName label file, specify null for default.
     * @param key label key
     * @param params Label parameters. List element 0 is used as param 1, element 1 as param 2 and so on
     * @param usePlaceholder Specify true to return label placeholder if the label is not defined. Specify false to return null instead. 
     * @return Label text
     * @throws WGException
     */
    public String label(String containerName, String fileName, String key, List<? extends Object> params, boolean usePlaceholder) throws WGException {
        
    	HashMap<String,Object> config = new HashMap<String,Object>();
    	config.put("container", containerName);
    	config.put("file", fileName);
    	config.put("params", params);
    	config.put("placeholder", usePlaceholder);
    	
    	return label(key, config);
    	
    }
    
    /**
     * Returns a WebTML label from the current design and accepts label parameters
     * @param config map with keys container, file, params, placeholder, language
     * @return Label text
     * @throws WGException
     */
    public String label(String key, Map<String,Object> config) throws WGException {
        
        if (key == null) {
            return null;
        }
        
        // Eventually get defaults for label information
        WGDatabase designDB = _designContext.getDesignDB();
        
        String containerName = (String)config.get("container");
        if (containerName == null) {
            if (_wga.isTMLContextAvailable()) {
                containerName = (String) _wga.tmlcontext().option(Base.OPTION_DEFAULT_LABELCONTAINER + designDB.getDbReference());
            }
            if (containerName == null) {
                containerName = WGAResourceBundleManager.CONTAINER_DEFAULT;
            }
        }
        
        String fileName = (String)config.get("file");
        if (fileName == null) {
            if (_wga.isTMLContextAvailable()) {
                fileName = (String) _wga.tmlcontext().option(Base.OPTION_DEFAULT_LABELFILE + designDB.getDbReference());
            }
            if (fileName == null) {
                fileName = WGAResourceBundleManager.FILE_DEFAULT;
            }
        }
        
        // Support for @base syntax in key, file name, although it technically belongs to the container name (which nobody knows to be settable)
        boolean addBase = false;
        if (fileName.endsWith("@base")) {
            fileName = fileName.substring(0, fileName.indexOf("@base"));
            addBase = true;
        }
        
        if (key.endsWith("@base")) {
            key = key.substring(0, key.indexOf("@base"));
            addBase = true;
        }
        
        if (addBase && !containerName.endsWith("@base")) {
            containerName = containerName + "@base";
        }
       
        // Resolve container name reference, eventually change design db
        DesignResourceReference containerRef = resolveReference(containerName);
        if (!containerRef.getDesignApp().equals(designDB.getDbReference())) {
            designDB = _wga.db(containerRef.getDesignApp());
            if (designDB == null || !designDB.isSessionOpen()) {
                return "(Error retrieving label: Unknown design app)";
            }
        }
                
        WGAResourceBundleManager manager = _wga.getCore().getResourceBundleManager(designDB);
        
        String label = null;
        String forceLanguage = (String)config.get("language");
        if(forceLanguage==null && _wga.isTMLContextAvailable()) {
           	forceLanguage = (String) _wga.tmlcontext().option(Base.OPTION_DEFAULT_LABELLANGUAGE);
        }
        if(forceLanguage==null)
        	forceLanguage = (String) designDB.getAttribute(WGACore.DBATTRIB_FORCE_LABEL_LANGUAGE);
        if (forceLanguage != null) {
            Locale prefLangLocale = WGLanguage.languageNameToLocale(forceLanguage);
            try {
                label = LanguageBehaviourTools.fetchLabelForLanguage(manager, containerRef.getResourceName(), fileName, key, prefLangLocale);
            }
            catch (IOException e) {
                throw new WGAServerException("Exception retrieving label " + containerRef.getResourceName() + "/" + fileName + "/" + key + " for language " + prefLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
            }
        }
        else if(_wga.isTMLContextAvailable()){
            LanguageBehaviour langBehaviour = LanguageBehaviourTools.retrieve(designDB);
            label = langBehaviour.webtmlFetchLabel(manager, (TMLContext) _wga.tmlcontext(), containerRef.getResourceName(), fileName, key);
        }
        else{
            Locale prefLangLocale = WGLanguage.languageNameToLocale(designDB.getDefaultLanguage());
            try {
                label = LanguageBehaviourTools.fetchLabelForLanguage(manager, containerRef.getResourceName(), fileName, key, prefLangLocale);
            }
            catch (IOException e) {
                throw new WGAServerException("Exception retrieving label " + containerRef.getResourceName() + "/" + fileName + "/" + key + " for language " + prefLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
            }        	
        }
        
        // If no label available we return the key prefixed with "#"
        
        Boolean usePlaceholder = (Boolean)config.get("placeholder");
        if(usePlaceholder==null)
        	usePlaceholder=true;
        
        if (label != null) {
        	List<Object> params = (List<Object>)config.get("params");
            return applyLabelParams(label, params);
        }
        else if (usePlaceholder) {
            return "#" + fileName + "." + key;
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Returns an OpenWGA system label from the default bundle
     * @param labelKey Label key 
     * @return Label text
     * @throws WGAServerException
     */
    @CodeCompletion
    public String systemLabel(String labelKey) throws WGException {
        return systemLabel(WGAResourceBundleManager.CONTAINER_DEFAULT, labelKey, null);
    }
    
    /**
     * Returns an OpenWGA system label from the default bundle and accepts label parameters
     * @param labelKey Label key
     * @param params Label parameters. List element 0 is used as param 1, element 1 as param 2 and so on
     * @throws WGAServerException
     */
    @CodeCompletion
    public String systemLabel(String labelKey, List<String> params) throws WGException {
        return systemLabel(WGAResourceBundleManager.CONTAINER_DEFAULT, labelKey, params);
    }
    
    /**
     * Returns an OpenWGA system label
     * @param systemBundleName Label bundle name.
     * @param labelKey Label key
     * @return Label text
     * @throws WGAServerException
     */
    @CodeCompletion
    public String systemLabel(String systemBundleName, String labelKey) throws WGException {
        return systemLabel(systemBundleName, labelKey, null);
    }
    
    /**
     * Returns an OpenWGA system label and accepts label parameters
     * @param systemBundleName Label bundle name.
     * @param labelKey Label key
     * @param params Label parameters. List element 0 is used as param 1, element 1 as param 2 and so on
     * @return Label text
     * @throws WGAServerException
     */
    @CodeCompletion
    public String systemLabel(String systemBundleName, String labelKey, List<String> params) throws WGException {
        
        String label = _wga.getCore().getSystemLabel(systemBundleName, labelKey, _wga.isRequestAvailable() ? _wga.getRequest() : null);
        if (label != null) {
            return applyLabelParams(label, params);
        }
        else {
            return "";
        }

    }

    /**
     * Applys label params to a label string
     * @param label The label string
     * @param params Parameters
     * @return
     * @throws WGException
     */
    public String applyLabelParams(String label, List<? extends Object> params) throws WGException {
    
        // If no labelparam opener in label, we can bypass this operation
        if (label.indexOf(LABELPARAM_OPENER) == -1) {
            return label;
        }
        
        if (params == null) {
            params = new ArrayList<Object>();
        }
        
        Object param;
        for (int idx=0; idx < 6; idx++) {
            if (idx < params.size()) {
                param = params.get(idx);
                if (param == null) {
                    param = "";
                }
                if (_designContext.getVersionCompliance().isAtLeast(6, 2)) {
                    try {
                        param = _wga.encode(getTmlDefaultEncoding(), param);
                    }
                    catch (FormattingException e) {
                        _wga.server().getLog().error("Exception encoding label parameter with WebTML default encoding", e);
                        param = "";
                    }
                }
            }
            else {
                param = "";
            }
            label = WGUtils.strReplace(label, LABELPARAM_OPENER + (idx+1) + LABELPARAM_CLOSER, String.valueOf(param), true);
        }
        return label;
        
    }
    
    /**
     * Returns the default WebTML encoding of this design
     */
    public String getTmlDefaultEncoding() throws WGException {
        return (String) db().getAttribute(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING);
    }

    /**
     * Retrieves all labels of a label file from the design in a specific language
     * @param context WebTML context. Obsolete parameter. May be null.
     * @param container Label container, specify null for default.
     * @param file Label file, specify null for default.
     * @param language Language code to retrieve labels for
     * @return Label bundle
     * @throws WGAPIException
     * @throws IOException
     * @deprecated Bogus context parameter. Use {@link #getLabelBundle(String, String, String)}.
     */
    @CodeCompletion
    public PropertyResourceBundle getLabelBundle(Context context, String container, String file, String language) throws WGException, IOException {
        return getLabelBundle(container, file, language);
    }
    
    /**
     * Retrieves all labels of a label file from the design in a specific language
     * @param container Label container, specify null for default.
     * @param file Label file, specify null for default.
     * @param language Language code to retrieve labels for
     * @return Label bundle
     * @throws WGAPIException
     * @throws IOException
     */
    public PropertyResourceBundle getLabelBundle(String container, String file, String language) throws WGException, IOException {
        
        WGAResourceBundleManager manager = _wga.getCore().getResourceBundleManager(_designContext.getDesignDB());
        
        if (container == null) {
            DesignResourceReference containerRef = resolveReference(WGAResourceBundleManager.CONTAINER_DEFAULT);
            container = containerRef.getResourceName();
        }
        
        if (file == null) {
            file = WGAResourceBundleManager.FILE_DEFAULT;
        }
        
        Locale locale = WGLanguage.languageNameToLocale(language);
        
        return manager.getBundle(container, file, locale);
        
    }

    
    /**
     * Defines a "TMLScript global" variable
     * @param context Obsolete parameter. May be null.
     * @param name Global name, must begin with uppercase letter
     * @param ref Object to store as global
     * @deprecated Bogus context parameter. Use {@link #registerGlobal(String, Object)}.
     */
    public void registerGlobal(Context context, String name, Object ref) throws WGException {
        
        if (ref instanceof String) {
            _wga.getCore().getTmlscriptGlobalRegistry().registerGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_PACKAGE_OR_CLASS, ref));
        }
        else {
            _wga.getCore().getTmlscriptGlobalRegistry().registerGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_OBJECT, ref));
        }
        
        
    }
    
    /**
     * Defines a "TMLScript global" variable
     * @param name Global name, must begin with uppercase letter
     * @param ref Object to store as global
     */
    public void registerGlobal(String name, Object ref) throws WGException {
        
        if (ref instanceof String) {
            _wga.getCore().getTmlscriptGlobalRegistry().registerGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_PACKAGE_OR_CLASS, ref));
        }
        else {
            _wga.getCore().getTmlscriptGlobalRegistry().registerGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_OBJECT, ref));
        }
        
        
    }

    
    /**
     * Removes a TMLScript global
     * This method is intended to remove previously registered TMLScript globals when they should become unavailable, for example on disconnecting the app/plugin which provides the functionality.
     * This is only necessary for TMLScript globals of real global scope. TMLScript globals of app scope (like registered with {@link #registerDbGlobal(Context, String, Object)}) are automatically cleared once their scope app disconnects.
     * @param context Obsolete parameter. May be null.
     * @param globalName Name of the global
     * @deprecated Bogus context parameter. Use {@link #unregisterGlobal(String)}.
     */
    public void unregisterGlobal(Context context, String globalName) throws WGException {
        unregisterGlobal(globalName);
    }
    
    /**
     * Removes a TMLScript global
     * This method is intended to remove previously registered TMLScript globals when they should become unavailable, for example on disconnecting the app/plugin which provides the functionality.
     * This is only necessary for TMLScript globals of real global scope. TMLScript globals of app scope (like registered with {@link #registerDbGlobal(Context, String, Object)}) are automatically cleared once their scope app disconnects.
     * @param globalName Name of the global
     */
    public void unregisterGlobal(String globalName) throws WGException {
        _wga.getCore().getTmlscriptGlobalRegistry().unregisterGlobal(globalName);
    }
    
    /**
     * Defines a "TMLScript db global" variable with a scope limited to the current database
     * @param context WebTML context. Obsolete parameter. May be null.
     * @param name Global name, must begin with uppercase letter
     * @param ref Object to store as global
     * @deprecated Bogus context parameter. Use {@link #registerAppGlobal(String, Object)}.
     */
    public void registerDbGlobal(Context context, String name, Object ref) throws WGException {
        registerAppGlobal(name, ref);
    }
    
    
    /**
     * Defines a "TMLScript App global" variable with a scope limited to the current application
     * @param name Global name, must begin with uppercase letter
     * @param ref Object to store as global
     * @deprecated Misnamed. Use {@link #registerAppGlobal(String, Object)}.
     */
    public void registerDbGlobal(String name, Object ref) throws WGException {
        registerAppGlobal(name, ref);
    }
    
    /**
     * Defines a "TMLScript App global" variable with a scope limited to the current application
     * @param name Global name, must begin with uppercase letter
     * @param ref Object to store as global
     */
    public void registerAppGlobal(String name, Object ref) throws WGException {
        
        if (ref instanceof String) {
            _wga.getCore().getTmlscriptGlobalRegistry().registerAppGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_PACKAGE_OR_CLASS, ref), _designContext.getDesignDB());
        }
        else {
            _wga.getCore().getTmlscriptGlobalRegistry().registerAppGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_OBJECT, ref), _designContext.getDesignDB());
        }
        
    }

    /**
     * Returns the WGAPI database object for the application of the current design
     */
    public WGDatabase db() throws WGException {
        WGDatabase designDB = _designContext.getDesignDB();
        if (designDB != null) {
            return designDB;
        }
        else {
            throw new UnavailableResourceException("Unavailable design app: " + _designContext.getBaseReference().getDesignApp());
        }
    }
    
    /**
     * Returns the App of the current design
     */
    public App app() throws WGException {
        return _wga.app(_designContext.getDesignDB());
    }
    
    /**
     * Returns the Plugin objects for the OpenWGA plugins that provide the current design
     * OpenWGA plugins are often used as design providers for OpenWGA applications. In that constellation the plugins let an application inherit their design to provide the application code. This may be a single plugin but also multiple plugins which together represent a single design by using "overlay chaining". 
     * Using this method on the design context of such an "design consumer application" will return information about the plugins from which the design is originated, so it can be used to detect if a design uses a special design plugin.
     * The order in which plugins are returned are in the order of overlays. First comes the base plugin, then the overlay plugin of the base, then the overlay of this one, and so on.
     * @throws WGException
     */
    public List<Plugin> plugins() throws WGException {
        
        WGDatabase designDb = _designContext.getDesignDB();
        
        List<Plugin> plugins = new ArrayList<Plugin>();
        
        // Look if the design db is a plugin itself
        if (designDb.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
            plugins.add(new Plugin(_wga, designDb));
            return plugins;
        }
        
        WGDesignProvider provider = designDb.getDesignProvider();
        addDesignPlugins(plugins, provider);
        
        return plugins;
        
    }
    
    private void addDesignPlugins(List<Plugin> plugins, WGDesignProvider provider) throws WGException {
        
        if (provider instanceof PluginDesignProvider) {
            String dbKey = ((PluginDesignProvider) provider).getDesignDBKey();
            WGDatabase db = _wga.db(dbKey);
            if (db != null) {
                plugins.add(new Plugin(_wga, db));
            }
        }
        else if (provider instanceof OverlayDesignProvider) {
            OverlayDesignProvider overlayProvider = (OverlayDesignProvider) provider;
            addDesignPlugins(plugins, overlayProvider.getOriginal());
            addDesignPlugins(plugins, overlayProvider.getOverlay());
        }
        
    }

    /**
     * Returns the Plugin object for the OpenWGA plugin that provides the current design
     * OpenWGA plugins are often used as design providers for OpenWGA applications. In that constellation the plugin lets an application inherit its design to provide the application code.
     * Using this method on the design context of such an "design consumer application" will return information about the plugin from which the design is originated. So one possible use case of this method is using it inside the code of an OpenWGA plugin that is intended as a design provider, to evaluate or display information about the plugin providing the current design.
     * It however will also work from the design context of the plugin application itself and just return the information of the current plugin.
     * This method does only return plugins used as base design. To find out about overlay plugins use {@link #plugins()}.
     * @throws WGException
     */
    public Plugin plugin() throws WGException {
        
        WGDatabase designDb = _designContext.getDesignDB();
        
        // Look if the design db is a plugin itself
        if (designDb.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
            return new Plugin(_wga, designDb);
        }
        
        // Follow to the plugin design provider
        WGDesignProvider provider = designDb.getDesignProvider();
        PluginDesignProvider pluginProvider = null;
        
        if (provider instanceof PluginDesignProvider) {
            pluginProvider = (PluginDesignProvider) provider;
        }
        else if (provider instanceof OverlayDesignProvider) {
            OverlayDesignProvider overlayProvider = (OverlayDesignProvider) provider;
            pluginProvider = overlayProvider.getOriginal();
        }
        else {
           return null;
        }
        
        String dbKey = pluginProvider.getDesignDBKey();
        
        
        WGDatabase db = _wga.db(dbKey);
        if (db == null) {
            return null;
        }
        
        return new Plugin(_wga, db);
        
        
    }
    
    /**
     * Calls a WebTML action
     * This is a variant of {@link WGA#callAction(Context, String, List, DesignResourceReference, GlobalExpressionScope)} which is able to call actions that are defined in or for the current design. Otherwise it works just like its pendant on the WGA object.
     * @param context WebTML context under which to execute the action
     * @param actionID ID of the action
     * @param actionArgs Action arguments. List element 0 will be available as WebTML variable "tmlparam1", element 1 as "tmlparam2" and so on
     * @param baseReference The base reference from which to resolve the action ID. Specify null for no base reference.
     * @param globalScope The global scope holding global objects to be available to the action.
     * @return Return value of the action, null if there is none
     * @throws WGException
     */
    public Object callAction(Context context, String actionID, List<Object> actionArgs, DesignResourceReference baseReference, GlobalExpressionScope globalScope) throws WGException {

        // Locate action
        if (baseReference == null) {
            baseReference = _designContext.getBaseReference();
        }
        
        TMLContext tmlContext = (TMLContext) context;
        TMLAction action = tmlContext.getActionByID(actionID, null, baseReference);
        if (action == null) {
            throw new WGAServerException("Could not retrieve action for ID '" + actionID + "'");
        }
        
        // Call action
        TMLActionLink actionLink = action.createActionLink(null, actionArgs, tmlContext);
        Map<String, Object> globalScopeObjects = globalScope != null ? globalScope.getObjects() : null;
        return tmlContext.callCustomAction(action, actionLink, globalScopeObjects).getResult();
        

    }
    
    /**
     * Calls the TMLScript module, adressed by the current base reference, as WebTML action. The current environments TMLContext is used for context
     * @param actionArgs Action arguments. List element 0 will be available as WebTML variable "tmlparam1", element 1 as "tmlparam2" and so on
     * @return Return value of the action, null if there is none
     * @throws WGException
     */
    public Object callAction() throws WGException {
        return callAction(_wga.tmlcontext(), getBaseReference().toString(), Collections.emptyList(), getBaseReference(), null);
    }
    
    /**
     * Calls the TMLScript module, adressed by the current base reference, as WebTML action. The current environments TMLContext is used for context
     * @param actionArgs Action arguments. List element 0 will be available as WebTML variable "tmlparam1", element 1 as "tmlparam2" and so on
     * @return Return value of the action, null if there is none
     * @throws WGException
     */
    public Object callAction(List<Object> actionArgs) throws WGException {
        return callAction(_wga.tmlcontext(), getBaseReference().toString(), actionArgs, getBaseReference(), null);
    }
    
    /**
     * Calls the TMLScript module, adressed by the current base reference, as WebTML action
     * @param context WebTML context under which to execute the action
     * @param actionArgs Action arguments. List element 0 will be available as WebTML variable "tmlparam1", element 1 as "tmlparam2" and so on
     * @return Return value of the action, null if there is none
     * @throws WGException
     */
    public Object callAction(Context cx, List<Object> actionArgs) throws WGException {
        return callAction(cx, getBaseReference().toString(), actionArgs, getBaseReference(), null);
    }
    
    /**
     * Calls the TMLScript module, adressed by the current base reference, as WebTML action
     * @param context WebTML context under which to execute the action
     * @param additionalObjects Objects to be globally available to the action
     * @param actionArgs Action arguments. List element 0 will be available as WebTML variable "tmlparam1", element 1 as "tmlparam2" and so on
     * @return Return value of the action, null if there is none
     * @throws WGException
     */
    public Object callAction(Context cx, List<Object> actionArgs, final Map<String,Object> additionalObjects) throws WGException {
        return callAction(cx, getBaseReference().toString(), actionArgs, getBaseReference(), new GlobalExpressionScope() {

            @Override
            public Map<String, Object> getObjects() {
                return additionalObjects;
            }
            
        });
    }
    
    /**
     * Returns if the current design is actually customized
     */
    public boolean isCustomized() throws WGException {
        WGDesignProvider designProvider = _designContext.getDesignDB().getDesignProvider();
        return (designProvider instanceof OverlayDesignProvider);
    }
    
    /**
     * Returns if the current design is customizable
     * This, unlike {@link #isCustomized()},  also returns true if the design is customizable - so may contain overlay resources - although it is not used with a customization.
     */
    public boolean isCustomable() throws WGException {
        if (_customizable == null) {
            WGDesignProvider designProvider = _designContext.getDesignDB().getDesignProvider();
            CSConfig config = getConfig();
            if(config==null){
            	/*
            	 * #00005190
            	 * Not all content stores support csconfig.xml - e. E. Custom-Domino-Databases.
            	 * Simply return false in this case.
            	 */
            	//_wga.getLog().error("Unable to find csconfig.xml for design " + _designContext.getDesignDB().getDbReference());
            	return false;
            }
            PublisherOption po = config.findPublisherOption(PublisherOption.OPTION_OVERLAY_SUPPORT);
            _customizable = new Boolean(po != null && !OverlaySupport.NONE.equals(po.getValue()));
        }
        return _customizable;
    }
    
    /**
     * Returns the design configuration
     */
    public CSConfig getConfig() throws WGException {
        return (CSConfig) _designContext.getDesignDB().getAttribute(WGACore.DBATTRIB_CSCONFIG);
    }

    /**
     * Creates a {@link FormInfo} to be used to create a new WebTML form
     * @param context The WebTML context for the form
     * @param id The ID of the form
     * @return The form info object
     * @throws WGException
     */
    public FormInfo createFormInfo(Context context, String id) throws WGException {
        
        TMLFormInfo formInfo = ((TMLContext) context).createforminfo(id);
        formInfo.setDefinitionDatabase(_designContext.getDesignDB().getDbReference());
        return formInfo;
        
    }
    
    /**
     * Returns the WebTML design context behind this server API design context
     */
    @CodeCompletion
    public TMLDesignContext getDesignContext() throws WGException {
        return _designContext;
    }
    
    /**
     * Returns the base reference where this design points to
     * A design object may point to a base reference. This is a design resource name which is used for relative adressation of other designs, for example when using {@link #resolve(String)}.
     * This may return an empty string if the design object has no particular base reference.
     */
    @CodeCompletion
    public DesignResourceReference getBaseReference() {
        return _designContext.getBaseReference().normalize();
    }
    
    /**
     * Returns a WGAPI script module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @throws WGAServerException
     * @deprecated Use {@link #getScriptModule(String)}
     */
    public WGScriptModule getScriptModule() throws WGException {
        try {
            
            if (_designContext.getBaseReference().getResourceName().trim().equals("")) {
                throw new WGAServerException("Design context has no resource reference");
            }
            
            return (WGScriptModule) db().getScriptModule(_designContext.getBaseReference().getResourceName());
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception retrieving script module " + _designContext.getBaseReference().getResourceName());
        }
    }
    
    
    /**
     * Returns a WGAPI TMLScript module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @throws WGAServerException
     */
    public WGScriptModule getTMLScriptModule() throws WGException {
        return getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT);
    }
    
    /**
     * Returns a WGAPI JavaScript module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @throws WGAServerException
     */
    public WGScriptModule getJavaScriptModule() throws WGException {
        return getScriptModule(WGScriptModule.CODETYPE_JS);
    }
    
    /**
     * Returns a WGAPI CSS module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @throws WGAServerException
     */
    public WGScriptModule getCSSModule() throws WGException {
        return getScriptModule(WGScriptModule.CODETYPE_CSS);
    }

    /**
     * Returns a WGAPI XML module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @throws WGAServerException
     */
    public WGScriptModule getXMLModule() throws WGException {
        return getScriptModule(WGScriptModule.CODETYPE_XML);
    }

    
    /**
     * Returns a WGAPI script module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @param codeType The script code type
     * @throws WGAServerException
     */
    public WGScriptModule getScriptModule(String codeType) throws WGException {
        try {
            
            if (_designContext.getBaseReference().getResourceName().trim().equals("")) {
                throw new WGAServerException("Design context has no resource reference");
            }
            
            return (WGScriptModule) db().getScriptModule(_designContext.getBaseReference().getResourceName(), codeType);
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception retrieving script module " + _designContext.getBaseReference().getResourceName(), e);
        }
    }
    
    /**
     * Returns a WGAPI WebTML module object for a module at this designs base reference (see {@link #getBaseReference()})
     * @param mediaKey The media ke for which to retrieve a module
     * @throws WGAServerException
     */
    public WGTMLModule getTMLModule(String mediaKey) throws WGException {
        try {
            
            if (_designContext.getBaseReference().getResourceName().trim().equals("")) {
                throw new WGAServerException("Design context has no resource reference");
            }
            
            return db().getTMLModule(_designContext.getBaseReference().getResourceName(), mediaKey);
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception retrieving WebTML module " + _designContext.getBaseReference().getResourceName(), e);
        }
    }
    
    /**
     * Retrieves the code of the CSS module under the base reference of this design.
     * This method also does post processing of the code, which may need an HTTP call available in the current environment to work. 
     * @throws WGException
     */
    public String getCSSCode() throws WGException {
            
        WGScriptModule mod = getCSSModule();
        if (mod == null) {
            throw new WGAServerException("No CSS module under name: " + getBaseReference().toString());
        }

        return getModuleCode(mod, true);
        
    }
    
    /**
     * Retrieves the code of the JavaScript module under the base reference of this design.
     * This method also does post processing of the code, which may need an HTTP call available in the current environment to work. 
     * @throws WGException
     */
    public String getJavaScriptCode() throws WGException {
    	return getJavaScriptCode(true);
    }
    public String getJavaScriptCode(Boolean compress) throws WGException {
        
        WGScriptModule mod = getJavaScriptModule();
        if (mod == null) {
            throw new WGAServerException("No JavaScript module under name: " + getBaseReference().toString());
        }

        return getModuleCode(mod, compress);
        
    }
    
    /**
     * Retrieves the code of the TMLScript module under the base reference of this design.
     * This method also does post processing of the code, which may need an HTTP call available in the current environment to work. 
     * @throws WGException
     */
    public String getTMLScriptCode() throws WGException {
        
        WGScriptModule mod = getTMLScriptModule();
        if (mod == null) {
            throw new MissingDesignResourceException("No TMLScript module under name: " + getBaseReference().toString());
        }

        return getModuleCode(mod, true);
        
    }
    
    /**
     * Retrieves the code of the XML module under the base reference of this design.
     * This method also does post processing of the code, which may need an HTTP call available in the current environment to work. 
     * @throws WGException
     */
    public String getXMLCode() throws WGException {
        
        WGScriptModule mod = getXMLModule();
        if (mod == null) {
            throw new WGAServerException("No XML module under name: " + getBaseReference().toString());
        }

        return getModuleCode(mod, true);
        
    }

    public Integer getModuleCodeHash(WGScriptModule mod) throws InstantiationException, IllegalAccessException, WGException {
        PostProcessResult result = null;
        WGPDispatcher dispatcher = _wga.getCore().getDispatcher();
        if (dispatcher != null && _wga.isRequestAvailable() && _wga.isResponseAvailable()) {
            result = dispatcher.postProcessDesignResource(
                mod, 
                _wga.getRequest(), 
                _wga.getResponse(),
                true
            );
            if (result != null) {
                return result.getHash();
            }
        }
        return mod.getCode().hashCode();
    }
    
    private String getModuleCode(WGScriptModule mod, Boolean compress) throws WGException, WGAServerException {
        try {
            PostProcessResult result = null;
            WGPDispatcher dispatcher = _wga.getCore().getDispatcher();
            if (dispatcher != null && _wga.isRequestAvailable() && _wga.isResponseAvailable()) {
                result = dispatcher.postProcessDesignResource(
                    mod, 
                    _wga.getRequest(), 
                    _wga.getResponse(),
                    compress
                );
            }
            
            if (result != null) {
                return result.getCode();
            }
            else {
                return mod.getCode();
            }
            
        }
        catch (WGException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGAServerException("Exception retrieving module code", e);
        }
    }
    
    /**
     * Returns a WGAPI file container object for a module at this designs base reference (see {@link #getBaseReference()})
     * @throws WGAServerException
     */
    public WGFileContainer getFileContainer() throws WGException {
        try {
            
            if (_designContext.getBaseReference().getResourceName().trim().equals("")) {
                throw new WGAServerException("Design context has no resource reference");
            }
            
            return db().getFileContainer(_designContext.getBaseReference().getResourceName());
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception retrieving file container " + _designContext.getBaseReference().getResourceName());
        }
    }
    
    /**
     * Resolves a database key
     * This will resolve plugin shortcuts to actual database keys, while leaving regular database keys untouched
     * @param  dbkey Database key to resolve
     * @return Resolved database key
     * @throws WGAPIException
     * @throws WGException
     */
    public String resolveDbKey(String dbkey) throws WGException {
        
        if (dbkey == null) {
            return null;
        }
        
        if (dbkey.startsWith("@")) {
            String pluginShortcut = dbkey.substring(1);
            WGDatabase designDB = _designContext.getDesignDB();
            if (designDB == null) {
                throw new WGAServerException("Unable to allocate design database");
            }
            
            @SuppressWarnings("unchecked")
            Map<String,String> pluginShortcuts = (Map<String,String>) designDB.getAttribute(WGACore.DBATTRIB_PLUGIN_SHORTCUTS);
            String pluginUniqueName = (String) pluginShortcuts.get(pluginShortcut);
            if (pluginUniqueName != null) {
                Plugin plugin = _wga.plugin(pluginUniqueName);
                if (plugin != null) {
                    String pluginKey = plugin.getDbKey();
                    if (pluginKey != null) {
                        return pluginKey;
                    }
                }
            }
        }
        
        else if (dbkey.startsWith("#")) {
            String realDbKey = _wga.getCore().getContentdbUuidsToKeys().get(dbkey.substring(1));
            if (realDbKey != null) {
                return realDbKey;
            }
            
        }
        
        return dbkey;
        
    }
    
    /**
     * Resolves a design reference relatively to the current design
     * This method can be used to address another design resource relative to the current design context. It takes all forms of relative addressation in WebTML into account, like local and overlay references, and therefor can be used just like the design reference attributes in WebTML tags like ref of <tml:include>.
     * The result if this method is again a Design object whose base reference points to the resolved design resource name. You can use it to retrieve the resource that you want to, maybe via its direct retrieval methods like Design.getFileContainer().
     * @param refDB Database key of an app to address. Specify null to stay in the current app.
     * @param refString The reference string addressing the resource
     * @return A design object for the design reference
     * @throws WGException 
     */
    @CodeCompletion
    public Design resolve(String refDB, String refString) throws WGException {
        return resolve(new DesignResourceReference(refDB, refString));
    }
    
    /**
     * Resolves a design reference relatively to the current design
     * This method can be used to address another design resource relative to the current design context. It takes all forms of relative addressation in WebTML into account, like local and overlay references, and therefor can be used just like the design reference attributes in WebTML tags like ref of <tml:include>.
     * The result if this method is again a Design object whose base reference points to the resolved design resource name. You can use it to retrieve the resource that you want to, maybe via its direct retrieval methods like Design.getFileContainer().
     * @param refString The reference string addressing the resource
     * @return A design object for the design reference
     * @throws WGException 
     */
    public Design resolve(String refString) throws WGException {
        return resolve(new DesignResourceReference(null, refString));
    }
    
    /**
     * Resolves a design reference relatively to the current design
     * This method can be used to address another design resource relative to the current design context. It takes all forms of relative addressation in WebTML into account, like local and overlay references, and therefor can be used just like the design reference attributes in WebTML tags like ref of <tml:include>.
     * The result if this method is again a Design object whose base reference points to the resolved design resource name. You can use it to retrieve the resource that you want to, maybe via its direct retrieval methods like Design.getFileContainer().
     * @param ref Design resouce reference to resolve
     * @return A design object for the design reference
     * @throws WGException 
     */
    public Design resolve(DesignResourceReference ref) throws WGException {
        
        if (ref.getResourceName() == null) {
            return null;
        }
        
        DesignResourceReference refObj = resolveReference(ref);
        WGDatabase designdb = _wga.db(refObj.getDesignApp());
        if(designdb==null)
        	return null;
        return new Design(_wga, _designContext.createContextDelegate(designdb, refObj.toString()));
    }
    
    /**
     * Resolves a system script of the given name.
     * This method searches for a system resource at its allowed locations. A system resource typically is qualified by "wga:" in its name.
     * Preferred are locations from an overlay (if available), otherwise locations from the base design are used.
     * The method can be instructed to fallback to using the name unqualified by "wga:" if earlier OpenWGA versions supported this location.
     * @param name Name of the script module, without the "wga:" prefix
     * @param type Code type of the script
     * @param unqualifiedFallback Specify true if you also want to search for resources not qualified by "wga:"
     * @return The design context of the resource or null if the resource is unavailable
     */
    public Design resolveSystemScriptModule(String name, String type, boolean unqualifiedFallback) throws WGException {
        return resolveSystemResource(name, WGDocument.TYPE_CSSJS, type, unqualifiedFallback);
    }
    
    /**
     * Resolves a system script of the given name.
     * This method searches for a system resource at its allowed locations. A system resource typically is qualified by "wga:" in its name.
     * Preferred are locations from an overlay (if available), otherwise locations from the base design are used.
     * @param name Name of the script module, without the "wga:" prefix
     * @param type Code type of the script
     * @return The design context of the resource or null if the resource is unavailable
     */
    public Design resolveSystemScriptModule(String name, String type) throws WGException {
        return resolveSystemResource(name, WGDocument.TYPE_CSSJS, type, false);
    }
    
    /**
     * Resolves a system file container of the given name.
     * This method searches for a system resource at its allowed locations. A system resource typically is qualified by "wga:" in its name.
     * Preferred are locations from an overlay (if available), otherwise locations from the base design are used.
     * The method can be instructed to fallback to using the name unqualified by "wga:" if earlier OpenWGA versions supported this location.
     * @param name Name of the system resource, without the "wga:" prefix
     * @param unqualifiedFallback Specify true if you also want to search for resources not qualified by "wga:"
     * @return The design context of the resource or null if the resource is unavailable
     */
    public Design resolveSystemFileContainer(String name, boolean unqualifiedFallback) throws WGException {
        return resolveSystemResource(name, WGDocument.TYPE_FILECONTAINER, null, unqualifiedFallback);
    }
    
    /**
     * Resolves a system file container of the given name.
     * This method searches for a system resource at its allowed locations. A system resource typically is qualified by "wga:" in its name.
     * Preferred are locations from an overlay (if available), otherwise locations from the base design are used.
     * @param name Name of the system resource, without the "wga:" prefix
     * @return The design context of the resource or null if the resource is unavailable
     */
    public Design resolveSystemFileContainer(String name) throws WGException {
        return resolveSystemResource(name, WGDocument.TYPE_FILECONTAINER, null, false);
    }

    
    /**
     * Resolves a system WebTML module of the given name.
     * This method searches for a system resource at its allowed locations. A system resource typically is qualified by "wga:" in its name.
     * Preferred are locations from an overlay (if available), otherwise locations from the base design are used.
     * The method can be instructed to fallback to using the name unqualified by "wga:" if earlier OpenWGA versions supported this location.
     * @param name Name of the WebTML module, without the "wga:" prefix
     * @param mediaKey Media key of the module
     * @param unqualifiedFallback Specify true if you also want to search for resources not qualified by "wga:"
     * @return The design context of the resource or null if the resource is unavailable
     */
    public Design resolveSystemTMLModule(String name, String mediaKey, boolean unqualifiedFallback) throws WGException {
        return resolveSystemResource(name, WGDocument.TYPE_TML, mediaKey, unqualifiedFallback);
    }
    
    /**
     * Resolves a system WebTML module of the given name.
     * This method searches for a system resource at its allowed locations. A system resource typically is qualified by "wga:" in its name.
     * Preferred are locations from an overlay (if available), otherwise locations from the base design are used.
     * @param name Name of the WebTML module, without the "wga:" prefix
     * @param mediaKey Media key of the module
     * @return The design context of the resource or null if the resource is unavailable
     */
    public Design resolveSystemTMLModule(String name, String mediaKey) throws WGException {
        return resolveSystemResource(name, WGDocument.TYPE_TML, mediaKey, false);
    }
    
    /**
     * Returns the file encoding of text files inside the current design
     * If the design is composed of multiple base/overlay designs the encoding of the design is
     * returned to which the current base reference is pointing.
     */
    public String getFileEncoding() {
        WGDesignProvider prov = _designContext.getDesignDB().getDesignProvider();
        if (prov instanceof OverlayDesignProvider) {
            prov = ((OverlayDesignProvider) prov).getDesignProvider(0, getBaseReference().getResourceName(), null);
        }
        return prov.getFileEncoding();
    }
    
    /**
     * Searches for a system file container on the current design containing the given file.
     * Either returns a Design for the container having this file or null, if the file is nowhere found.
     * @param fileName
     * @return
     * @throws WGException
     */
    public Design resolveSystemContainerWithFile(String fileName) throws WGException {
        
        Design design = resolve("overlay:system");
        if (design.getFileContainer() != null && design.getFileContainer().hasFile(fileName)) {
            return design;
        }
        
        design = resolve("system");
        if (design.getFileContainer() != null && design.getFileContainer().hasFile(fileName)) {
            return design;
        }
        
        return null;

        
    }
    
    private Design resolveSystemResource(String name, int docType, String codeType, boolean unqualifiedFallback) throws WGException {
        
    	ArrayList<Design> result = resolveSystemResources(name, docType, codeType, unqualifiedFallback);
    	if(result.size()>0)
    		return result.get(0);
    	else return null;
    	
    }
    
    public ArrayList<Design> resolveSystemResources(String name, int docType, String codeType, boolean unqualifiedFallback) throws WGException {
        
    	ArrayList<Design> result = new ArrayList<Design>();
    	
        WGDesignDocument mod = null;
        if (isCustomable()) {
            mod =_designContext.getDesignDB().getDesignObject(docType, "overlay:wga:" + name, codeType);
            if (mod != null) {
                result.add(resolve(mod.getName()));
            }
        }
        
        mod = _designContext.getDesignDB().getDesignObject(docType, "wga:" + name, codeType);
        if (mod != null) {
        	result.add(resolve(mod.getName()));
        }
        
        if (unqualifiedFallback) {
            if (isCustomable()) {
                mod = _designContext.getDesignDB().getDesignObject(docType, "overlay:" + name, codeType);
                if (mod != null) {
                    result.add(resolve(mod.getName()));
                }
            }
            
            mod = _designContext.getDesignDB().getDesignObject(docType, name, codeType);
            if (mod != null) {
                result.add(resolve(mod.getName()));
            }
        }
        
        return result;
    }
    
    /**
     * Resolves a design reference relatively to the current design
     * This method can be used to address another design resource relative to the current design context. It takes all forms of relative addressation in WebTML into account, like local and overlay references, and therefor can be used just like the design reference attributes in WebTML tags like ref of <tml:include>.
     * @param refString The reference string addressing the resource
     * @return A design resource reference
     * @throws WGException 
     */
    @CodeCompletion
    public DesignResourceReference resolveReference(String refString) throws WGException {
        return resolveReference(new DesignResourceReference(null, refString));
    }
    
    /**
     * Resolves a design reference relatively to the current design
     * This method can be used to address another design resource relative to the current design context. It takes all forms of relative addressation in WebTML into account, like local and overlay references, and therefor can be used just like the design reference attributes in WebTML tags like ref of <tml:include>.
     * @param ref Design resouce reference to resolve
     * @return A design resource reference
     * @throws WGException 
     */
    @CodeCompletion
    public DesignResourceReference resolveReference(DesignResourceReference ref) throws WGException {
        DesignResourceReference baseReference = _designContext.getBaseReference().normalize();
        
        // Determine design app. From ref string, injected parameter or base reference, in this priority order
        String designApp = null;
        boolean explicitDesignAppChoice = false;
        ref = ref.normalize();
        
        if (designApp == null) {
            if (ref.getDesignApp() != null) {
                designApp = ref.getDesignApp();
                explicitDesignAppChoice = true;
            }
            else {
                designApp = baseReference.getDesignApp();
            }
        }
        
        // Resolve dbkey
        designApp = resolveDbKey(designApp);
        boolean designAppChanged = !designApp.equals(baseReference.getDesignApp());
        
        // Isolate @ flag
        String refString = ref.getResourceName();
        int atIdx = refString.indexOf("@");
        String flag = null;
        if (atIdx != -1) {
            flag = refString.substring(atIdx + 1);
            refString = refString.substring(0, atIdx);
        }
        
        // Process reference path to reference name
        refString = TMLContext.processReferencePath(refString, baseReference.getResourceName());
        
        // Special rules for overlay folder/design 
        if (baseReference != null && (baseReference.getResourceName().startsWith(OverlayDesignProvider.OVERLAY_PREFIX) || baseReference.getResourceName().equals(OverlayDesignProvider.OVERLAY_FOLDER))) {
            
            // References always go to overlay, unless the "@base" flag is used or we change the design app
            if (!designAppChanged && !"base".equals(flag) && !refString.startsWith(OverlayDesignProvider.OVERLAY_PREFIX) && !refString.equals(OverlayDesignProvider.OVERLAY_FOLDER)) {
                refString = refString.trim().equals("") ? OverlayDesignProvider.OVERLAY_FOLDER : OverlayDesignProvider.OVERLAY_PREFIX + refString;
            }
            
            /*
            // If "@base" flag is used the reference must go outside the overlay (cannot happen normally (which we shall test right now by commenting it out))
            else if ("base".equals(flag) && refString.startsWith(OverlayDesignProvider.OVERLAY_PREFIX)) {
                refString = refString.substring(OverlayDesignProvider.OVERLAY_PREFIX.length());
            }*/
        }
        
        DesignResourceReference refObj = new DesignResourceReference(designApp, refString);
        refObj.setExplicitDesignAppChoice(explicitDesignAppChoice);
        return refObj;
    }
    
    /**
     * Returns the version compliance of the current design
     * The version compliance of an OpenWGA design determines, for which version of OpenWGA the design was developed. It is used by OpenWGA to adapt its behaviour to what is expected from this design. It is determined in design configuration (see Tab "Design Configuration").
     */
    public Version getVersionCompliance() throws WGException {
        return _designContext.getVersionCompliance();
        
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_designContext == null) ? 0 : _designContext.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Design other = (Design) obj;
        if (_designContext == null) {
            if (other._designContext != null)
                return false;
        }
        else if (!_designContext.equals(other._designContext))
            return false;
        return true;
    }
    
    @Override
    public String toString() {
        return getBaseReference().normalize().toString();
   }
    
    /**
     * Starts a problem occasion
     * @param occasion Key string of the occasion
     * @param clearOldProblems Whether already registered problems for this occasion key should be cleared now
     * @return A problem occasion object to be used with {@link #addProblem(ProblemOccasion, String, Map)}
     */
    public DesignOccasion startProblemOccasion(String occasion, boolean clearOldProblems) throws WGException {
        DesignOccasion occ = new DesignOccasion(this, occasion, clearOldProblems);
        if (clearOldProblems) {
            _wga.getCore().getProblemRegistry().clearProblemOccasion(occ);
        }
        return occ;
        
    }
    
    /**
     * Starts a problem occasion and clears all already registered problems for this occasion key
     * @param occasion Key string of the occasion
     * @return A problem occasion object to be used with {@link #addProblem(ProblemOccasion, String, Map)}
     */
    public DesignOccasion startProblemOccasion(String occasion) throws WGException {
        return startProblemOccasion(occasion, true);
    }
    
    /**
     * Adds a problem to the problem registry
     * @param occasion An occasion object created via {@link #startProblemOccasion(String)}
     * @param key Key string of the problem
     */
    public void addProblem(ProblemOccasion occasion, String key) throws WGException {
        addProblem(occasion, key, null);
    }
    
    /**
     * Adds a problem to the problem registry
     * @param occasionKey Key string of the occasion
     * @param key Key string of the problem
     */
    public void addProblem(String occasionKey, String key) throws WGException {
        ProblemOccasion occ = startProblemOccasion(occasionKey, false);
        addProblem(occ, key, null);
    }
    
    /**
     * Adds a problem to the problem registry
     * @param occasionKey Key string of the occasion
     * @param key Key string of the problem
     * @param params Additional parameters. Use constants ADDPROBLEM_PARAM_... for predefined parameters. All others are used as message variables.
     */
    public void addProblem(String occasionKey, String key, Map<Object,Object> params) throws WGException {
        
        ProblemOccasion occ = startProblemOccasion(occasionKey, false);
        addProblem(occ, key, params);
        
    }
    
    /**
     * Adds a problem to the problem registry
     * @param occasion An occasion object created via {@link #startProblemOccasion(String)}
     * @param key Key string of the problem
     * @param params Additional parameters. Use constants ADDPROBLEM_PARAM_... for predefined parameters. All others are used as message variables.
     */
    public void addProblem(ProblemOccasion occasion, String key, Map<Object,Object> params) throws WGException {
        
        if (params == null) {
            params = new HashMap<Object, Object>();
        }
        
        ProblemSeverity severity = ProblemSeverity.LOW;
        Throwable error = null;
       
        Problem.Vars vars = new Problem.Vars();
        for (Map.Entry<Object,Object> param : params.entrySet()) {
            
            if (param.getKey().equals(ADDPROBLEM_PARAM_ERROR)) {
                error = (Throwable) param.getValue();
            }
            else if (param.getKey().equals(ADDPROBLEM_PARAM_SEVERITY)) {
                severity = ProblemSeverity.valueOf(ProblemSeverity.class, String.valueOf(param.getValue()).toUpperCase());
            }
            else {
                vars.var(String.valueOf(param.getKey()), param.getValue());
            }
            
        }
        
        _wga.getCore().getProblemRegistry().addProblem(Problem.create(occasion, key, severity, vars, error));
        
    }
    
    /**
     * Returns the media key of the current WebTML medium of the environment. Is null when not on a WebTML request.
     * @throws WGAServerException
     */
    public String getTmlMedium() throws WGException {
        
        TMLOption option =  _designContext.getOption(Base.OPTION_TMLMODULE_MEDIAKEY);
        if (option != null && option.getValue() != null) {
            return (String) option.getValue();
        }
        
        if (_wga.isRequestAvailable()) {
            String mediaKey = (String) _wga.getRequest().getAttribute(WGACore.ATTRIB_MEDIAKEY);
            if (mediaKey != null) {
                return mediaKey;
            }
        }
        
        return null;
        
    }
    

    /**
     * Creates a URL to render a WebTML module, addressed by the base reference of this design object, in a contextless request for the given langiage
     * @param lang The language code
     * @return The URL
     * @throws WGException
	 */
    public String layoutURLforLang(String lang) throws WGException {
    	return layoutURLforLang(lang, null);
    }
    /**
     * Creates a URL to render a WebTML module, addressed by the base reference of this design object, in a contextless request for the given langiage
     * @param lang The language code
     * @param medium The media key for which to create the URL or null
     * @return The URL
     * @throws WGException
	 */
    public String layoutURLforLang(String lang, String medium) throws WGException {
        String resource = getBaseReference().getResourceName();
        if (WGUtils.isEmpty(resource)) {
            throw new WGAServerException("The design object needs to have a base reference to generate a layout URL. Create a design with a base reference using the resolve() method.");
        }
        WGDatabase db = _designContext.getDesignDB();
        TMLContext cx = (TMLContext)_wga.createTMLContext(db.getDummyContent(lang), null);
        return cx.getURLBuilder().buildLayoutURL(cx, db.getDbReference(), medium, resource);
    }
    
    /**
     * Creates a URL to render a WebTML module, addressed by the base reference of this design object, in a contextless request
     * @param medium The media key for which to create the URL
     * @return The URL
     * @throws WGException
     */
    public String layoutURL(String medium) throws WGException {
        
        String resource = getBaseReference().getResourceName();
        if (WGUtils.isEmpty(resource)) {
            throw new WGAServerException("The design object needs to have a base reference to generate a layout URL. Create a design with a base reference using the resolve() method.");
        }
        
        TMLContext cx;
        if (_wga.isTMLContextAvailable()) {
            cx = (TMLContext) _wga.tmlcontext();
        }
        else {
            cx = (TMLContext) _wga.createTMLContext(_designContext.getDesignDB());
        }
        return cx.getURLBuilder().buildLayoutURL(cx, _designContext.getDesignDB().getDbReference(), medium, resource);
        
    }
    
    /**
     * Creates a URL to render a WebTML module, adressed by the base reference of this design object, in a contextless request
     * This variant uses the WebTML medium of the current script environment.
     * @return The URL
     * @throws WGException
     */
    public String layoutURL() throws WGException {
        return layoutURL(null);
    }
    
    /**
     * Creates a URL serving the code of a publishable script module resource, adressed by the base reference of this design object
     * @param codeType The code type of the script module, either "css" or "js"
     * @return The URL
     * @throws WGException
     */
    public String scriptURL(String codeType) throws WGException {
        
        String resource = getBaseReference().getResourceName();
        if (WGUtils.isEmpty(resource)) {
            throw new WGAServerException("The design object needs to have a base reference to generate a script URL. Create a design with a base reference using the resolve() method.");
        }
        
        TMLContext cx;
        if (_wga.isTMLContextAvailable()) {
            cx = (TMLContext) _wga.tmlcontext();
        }
        else {
            cx = (TMLContext) _wga.createTMLContext(_designContext.getDesignDB());
        }
        return cx.getURLBuilder().buildScriptURL(cx, _designContext.getDesignDB().getDbReference(), codeType, resource);
        
    }
    
    /**
     * Returns the names of all WebTML modules of the given media key whose name either equals the base reference or who are inside the folder adressed by the base reference
     * @param mediaKey The media key of modules to return
     * @param descend Specify true to return all contained modules at any folder level. Specify false to return only modules directly inside the base reference folder. 
     * @return List of module names
     * @throws WGException
     */
    public List<String> getTMLModuleNames(String mediaKey, boolean descend) throws WGException {
        
        List<String> moduleNames = new ArrayList<String>();
        String ref = _designContext.getBaseReference().getResourceName();
        for (WGTMLModule mod : _designContext.getDesignDB().getTMLModules()) {
            if (mediaKey.equals(mod.getMediaKey())) {
                addIfContained(moduleNames, ref, mod.getName(), descend);
            }
        }
        
        return moduleNames;
        
    }
    
    /**
     * Returns the names of all script modules of the given code type whose name either equals the base reference or who are inside the folder adressed by the base reference 
     * @param codeType The code type of modules to return
     * @param descend Specify true to return all contained modules at any folder level. Specify false to return only modules directly inside the base reference folder. 
     * @return List of module names
     * @throws WGException
     */
    public List<String> getScriptModuleNames(String codeType, boolean descend) throws WGException {
        
        List<String> moduleNames = new ArrayList<String>();
        String ref = _designContext.getBaseReference().getResourceName();
        for (WGScriptModule mod : _designContext.getDesignDB().getScriptModules()) {
            if (codeType.equals(mod.getCodeType())) {
                addIfContained(moduleNames, ref, mod.getName(), descend);
            }
        }
        
        return moduleNames;
        
    }
    
    /**
     * Returns the names of all file containers whose name either equals the base reference or who are inside the folder adressed by the base reference 
     * @param descend Specify true to return all contained file containers at any folder level. Specify false to return only containers directly inside the base reference folder. 
     * @return List of file container names
     * @throws WGException
     */
    public List<String> getFileContainerNames(boolean descend) throws WGException {
        
        List<String> moduleNames = new ArrayList<String>();
        String ref = _designContext.getBaseReference().getResourceName();
        for (WGFileContainer mod : _designContext.getDesignDB().getFileContainers()) {
            addIfContained(moduleNames, ref, mod.getName(), descend);
        }
        
        return moduleNames;
        
    }

    private void addIfContained(List<String> moduleNames, String ref, String name, boolean descend) {
        if (name.equals(ref)) {
            moduleNames.add(name);
        }
        else if ("".equals(ref) || name.startsWith(ref + ":")) {
            
            if (descend) {
                moduleNames.add(name);
            }
            else {
                String remainder = name.substring(ref.length() + 1);
                if (remainder.indexOf(":") == -1) {
                    moduleNames.add(name);
                }
            }
        }
    }
    
    /**
     * Resolve scriptlets in the given input
     * @param context The WebTML context under which to execute the scriptlets
     * @param input The input string
     * @param objects Objects to make available to custom scriptlets
     * @return The input string with resolved scriptlets
     * @throws WGException
     */
    public String resolveScriptlets(Context context, String input, Map<String,Object> objects) throws WGException {
        TMLContext cx = ((TMLContext) context).designContext(_designContext);
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        Map<String,Object> scriptletOptions = new HashMap<String, Object>();
        scriptletOptions.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_MACROS);
        scriptletOptions.put(RhinoExpressionEngine.SCRIPTLETOPTION_OBJECTS, (objects != null ? objects : Collections.emptyMap()));
        return engine.resolveScriptlets(input, cx, scriptletOptions);
    }

    /**
     * Resolve scriptlets in the given input
     * @param context The WebTML context under which to execute the scriptlets
     * @param input The input stream
     * @return The input string with resolved scriptlets
     * @throws WGException
     */
    public String resolveScriptlets(Context context, String input) throws WGException {
        return resolveScriptlets(context, input, null);
    }

    /**
     * Resolve scriptlets in the given input. This variant uses the environments WebTML context.
     * @param input The input stream
     * @return The input string with resolved scriptlets
     * @throws WGException
     */
    public String resolveScriptlets(String input) throws WGException {
        return resolveScriptlets(_wga.tmlcontext(), input, null);
    }
    
    /**
     * Returns the WebTML scope that is currently actve
     * @throws WGException
     */
    public String getWebtmlScope() throws WGException {
        
        TMLOption scope = getDesignContext().getOption(Base.OPTION_WEBTML_SCOPE);
        if (scope != null) {
            return scope.getValue().toString();
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Returns the resource name of the base reference of this design object
     */
    public String getResourceName() {
        return getBaseReference().getResourceName();
    }
    
    /**
     * Generates a URL to a file from design
     * @param dbKey The database key of the app holding the design. Specify null to use the current design.
     * @param containerName The name of the file container in which the file resides
     * @param fileName The name of the file
     * @return Generated URL
     * @throws WGException
     */
    public String fileURL(String dbKey, String containerName, String fileName) throws WGException {

        String resource = getBaseReference().getResourceName();
        if (WGUtils.isEmpty(resource)) {
            throw new WGAServerException("The design object needs to have a base reference to generate a File URL. Create a design with a base reference using the resolve() method.");
        }
        
        TMLContext cx;
        if (_wga.isTMLContextAvailable()) {
            cx = (TMLContext) _wga.tmlcontext();
        }
        else {
            cx = (TMLContext) _wga.createTMLContext(_designContext.getDesignDB());
        }    	
    	
    	WGAURLBuilder builder = cx.getURLBuilder(); 

        if (builder instanceof WGASpecificFileURLBuilder) {
            if (dbKey == null) {
                dbKey = getBaseReference().getDesignApp();
            }
            return ((WGASpecificFileURLBuilder) builder).buildDesignFileURL(cx, dbKey, containerName, fileName);
        }
        else {
            throw new WGAServerException("The URLBuilder used for this project does not support design-specific file URLs as it does not implement " + WGASpecificFileURLBuilder.class.getName() + ": " + builder.getClass().getName());
        }
    }
    
    /**
     * Generates a URL to a file from design
     * @param containerName The name of the file container in which the file resides
     * @param fileName The name of the file
     * @return Generated URL
     * @throws WGException
     */
    public String fileURL(String containerName, String fileName) throws WGException {
        return fileURL(null, containerName, fileName);
    }
    
    /**
     * Generates a URL to a file from the file container that is referenced by this design object
     * @param fileName The name of the file
     * @return Generated URL
     * @throws WGException
     */
    public String fileURL(String fileName) throws WGException {
        return fileURL(null, getBaseReference().getResourceName(), fileName);
    }
    

    public Object executeTMLScript() throws WGException{
   		return WGA.get().tmlscript().runScript(this, getTMLScriptCode());
    }
    public Object executeTMLScript(Context context) throws WGException{
   		return WGA.get().tmlscript().runScript(this, context, getTMLScriptCode());
    }
    

}