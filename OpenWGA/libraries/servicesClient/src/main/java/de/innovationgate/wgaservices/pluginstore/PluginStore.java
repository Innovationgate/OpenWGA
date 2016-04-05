/*******************************************************************************
 * Copyright (c) 2009, 2010 Innovation Gate GmbH.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Innovation Gate GmbH - initial API and implementation
 ******************************************************************************/

package de.innovationgate.wgaservices.pluginstore;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.activation.DataHandler;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import de.innovationgate.wgaservices.ActionCaller;
import de.innovationgate.wgaservices.ClientFactory;
import de.innovationgate.wgaservices.WGAServiceException;
import de.innovationgate.wgaservices.WGAServices;
import de.innovationgate.wgaservices.types.Form;
import de.innovationgate.wgaservices.types.PluginInfo;
import de.innovationgate.wgaservices.types.RemoteSession;

public class PluginStore {
    
    private static PluginStore _instance = null;
    
    public static final String PLUGIN_STORE_URL = "http://pluginstore.openwga.com";
    private static final String PLUGIN_STORE_USER = "reader";
    private static final String PLUGIN_STORE_PASSWORD = "reader";
    private static final String PLUGIN_STORE_DBKEY = "pluginstore";
    private static final String PLUGIN_STORE_DOMAIN = "pluginstore";
    
    private Map<String, CachedPluginSet> _cachedPluginsForEnvironment = new HashMap<String, CachedPluginSet>();

    
    private class CachedPluginSet {
        private Date _retrieved;
        private Map<String,PluginStorePlugin> _plugins;

        public void setRetrieved(Date retrieved) {
            _retrieved = retrieved;
        }
        public Map<String, PluginStorePlugin> getPlugins() {
            return _plugins;
        }
        public void setPlugins(Map<String, PluginStorePlugin> plugins) {
            _plugins = plugins;
        }
        public boolean isValid(Date lastModified) {
            if (lastModified.after(_retrieved)) {
                return false;
            }
            return true;
        }                
    }
    
    private PluginStore() {        
    }
    
    public static PluginStore getInstance() {
        if (_instance == null) {
            _instance = new PluginStore();
        }
        return _instance;
    }
    

    public Map<String,PluginStorePlugin> getAvailablePlugins(Environment environment) throws IOException {
        Map<String, PluginStorePlugin> cachedPlugins = retrievePluginsFromCache(environment);
        if (cachedPlugins != null) {
            return cachedPlugins;
        } else {
            try {
                ArrayList<String> params = new ArrayList<String>();
                params.add(environment.getWgaVersion().toMainVersionString());
                params.add(environment.getJavaVersion().toMainVersionString());
                ActionCaller actionCaller = createActionCaller();
                String result = (String) actionCaller.callAction("getPlugins", params);            
                XStream xstream = new XStream(new DomDriver());
                @SuppressWarnings("unchecked")
                Map<String,Object> pluginsRaw = (Map<String,Object>) xstream.fromXML(result);
                if (pluginsRaw.containsKey("msg")) {
                    throw new IOException((String)pluginsRaw.get("msg"));
                }
                Map<String,PluginStorePlugin> plugins = new HashMap<String, PluginStorePlugin>();
                for (String uname : pluginsRaw.keySet()) {
                    @SuppressWarnings("unchecked")
                    PluginStorePlugin plugin = PluginStorePlugin.createFrom(uname, (Map<String,Object>)pluginsRaw.get(uname));
                    plugins.put(uname, plugin);
                }
                addPluginsToCache(environment, plugins);
                return plugins;
            }
            catch (Throwable e) {
                if (e instanceof IOException) {
                    throw (IOException)e;
                } else {
                    IOException ioe = new IOException("Unable to retrieve available plugins from plugin store. Message: " + e.getMessage());
                    ioe.setStackTrace(e.getStackTrace());
                    throw ioe;
                }
            }
        }
    }
    
    public List<PluginStorePlugin> getAvailableOverlayPlugins(Environment environment) throws IOException {       
        Map<String,PluginStorePlugin> availablePlugins = getAvailablePlugins(environment);
        
        List<PluginStorePlugin> plugins = new ArrayList<PluginStorePlugin>();
        for (PluginStorePlugin plugin : availablePlugins.values()) {
            if (plugin.hasOverlaySupport()) {
                plugins.add(plugin);        
            }
        }
                
        return plugins;
    }
    
    public InputStream getPluginContent(PluginStorePlugin plugin) throws IOException {
        try {
            ArrayList<String> params = new ArrayList<String>();
            List<String> filenames = new ArrayList<String>();
            filenames.add(plugin.getFilename());
            XStream xstream = new XStream(new DomDriver());
            params.add(xstream.toXML(filenames));
            ActionCaller actionCaller = createActionCaller();
            Form result = (Form) actionCaller.callAction("getPluginFiles", params);
            
            if (result != null && result.getAttachments().size() > 0) {
                DataHandler file = (DataHandler) result.getAttachments().values().iterator().next();
                return file.getInputStream();
            }
            
            return null;
        }
        catch (Throwable e) {
            if (e instanceof IOException) {
                throw (IOException)e;
            } else {
                IOException ioe = new IOException("Unable to retrieve plugin content for plugin '" + plugin.getFilename() + "'. Details: " + e.getMessage());
                ioe.setStackTrace(e.getStackTrace());
                throw ioe;
            }
        }
    }
    
    private ActionCaller createActionCaller() throws MalformedURLException, WGAServiceException {
        WGAServices services = ClientFactory.createCoreServiceClient(PLUGIN_STORE_URL);
        RemoteSession session = services.login(PLUGIN_STORE_DOMAIN, PLUGIN_STORE_USER, PLUGIN_STORE_PASSWORD);        
        return ClientFactory.createActionCaller(services, session, PLUGIN_STORE_DBKEY);
    }

    public List<PluginStorePlugin> getAvailableThemePlugins(Environment environment, PluginInfo basePluginInfo) throws  IOException {
        List<PluginStorePlugin> plugins = new ArrayList<PluginStorePlugin>();

        Map<String,PluginStorePlugin> availablePlugins = getAvailablePlugins(environment);
        
        for (PluginStorePlugin plugin : availablePlugins.values()) {
            if (plugin.isTheme()) {
                if (plugin.getOverlayBasePluginName() != null && plugin.getOverlayBasePluginName().equals(basePluginInfo.getUniqueName())) {
                    if (plugin.getOverlayBasePluginVersion() != null && basePluginInfo.getVersion() != null) { 
                        de.innovationgate.wgaservices.types.Version vOverlayBase = new de.innovationgate.wgaservices.types.Version();
                        vOverlayBase.setMajorVersion(plugin.getOverlayBasePluginVersion().getMajorVersion());
                        vOverlayBase.setMinorVersion(plugin.getOverlayBasePluginVersion().getMinorVersion());
                        vOverlayBase.setMaintenanceVersion(plugin.getOverlayBasePluginVersion().getMaintenanceVersion());
                        vOverlayBase.setPatchVersion(plugin.getOverlayBasePluginVersion().getPatchVersion());
                        if (basePluginInfo.getVersion().compareTo(vOverlayBase) >= 0) {
                            // this a compatible theme for the given base plugin
                            plugins.add(plugin);
                        }
                    }
                }
            }
        }
        return plugins;
    }
    
    
    public List<PluginStorePlugin> getDependencies(Environment environment, PluginStorePlugin plugin) throws  IOException {
        List<PluginStorePlugin> plugins = new ArrayList<PluginStorePlugin>();
        
        try {
            ArrayList<String> params = new ArrayList<String>();
            params.add(plugin.getFilename());
            params.add(environment.getWgaVersion().toMainVersionString());
            params.add(environment.getJavaVersion().toMainVersionString());
            ActionCaller actionCaller = createActionCaller();
            String result = (String) actionCaller.callAction("getPluginDependencies", params);            
            XStream xstream = new XStream(new DomDriver());
            @SuppressWarnings("unchecked")
            Map<String,Object> pluginsRaw = (Map<String,Object>) xstream.fromXML(result);
            if (pluginsRaw.containsKey("msg")) {
                throw new IOException((String)pluginsRaw.get("msg"));
            }
            for (String uname : pluginsRaw.keySet()) {
                @SuppressWarnings("unchecked")
                PluginStorePlugin currentPlugin = PluginStorePlugin.createFrom(uname, (Map<String,Object>)pluginsRaw.get(uname));
                plugins.add(currentPlugin);
            }            
        }
        catch (Throwable e) {
            if (e instanceof IOException) {
                throw (IOException)e;
            } else {
                IOException ioe = new IOException("Unable to retrieve dependency plugins from plugin store. Message: " + e.getMessage());
                ioe.setStackTrace(e.getStackTrace());
                throw ioe;
            }
        }
        
        return plugins;
    }
    
    public Date getLastModified() throws IOException {
        try {
            ArrayList<String> params = new ArrayList<String>();
            ActionCaller actionCaller = createActionCaller();
            String result = (String) actionCaller.callAction("getStoreMetadata", params);            
            XStream xstream = new XStream(new DomDriver());
            @SuppressWarnings("unchecked")
            Map<String,Object> rawResult = (Map<String,Object>) xstream.fromXML(result);
            if (rawResult.containsKey("msg")) {
                throw new IOException((String)rawResult.get("msg"));
            }
            return (Date) rawResult.get("lastmodified");
        } catch (Throwable e) {
            if (e instanceof IOException) {
                throw (IOException)e;
            } else {
                IOException ioe = new IOException("Unable to retrieve last modified date of plugin store. Details: " + e.getMessage());
                ioe.setStackTrace(e.getStackTrace());
                throw ioe;
            }
        }
    }
    
    private Map<String, PluginStorePlugin> retrievePluginsFromCache(Environment env) throws IOException {        
        CachedPluginSet cachedPluginSet = _cachedPluginsForEnvironment.get(createCacheKey(env));
        if (cachedPluginSet != null && cachedPluginSet.isValid(getLastModified())) {
            return cachedPluginSet.getPlugins();
        } else {
            _cachedPluginsForEnvironment.remove(createCacheKey(env));           
        }
        return null;
    }    
    
    private void addPluginsToCache(Environment env, Map<String, PluginStorePlugin> plugins) throws IOException {
        CachedPluginSet set = new CachedPluginSet();
        set.setPlugins(plugins);
        set.setRetrieved(getLastModified());
        _cachedPluginsForEnvironment.put(createCacheKey(env), set);
    }
        
    private String createCacheKey(Environment environment) {
        return environment.getWgaVersion().toMainVersionString() + "/" + environment.getJavaVersion().toMainVersionString();
    }
    
}
