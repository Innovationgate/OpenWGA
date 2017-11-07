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

package de.innovationgate.wgpublisher.webtml.utils;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.utils.DESEncrypter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.filter.WGAVirtualHostingFilter;

/**
 * WGA version of the URLBuilder from WGUtils, including control of some WGA-only features, especially var parameters.
 * Var parameters are encrypted URL parameters that are added to the WebTML variables of the request whose URL contains them. 
 */
public class URLBuilder extends de.innovationgate.utils.URLBuilder implements Cloneable {
    
    /**
     * Name of the URL parameter carrying data about var parameters
     */
    public static final String URLPARAM_VARS = "$vars";
    private WGA _wga;
    private Map<String,Object> _varParameters = new HashMap<String, Object>(); 
    private boolean _sessionBoundParams = false;
    private boolean _absoluteUrlGiven = true;
    
    /**
     * Constructor using a URL
     * @param wga The WGA context object
     * @param url The url that this builder should load
     * @throws WGException
     * @throws GeneralSecurityException
     * @throws IOException
     */
    public URLBuilder(WGA wga, URL url) throws WGException, GeneralSecurityException, IOException {
        super(url, wga.getCore().getCharacterEncoding());
        
        _wga = wga;
        
        String varParam = (String)_parameters.get(URLPARAM_VARS);
        if (varParam != null) {
            
            VarParamsMap varParams = extractVarParameters(varParam, _wga.getCore());
            if (varParams != null) {
            
            // Wrong session? Discard var params
            if (_wga.isRequestAvailable() && _wga.session().isAvailable() && !varParams.isValidRequest(_wga.getRequest())) {
                _wga.server().getLog().warn("Var params for wrong session/request used by client " +_wga.getRequest().getRemoteAddr() + ". Not used.");
            }
            else {
                _varParameters.putAll(varParams);
                _parameters.remove(URLPARAM_VARS);
            }
            
            }
        }
    }
    
    /**
     * Constructor using a URL with a TMLContext as context parameter
     * @param context WebTML context
     * @param url The url that this builder should load
     * @throws WGException
     * @throws GeneralSecurityException
     * @throws IOException
     */
    public URLBuilder(TMLContext context, URL url) throws WGException, GeneralSecurityException, IOException {
        this(WGA.get(context), url);
    }

    @Override
 	public URLBuilder setPath(String path) {
    	try {
			super.setPath(path, true);
		} catch (UnsupportedEncodingException | MalformedURLException e) {
			e.printStackTrace();
		}
    	return this;
 	}
    
    /**
     * Reades var parameters from an encrypted var parameter value
     * @param varParam The var parameter value
     * @param core WGACore object
     * @return A decrypted map of var parameters
     * @throws GeneralSecurityException
     * @throws IOException
     */
    @SuppressWarnings("deprecation")
    public static VarParamsMap extractVarParameters(String varParam, WGACore core) throws GeneralSecurityException, IOException {
        
        byte[] zipped = null;
        
        try {
             zipped = core.getSymmetricEncryptionEngine().decryptBase64Web(varParam);
        }
        catch (Exception e) {
        }
        
        // Then try DES for backward compatibility (#00003554)
        if (zipped == null) {
            DESEncrypter desEncrypter = core.getDesEncrypter();
            zipped = desEncrypter.decrypt(varParam);
        }
        
        String xml = WGUtils.unzipString(zipped);
        if (xml == null) {
            return new VarParamsMap();
        }
        
        VarParamsMap varParams = (VarParamsMap) XStreamUtils.XSTREAM_CLONING.fromXML(xml);
        return varParams;
    }
    
    /**
     * Reads the value of the var parameter of the given name
     * @param name Var parameter name
     */
    public Object getVarParameter(String name) {
        return _varParameters.get(name);
    }
    
    /**
     * Removes the var parameter of the given name
     * @param name Var parameter name
     */
    public URLBuilder removeVarParameter(String name) {
        _varParameters.remove(name);
        return this;
    }
    
    /**
     * Tests if a var parameter of the given name exists
     * @param name Var parameter name
     */
    public boolean hasVarParameter(String name) {
        return _varParameters.containsKey(name);
    }
    
    /**
     * Sets a var parameter
     * @param name Var parameter name
     * @param value Var parameter value
     */
    public URLBuilder setVarParameter(String name, Object value) {
        _varParameters.put(name, value);
        return this;
    }
    public URLBuilder setVarParameter(Map<String,Object> params) {
        _varParameters.putAll(params);
        return this;
    }
    
    /**
     * Removes all var parameters
     */
    public URLBuilder clearVarParameters() {
        _varParameters.clear();
        return this;
    }
    
    @Override
    protected Map<String, Object> getRebuildParameters() {

        try {
            if (_varParameters.size() > 0) {
                VarParamsMap varParams = (_sessionBoundParams ? new VarParamsMap(_wga.session().getJavaHttpSession()) : new VarParamsMap());
                varParams.putAll(_varParameters);
                varParams.setPath(_pathUndecoded);
                String xml = XStreamUtils.XSTREAM_CLONING.toXML(varParams);
                byte[] zipped = WGUtils.zipString(xml);
                String encrypted = _wga.getCore().getSymmetricEncryptionEngine().encryptBase64Web(zipped);
                
                Map<String,Object> allParameters = new HashMap<String,Object>();
                allParameters.putAll(super.getRebuildParameters());
                allParameters.put(WGPDispatcher.URLPARAM_VARS, encrypted);
                return allParameters;
            }
            else {
                return super.getRebuildParameters();
            }
        }
        catch (Exception e) {
            throw new IllegalStateException(e);
        }        
        
    }

    @Override
    public URLBuilder setProtocol(String protocol) {
        try {
            super.setProtocol(protocol);
            
            // Adapt the port to the set protocol, based on configured default ports
            if (_wga.isTMLContextAvailable()) {
                try {
                    int port = _wga.getCore().getDefaultPort(_wga.tmlcontext().db(), protocol);
                    setPort(port);
                }
                catch (UnavailableResourceException e) {
                    // Cannot happen as we tested before
                }
            }
            
            return this;
        }
        catch (WGException e) {
            throw new RuntimeException(e);
        }
        
        
    }

    /**
     * Returns if the var parameters are bound to the user session for which they were created
     */
    public boolean isSessionBoundParams() {
        return _sessionBoundParams;
    }

    /**
     * Sets if the var parameters are bound to the user session for which they were created
     * @param sessionBoundParams
     */
    public URLBuilder setSessionBoundParams(boolean sessionBoundParams) {
        _sessionBoundParams = sessionBoundParams;
        return this;
    }

    /**
     * Returns the names of all existing var parameters
     */
    public Set<String> getVarParameterNames() {
        return Collections.unmodifiableSet(_varParameters.keySet());
    }

    /**
     * Returns if the URLBuilder object was constructed from an absolute URL
     */
    public boolean isAbsoluteUrlGiven() {
        return _absoluteUrlGiven;
    }

    /**
     * Sets if the URLBuilder object was constructed from an absolute URL
     */
    public void setAbsoluteUrlGiven(boolean fullUrlGiven) {
        _absoluteUrlGiven = fullUrlGiven;
    }
    
    /**
     * Returns an absolute URL if an absolute URL was given to to the URLBuild object on construction and a relative URL (complete path without host, protocol and port) if not
     */
    public String buildLikeGiven() {
        return super.build(isAbsoluteUrlGiven());
    }
    
    @Override
    public URLBuilder clone() throws CloneNotSupportedException {
        URLBuilder clone = (URLBuilder) super.clone();
        clone._varParameters = new HashMap<String,Object>(_varParameters);
        return clone;
    }
    
    /**
     * Enforces all settings about URLs from the given database, regarding allowed/enforced protocols, hosts and ports
     */
    public URLBuilder enforceDatabaseSettings(Database database) throws WGException {
        
        // Enforced URL settings
        String redirectProtocol = (String) database.getPublisherOption(WGACore.DBATTRIB_REDIRECTPROTOCOL);
        if (redirectProtocol != null) {
            setProtocol(redirectProtocol);
        }
        
        String redirectHost = (String) database.getPublisherOption(WGACore.DBATTRIB_REDIRECTHOST);
        if (redirectHost != null) {
            setHost(redirectHost);
        }
        
        Integer redirectPort = (Integer) database.getPublisherOption(WGACore.DBATTRIB_REDIRECTPORT);
        if (redirectPort != null) {
            setPort(redirectPort);
        }
        
        // Defaulting port if no port given
        if (getPort() == -1 && redirectPort == null) {
            int defaultPort = _wga.getCore().getDefaultPort(database.db(), getProtocol());
            if (!isDefaultPortForProtocol(defaultPort, getProtocol())) {
                setPort(defaultPort);
            }
        }
        
        // Looking up host on virtual hosts. If host is not allowed for this database change it to the preferred host. (#00004196)
        VirtualHost currentHost = WGAVirtualHostingFilter.findMatchingHost(_wga.getCore().getWgaConfiguration().getVirtualHosts(), getHost());
        if (currentHost != null && !WGAVirtualHostingFilter.isDBKeyAllowed(_wga.getCore().getWgaConfiguration(), currentHost, database.getDbKey())) {
            VirtualHost preferredHost = WGAVirtualHostingFilter.findPreferredHostForDatabase(_wga.getCore().getWgaConfiguration(), database.getDbKey());
            if (preferredHost != null && !preferredHost.getServerAliases().contains(getHost())) {
                setHost(preferredHost.getServername());
                if (preferredHost.isHideDefaultDatabaseInURL() && database.getDbKey().equalsIgnoreCase(preferredHost.getDefaultDatabase())) {
                    removeDatabaseKey(database.getDbKey());
                }
            }
        }
        
        return this;
        
    }

    private void removeDatabaseKey(String dbKey) {

        try {
            List<String> pathElements = WGUtils.deserializeCollection(getPath(), "/");
            if (pathElements.size() == 0) {
                return;
            }
            
            int dbKeyPos = 0;
            String contextPath = new URL(_wga.server().getBaseURL()).getPath();
            if (!WGUtils.isEmpty(contextPath) && contextPath.equals("/" + pathElements.get(0))) {
                dbKeyPos = 1;
            }
            
            if (pathElements.size() <= dbKeyPos) {
                return;
            }
            
            if (pathElements.get(dbKeyPos).equals(dbKey)) {
                pathElements.remove(dbKeyPos);
            }
            
            setPath(WGUtils.serializeCollection(pathElements, "/"));
            
            
        }
        catch (Exception e) {
            _wga.getLog().error("Exception removing database key from built URL", e);
        }
        
    }

}
