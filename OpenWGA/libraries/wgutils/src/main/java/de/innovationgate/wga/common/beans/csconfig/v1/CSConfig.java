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

package de.innovationgate.wga.common.beans.csconfig.v1;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.vfs2.FileObject;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.mapper.CannotResolveClassException;
import com.thoughtworks.xstream.io.xml.DomDriver;

import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.model.VersionCompliance;
import de.innovationgate.wga.model.WGADesignConfigurationModel;

public class CSConfig {
    
    protected static final XStream XSTREAM = new XStream(new DomDriver());
    
    public static Class<? extends CSConfig> getCSConfigClassForCompliance(String versionCompliance) {
        
        Version complianceVersion = getComplianceVersion(versionCompliance);
        
        if (complianceVersion != null && complianceVersion.isAtLeast(7, 1)) {
            return de.innovationgate.wga.common.beans.csconfig.v5.CSConfig.class;
        }
        if (complianceVersion != null &&  complianceVersion.isAtLeast(7, 0)) {
            return de.innovationgate.wga.common.beans.csconfig.v4.CSConfig.class;
        } else if (complianceVersion != null &&  complianceVersion.isAtLeast(5, 0)) {
            return de.innovationgate.wga.common.beans.csconfig.v3.CSConfig.class;
        }
        else if (VERSIONCOMPLIANCE_WGA41.equals(versionCompliance)) {
            return de.innovationgate.wga.common.beans.csconfig.v2.CSConfig.class;
        }
        else {
            return de.innovationgate.wga.common.beans.csconfig.v1.CSConfig.class;
        }
        
    }
    
    public static Class<? extends CSConfig> getCSConfigClassForMinimumWGAVersion(Version version) {
        if (version.isAtLeast(7,1)) {
            return de.innovationgate.wga.common.beans.csconfig.v5.CSConfig.class;
        }
        else {
            return de.innovationgate.wga.common.beans.csconfig.v4.CSConfig.class;
        }
         
    }
    
    public static Version getMinimumWGAVersion(CSConfig csConfig) {
        if (csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            return ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) csConfig).getMinimumWGAVersion();
        }
        else {
            return csConfig.getComplianceVersion();
        }
    }
    
    public static CSConfig instantiateCSConfig(Class<? extends CSConfig> clazz) {
        
        try {            
            CSConfig csconfig = (CSConfig) clazz.newInstance();
            return csconfig;
        }
        catch (InstantiationException e) {
            throw new RuntimeException("Cannot instantiate " + clazz.getName(), e);
        }
        catch (IllegalAccessException e) {
            throw new RuntimeException("Cannot instantiate " + clazz.getName(), e);
        }
        
    }
    
    // Constants about version compliance
    // Format "wga[majorVersion].[minorVersion]
    // Compliance string with other version digits are illegal
    public static final String VERSIONCOMPLIANCE_WGA3 = "wga3";
    public static final String VERSIONCOMPLIANCE_WGA4 = "wga4";
    public static final String VERSIONCOMPLIANCE_WGA41 = "wga4.1";
    public static final String VERSIONCOMPLIANCE_WGA50 = "wga5.0";
    public static final String VERSIONCOMPLIANCE_WGA51 = "wga5.1";
    public static final String VERSIONCOMPLIANCE_WGA52 = "wga5.2";
    public static final String VERSIONCOMPLIANCE_WGA53 = "wga5.3";
    public static final String VERSIONCOMPLIANCE_WGA54 = "wga5.4";
    public static final String VERSIONCOMPLIANCE_WGA55 = "wga5.5";
    public static final String VERSIONCOMPLIANCE_WGA60 = "wga6.0";
    public static final String VERSIONCOMPLIANCE_WGA61 = "wga6.1";
    public static final String VERSIONCOMPLIANCE_WGA62 = "wga6.2";
    public static final String VERSIONCOMPLIANCE_WGA63 = "wga6.3";
    public static final String VERSIONCOMPLIANCE_WGA70 = "wga7.0";
    public static final String VERSIONCOMPLIANCE_WGA71 = "wga7.1";
    public static final String VERSIONCOMPLIANCE_WGA72 = "wga7.2";
    public static final String VERSIONCOMPLIANCE_WGA73 = "wga7.3";
    public static final String VERSIONCOMPLIANCE_WGA74 = "wga7.4";
    public static final String VERSIONCOMPLIANCE_WGA75 = "wga7.5";
    public static final String VERSIONCOMPLIANCE_WGA76 = "wga7.6";
    public static final String VERSIONCOMPLIANCE_WGA77 = "wga7.7";
    public static final String VERSIONCOMPLIANCE_WGA78 = "wga7.8";
    public static final String VERSIONCOMPLIANCE_WGA79 = "wga7.9";
    public static final String VERSIONCOMPLIANCE_WGA710 = "wga7.10";
    
    private String initScript = null;
    private String connectionScript = null;
    private int anonymousAccessLevel = -1;
    private int defaultAccessLevel = -1;
    private String versionCompliance = VersionCompliance.VERSIONCOMPLIANCE_DEFAULT;
    
    private List publisherOptions = new ArrayList();
    private List encoderMappings = new ArrayList();
    private List elementMappings = new ArrayList();
    private List remoteActions = new ArrayList();
    private List mediaKeys = new ArrayList();
    private List roles = new ArrayList();
    private List jobDefinitions = new ArrayList();
    
    private PluginConfig pluginConfig = null;
    
    public CSConfig() {
        super();
    }

    public static CSConfig load(File file) throws IOException, InvalidCSConfigVersionException {
        try {
        CSConfig csConfig = (CSConfig) XStreamUtils.loadUtf8FromFile(XSTREAM, file);
        csConfig.init();
        return csConfig;
    }
        catch (CannotResolveClassException e) {
            throw new InvalidCSConfigVersionException(file);
        }
    }
    
    public static CSConfig load(InputStream in) throws IOException, InvalidCSConfigVersionException {
    	return load(in, false);
    }
    
    public static CSConfig load(InputStream in, boolean forceClose) throws IOException, InvalidCSConfigVersionException {
        boolean marked = false;
        try {
            if (in.markSupported()) {
                in.mark(1024 * 64);
                marked = true;
            }
        CSConfig csConfig = (CSConfig) XStreamUtils.loadUtf8FromInputStream(XSTREAM, in, forceClose);
        csConfig.init();
        return csConfig;
    }
        catch (CannotResolveClassException e) {
            if (marked) {
                try {
                    in.reset();
                    throw new InvalidCSConfigVersionException(in);
                }
                catch (IOException ee) {
                }
    
            }

            throw new InvalidCSConfigVersionException();

        }
    }
    
    public static CSConfig load(FileObject file) throws IOException, InvalidCSConfigVersionException {
        try {
        CSConfig csConfig = (CSConfig) XStreamUtils.loadUtf8FromFileObject(XSTREAM, file);
        csConfig.init();
        return csConfig;
    }
        catch (CannotResolveClassException e) {
            throw new InvalidCSConfigVersionException(file);
        }
    }
    
    public void write(File file) throws IOException {
        XStreamUtils.writeUtf8ToFile(this, XSTREAM, file);
    }
    
    public void write(FileObject file) throws IOException {
        XStreamUtils.writeUtf8ToFileObject(this, XSTREAM, file);
    }
    
    public List getElementMappings() {
        return elementMappings;
    }
    public void setElementMappings(List elementMappings) {
        this.elementMappings = elementMappings;
    }
    public List getEncoderMappings() {
        return encoderMappings;
    }
    public void setEncoderMappings(List encoderMappings) {
        this.encoderMappings = encoderMappings;
    }
    public PluginConfig getPluginConfig() {
        return pluginConfig;
    }
    public void setPluginConfig(PluginConfig pluginConfig) {
        this.pluginConfig = pluginConfig;
    }
    public List getPublisherOptions() {
        return publisherOptions;
    }
    public void setPublisherOptions(List publisherOptions) {
        this.publisherOptions = publisherOptions;
    }
    public List getRemoteActions() {
        return remoteActions;
    }
    public void setRemoteActions(List remoteActions) {
        this.remoteActions = remoteActions;
    }
    
    public void init() {
        
        if (getMediaKeys() == null) {
            setMediaKeys(new ArrayList());
        }
        if (getRoles() == null) {
            setRoles(new ArrayList());
        }
        if (getJobDefinitions() == null) {
            setJobDefinitions(new ArrayList());
        }
        
        if (getVersionCompliance() == null) {
            setVersionCompliance(VersionCompliance.VERSIONCOMPLIANCE_DEFAULT);
        }
        
        if (getPluginConfig() != null) {
            if (getPluginConfig().getDependencies() == null) {
                getPluginConfig().setDependencies(new ArrayList());
            }
            
            if (getPluginConfig().getTitle() == null) {
                getPluginConfig().setTitle("(no title)");
            }
            
            if (getPluginConfig().getPersonalisationMode() == 0) {
                getPluginConfig().setPersonalisationMode(Constants.PERSMODE_LOGIN);
            }
            
        }
        
    }

    public List getMediaKeys() {
        return mediaKeys;
    }

    public void setMediaKeys(List mediaKeys) {
        this.mediaKeys = mediaKeys;
    }
    
    public RemoteAction findRemoteAction(String moduleName) {
        
        Iterator actions = getRemoteActions().iterator();
        while (actions.hasNext()) {
            RemoteAction action = (RemoteAction) actions.next();
            if (action.getModuleName().equalsIgnoreCase(moduleName)) {
                return action;
            }
        }
        
        return null;
    }
    
    public PublisherOption findPublisherOption(String name) {
        
        Iterator options = getPublisherOptions().iterator();
        while (options.hasNext()) {
            PublisherOption option = (PublisherOption) options.next();
            if (option.getName().equalsIgnoreCase(name)) {
                return option;
            }
        }
        
        return null;
    }

    public String getInitScript() {
        return initScript;
    }

    public void setInitScript(String initScript) {
        this.initScript = initScript;
    }

    public int getAnonymousAccessLevel() {
        return anonymousAccessLevel;
    }

    public void setAnonymousAccessLevel(int anonymousAccessLevel) {
        this.anonymousAccessLevel = anonymousAccessLevel;
    }

    public int getDefaultAccessLevel() {
        return defaultAccessLevel;
    }

    public void setDefaultAccessLevel(int defaultAccessLevel) {
        this.defaultAccessLevel = defaultAccessLevel;
    }

    public List getJobDefinitions() {
        return jobDefinitions;
    }

    public void setJobDefinitions(List jobDefinitions) {
        this.jobDefinitions = jobDefinitions;
    }

    public List getRoles() {
        return roles;
    }

    public void setRoles(List roles) {
        this.roles = roles;
    }

    public String getVersionCompliance() {
        return versionCompliance;
    }
    
    public Version getComplianceVersion() {
        return getComplianceVersion(getVersionCompliance());
    }

    public void setVersionCompliance(String versionCompliance) {
        this.versionCompliance = versionCompliance;
    }

    public String getConnectionScript() {
        return connectionScript;
    }

    public void setConnectionScript(String connectionScript) {
        this.connectionScript = connectionScript;
    }

    /**
     * Returns the corresponding OpenWGA version for a given version compliance string
     * @param complianceString
     */
    public static Version getComplianceVersion(String complianceString) {
        VersionCompliance compliance = VersionCompliance.get(complianceString);
        if (compliance == null) {
            compliance = VersionCompliance.get(VersionCompliance.VERSIONCOMPLIANCE_DEFAULT);
        }
        return compliance.toWGAVersion();
    }

    /**
     * Tries to determine the minimum WGA version of the given csconfig.xml data.
     * Fails silently if something goes wrong.
     * @param in The data of the csconfig.xml
     * @return A string describing the target version of the csconfig.xml
     */
    public static String determineMinimumWGAVersion(InputStream in) {
        String version = "(unknown)";
        try {
            Document doc = (new SAXReader()).read(in);
            Element wgaVersionElement = (Element) doc.selectSingleNode("//minimumWGAVersion");
            version = wgaVersionElement.elementText("majorVersion") + "." + wgaVersionElement.elementText("minorVersion") + "." + wgaVersionElement.elementText("maintenanceVersion"); 
        }
        catch (Exception ee) {
        }
        return version;
    }
    
    public String getDesignDefinitionFileName() {
        return DesignDirectory.SYNCINFO_FILE;
    }
    
    public void importOverlayConfig(CSConfig overlayConfig) {
        
        mediaKeys.addAll(overlayConfig.getMediaKeys());
        
    }

}
