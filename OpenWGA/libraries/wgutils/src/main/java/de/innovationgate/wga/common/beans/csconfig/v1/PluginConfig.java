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

import java.util.ArrayList;
import java.util.List;

import de.innovationgate.wga.common.Constants;

public class PluginConfig {
    
    public static Class<? extends PluginConfig> getPluginConfigClassForCompliance(CSConfig csconfig) {
        
        Version complianceVersion;
        if (csconfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) {
            complianceVersion = ((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig) csconfig).getMinimumWGAVersion();
        }
        else {
            complianceVersion = CSConfig.getComplianceVersion(csconfig.getVersionCompliance());
        }
        
        if (complianceVersion != null && complianceVersion.isAtLeast(7, 1)) {
            return de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig.class;
        }
        else if (complianceVersion != null && complianceVersion.isAtLeast(7, 0)) {
            return de.innovationgate.wga.common.beans.csconfig.v4.PluginConfig.class;
        } else if (complianceVersion != null &&  complianceVersion.isAtLeast(5, 0)) {
            return de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig.class;
        }
        else {
            return de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig.class;
        }        
    }
    
    public static PluginConfig instantiatePluginConfigForCompliance(CSConfig csConfig) {
        
        Class<? extends PluginConfig> clazz = getPluginConfigClassForCompliance(csConfig);
        try {
            
            PluginConfig pluginConfig =  clazz.newInstance();
            if (pluginConfig instanceof de.innovationgate.wga.common.beans.csconfig.v4.PluginConfig) {
                ((de.innovationgate.wga.common.beans.csconfig.v4.PluginConfig)pluginConfig).setCsConfig((de.innovationgate.wga.common.beans.csconfig.v4.CSConfig)csConfig);
            }
            return pluginConfig;
        }
        catch (InstantiationException e) {
            throw new RuntimeException("Cannot instantiate " + clazz.getName(), e);
        }
        catch (IllegalAccessException e) {
            throw new RuntimeException("Cannot instantiate " + clazz.getName(), e);
        }
        
    }
    
    public static final String PLUGIN_DBKEY_PREFIX = "plugin-";
    public static final String WGAPLUGIN_SUFFIX = ".wgaplugin";
    public static final String AUTHSOURCE_DEFAULT_DOMAIN = "$DEFAULT";
    
    private PluginID id = new PluginID();

    private String title;
    private String description;
    private String vendor;
    private String webHomepage;
    
    private Version minimumWGAVersion = new Version(4,0,0);
    private Version minimumJavaVersion = new Version(1,4,0);
    
    private String pluginHomepage;
    
    private List<PluginID> dependencies = new ArrayList<PluginID>();
    
    private int personalisationMode = Constants.PERSMODE_LOGIN;
    
    private boolean usageAsDesignProvider;
    private boolean usageAsAuthSource;
    private boolean usageAsContentStore;
    
    private boolean showOnStartPage;

    private String authentication;

    public String getAuthentication() {
        return authentication;
    }
    public void setAuthentication(String authentication) {
        this.authentication = authentication;
    }
    public List<PluginID> getDependencies() {
        return dependencies;
    }
    public void setDependencies(List<PluginID> dependencies) {
        this.dependencies = dependencies;
    }
    public String getDescription() {
        return description;
    }
    public void setDescription(String description) {
        this.description = description;
    }
    public PluginID getId() {
        return id;
    }
    public void setId(PluginID id) {
        this.id = id;
    }
    public Version getMinimumJavaVersion() {
        return minimumJavaVersion;
    }
    public void setMinimumJavaVersion(Version minimumJavaVersion) {
        this.minimumJavaVersion = minimumJavaVersion;
    }
    public Version getMinimumWGAVersion() {
        return minimumWGAVersion;
    }
    public void setMinimumWGAVersion(Version minimumWGAVersion) {
        this.minimumWGAVersion = minimumWGAVersion;
    }
    public String getPluginHomepage() {
        return pluginHomepage;
    }
    public void setPluginHomepage(String pluginHomepage) {
        this.pluginHomepage = pluginHomepage;
    }
    public boolean isUsageAsAuthSource() {
        return usageAsAuthSource;
    }
    public void setUsageAsAuthSource(boolean usageAsAuthSource) {
        this.usageAsAuthSource = usageAsAuthSource;
    }
    public boolean isUsageAsContentStore() {
        return usageAsContentStore;
    }
    public void setUsageAsContentStore(boolean usageAsContentStore) {
        this.usageAsContentStore = usageAsContentStore;
    }
    public boolean isUsageAsDesignProvider() {
        return usageAsDesignProvider;
    }
    public void setUsageAsDesignProvider(boolean usageAsDesignProvider) {
        this.usageAsDesignProvider = usageAsDesignProvider;
    }
    public String getTitle() {
        return title;
    }
    public void setTitle(String title) {
        this.title = title;
    }
    public boolean isShowOnStartPage() {
        return showOnStartPage;
    }
    public void setShowOnStartPage(boolean showOnStartPage) {
        this.showOnStartPage = showOnStartPage;
    }
    
    public String toXML() {
        return CSConfig.XSTREAM.toXML(this);
    }
    
    public static PluginConfig fromXML(String xml) {
        return (PluginConfig) CSConfig.XSTREAM.fromXML(xml);
    }
    public int getPersonalisationMode() {
        return personalisationMode;
    }
    public void setPersonalisationMode(int personalisationMode) {
        this.personalisationMode = personalisationMode;
    }
    public String getVendor() {
        return vendor;
    }
    public void setVendor(String vendor) {
        this.vendor = vendor;
    }
    public String getWebHomepage() {
        return webHomepage;
    }
    public void setWebHomepage(String webHomepage) {
        this.webHomepage = webHomepage;
    }

}
