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

package de.innovationgate.wga.common.beans;

import org.dom4j.Element;

import de.innovationgate.wga.common.WGAXML;

/**
 * Represents a design configuration for a WGA database
 */
public class DesignConfiguration {
    
    private String provider = "none";
    private String key = "";
    private String mode = "";
    private String detailInfo = "";
    
    private boolean lookupDesignVariants = false;
    private boolean autoUpdate = true;
    
    /**
     * Creates a design configuration without provider
     */
    public DesignConfiguration() {
        
    }
    
    /**
     * Creates a design configuration with the given provider information
     * @param provider The design provider type. Use constants DESIGNPROVIDER_... of {@link WGAXML}
     * @param key The key of the design provider
     * @param mode The design mode
     * @param detailInfo Detail information neccessary for the design provider type
     */
    public DesignConfiguration(String provider, String key, String mode, String detailInfo) {
        super();
        this.provider = provider;
        this.key = key;
        this.mode = mode;
        this.detailInfo = detailInfo;
    }
    
    /**
     * Creates a design configuration from a design element in wga.xml
     * @param design The design element of a database in wga.xml
     */
    public DesignConfiguration(Element design) {
        provider = design.attributeValue("provider", "none");
        key = design.attributeValue("key", "");

        if (provider.equals(WGAXML.DESIGNPROVIDER_SYNC)) {
            mode = design.attributeValue("mode", "virtual");
        }
        else if (provider.equals(WGAXML.DESIGNPROVIDER_DB)) {
            design.attributeValue("mode", WGAXML.DESIGNSHARING_MODE_ANONYMOUS);
            mode = design.attributeValue("mode", "anonymous");
        }
        
        lookupDesignVariants = Boolean.valueOf(design.attributeValue("lookupvariants", "false")).booleanValue();
        autoUpdate = Boolean.valueOf(design.attributeValue("autoupdate", "true")).booleanValue();
        detailInfo = design.getTextTrim();
    }
    
    /**
     * Creates a design configuration for a global wga design
     * @deprecated for use in WGA5
     */
    public DesignConfiguration(String designRoot, String designRef) {
        provider = WGAXML.DESIGNPROVIDER_SYNC;
        key = designRef;
        mode = "virtual";       
        lookupDesignVariants = false;
        autoUpdate = true;
        detailInfo = designRoot + "/" + designRef;
    }        
    
    /**
     * Return the detail information for the design provider
     */
    public String getDetailInfo() {
        return detailInfo;
    }
    /**
     *Sets the detail information for the design provider
     * @param detailInfo
     */
    public void setDetailInfo(String detailInfo) {
        this.detailInfo = detailInfo;
    }
    /**
     * Returns the key of the design provider
     */
    public String getKey() {
        return key;
    }
    /**
     * Sets the key of the design provider. The individual meaning of the key is defined by the
     * design provider type that is used.
     * @param key
     */
    public void setKey(String key) {
        this.key = key;
    }
    /**
     * Returns if the design sharing should lookup design variants
     */
    public boolean isLookupDesignVariants() {
        return lookupDesignVariants;
    }
    /**
     * Sets if the design sharing should lookup design variants
     * @param lookupDesignVariants
     */
    public void setLookupDesignVariants(boolean lookupDesignVariants) {
        this.lookupDesignVariants = lookupDesignVariants;
    }
    /**
     * Returns the mode of the design provider
     */
    public String getMode() {
        return mode;
    }
    /**
     * Sets the mode of the design provider. The individual meaning and possible values of the mode
     * are defined by the design provider type that is used.
     * @param mode
     */
    public void setMode(String mode) {
        this.mode = mode;
    }
    /**
     * Returns the design provider type as Constant DESIGNPROVIDER_... of {@link WGAXML}
     */
    public String getProvider() {
        return provider;
    }
    /**
     * Set the design provider type. Use Constants DESIGNPROVIDER_... of {@link WGAXML}
     * @param provider
     */
    public void setProvider(String provider) {
        this.provider = provider;
    }

    /**
     * Returns if the design synchronisation should automatically update designs when their files are modified
     */
    public boolean isAutoUpdate() {
        return autoUpdate;
    }

    /**
     * Sets if the design synchronisation should automatically update designs when their files are modified
     * @param autoUpdate
     */
    public void setAutoUpdate(boolean autoUpdate) {
        this.autoUpdate = autoUpdate;
    }
    
}
