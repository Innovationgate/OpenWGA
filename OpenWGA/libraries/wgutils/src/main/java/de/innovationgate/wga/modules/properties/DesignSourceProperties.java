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

package de.innovationgate.wga.modules.properties;

import java.util.Locale;

import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * A module properties object used by design source modules (WGAPI type DesignSourceModuleType)
 *
 */
public class DesignSourceProperties {
    
    private boolean _singleton;
    private String _singletonUID = null;
    public String getSingletonUID() {
        return _singletonUID;
    }

    public void setSingletonUID(String name) {
        _singletonUID = name;
    }

    private String _singletonTitle = null;
    private String _singletonDescription = null;
    private LocalisationBundleLoader _singletonTitleBundleLoader = null;
    private Class _designProviderClass = null;

    /**
     * Returns a title to be used for the singleton
     * @param locale A locale to use if the title can be localized
     */
    public String getSingletonTitle(Locale locale) {
        if (_singletonTitleBundleLoader != null && _singletonTitle != null) {
            return _singletonTitleBundleLoader.getBundle(locale).getString(_singletonTitle);
        }
        else {
            return _singletonTitle;
        }
    }
    
    /**
     * Returns a title to be used for the singleton
     * @param locale A locale to use if the title can be localized
     */
    public String getSingletonDescription(Locale locale) {
        if (_singletonTitleBundleLoader != null && _singletonDescription != null) {
            return _singletonTitleBundleLoader.getBundle(locale).getString(_singletonDescription);
        }
        else {
            return _singletonDescription;
        }
    }

    /**
     * Sets EITHER unlocalized singleton title to use OR - if a singleton title bundle loader is set - a label key to use with the loader to retrieve a localized title. 
     */
    public void setSingletonTitle(String singletonTitle) {
        _singletonTitle = singletonTitle;
    }
    
    /**
     * Sets EITHER unlocalized singleton description to use OR - if a singleton title bundle loader is set - a label key to use with the loader to retrieve a localized description. 
     */
    public void setSingletonDescription(String singletonDesc) {
        _singletonDescription = singletonDesc;
    }

    /**
     * Returns if this design source is a "singleton", i.e. it does not need/cannot be configured but is a single design source that is automatically available
     */
    public boolean isSingleton() {
        return _singleton;
    }

    /**
     * Sets if the design source is a singleton
     */
    public void setSingleton(boolean singleton) {
        _singleton = singleton;
    }

    /**
     * Returns the bundle loader to retrieve the singleton title
     */
    public LocalisationBundleLoader getSingletonTitleBundleLoader() {
        return _singletonTitleBundleLoader;
    }

    /**
     * Sets a bundle loader to provide localized singleton titles and descriptions. If that is set {@link #setSingletonTitle(String)} should be set to the key under which the title is retrievable.
     */
    public void setSingletonTitleBundleLoader(LocalisationBundleLoader singletonTitleBundleLoader) {
        _singletonTitleBundleLoader = singletonTitleBundleLoader;
    }

    /**
     * Returns the design provider class used by this design source.
     */
    public Class getDesignProviderClass() {
        return _designProviderClass;
    }

    /**
     * Sets the design provider class used by this design source
     */
    public void setDesignProviderClass(Class designProviderClass) {
        _designProviderClass = designProviderClass;
    }

}
