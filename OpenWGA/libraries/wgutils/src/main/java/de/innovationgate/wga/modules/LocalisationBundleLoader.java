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

package de.innovationgate.wga.modules;

import java.util.Locale;
import java.util.ResourceBundle;

import de.innovationgate.utils.WGUtils;

/**
 * A tool class to that reads localized information from a bundle of property files. 
 */
public class LocalisationBundleLoader implements BundleLoader {
    
    private String _resourceName;
    private ClassLoader _loader;

    /**
     * Creates a bundle loader using a properties file under the given resource name and the given loader to laod it
     * @param resourceName The base resource name of property files containing labels. Specify the full qualified resource path but omit language code and ".properties" suffix. For example "de/innovationgate/wga/mylabelfile"
     * @param loader A class loader used to retrieve the property file resources
     */
    public LocalisationBundleLoader(String resourceName, ClassLoader loader) {
        _resourceName = resourceName;
        _loader = loader;
    }
    
    /**
     * Creates a bundle loader loading a file &lt;classname&gt;_&lt;fileQualifier&gt;.properties from the class folder of the given class 
     * @param refClass Reference class. Its folder and classname is used for finding the labels file
     * @param fileQualifier Part of the properties file name after the classname and before language codes. Specify null for the properties file only having the class name
     */
    public LocalisationBundleLoader(Class refClass, String fileQualifier) {
        this(WGUtils.getPackagePath(refClass) + "/" + refClass.getSimpleName() + (fileQualifier != null ? "_" + fileQualifier : ""), refClass.getClassLoader());
    }
    
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.modules.BundleLoader#getBundle(java.util.Locale)
     */
    @Override
    public ResourceBundle getBundle(Locale locale) {
        return ResourceBundle.getBundle(_resourceName, locale, _loader);
    }

}
