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

package de.innovationgate.wga.modules.options;

import java.nio.charset.Charset;
import java.util.Locale;

import org.apache.commons.lang.CharSet;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * Option type for storing text encodings.
 * This type offers the current platform encoding plus frequently used encodings UTF-8 and ISO-8859-1 while allowing custom inputs.
 *
 */
public class TextEncodingOptionType extends PredefinedValuesOptionType {
    
    private static LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(TextEncodingOptionType.class) + "/optiontypes", TextEncodingOptionType.class.getClassLoader());
    public static final TextEncodingOptionType INSTANCE = new TextEncodingOptionType();
    
    @Override
    public boolean isRestricted() {
        return false;
    }

    private TextEncodingOptionType() {
        super(_bundleLoader, "textencoding");
        addValue("UTF-8");
        addValue("ISO-8859-1");
        String defaultCharset = Charset.defaultCharset().name();
        if (!getValueProvider(null).getProvidedValues().contains(defaultCharset)) {
            addValue(defaultCharset);
        }
    }

}
