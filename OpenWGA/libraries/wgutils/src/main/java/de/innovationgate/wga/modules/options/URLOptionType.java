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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Locale;


/**
 * An option type holding and validating a complete URL
 */
public class URLOptionType extends StringOptionType implements ConvertingOptionType {

    @Override
    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        try {
            new URL(value);
        }
        catch (MalformedURLException e) {
            throw new OptionValueValidationException("This is no valid URL");
        }
        
    }

    public String convert(Object value) throws OptionConversionException {
        if (value == null) {
            return null;
        }

        // Remove trailing slash
        String str = String.valueOf(value);
        if (str.endsWith("/")) {
            str = str.substring(0, str.length() - 1);
        }
        return str;
        
    }

    public Object unconvert(String value) throws OptionConversionException {
        return value;
    }

}
