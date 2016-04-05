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

import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import de.innovationgate.wga.config.WGAConfiguration;

/**
 * An option type allowing to enable/disable operations for a content share: create, move, delete
 */
public class ShareOpsOptionType extends CommaSeparatedListOptionType {
    
    public static final ShareOpsOptionType INSTANCE = new ShareOpsOptionType();
    
    public static final String ALL_OPERATIONS = "create,move,delete";
    
    private ShareOpsOptionType() {
    }

    @Override
    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return ShareOpsValueProvider.INSTANCE;
    }

    @Override
    public boolean isRestricted() {
        return true;
    }

    @Override
    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(cx.getConfigCopy());
        List providedValues = provider.getProvidedValues();
        
        Iterator<String> values;
        try {
            values = ((List<String>) unconvert(value)).iterator();
        }
        catch (OptionConversionException e) {
            throw new OptionValueValidationException("The values could not be parsed as commaseparated list: " + value);
        }
        
        while (values.hasNext()) {
            String singleValue = (String) values.next();
            if (!providedValues.contains(singleValue)) {
                throw new OptionValueValidationException("The value '" + singleValue + "' is not allowed");
            }
        }
    }
    
    

}
