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

import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.options.ConvertingOptionType;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.StringOptionType;


/**
 * Option type meant to store comma separated lists "a,b,c" in a string
 */
public class SemicolonSeparatedListOptionType extends StringOptionType implements ConvertingOptionType {
    
    public static final SemicolonSeparatedListOptionType INSTANCE = new SemicolonSeparatedListOptionType();

    public String convert(Object value) throws OptionConversionException {
        List<String> list = (List<String>) value;
        return WGUtils.serializeCollection(list, ";");
    }

    public Object unconvert(String value) throws OptionConversionException {
        return WGUtils.deserializeCollection(value, ";");
    }

    @Override
    public boolean isMultiValue() {
        return true;
    }

}
