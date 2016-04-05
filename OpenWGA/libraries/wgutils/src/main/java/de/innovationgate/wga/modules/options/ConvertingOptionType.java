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

import de.innovationgate.wga.modules.OptionDefinitionsMap;

/**
 * Interface for an option type that wishes to define some special conversion/unconversion method for it's values, meant to be used when the options are stored to/read from some kind of persistent form..
 * This is used to convert/uncovert the string option value to/from it's intended "real" datatype, which is determined by {@link OptionType#getDataTypeHint()}.
 * It can also be used to encode/decode the option value if that is intended (for example for encrypting passwords)
 */
public interface ConvertingOptionType extends OptionType {
    
    public String convert(Object value) throws OptionConversionException;
    public Object unconvert(String value) throws OptionConversionException;

}
