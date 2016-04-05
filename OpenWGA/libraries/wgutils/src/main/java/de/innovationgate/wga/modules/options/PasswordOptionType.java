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

import java.io.IOException;
import java.util.MissingResourceException;

import de.innovationgate.utils.Base64;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.types.PasswordEncodingType;

/**
 * An option that stores passwords in some encoded way to provide some level of protection
 */
public class PasswordOptionType extends StringOptionType implements ConvertingOptionType {
    
    private ModuleRegistry _registry;

    public PasswordOptionType(ModuleRegistry registry) {
        _registry = registry;
    }

    public String convert(Object v) throws OptionConversionException {
        
        String value = String.valueOf(v);
        
        // We do not encode empty password strings
        if (value.equals("")) {
            return value;
        }
        
        // Try to retrieve the default encoder
        PasswordOptionEncoder encoder = (PasswordOptionEncoder) _registry.getContextObjects().get(PasswordEncodingType.class);
        if (encoder == null) {
            encoder = new Base64();
        }
        
        // Build String from encoder type and encoded password
        try {
            String encoded = encoder.encodePassword(value);
            String complete = "[" + encoder.getEncodingKey() + "] " + encoded;
            return complete;
        }
        catch (PasswordEncodingException e) {
            throw new OptionConversionException("Exception encoding password", e);
        }
    }

    public Object unconvert(String value) throws OptionConversionException {
        
        PasswordOptionEncoder encoder = null;
        
        // Empty password strings are stored "as is" and do not need to be decoded
        if (value.equals("")) {
            return value;
        }
        
        // Try to extract encoding header
        String encoding = null;
        if (value.startsWith("[")) {
            int closepos = value.indexOf("]");
            if (closepos != -1) {
                encoding = value.substring(1, closepos);
                value = value.substring(closepos + 2);
            }
        }
        
        // If encoding header present try to fetch a matching encoder. Must fail if the encoder cannot be allocated.
        if (encoding != null) {
            try {
                ModuleDefinition modDef = _registry.getModuleDefinitionByKey(PasswordEncodingType.class, encoding);
                if (modDef != null) {
                    encoder = (PasswordOptionEncoder) _registry.instantiate(modDef);
                }
                else {
                    throw new MissingResourceException("Unknown password encoding '" + encoding + "'", null, encoding);
                }
            }
            catch (Exception e) {
                throw new OptionConversionException("Exception decoding password", e);
            }
        }
        
        // Finally do the decode
        try {
            if (encoder != null) {
                return encoder.decodePassword(value);
            }
            else {
                // Fallback behaviour - We try to decode as Base64. If that does not work we merely return the value
                Base64 base64 = new Base64();
                try {
                    return base64.decodePassword(value);
                }
                catch (PasswordEncodingException e) {
                    return value;
                }
            }
        }
        catch (PasswordEncodingException e) {
            throw new OptionConversionException("Error decoding password", e);
        }
    }

    @Override
    public boolean isEmptyAllowed() {
        return true;
    }

}
