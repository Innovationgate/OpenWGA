/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.webgate.api.utils;

import java.util.HashMap;
import java.util.Iterator;
import de.innovationgate.utils.WGUtils;

/**
 * Parser for native query options like they are used on WGAPI queries.
 * Options are put with key and value into the map. If the option has no value then the value "true" is assumed. Keys are lowercased.
 */
public class NativeQueryOptions extends HashMap<String,String> {
    
    private static final long serialVersionUID = 1L;

    /**
     * Parsing options from a string
     * @param optionsStr The options string
     */
    public NativeQueryOptions(String optionsStr) {
        super();

        if (optionsStr == null) {
            return;
        }
        
        Iterator<String> nativeOptions = WGUtils.deserializeCollection(optionsStr, ",", true, '\'').iterator();
        while (nativeOptions.hasNext()) {
            String option = (String) nativeOptions.next();
            String optionName = option;
            String optionValue = "true";

            int colonPos = option.indexOf(":");
            if (colonPos != -1) {
                optionName = option.substring(0, colonPos).toLowerCase();
                optionValue = option.substring(colonPos+1).trim();
                if (optionValue.startsWith("'") && optionValue.endsWith("'")) {
                    optionValue = optionValue.substring(1, optionValue.length() - 1);
                }
            }
            put(optionName, optionValue);
            
            
        }
        
    }
    
    /**
     * Returns if the option is given as true. This might be by specifying without a value or with value "true".
     * @param optionName The option
     */
    public boolean isOptionTrue(String optionName) {
        return "true".equals(get(optionName.toLowerCase()));
    }

}
