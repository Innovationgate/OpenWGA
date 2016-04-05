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

package de.innovationgate.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Tools to convert values to other values. Most usable for converting configurations into desired types, where the source type may differ.
 */
public abstract class ConversionUtils {
    
    
    /**
     * Converts arbitrary objects into their String counterpart
     * Normally just uses toString() conversion, but some special behaviours are implemented:
     * <ul>
     * <li>Lists are serialized into comma-separated strings
     * <li>Maps are also serialized into comma-separated strings, where each entry is converted to "key:value", both respectively string-converted.
     * </ul>
     * @param obj Input object
     */
    public static String getString(Object obj) {
        
        if (obj instanceof List<?>) {
            return WGUtils.serializeCollection((List<?>) obj, ",", new ObjectFormatter() {
                @Override
                public String format(Object obj) throws FormattingException {
                    return ConversionUtils.getString(obj);
                }
            });
        }
        else if (obj instanceof Map<?,?>) {
            List<String> entries = new ArrayList<String>();
            for (Map.Entry<?,?> entry : ((Map<?,?>) obj).entrySet()) {
                String key = getString(entry.getKey());
                String value = getString(entry.getValue());
                entries.add(key + ":" + value);
            }
            return getString(entries);
        }
        
        else {
            return obj.toString();
        }
        
        
    }
    
    /**
     * Converts arbitrary objects into their integer counterpart
     * Interpretable as integer are all {@link java.lang.Number} instances and all strings that represent representations of numbers,
     * also all other objects whose {@link #toString()} returns somthing that may be interpreted as number.
     * Non-interpretable objects and null return the default given as argument
     * @param obj Input object
     * @param def Default for returning on unconvertible objects (and null objects)
     */
    public static Integer getInteger(Object obj, int def) {
        
        if (obj instanceof Number) {
            return ((Number) obj).intValue();
        }
        
        try {
            Long l = Long.parseLong(String.valueOf(obj));
            return l.intValue(); 
        }
        catch (NumberFormatException e) {
        }
        
        try {
            Double d = Double.parseDouble(String.valueOf(obj));
            return d.intValue();
        }
        catch (NumberFormatException e) {
        }
        
        return def;
        
    }
    
    /**
     * Converts arbitrary objects into their boolean counterpart
     * Boolean true are:
     * <ul>
     * <li>Boolean.TRUE
     * <li>All strings that {@link #stringToBoolean(String)} thinks are true
     * <li>The numbers 1 and -1 
     *  </ul>
     *  All other objects evaluate to the default given as argument.
     * @param obj Input object
     * @param def Default for returning on unconvertible objects (and null objects)
     */
    public static boolean getBoolean(Object obj, boolean def) {
        
        if (obj instanceof Boolean) {
            return (Boolean) obj;
        }
        
        if (obj instanceof String) {
            return WGUtils.stringToBoolean((String) obj);
        }
        else if (obj instanceof Number) {
            Number num = (Number) obj;
            return num.intValue() == 1 || num.intValue() == -1;
        }
        else {
           return def;
        }
        
    }

}
