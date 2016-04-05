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

package de.innovationgate.wgpublisher.webtml.utils;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import de.innovationgate.utils.ISO8601DateFormat;

public class JsonUtils {
    
    private TMLContext _cx;

    public JsonUtils(TMLContext cx) {
        _cx = cx;
    }

    public Object jsonToJava(JsonElement jsonValue) {
        Object value = null;
        if (jsonValue.isJsonNull()) {
            value = null;
        }
        else if (jsonValue.isJsonPrimitive()) {
            JsonPrimitive prim = (JsonPrimitive) jsonValue;
            if (prim.isNumber()) {
                value = prim.getAsDouble();
            }
            else if (prim.isBoolean()) {
                value = prim.getAsBoolean();
            }
            else {
                value = prim.getAsString();
            }
            value = jsonToJavaConversions(value);
        }
        else if (jsonValue.isJsonArray()) {
            JsonArray array = jsonValue.getAsJsonArray();
            List<Object> list = new ArrayList<Object>();
            for (JsonElement element : array) {
                list.add(jsonToJava(element));
            }
        }
        else if (jsonValue.isJsonObject()) {
            JsonObject obj = jsonValue.getAsJsonObject();
            Map<String,Object> map = new HashMap<String, Object>();
            for (Map.Entry<String,JsonElement> entry : obj.entrySet()) {
                map.put(String.valueOf(entry.getKey()), jsonToJava(entry.getValue()));
            }
        }
         
        return value;
    }

    public Object jsonToJavaConversions(Object value) {
        if (value instanceof String) {
            if (((String) value).endsWith("Z")) {
                try {
                    return ISO8601DateFormat.INSTANCE.parse((String) value);
                }
                catch (ParseException e) {
                }
            }
            
            if (((String) value).startsWith("db:")) {
                TMLContext target = _cx.context((String) value, false);
                if (target != null) {
                    return target;
                }
            }
        }
        return value;
    }
    
    

}
