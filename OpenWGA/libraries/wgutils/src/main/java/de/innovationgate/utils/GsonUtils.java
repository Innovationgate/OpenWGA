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

import java.io.IOException;

import com.google.gson.GsonBridge;
import com.google.gson.JsonElement;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

/**
 * Utilities for usign GSON
 */
public abstract class GsonUtils extends GsonBridge {
    
    /**
     * Reads the next string value from the reader or null if it is a null value
     * @param reader The reader
     * @return String value or null
     * @throws IOException
     */
    public static String nextStringOrNull(JsonReader reader) throws IOException {
        if (reader.peek() != JsonToken.NULL) {
            return reader.nextString();
        }
        else {
            reader.nextNull();
            return null;
        }
    }
    
    /**
     * Reads the next string value from the reader or null if it is a null value
     * @param reader The reader
     * @return Boolean value or null
     * @throws IOException
     */
    public static Boolean nextBooleanOrNull(JsonReader reader) throws IOException {
        if (reader.peek() != JsonToken.NULL) {
            return reader.nextBoolean();
        }
        else {
            reader.nextNull();
            return null;
        }
    }
    
    /**
     * Reads the next double value from the reader or null if it is a null value
     * @param reader The reader
     * @return Double value or null
     * @throws IOException
     */
    public static Double nextDoubleOrNull(JsonReader reader) throws IOException {
        if (reader.peek() != JsonToken.NULL) {
            return reader.nextDouble();
        }
        else {
            reader.nextNull();
            return null;
        }
    }
    
    /**
     * Reads the next integer value from the reader or null if it is a null value
     * @param reader The reader
     * @return Integer value or null
     * @throws IOException
     */
    public static Integer nextIntegerOrNull(JsonReader reader) throws IOException {
        if (reader.peek() != JsonToken.NULL) {
            return reader.nextInt();
        }
        else {
            reader.nextNull();
            return null;
        }
    }
    /**
     * Reads the next long value from the reader or null if it is a null value
     * @param reader The reader
     * @return Long value or null
     * @throws IOException
     */
    public static Long nextLongOrNull(JsonReader reader) throws IOException {
        if (reader.peek() != JsonToken.NULL) {
            return reader.nextLong();
        }
        else {
            reader.nextNull();
            return null;
        }
    }
    
    public static <T extends JsonElement> T deepCopy(T element) {
        return GsonBridge.deepCopy(element);
    }

}
