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

import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

/**
 * An ISO8601 compliant date format suitable for securely serializing dates, for output to JavaScriptfor example.
 * Uses english locale and UTC time zome, therefor formatted dates should be readable on every other system.
 */
public class ISO8601DateFormat extends SimpleDateFormat {
    
    public static final ISO8601DateFormat INSTANCE = new ISO8601DateFormat();
    
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public ISO8601DateFormat() {
        super("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        setTimeZone(TimeZone.getTimeZone("UTC"));
    }

}
