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

import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;

/**
 * Tool class to encode/decode strings for usage as HTTP cookie values
 * Especially works around limitations of cookie value characters implemented in Apache Tomcat 6.0, which prevents using control characters and non-ASCII characters as cookie values.
 * This implementation will:
 * <ul>
 * <li>Omit ASCII control characters
 * <li>Encode non-ASCII characters and the percent character URL-encoded
 * <li>Return null if a value cannot be encoded/decoded
 * </ul>
 */
public class CookieValueEncoder {
    
    /**
     * Encodes the value
     * @param value Input value
     * @return Encoded value or null if not encodeable
     */
    public static String encode(String value) {
        
        StringBuffer out = new StringBuffer();
        int len = value.length();
        for (int i = 0; i < len; i++) {
            char c = value.charAt(i);
            
            // Omit control characters completely
            if (c < 0x20 || c == 0x7f) {
                continue;
            }
            
            // Encode characters beyond ASCII plus the percent character
            if (c > 0x7f || c == '%') {
                try {
                    out.append(URIUtil.encodeQuery(String.valueOf(c)));
                }
                catch (URIException e) {
                    continue;
                }
                continue;
            }
            
            // Regular output
            out.append(c);
            
            
        }
        
        return out.toString();
        
    }
    
    /**
     * Decodes the value
     * @param value The encoded value
     * @return Decoded value or null if not decodeable
     */
    public static String decode(String value) {
        try {
            return URIUtil.decode(value);
        }
        catch (URIException e) {
            return null;
        }
    }

}
