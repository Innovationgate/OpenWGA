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

import java.io.IOException;
import java.io.Writer;

import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.WGUtils;

public class CRLFEncoder implements ObjectFormatter {
    
    class CRLFProcessor implements ReplaceProcessor {

        public int replace(String text, int from, int to, Writer out) throws IOException {
            
            // Determine if this linefeed is already preceeded by a carriage return. If so only put out the line feed again
            if (from > 0) {
                char prevChar = text.charAt(from - 1);
                if (prevChar == '\r') {
                    out.write("\n");
                    return to;
                }
            }
            
            // In all other cases enhance the linefeed by a carriage return
            out.write("\r\n");
            return to;
            
        }
        
    }

    public String format(Object obj) {
        
        return WGUtils.strReplace(String.valueOf(obj), "\n", new CRLFProcessor(), true, false);
        
    }

}
