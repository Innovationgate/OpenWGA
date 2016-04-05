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

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.WGUtils;

public class OneLineFormatter implements ObjectFormatter, ReplaceProcessor {

    @Override
    public String format(Object obj) throws FormattingException {

        String str = WGUtils.strReplace(String.valueOf(obj), "\n", this, true);
        str = WGUtils.strReplace(str, "\r", this, true);
        return str;
        
    }

    @Override
    public int replace(String text, int from, int to, Writer out) throws IOException {

        boolean useSpace = true;
        
        if (from != 0) {
            char before = text.charAt(from - 1);
            if (Character.isWhitespace(before)) {
                useSpace = false;
            }
        }
        
        if (useSpace == true && to < text.length()-1) {
            char after = text.charAt(to+1);
            if (Character.isWhitespace(after)) {
                useSpace = false;
            }
        }
        
        if (useSpace) {
            out.write(" ");
        }
        
        return to+1;
        
    }

}
