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

package de.innovationgate.wgpublisher.problems;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.Set;

import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

import de.innovationgate.utils.XStreamUtils;

@Root(name="ConfirmedProblems")
public class ConfirmedProblems {
    
    public static ConfirmedProblems load(InputStream in) throws IOException {
        return (ConfirmedProblems) XStreamUtils.loadUtf8FromInputStream(XStreamUtils.XSTREAM_CLONING, in);
    }
    
    public void write(OutputStream out) throws IOException {
        XStreamUtils.writeUtf8ToOutputStream(this, XStreamUtils.XSTREAM_CLONING, out);
    }

    @ElementList(inline=true)
    private Set<ProblemPath> _paths = new HashSet<ProblemPath>();

    public Set<ProblemPath> getPaths() {
        return _paths;
    }
    
}
