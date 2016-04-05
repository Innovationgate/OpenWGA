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

package de.innovationgate.wgpublisher.design.sync;

import java.util.Iterator;
import java.util.regex.PatternSyntaxException;

import org.apache.commons.vfs2.FileSelectInfo;
import org.apache.commons.vfs2.FileSelector;
import org.apache.commons.vfs2.FileType;

import de.innovationgate.utils.PatternListVerifier;
import de.innovationgate.wgpublisher.WGACore;

public class DesignFileValidator extends PatternListVerifier implements FileSelector {

    public static final int DEFAULT_POLLING_INTERVAL = 1;
    public DesignFileValidator(de.innovationgate.wga.config.DesignConfiguration config, WGACore core) {
        Iterator<String> fileExclusions = config.getFileExclusions().iterator();
        while (fileExclusions.hasNext()) {
            String patternStr = fileExclusions.next();
            try {
               addPattern(patternStr);
            }
            catch (PatternSyntaxException e) {
                core.getLog().error("Cannot parse file exclusion as regular expression: " + patternStr);
            }
        }
    }

    public boolean isValidFileName(String fileName) {
        return (verify(fileName) == null);
    }

    @Override
    public boolean includeFile(FileSelectInfo fileInfo) throws Exception {
        return verify(fileInfo.getFile().getName().getBaseName()) == null;
    }

    @Override
    public boolean traverseDescendents(FileSelectInfo fileInfo) throws Exception {
        return verify(fileInfo.getFile().getName().getBaseName()) == null;
    }
    
}
