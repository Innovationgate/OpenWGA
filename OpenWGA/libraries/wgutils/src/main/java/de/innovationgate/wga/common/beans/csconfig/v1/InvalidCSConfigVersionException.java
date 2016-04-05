/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.wga.common.beans.csconfig.v1;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import org.apache.commons.vfs2.FileObject;

public class InvalidCSConfigVersionException extends Exception {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _targetVersion = "(unknown)";
    
    public InvalidCSConfigVersionException() {
    }

    public InvalidCSConfigVersionException(File file) {
        super("The version of design configuration is too high for this WGA version");
        try {
            FileInputStream in = new FileInputStream(file);
            _targetVersion = CSConfig.determineMinimumWGAVersion(in);
            in.close();
        }
        catch (Exception e) {
        }
    }
    
    public InvalidCSConfigVersionException(InputStream in) {
        super("The version of design configuration is too high for this WGA version");
        _targetVersion = CSConfig.determineMinimumWGAVersion(in);
    }

    public InvalidCSConfigVersionException(FileObject file) {
        super("The version of design configuration is too high for this WGA version");
        try {
            InputStream in = file.getContent().getInputStream();
            _targetVersion = CSConfig.determineMinimumWGAVersion(in);
            in.close();
        }
        catch (Exception e) {
        }
    }

    public String getTargetVersion() {
        return _targetVersion;
    }

}
