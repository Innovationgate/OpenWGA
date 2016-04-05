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

import java.util.List;

import de.innovationgate.utils.WGUtils;

public class PluginID {
    
    private String uniqueName;
    private Version version = new Version(1,0,0);

    public String getUniqueName() {
        return uniqueName;
    }
    public void setUniqueName(String uniqueName) {
        this.uniqueName = uniqueName;
    }
    public Version getVersion() {
        return version;
    }
    public void setVersion(Version version) {
        this.version = version;
    }
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((uniqueName == null) ? 0 : uniqueName.hashCode());
        result = PRIME * result + ((version == null) ? 0 : version.hashCode());
        return result;
    }
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final PluginID other = (PluginID) obj;
        if (uniqueName == null) {
            if (other.uniqueName != null)
                return false;
        }
        else if (!uniqueName.equals(other.uniqueName))
            return false;
        if (version == null) {
            if (other.version != null)
                return false;
        }
        else if (!version.equals(other.version))
            return false;
        return true;
    }
    
    public String buildQualifiedFileName() {
        return getUniqueName() + "-" + getVersion().getMainVersionString() + PluginConfig.WGAPLUGIN_SUFFIX; 
    }
    
    public static PluginID parseQualifiedFileName(String fileName) {
        
        // Cut off suffix
        if (fileName.endsWith(PluginConfig.WGAPLUGIN_SUFFIX)) {
            fileName = fileName.substring(0, fileName.length() - PluginConfig.WGAPLUGIN_SUFFIX.length());
        }
        
        
        int dashPos = fileName.lastIndexOf("-");
        if (dashPos == -1) {
            throw new IllegalArgumentException("File name is no qualified plugin file name: " + fileName);
        }
        
        String name = fileName.substring(0, dashPos);
        String versionStr = fileName.substring(dashPos + 1);
        Version version = new Version(versionStr);
        
        PluginID id = new PluginID();
        id.setUniqueName(name);
        id.setVersion(version);
        return id;
        
    }
    
    public String buildShortFileName() {
        String baseFileName = buildShortName();
        return baseFileName + "-" + getVersion().getMainVersionString() + PluginConfig.WGAPLUGIN_SUFFIX;
    }
    public String buildShortName() {
        String baseFileName = getUniqueName();
        int lastPointPos = baseFileName.lastIndexOf(".");
        if (lastPointPos != -1) {
            baseFileName = baseFileName.substring(lastPointPos + 1);
        }
        return baseFileName;
    }
    public String toString() {
        return getUniqueName() + " Version " + getVersion().toString();
    }
    
    
    public PluginID() {
    }
    
    public PluginID(String str) {
        
        int versionIdx = str.indexOf(" Version ");
        if (versionIdx == -1) {
            throw new IllegalArgumentException("String '" + str + "' represents no version");
        }
        
        uniqueName = str.substring(0, versionIdx );
        version = new Version(str.substring(versionIdx + 9));
        
    }
    
    

}
