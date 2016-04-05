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

package de.innovationgate.wgaservices.types;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;



/**
 * A plugin version number
 */
public class Version implements Comparable {
      
    private int majorVersion = 0;
    private int minorVersion = 0;
    private int maintenanceVersion = 0;
    private int patchVersion = 0;
    private int buildVersion = 0;
    
    public Version() {        
    }
    
    public Version(String verString) {
        
        if (verString.indexOf("Build") != -1) {
            StringTokenizer tokenizer = new StringTokenizer(verString, "Build");
            verString = tokenizer.nextToken();            
            setBuildVersion(Integer.parseInt(((String)tokenizer.nextToken()).trim()));
        }
        
        StringTokenizer tokenizer = new StringTokenizer(verString, ".");
        if (tokenizer.countTokens() < 3) {
            throw new IllegalArgumentException("Version string '" + verString + "' is not valid");
        }
        
        setMajorVersion(Integer.parseInt(((String) tokenizer.nextToken()).trim()));
        setMinorVersion(Integer.parseInt(((String) tokenizer.nextToken()).trim()));
        setMaintenanceVersion(Integer.parseInt(((String) tokenizer.nextToken()).trim()));
        
        if (tokenizer.hasMoreTokens()) {
            setPatchVersion(Integer.parseInt(((String) tokenizer.nextToken()).trim()));
        }
        
        
    }
    
    public Version(int majorVersion, int minorVersion, int maintenanceVersion) {
        super();
        this.majorVersion = majorVersion;
        this.minorVersion = minorVersion;
        this.maintenanceVersion = maintenanceVersion;
    }
    public Version(int majorVersion, int minorVersion, int maintenanceVersion, int patchVersion, int buildVersion) {
        super();
        this.majorVersion = majorVersion;
        this.minorVersion = minorVersion;
        this.maintenanceVersion = maintenanceVersion;
        this.patchVersion = patchVersion;
        this.buildVersion = buildVersion;
    }

    public int getBuildVersion() {
        return buildVersion;
    }
    public void setBuildVersion(int buildVersion) {
        this.buildVersion = buildVersion;
    }
    public int getMaintenanceVersion() {
        return maintenanceVersion;
    }
    public void setMaintenanceVersion(int maintenanceVersion) {
        this.maintenanceVersion = maintenanceVersion;
    }
    public int getMajorVersion() {
        return majorVersion;
    }
    public void setMajorVersion(int majorVersion) {
        this.majorVersion = majorVersion;
    }
    public int getMinorVersion() {
        return minorVersion;
    }
    public void setMinorVersion(int minorVersion) {
        this.minorVersion = minorVersion;
    }
    public int getPatchVersion() {
        return patchVersion;
    }
    public void setPatchVersion(int patcjVersion) {
        this.patchVersion = patcjVersion;
    }
    
    public int compareTo(Object arg0) {
        
        Version otherVersion = (Version) arg0;
        if (equals(otherVersion)) {
            return 0;
        }
        
        int comp = getMajorVersion() - otherVersion.getMajorVersion();
        if (comp != 0) {
            return comp;
        }
        
        comp = getMinorVersion() - otherVersion.getMinorVersion();
        if (comp != 0) {
            return comp;
        }

        comp = getMaintenanceVersion() - otherVersion.getMaintenanceVersion();
        if (comp != 0) {
            return comp;
        }
        
        comp = getPatchVersion() - otherVersion.getPatchVersion();
        if (comp != 0) {
            return comp;
        }
        
        comp = getBuildVersion() - otherVersion.getBuildVersion();
        if (comp != 0) {
            return comp;
        }
        
        // Should never happen since we compare via equals at the beginning
        return 0;
        
    }

    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + maintenanceVersion;
        result = PRIME * result + majorVersion;
        result = PRIME * result + minorVersion;
        result = PRIME * result + patchVersion;
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final Version other = (Version) obj;
        if (maintenanceVersion != other.maintenanceVersion)
            return false;
        if (majorVersion != other.majorVersion)
            return false;
        if (minorVersion != other.minorVersion)
            return false;
        if (patchVersion != other.patchVersion)
            return false;
        return true;
    }
    
    public boolean isAtLeast(int major, int minor) {
        Version compVersion = new Version(major, minor, 0);
        return (compareTo(compVersion) >= 0);
    }

    public String toString() {
        StringBuffer version = new StringBuffer();
        version.append(getMajorVersion());
        version.append(".");
        version.append(getMinorVersion());
        version.append(".");
        version.append(getMaintenanceVersion());
        
        if (getPatchVersion() != 0) {
            version.append(".");
            version.append(getPatchVersion());   
        }
        
        if (getBuildVersion() != 0) {
            version.append(" Build " + getBuildVersion());
        }
        
        return version.toString();        
    }
    
    
    public String toMainVersionString() {
        List<Integer> items = new ArrayList<Integer>();
        items.add(new Integer(getMajorVersion()));
        items.add(new Integer(getMinorVersion()));
        items.add(new Integer(getMaintenanceVersion()));
        
        if (getPatchVersion() != 0) {
            items.add(new Integer(getPatchVersion()));   
        }
        
        String verString = "";
        for (Integer v : items) {            
            verString += v + ".";
        }
        verString = verString.substring(0, verString.lastIndexOf("."));
        return verString;
    }
}
