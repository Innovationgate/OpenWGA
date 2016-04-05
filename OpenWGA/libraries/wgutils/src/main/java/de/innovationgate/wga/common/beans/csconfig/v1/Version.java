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

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import de.innovationgate.utils.WGUtils;

/**
 * Represents a version of any OpenWGA component
 * 
 * OpenWGA versions consist of up to 5 parts
 * <ul>
 * <li> {@link #getMajorVersion()} - Major feature version. Denotes main feature streams.
 * <li> {@link #getMinorVersion()} - Minor feature version. Denotes feature additions to the main feature stream.
 * <li> {@link #getMaintenanceVersion()} - Maintenance version. Denotes maintenance versions upon the major/minor stream.
 * <li> {@link #getPatchVersion()} - Patch version. Denote a patch to any major/minor/maintenance release
 * <li> {@link #getBuildVersion()} - Build version. Is a sequential number uniquely identifying each build.
 * <ul>
 * 
 * The versions major, minor, maintenance and patch together form the release version of a component which may be up to 4 digits long
 * The build version is separate, as it does not only denote release builds but all other pre-release builds too.
 */
public class Version implements Comparable<Version>, Serializable {
    
    public static class GsonTypeAdapter extends TypeAdapter<Version> {

        @Override
        public Version read(JsonReader in) throws IOException {
            return new Version(in.nextString());
        }

        @Override
        public void write(JsonWriter out, Version version) throws IOException {
            out.value(version.toString());
        }
        
    }
    

    private static final long serialVersionUID = 1L;

    /**
     * Returns the version of the Java runtime as a Version object
     */
    public static Version getJavaVersion() {
        return getJavaVersion(System.getProperty("java.version"));
    }
    
    /**
     * Parses a Java version string and returns the version as Version object
     * @param version Java version string
     */
    public static Version getJavaVersion(String version) {
        
        try {
        
            // Remove additional information from version string, like update number and special qualifier
            // For possible version string formats see http://java.sun.com/j2se/versioning_naming.html
        int sublinePos = version.indexOf("_");
        if (sublinePos != -1) {
            version = version.substring(0, sublinePos);
            }
            
            int dashPos = version.indexOf("-");
            if (dashPos != -1) {
                version = version.substring(0, dashPos);
        }
        
        List elems = WGUtils.deserializeCollection(version, ".");
        int major = Integer.parseInt((String) elems.get(0));
        int minor = Integer.parseInt((String) elems.get(1));
        int maintenance = Integer.parseInt((String) elems.get(2));
        
        return new Version(major, minor, maintenance);
        }
        catch (NumberFormatException e) {
           Logger.getLogger("wga.utils").error("Error retrieving java version", e);
           
           // We default to minimum version needed to run WGA
           return new Version(1,4,0);           
        }
        
    }
    
    private int majorVersion = 0;
    private int minorVersion = 0;
    private int maintenanceVersion = 0;
    private int patchVersion = 0;
    private int buildVersion = 0;
    
    /**
     * Default constructor for serialisation purposes
     */
    public Version() {
        
    }
    
    /**
     * Constructor with 3 main version digits as arguments
     * @param majorVersion
     * @param minorVersion
     * @param maintenanceVersion
     */
    public Version(int majorVersion, int minorVersion, int maintenanceVersion) {
        super();
        this.majorVersion = majorVersion;
        this.minorVersion = minorVersion;
        this.maintenanceVersion = maintenanceVersion;
    }
    
    /**
     * Constructor with all 5 version digits as arguments
     * @param majorVersion
     * @param minorVersion
     * @param maintenanceVersion
     * @param patchVersion
     * @param buildVersion
     */
    public Version(int majorVersion, int minorVersion, int maintenanceVersion, int patchVersion, int buildVersion) {
        super();
        this.majorVersion = majorVersion;
        this.minorVersion = minorVersion;
        this.maintenanceVersion = maintenanceVersion;
        this.patchVersion = patchVersion;
        this.buildVersion = buildVersion;
    }
    
    /**
     * Constructor parsing a standard OpenWGA version string 
     * @param verString Version string
     */
    public Version(String verString) {
        
        if (verString.indexOf("Build") != -1) {
            List items = WGUtils.deserializeCollection(verString, "Build", true);
            verString = (String) items.get(0);
            setBuildVersion(Integer.parseInt((String) items.get(1)));
        }
        
        List items = WGUtils.deserializeCollection(verString, ".");
        if (items.size() < 3) {
            throw new IllegalArgumentException("Version string '" + verString + "' is not valid");
        }
        
        setMajorVersion(Integer.parseInt((String) items.get(0)));
        setMinorVersion(Integer.parseInt((String) items.get(1)));
        setMaintenanceVersion(Integer.parseInt((String) items.get(2)));
        
        if (items.size() >= 4) {
            setPatchVersion(Integer.parseInt((String) items.get(3)));
        }
        
        
    }
    /**
     * Returns the build version
     */
    public int getBuildVersion() {
        return buildVersion;
    }
    /**
     * Sets the build version
     */
    public void setBuildVersion(int buildVersion) {
        this.buildVersion = buildVersion;
    }
    /**
     * Returns the maintenance version
     */
    public int getMaintenanceVersion() {
        return maintenanceVersion;
    }
    /**
     * Sets the maintenance version
     */
    public void setMaintenanceVersion(int maintenanceVersion) {
        this.maintenanceVersion = maintenanceVersion;
    }
    /**
     * Returns the major version
     */
    public int getMajorVersion() {
        return majorVersion;
    }
    /**
     * Sets the major version
     */
    public void setMajorVersion(int majorVersion) {
        this.majorVersion = majorVersion;
    }
    /**
     * Returns the minor version
     */
    public int getMinorVersion() {
        return minorVersion;
    }
    /**
     * Sets the minor version
     */
    public void setMinorVersion(int minorVersion) {
        this.minorVersion = minorVersion;
    }
    /**
     * Returns the patch version
     */
    public int getPatchVersion() {
        return patchVersion;
    }
    /**
     * Sets the patch version
     */
    public void setPatchVersion(int patchVersion) {
        this.patchVersion = patchVersion;
    }

    /**
     * Serializes the version as standard OpenWGA version string
     */
    public String toString() {
        
        String verString = getMainVersionString();
        if (getBuildVersion() != 0) {
            verString += " Build " + getBuildVersion();
        }
        
        return verString;
        
    }

    /**
     * Returns the main version string, consisting of major, minor, maintenance version plus patch version if != 0
     */
    public String getMainVersionString() {
        List items = new ArrayList();
        items.add(new Integer(getMajorVersion()));
        items.add(new Integer(getMinorVersion()));
        items.add(new Integer(getMaintenanceVersion()));
        
        if (getPatchVersion() != 0) {
            items.add(new Integer(getPatchVersion()));   
        }
        
        String verString = WGUtils.serializeCollection(items, ".");
        return verString;
    }
    
    public String getFeatureVersionString() {
        return getMajorVersion() + "." + getMinorVersion();
    }
    
    public int compareTo(Version arg0) {
        
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
    
    /**
     * Checks if this version is the given feature version or higher
     * @param major Major version
     * @param minor Minor version
     * @return true if this version is equal or higher
     */
    public boolean isAtLeast(int major, int minor) {
        Version compVersion = new Version(major, minor, 0);
        return (compareTo(compVersion) >= 0);
    }
    
    /**
     * Checks if this version equals the given version or is higher
     * @param v
     * @return true if the version is equal or higher than the parameter version
     */
    public boolean isAtLeast(Version v) {
        return (compareTo(v) >= 0);
    }
    
    /**
     * Returns true if the given version has the same major/minor version 
     * @param version
     */
    public boolean isSameFeatureVersion(Version version) {
        return (version.getMajorVersion() == getMajorVersion() && version.getMinorVersion() == getMinorVersion());
    }

}
