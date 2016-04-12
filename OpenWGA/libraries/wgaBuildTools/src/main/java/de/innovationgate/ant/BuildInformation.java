/*
 * Created on 17.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package de.innovationgate.ant;

/**
 * @author ow
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class BuildInformation {
    
    private int majorVersion;
    private int minorVersion;
    private int maintenanceVersion;
    private int patchVersion;
    private int build;


    
    /**
     * @return Returns the build.
     */
    public int getBuild() {
        return build;
    }
    /**
     * @param build The build to set.
     */
    public void setBuild(int build) {
        this.build = build;
    }
    /**
     * @return Returns the maintenanceVersion.
     */
    public int getMaintenanceVersion() {
        return maintenanceVersion;
    }
    /**
     * @param maintenanceVersion The maintenanceVersion to set.
     */
    public void setMaintenanceVersion(int maintenanceVersion) {
        this.maintenanceVersion = maintenanceVersion;
    }
    /**
     * @return Returns the majorVersion.
     */
    public int getMajorVersion() {
        return majorVersion;
    }
    /**
     * @param majorVersion The majorVersion to set.
     */
    public void setMajorVersion(int majorVersion) {
        this.majorVersion = majorVersion;
    }
    /**
     * @return Returns the minorVersion.
     */
    public int getMinorVersion() {
        return minorVersion;
    }
    /**
     * @param minorVersion The minorVersion to set.
     */
    public void setMinorVersion(int minorVersion) {
        this.minorVersion = minorVersion;
    }
    /**
     * @return Returns the patchVersion.
     */
    public int getPatchVersion() {
        return patchVersion;
    }
    /**
     * @param patchVersion The patchVersion to set.
     */
    public void setPatchVersion(int patchVersion) {
        this.patchVersion = patchVersion;
    }
    /**
     * @return Returns the releaseString.
     */
    public String getReleaseString() {
        String releaseString = majorVersion + "." + minorVersion + "." + maintenanceVersion;
        if (patchVersion > 0) {
            releaseString += " Patch " + patchVersion;
        }
        
        releaseString += " (Build " + build + ")";
        
        return releaseString;
    }

}
