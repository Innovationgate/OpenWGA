/*
 * Created on 13.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package de.innovationgate.ant;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import org.apache.tools.ant.BuildException;

import de.innovationgate.wga.common.beans.csconfig.v1.Version;

/**
 * @author ow
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class BuildSignatureTask extends BuildInformationTask {

    public static final DateFormat TIMESTAMP = new SimpleDateFormat("yyyyMMdd_HHmm");
    
    public static DecimalFormat FORMAT_TAG_BUILDNR = new DecimalFormat("00000");

    private String target;

    private String debug;
    
    public static String createVcsTag(Version version) {
        
        StringBuilder b = new StringBuilder();
        b.append("b");
        b.append(FORMAT_TAG_BUILDNR.format(version.getBuildVersion()));
        b.append("_v");
        b.append(version.getMajorVersion());
        b.append("_");
        b.append(version.getMinorVersion());
        b.append("_");
        b.append(version.getMaintenanceVersion());
        if (version.getPatchVersion() > 0) {
            b.append("_");
            b.append(version.getPatchVersion());
        }
        
        return b.toString();
        
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.apache.tools.ant.Task#execute()
     */
    public void execute() throws BuildException {

        try {
            // Find necessary ant properties
            String baseDir = getProject().getProperty("basedir");
            File buildPropertiesFile = new File(baseDir, target);
            String target = getTarget();
            if (target == null) {
                throw new BuildException("You must specify an attribute target!");
            }
            String buildType = getProject().getProperty("build.type");
            
            // Look if we skip signature creation
            if (buildType != null && buildType.equals("k")) {
                if (!buildPropertiesFile.exists()) {
                    throw new BuildException("Cannot keep signature because there is no build properties file at " + buildPropertiesFile.getPath());
                }
                Properties props = new Properties();
                props.load(new FileInputStream(buildPropertiesFile));
                log("Keeping old build signature " + props.getProperty("signature") + " (" + (props.getProperty("finalBuild").equals("true") ? "final" : "interim") + " build) from file: " + buildPropertiesFile.getPath());
                getProject().setProperty("releaseName", props.getProperty("releaseName"));
                return;
            }
            
            // Retrieve build information
            BuildInformation bi = retrieveBuildInformation();
    
            // Create signature consisting of builder name and date/time of build
            String userName = getProject().getProperty("wga.builduser");
            if (userName == null) {
                System.getProperty("user.name");
            }
            
            String timeStamp = TIMESTAMP.format(new Date());
            String jenkinsBuild = getProject().getProperty("ENV.BUILD_NUMBER");
            String signature;
            if (jenkinsBuild != null) {
                signature = userName + "_" + jenkinsBuild + "_" + timeStamp;
            }
            else {
                signature = userName + "_" + timeStamp;
            }
    
            // Store as properties
            Properties props = new Properties();
            props.setProperty("signature", signature);
            props.setProperty("releaseName", bi.getReleaseString());
            props.setProperty("majorVersion", String.valueOf(bi.getMajorVersion()));
            props.setProperty("minorVersion", String.valueOf(bi.getMinorVersion()));
            props.setProperty("maintenanceVersion", String.valueOf(bi.getMaintenanceVersion()));
            props.setProperty("patchVersion", String.valueOf(bi.getPatchVersion()));
            props.setProperty("build", String.valueOf(bi.getBuild()));
            props.setProperty("tag", createVcsTag(new Version(bi.getMajorVersion(), bi.getMinorVersion(), bi.getMaintenanceVersion(), bi.getPatchVersion(), bi.getBuild())));
    
            getProject().setProperty("releaseName", bi.getReleaseString());
    
            // Write properties file
            if (buildPropertiesFile.exists()) {
                buildPropertiesFile.delete();
            }
            if (buildPropertiesFile.createNewFile() == false) {
                throw new BuildException("Could not create file at position " + target);
            }
            FileOutputStream out = new FileOutputStream(buildPropertiesFile);
            props.store(out, "Build information");
            out.close();
            log("Wrote build signature " + signature + " to file: " + buildPropertiesFile.getPath());
        }
        catch (FileNotFoundException e) {
            throw new BuildException("Exception using properties file: " + e.getClass().getName() + " - " + e.getMessage(), e);
        }
        catch (IOException e) {
            throw new BuildException("Exception using properties file" + e.getClass().getName() + " - " + e.getMessage(), e);
        }

    }

    /**
     * @return Returns the target.
     */
    public String getTarget() {
        return target;
    }

    /**
     * @param target
     *            The target to set.
     */
    public void setTarget(String target) {
        this.target = target;
    }

    /**
     * @return Returns the debug.
     */
    public String getDebug() {
        return (debug == null ? "" : debug);
    }
    /**
     * @param debug The debug to set.
     */
    public void setDebug(String debug) {
        this.debug = debug;
    }
}