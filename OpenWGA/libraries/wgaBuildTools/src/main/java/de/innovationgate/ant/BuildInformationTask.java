/*
 * Created on 13.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package de.innovationgate.ant;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

/**
 * @author ow
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class BuildInformationTask extends Task {

    private String webinf;

    private String debug;
    
    /*
     * (non-Javadoc)
     * 
     * @see org.apache.tools.ant.Task#execute()
     */
    public void execute() throws BuildException {


        // Retrieve build information
        BuildInformation buildInformation = retrieveBuildInformation();

        // Create signature consisting of builder name and date/time of build
        String userName = getProject().getProperty("wga.builduser");
        if (userName == null) {
            System.getProperty("user.name");
        }
        
        // Store as properties
        getProject().setProperty("releaseName", buildInformation.getReleaseString());
        getProject().setProperty("majorVersion", String.valueOf(buildInformation.getMajorVersion()));
        getProject().setProperty("minorVersion", String.valueOf(buildInformation.getMinorVersion()));
        getProject().setProperty("maintenanceVersion", String.valueOf(buildInformation.getMaintenanceVersion()));
        getProject().setProperty("patchVersion", String.valueOf(buildInformation.getPatchVersion()));
        getProject().setProperty("build", String.valueOf(buildInformation.getBuild()));
        getProject().setProperty("releaseName", buildInformation.getReleaseString());

        

    }

    /**
     * @return
     * @throws MalformedURLException
     */
    protected BuildInformation retrieveBuildInformation() {

        BuildInformation info = new BuildInformation();
        String baseDir = getProject().getProperty("basedir");

        String webinf = getWebinf();
        if (webinf == null) {
            throw new BuildException("Attribute webinf must point to the WEB-INF-Directory of WGAPublisher");
        }
        
        /*
        File j2eeLib = new File(baseDir, "j2ee.jar");
        if (!j2eeLib.exists()) {
            throw new BuildException("Cannot locate j2ee library under path: " + j2eeLib.getPath());
        }*/

        File webinfDir = new File(baseDir, webinf);
        File classesDir = new File(webinfDir, "classes");
        File libDir = new File(webinfDir, "lib");

        try {
            List loaderURLs = new ArrayList();
            //loaderURLs.add(j2eeLib.toURL());
            loaderURLs.add(classesDir.toURL());

            String[] fileNames = libDir.list();
            if (fileNames != null) {
                for (int idx = 0; idx < fileNames.length; idx++) {
                    if (fileNames[idx].endsWith(".jar")) {
                        File jar = new File(libDir, fileNames[idx]);
                        loaderURLs.add(jar.toURL());
                    }
                }
            }
            
            if (getDebug().equals("true")) {
                Iterator urlsIt = loaderURLs.iterator();
                while (urlsIt.hasNext()) {
                    log(urlsIt.next().toString());
                }
            }
            
            URL[] urls = new URL[loaderURLs.size()];
            loaderURLs.toArray(urls);
   
            URLClassLoader loader = new URLClassLoader(urls);
            
            Class wgaVersionClass = null;
            try {
                wgaVersionClass = loader.loadClass("de.innovationgate.wgpublisher.WGAVersion");
            }
            catch (ClassNotFoundException e) {
                wgaVersionClass = loader.loadClass("de.innovationgate.wgpublisher.WGACore");
            }
            
            
            info.setMajorVersion(wgaVersionClass.getField("WGAPUBLISHER_MAJOR_VERSION").getInt(null));
            info.setMinorVersion(wgaVersionClass.getField("WGAPUBLISHER_MINOR_VERSION").getInt(null));
            info.setMaintenanceVersion(wgaVersionClass.getField("WGAPUBLISHER_MAINTENANCE_VERSION").getInt(null));
            info.setPatchVersion(wgaVersionClass.getField("WGAPUBLISHER_PATCH_VERSION").getInt(null));
            info.setBuild(wgaVersionClass.getField("WGAPUBLISHER_BUILD_VERSION").getInt(null));
            return info;
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new BuildException("Exception retrieving build information: " + e.getClass().getName() + " - " + e.getMessage(), e);
        }

    }

    /**
     * @return Returns the bin.
     */
    public String getWebinf() {
        return webinf;
    }

    /**
     * @param bin
     *            The bin to set.
     */
    public void setWebinf(String bin) {
        this.webinf = bin;
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