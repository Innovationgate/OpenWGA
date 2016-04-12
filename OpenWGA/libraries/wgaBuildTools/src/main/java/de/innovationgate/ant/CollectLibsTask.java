/*
 * Created on Nov 10, 2006 from ow
 *
 */
package de.innovationgate.ant;

import java.io.File;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

/**
 * Builds a classpath from jarfiles contained in the directory and stores it under a property. The classpath is formatted
 * To be used as property "Class-Path" in a manifest.mf file.
 * ANT attributes:
 * dir = The directory to scan for files with ending .jar
 * property = The name of the property to store the classpath under. File names will be divided by spaces.
 * targetdir = A target directory that will be used to qualify the classpath entries. 
 *             Should be adapted to the relative location of the library directory as seen from the location of the main jar.
 *             Example if the main jar (the jar containing the main executable class) is located under "/app/main.jar" and all
 *             jars collected here are located as "/app/lib/*.jar", then the targetdir attribute should be "lib". 
 */
public class CollectLibsTask extends Task {
    
    private String dir;
    private String targetdir;
    private String property;
    private String divider;

    public void execute() throws BuildException {
        
        StringBuffer libs = new StringBuffer();
        
        File dir = new File(getDir());
        if (!dir.isAbsolute()) {        
            dir = new File(getProject().getBaseDir(), getDir());
        }
        
        if (!dir.exists() || !dir.isDirectory()) {
            throw new BuildException("File '" + dir.getAbsolutePath() + "' does not exist or is no directory");
        }
        
        String divider = getDivider();
        if (divider == null) {
            divider = " ";
        }
        
        File[] files = dir.listFiles();
        for (int i = 0; i < files.length; i++) {
            File file = files[i];
            if (file.getName().endsWith(".jar")) {
                libs.append(getTargetdir());
                if (!getTargetdir().endsWith("/")) {
                    libs.append("/");
                }
                libs.append(file.getName()).append(divider);
            }
        }
        
        getProject().setProperty(getProperty(), libs.toString());
        
    }

    public String getDir() {
        return dir;
    }

    public void setDir(String dir) {
        this.dir = dir;
    }

    public String getTargetdir() {
        return targetdir;
    }

    public void setTargetdir(String targetdir) {
        this.targetdir = targetdir;
    }

    public String getProperty() {
        return property;
    }

    public void setProperty(String property) {
        this.property = property;
    }

    public String getDivider() {
        return divider;
    }

    public void setDivider(String divider) {
        this.divider = divider;
    }
    
}
