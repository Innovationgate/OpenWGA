package de.innovationgate.ant;

import java.io.File;
import java.text.DecimalFormat;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.model.WGADesignConfigurationModel;

public class WGAPluginInfo extends Task {
    
    public static DecimalFormat FORMAT_TAG_BUILDNR = new DecimalFormat("00000"); 

    public static String PROPERTY_EXPORT_FILENAME = "exportFilename";
	public static String PROPERTY_VERSION = "version";
	public static String PROPERTY_MAJOR = "majorVersion";
	public static String PROPERTY_MINOR = "minorVersion";
	public static String PROPERTY_MAINTENANCE = "maintenanceVersion";
	public static String PROPERTY_PATCH = "patchVersion";
	public static String PROPERTY_BUILD = "build";
	public static String PROPERTY_UID = "uid";
	public static String PROPERTY_TAG = "tag";
	
	private String designDir;
	private String propertyPrefix = "plugin."; 
	
	@Override
	public void execute() throws BuildException {
		try {
			if (designDir == null) {
				throw new BuildException("No design directory specified.");
			}
			File fDesignDir = null;
			if (designDir.startsWith("/")) {
				fDesignDir = new File(designDir);
			} else {
				fDesignDir = new File(getProject().getBaseDir(), designDir);
			}
			
			File designInfoFile = DesignDirectory.getDesignDefinitionFile(fDesignDir);
			
			if (designInfoFile == null || !designInfoFile.exists()) {
				throw new BuildException("Design definition at location '" + fDesignDir.getPath() + "' not found.");
			}

			WGADesignConfigurationModel model = new WGADesignConfigurationModel(designInfoFile);
			
			if (!model.hasPluginConfig()) {
				throw new BuildException("Specified design has no plugin config.");
			}
			setModelProperties(model);
			
		} 
		catch (Exception e) {
			if (e instanceof BuildException) {
				throw (BuildException) e;
			} else {
				throw new BuildException(e);
			}
		} 
	}


    protected void setModelProperties(WGADesignConfigurationModel model) {
        getProject().setNewProperty(propertyPrefix + PROPERTY_UID, model.getPluginUniqueName());
        getProject().setNewProperty(propertyPrefix + PROPERTY_VERSION, model.getPluginVersion());
        getProject().setNewProperty(propertyPrefix + PROPERTY_BUILD, Integer.toString(model.getPluginBuild()));
        
        Version version = new Version(model.getPluginVersion());
        version.setBuildVersion(model.getPluginBuild());
        getProject().setNewProperty(propertyPrefix + PROPERTY_MAJOR, String.valueOf(version.getMajorVersion()));
        getProject().setNewProperty(propertyPrefix + PROPERTY_MINOR, String.valueOf(version.getMinorVersion()));
        getProject().setNewProperty(propertyPrefix + PROPERTY_MAINTENANCE, String.valueOf(version.getMaintenanceVersion()));
        getProject().setNewProperty(propertyPrefix + PROPERTY_PATCH, String.valueOf(version.getPatchVersion()));
        getProject().setNewProperty(propertyPrefix + PROPERTY_TAG, BuildSignatureTask.createVcsTag(version));
    }


	public String getDesignDir() {
		return designDir;
	}


	public void setDesignDir(String designDir) {
		this.designDir = designDir;
	}


	public String getPropertyPrefix() {
		return propertyPrefix;
	}


	public void setPropertyPrefix(String propertyPrefix) {
		this.propertyPrefix = propertyPrefix;
	}
	
	

}
