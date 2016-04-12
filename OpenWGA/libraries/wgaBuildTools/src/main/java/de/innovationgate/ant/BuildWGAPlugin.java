package de.innovationgate.ant;

import java.io.File;

import org.apache.tools.ant.BuildException;

import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.model.WGADesignConfigurationModel;

public class BuildWGAPlugin extends WGAPluginInfo {
	
	private String toDir;
	
	private String toFile;
	
	private String build;
	
	private String obfuscate;
	
	private String increaseBuild;
	
	private String shortFileName;
	
	private String javaSourceDir = null;

	public static String PROPERTY_EXPORT_FILENAME = "exportFilename";
	public static String PROPERTY_EXPORT_FILEPATH = "exportFilepath";
	@Override
	public void execute() throws BuildException {
		try {
			if (getDesignDir() == null) {
				throw new BuildException("No design directory specified.");
			}
			File fDesignDir = null;
			if (getDesignDir().startsWith("/")) {
				fDesignDir = new File(getDesignDir());
			} else {
				fDesignDir = new File(getProject().getBaseDir(), getDesignDir());
			}
			
			File designInfoFile = DesignDirectory.getDesignDefinitionFile(fDesignDir);

			if (!designInfoFile.exists()) {
				throw new BuildException("Design definition '" + designInfoFile.getAbsolutePath() + "' not found.");
			}
			
			WGADesignConfigurationModel model = new WGADesignConfigurationModel(designInfoFile);
			
			if (!model.hasPluginConfig()) {
				throw new BuildException("Specified design has no plugin config.");
			}
			
			int buildNumber = model.getPluginBuild();
			if (build != null) {
				buildNumber = Integer.parseInt(build);
			}
			boolean bObfuscate = false;
			if (obfuscate != null) {
				bObfuscate = Boolean.valueOf(obfuscate);
			}
			boolean bIncreaseBuild = false;
			if (increaseBuild != null) {
				bIncreaseBuild = Boolean.valueOf(increaseBuild);
			}		
			
			File exportFile = null;
			if (toDir != null) {
				File exportDir = null;
				if (toDir.startsWith("/")) {
					exportDir = new File(toDir);
				} else {
					exportDir = new File(getProject().getBaseDir(), toDir);
				}
				
				if (!exportDir.exists()) {
					throw new BuildException("ExportDir '" + exportDir.getAbsolutePath() + "' does not exists.");
				}
				
				boolean bShortFileName = false;
				if (shortFileName != null) {
					bShortFileName = Boolean.valueOf(shortFileName);
				}
				exportFile = model.getPluginExportFile(exportDir, bShortFileName);
			}
			else {
				exportFile = new File(getProject().getBaseDir(), toFile);
			}	
			
			if (exportFile == null) {
				throw new BuildException("No export location specified.");
			}
			
			String javaSourceFolder = null;
            if (getJavaSourceDir() != null) {
                if (getJavaSourceDir().startsWith("/")) {
                    javaSourceFolder = getJavaSourceDir();
                }
                else {
                    javaSourceFolder = (new File(getProject().getBaseDir(), javaSourceDir)).getAbsolutePath();
                }
            }
			
			System.out.println("Exporting plugin '" + model.getPluginUniqueName() + "' to '" + exportFile.getAbsolutePath() + "'.");
			
			
			
			model.exportPlugin(exportFile, buildNumber, bObfuscate, bIncreaseBuild, javaSourceFolder);

			getProject().setNewProperty(getPropertyPrefix() + PROPERTY_EXPORT_FILENAME, exportFile.getName());
			getProject().setNewProperty(getPropertyPrefix() + PROPERTY_EXPORT_FILEPATH, exportFile.getAbsolutePath());
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


	public String getBuild() {
		return build;
	}

	public void setBuild(String build) {
		this.build = build;
	}

	public String getObfuscate() {
		return obfuscate;
	}

	public void setObfuscate(String obfuscate) {
		this.obfuscate = obfuscate;
	}

	public String getIncreaseBuild() {
		return increaseBuild;
	}

	public void setIncreaseBuild(String increaseBuild) {
		this.increaseBuild = increaseBuild;
	}

	public String getToDir() {
		return toDir;
	}

	public void setToDir(String toDir) {
		this.toDir = toDir;
	}

	public String getToFile() {
		return toFile;
	}

	public void setToFile(String toFile) {
		this.toFile = toFile;
	}

	public String getShortFileName() {
		return shortFileName;
	}

	public void setShortFileName(String shortFileName) {
		this.shortFileName = shortFileName;
	}


    public String getJavaSourceDir() {
        return javaSourceDir;
    }


    public void setJavaSourceDir(String javaSourceFolder) {
        this.javaSourceDir = javaSourceFolder;
    }
	
	

}
