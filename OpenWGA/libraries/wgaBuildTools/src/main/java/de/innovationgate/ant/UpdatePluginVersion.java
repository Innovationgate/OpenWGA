package de.innovationgate.ant;

import java.io.File;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.model.WGADesignConfigurationModel;

public class UpdatePluginVersion extends Task  {
    
    private String designDir;


    private String version;

    private String build;

    @Override
    public void execute() throws BuildException {
        try {
            if (getDesignDir() == null) {
                throw new BuildException("No design directory specified.");
            }
            
            if (getVersion() == null) {
                throw new BuildException("No version specified.");
            }
            
            if (getBuild() == null) {
                throw new BuildException("No build specified.");                
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
            
            model.setPluginVersion(version);
            
            model.setPluginBuild(Integer.parseInt(getBuild()));
            
            model.saveChanges();            
        }
        catch (Exception e) {
            if (e instanceof BuildException) {
                throw (BuildException) e;
            } else {
                throw new BuildException(e);
            }
        } 
    }
    
    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getBuild() {
        return build;
    }

    public void setBuild(String build) {
        this.build = build;
    }
    
    public String getDesignDir() {
        return designDir;
    }

    public void setDesignDir(String designDir) {
        this.designDir = designDir;
    }

}
