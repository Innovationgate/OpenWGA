/*
 * Created on 16.03.2007 from oliverweise
 *
 */
package de.innovationgate.ant;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

public class FAttribTask extends Task {
    
    private String file;
    private String readonly;

    public String getFile() {
        return file;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public void execute() throws BuildException {

        File file = new File(getProject().getProperty("basedir"), getFile());
        if (!file.exists()) {
            return;
        }
        

        boolean set = Boolean.valueOf(getReadonly()).booleanValue();
        String os = System.getProperty("os.name").toLowerCase();
        
        /*
        try {
            Process proc = Runtime.getRuntime().exec("whoami");
            LineNumberReader read = new LineNumberReader(new InputStreamReader(proc.getInputStream()));
            proc.waitFor();
            String line = read.readLine();
            read.close();
            log("Running as :" + line);
            proc = Runtime.getRuntime().exec("echo $SHELL");
            read = new LineNumberReader(new InputStreamReader(proc.getInputStream()));
            proc.waitFor();
            line = read.readLine();
            read.close();
            log("Shell :" + line);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }*/

        updateAttrib(file, set, os);
        
        
    }

    private void updateAttrib(File file, boolean set, String os) {
        
        if (file.isFile()) {
            
            boolean isReadonly = !file.canWrite();
            if (set == isReadonly) {
                return;
            }
            
            String filePath =  "\"" + file.getPath() + "\"";
        
            List commands = new ArrayList();
            if (os.indexOf("linux") != -1 || os.indexOf("mac os") != -1) {
                if (os.indexOf("mac os") != -1) {
                    commands.add(new ProcessBuilder(new String[] {"/bin/bash", "-c", "chflags nouchg " + filePath}));    
                }
                
                commands.add(new ProcessBuilder(new String[] {"/bin/bash", "-c", "chmod u" + (set ? "-" : "+") + "w " + filePath}));
            }
            else {
                commands.add(new ProcessBuilder(new String[] {"attrib ", (set ? "+" : "-") + "R ", filePath}));
            }
            
            log("Setting readony status of " + set + ": " + file.getPath());
            
            try {
                int returnCode = -1;
                ProcessBuilder command;
                Iterator commandsIt = commands.iterator();
                while (commandsIt.hasNext()) {
                    command = (ProcessBuilder) commandsIt.next();
                
                    //log("Executing: " + command);
                    command.redirectErrorStream(true);
                    Process proc = command.start();
                    LineNumberReader in = new LineNumberReader(new InputStreamReader(proc.getInputStream()));
                    
                    returnCode = proc.waitFor();

                    
                    if (returnCode != 0) {
                        String line = in.readLine();
                        in.close();
                        throw new BuildException("Statement exited with return code " + returnCode + ": " + line);
                    }
                }
            }
            catch (IOException e) {
               throw new BuildException("Error setting file readonly attribute: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }
            catch (InterruptedException e) {
            }
        }
        else {
            File[] files = file.listFiles();
            for (int i = 0; i < files.length; i++) {
                updateAttrib(files[i], set, os);
            }
        }
    }

    public String getReadonly() {
        return readonly;
    }

    public void setReadonly(String set) {
        this.readonly = set;
    }

}


