package de.innovationgate.ant;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;

import javax.activation.DataContentHandlerFactory;
import javax.activation.DataHandler;
import javax.mail.Transport;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wgaservices.ActionCaller;
import de.innovationgate.wgaservices.ClientFactory;
import de.innovationgate.wgaservices.WGACoreServices;
import de.innovationgate.wgaservices.WGAServiceException;
import de.innovationgate.wgaservices.WGAServices;
import de.innovationgate.wgaservices.types.Form;
import de.innovationgate.wgaservices.types.RemoteSession;

public class UpdateDownloadTask extends Task {
    
    private String _url;
    private String _user;
    private String _password;
    private String _db;
    private String _doc;
    
    private String _name;
    private String _file;
    private String _titleExtension;
    private String _fileNamePattern;
    
    
    public String getName() {
        return _name;
    }
    public void setName(String name) {
        _name = name;
    }
    public String getFile() {
        return _file;
    }
    public void setFile(String file) {
        _file = file;
    }
    public String getTitleExtension() {
        return _titleExtension;
    }
    public void setTitleExtension(String titleExtension) {
        _titleExtension = titleExtension;
    }
    public String getFileNamePattern() {
        return _fileNamePattern;
    }
    public void setFileNamePattern(String fileNamePattern) {
        _fileNamePattern = fileNamePattern;
    }
    @Override
    public void execute() throws BuildException {
        
        try {
            // StaX uses context class loader to find implementations
            Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
            
            Enumeration<URL> mailCaps = this.getClass().getClassLoader().getResources("META-INF/mailcap");
            log("Mailcaps:");
            while (mailCaps.hasMoreElements()) {
                URL url = (URL) mailCaps.nextElement();
                log(url.toString());
            }
            
            WGACoreServices services = ClientFactory.createCoreServiceClient(getUrl());
            RemoteSession session = services.adminLogin(getUser(), getPassword());
            ActionCaller caller = ClientFactory.createActionCaller(services, session, getDb());
            
            Form form = new Form("UpdateDownload");
            File file = new File(getFile());
            if (!file.exists()) {
                throw new BuildException("Download file " + file.getAbsolutePath() + " does not exist");
            }
            
            form.addFileAsAttachment(file);
            form.setField("newfilename", file.getName());
            form.setField("schema", getFileNamePattern());
            form.setField("titleextension", getTitleExtension());
            
            caller.setExecutionContext("name:" + getDoc());
            Object result = caller.callAction("downloads:updateDownload", form);
            if (Boolean.TRUE.equals(result)) {
                log("Update successfully finished");
            }
            else {
                log("Ooops...something went wrong! Better have a look at the server...");
            }
            
            
        }
        catch (Exception e) {
            e.printStackTrace();
            log("Exception: " + e.getClass().getName() + " - " + e.getMessage());
        }
        
        
    }
    public String getUrl() {
        return _url;
    }
    public void setUrl(String url) {
        _url = url;
    }
    public String getDb() {
        return _db;
    }
    public void setDb(String db) {
        _db = db;
    }
    public String getUser() {
        return _user;
    }
    public void setUser(String user) {
        _user = user;
    }
    public String getPassword() {
        return _password;
    }
    public void setPassword(String password) {
        _password = password;
    }
    public String getDoc() {
        return _doc;
    }
    public void setDoc(String doc) {
        _doc = doc;
    }

}
