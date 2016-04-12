/*
 * Created on 05.10.2007 from oliverweise
 *
 */
package de.innovationgate.ant;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.DOMWriter;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;
import org.jaxen.NamespaceContext;
import org.jaxen.SimpleNamespaceContext;
import org.jaxen.XPath;
import org.jaxen.dom4j.Dom4jXPath;
import org.jaxen.jdom.JDOMXPath;

public class ModifyTomcatServerXmlTask extends Task {
    
    private String serverxml;

    public String getServerxml() {
        return serverxml;
    }

    public void setServerxml(String webxml) {
        this.serverxml = webxml;
    }
    
    public void execute() throws BuildException {
        
        try {
            String baseDir = getProject().getProperty("basedir");
            File serverxmlFile = new File(baseDir, getServerxml());
            if (!serverxmlFile.exists()) {
                throw new BuildException("File does not exist: " + getServerxml());
            }
            
            Document doc = (new SAXReader()).read(serverxmlFile);
            boolean modified = false;
            
            // User Database
            Element resource = (Element) doc.selectSingleNode("/Server/GlobalNamingResources/Resource[@name='UserDatabase']");
            if (resource == null) {
                throw new BuildException("Cannot find resource UserDatabase in server.xml");
            }
            
            
            if (!resource.attributeValue("pathname", "").equals("${USER_HOME}/WGAJumpstart/config/tomcat-users.xml")) {
                modified = true;
                log("Setting variable ${USER_HOME}/WGAJumpstart/config/tomcat-users.xml for User Database");
                resource.addAttribute("pathname", "${USER_HOME}/WGAJumpstart/config/tomcat-users.xml");
            }
            else {
                log("Tomcat User Database already modified");
            }
            
            // Connector
            Element connector = (Element) doc.selectSingleNode("/Server/Service[@name='Catalina']/Connector[@protocol='HTTP/1.1']");
            if (connector == null) {
                throw new BuildException("Cannot find connector in server.xml");
            }

            if (!connector.attributeValue("port", "").equals("${portInput}")) {
                modified = true;
                log("Setting variable ${portInput} for tomcat HTTP port");
                connector.addAttribute("port", "${portInput}");
            }
            else {
                log("Tomcat HTTP port already modified");
            }
            
            // Host
            Element host = (Element) doc.selectSingleNode("/Server/Service[@name='Catalina']/Engine[@name='Catalina']/Host[@name='localhost']");
            if (host == null) {
                throw new BuildException("Cannot find host in server.xml");
            }
            
            if (!host.attributeValue("appBase", "").equals("${USER_HOME}/WGAJumpstart/webapps")) {
                modified = true;
                log("Setting variable ${USER_HOME}/WGAJumpstart/webapps for tomcat webapps dir");
                host.addAttribute("appBase", "${USER_HOME}/WGAJumpstart/webapps");
            }
            else {
                log("Tomcat webapps already modified");
            }
            
            if (!host.attributeValue("workDir", "").equals("${USER_HOME}/WGAJumpstart/work")) {
                modified = true;
                log("Setting variable ${USER_HOME}/WGAJumpstart/work for tomcat work dir");
                host.addAttribute("workDir", "${USER_HOME}/WGAJumpstart/work");
            }
            else {
                log("Tomcat work dir already modified");
            }
            
            if (modified) {
                log("Writing modified server.xml");
                OutputFormat outputFormat = OutputFormat.createPrettyPrint();
                outputFormat.setTrimText(true);
                outputFormat.setNewlines(true);
    
                XMLWriter writer = new XMLWriter(new FileOutputStream(serverxmlFile), outputFormat);
                writer.write(doc);
                writer.close();
            }
            
        }
        catch (Exception e) {
            if (e instanceof BuildException) {
                throw (BuildException) e;
            }
            else {
                throw new BuildException(e);
            }
        }
        
    }

}
