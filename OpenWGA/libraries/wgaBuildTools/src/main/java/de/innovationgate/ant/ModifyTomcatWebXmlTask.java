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

public class ModifyTomcatWebXmlTask extends Task {
    
    private String webxml;
    private String tagpooling;

    public String getWebxml() {
        return webxml;
    }

    public void setWebxml(String webxml) {
        this.webxml = webxml;
    }
    
    public void execute() throws BuildException {
        
        try {
            String baseDir = getProject().getProperty("basedir");
            File webxmlFile = new File(getWebxml());
            if (!webxmlFile.exists()) {
                throw new BuildException("File does not exist: " + webxmlFile.getAbsolutePath());
            }
            
            Document doc = (new SAXReader()).read(webxmlFile);
           
            // namespace Tomcat 5.5
            Map ns = new HashMap();
            ns.put( "jee", "http://java.sun.com/xml/ns/j2ee");
            NamespaceContext nsContext = new SimpleNamespaceContext(ns);       
            XPath xpath = new Dom4jXPath("/web-app/jee:servlet[jee:servlet-name='jsp']");
            xpath.setNamespaceContext(nsContext);            
            Element jspServlet = (Element) xpath.selectSingleNode(doc);
            if (jspServlet == null) {
                // namespace Tomcat 6.0
                ns = new HashMap();
                ns.put( "jee", "http://java.sun.com/xml/ns/javaee");
                nsContext = new SimpleNamespaceContext(ns);       
                xpath = new Dom4jXPath("/web-app/jee:servlet[jee:servlet-name='jsp']");
                xpath.setNamespaceContext(nsContext);            
                jspServlet = (Element) xpath.selectSingleNode(doc);                                
            }
            
            if (jspServlet == null) {
                throw new BuildException("Cannot find JspServlet in web.xml");
            }

            xpath = new Dom4jXPath("jee:init-param[jee:param-name='enablePooling']");
            xpath.setNamespaceContext( new SimpleNamespaceContext(ns));
            Element enablePooling = (Element) xpath.selectSingleNode(jspServlet);
            Element paramValue;
            if (enablePooling == null) {
                log("Creating param");
                Element param = jspServlet.addElement("init-param");
                Element paramName = param.addElement("param-name");
                paramName.addText("enablePooling");
                paramValue = param.addElement("param-value");
                
            }
            else {
                paramValue = enablePooling.element("param-value");
            }
            
            log("Setting enablePooling to: " + getTagpooling());
            paramValue.clearContent();
            paramValue.addText(getTagpooling());
            
            
            log("Writing modified web.xml");
            OutputFormat outputFormat = OutputFormat.createPrettyPrint();
            outputFormat.setTrimText(true);
            outputFormat.setNewlines(true);

            XMLWriter writer = new XMLWriter(new FileOutputStream(webxmlFile), outputFormat);
            writer.write(doc);
            writer.close();

            
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

    public String getTagpooling() {
        return tagpooling;
    }

    public void setTagpooling(String tagpooling) {
        this.tagpooling = tagpooling;
    }

}
