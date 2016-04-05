/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.wgpublisher.lucene;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.datatype.DatatypeDocumentFactory;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.beans.LuceneConfiguration;
import de.innovationgate.wga.common.beans.LuceneIndexFileRule;
import de.innovationgate.wga.common.beans.LuceneIndexItemRule;
import de.innovationgate.wgpublisher.WGACore;

/**
 * represent the configuration of the current lucene index
 * consists of information about
 * - indexingrules used for each db during indexing
 * - lastupdate information for each db
 * - last serviceStatus (running or idle) for each db in index
 * 
 */
public class LuceneIndexConfiguration {
       
    private WGACore _core;
    
    private File _configFile;        
    private Document _configDoc;
    private Element _configDocRoot;
    
    private boolean _newConfiguration = false;
    
    private static final DateFormat DATEFORMAT = new SimpleDateFormat("yyyyMMdd-HHmmss");
    
    private static final String VERSION = "1.0";   
    
      
    public LuceneIndexConfiguration(WGACore core, File indexDir) throws IllegalArgumentException, DocumentException {
        _core = core;
       
        _configFile = new File(indexDir, "wgalucene.xml");
        
        if (!_configFile.exists()) {
            _newConfiguration = true;
            _configDoc = DatatypeDocumentFactory.getInstance().createDocument();
            _configDocRoot = _configDoc.addElement("wgalucene");
            _configDocRoot.addAttribute("version", VERSION);
        }
        else {
            SAXReader saxReader = new SAXReader();
            saxReader.setDocumentFactory(DatatypeDocumentFactory.getInstance());

            _configDoc = saxReader.read(_configFile);

            _configDocRoot = _configDoc.getRootElement();
            
            // check if configfile has right version
            String version = _configDocRoot.attributeValue("version");
            if ( (version == null) || (!version.equals(VERSION)) ) {
                _core.getLog().warn("Unsupported index configuration found. New configuration will be created ...");
                _newConfiguration = true;
                _configDoc = DatatypeDocumentFactory.getInstance().createDocument();
                _configDocRoot = _configDoc.addElement("wgalucene");
                _configDocRoot.addAttribute("version", VERSION);                
            }            
        }                
    }
    
    /**
     * retrieve all dbs in indexConfiguration incl. indexingRules
     * @return HashMap using dbKey as key and indexingRules as value
     */
    public HashMap retrieveIndexedDbs() {
        HashMap indexedDbs = new HashMap();
        List dbElements = _configDocRoot.selectNodes("//db");
        if (dbElements != null) {
            Iterator it = dbElements.iterator();
            while (it.hasNext()) {
                Element dbElement = (Element) it.next();
                String dbKey = dbElement.attributeValue("key");
                Element configElement = dbElement.element("configuration");
                
                LuceneConfiguration config = new LuceneConfiguration();
                String luceneEnabled = configElement.attributeValue("enabled", "false");
                if (luceneEnabled.trim().equalsIgnoreCase("true")) {
                    config.setEnabled(true);
                } else {
                    config.setEnabled(false);
                }            
                //Lucene Indexing rules
                List itemRules = LuceneIndexItemRule.getRules(configElement);
                List fileRules = LuceneIndexFileRule.getRules(configElement);
                if (itemRules != null) {
                    config.setItemRules(itemRules);
                }
                if (fileRules != null) {
                    config.setFileRules(fileRules);
                }
                
                indexedDbs.put(dbKey, config);
            }
        }
        return indexedDbs;
    }    
    
    private Element fetchDBElement(String dbkey) {
        Element dbElement = (Element) _configDocRoot.selectSingleNode("db[@key='" + dbkey + "']");
        return dbElement;
    }       
    
    /**
     * creates a default dbElement
     * @param dbkey
     * @return
     */
    private Element addDBElement(String dbkey) {
        Element dbElement = (Element) _configDocRoot.selectSingleNode("db[@key='" + dbkey + "']");        
        if (dbElement == null) {            
            dbElement = _configDocRoot.addElement("db");
            dbElement.addAttribute("key", dbkey);
            dbElement.addElement("lastupdate");
            Element configElement = dbElement.addElement("configuration");
            configElement.addAttribute("enabled","false");
            configElement.addElement("itemrules");
            LuceneIndexItemRule.addDefaultRule(configElement);
            configElement.addElement("filerules");
            LuceneIndexFileRule.addDefaultRule(configElement);
        }
        return dbElement;
    }    
    
             
    /**
     * writes the current configuration to file
     * @throws IOException
     */
    private void writeConfigDoc() throws IOException {
        OutputFormat format = OutputFormat.createPrettyPrint();
        XMLWriter writer = new XMLWriter(new FileWriter(_configFile), format);
        writer.write(_configDoc);
        writer.flush();
        writer.close();
    }    
    
    public void addDBConfig(String dbKey, LuceneConfiguration config) {
        _core.getLog().info("Adding DBConfig for db '" + dbKey + "' to indexConfigFile '" + _configFile.getPath() + "'.");
        //update configfile
        addDBElement(dbKey);
        setDBConfig(dbKey, config);
        try {
            writeConfigDoc();
        }
        catch (IOException e) {
            _core.getLog().error("Could not add db '" + dbKey + " to indexConfigFile '" + _configFile.getPath() + "'." , e);
        }        
    }
    
    public void updateDBConfig(String dbKey, LuceneConfiguration config) {
        _core.getLog().info("Updating DBConfig for db '" + dbKey + "' in indexConfigFile '" + _configFile.getPath() + "'.");
        // update configfile
        setDBConfig(dbKey, config);
        try {
            writeConfigDoc();
        } catch (IOException e) {
            _core.getLog().error("Error updating indexing rules for db '" + dbKey + "' in indexConfigFile '" + _configFile.getPath() + "'." , e);
        }        
    }
              
    public void removeDBConfig(String dbKey) {
        _core.getLog().info("Removing DBConfig for db '" + dbKey + "' from indexConfigFile '" + _configFile.getPath() + "'.");
        Element dbElement = (Element) _configDocRoot.selectSingleNode("db[@key='" + dbKey + "']");        
        if (dbElement != null) {
            _configDocRoot.remove(dbElement);            
        }
        try {
            writeConfigDoc();
        } catch (IOException e) {
            _core.getLog().error("Cannot remove db '" + dbKey + "' from indexConfigFile '" + _configFile.getPath() + "'." , e);
        }        
    }    
    
    public void setLastUpdated(String dbKey, WGDatabaseRevision revision) {
        try {
            Element dbElement = fetchDBElement(dbKey);
            Element lastUpdateElement = dbElement.element("lastupdate");
            String persistentForm = revision.serialize();
            lastUpdateElement.setText(persistentForm);
            lastUpdateElement.addAttribute("class", revision.getClass().getName());
            writeConfigDoc();
        }
        catch (Exception e) {
            _core.getLog().error("Cannot set lastUpdate for db '" + dbKey + "' in indexConfigFile '" + _configFile.getPath() + "'." , e);
        }        
    }
    
    public void clearLastUpdated(String dbKey) {
        Element dbElement = fetchDBElement(dbKey);
        Element lastUpdateElement = dbElement.element("lastupdate");
        lastUpdateElement.clearContent();
        try {
            writeConfigDoc();
        } catch (IOException e) {
            _core.getLog().error("Cannot clear lastUpdate for db '" + dbKey + "' in indexConfigFile '" + _configFile.getPath() + "'." , e);
        }        
    }
    
    public WGDatabaseRevision getLastUpdated(String dbKey) {        
        Element dbElement = fetchDBElement(dbKey);
        if (dbElement != null) {
            Element lastUpdateElement = dbElement.element("lastupdate");
            if (lastUpdateElement.hasContent()) {
                try {
                    String lastUpdateString = lastUpdateElement.getText();
                    Class revisionClass = Class.forName(lastUpdateElement.attributeValue("class", Date.class.getName()));
                    return WGDatabaseRevision.deserialize(lastUpdateString);
                }
                catch (Exception e) {
                    _core.getLog().warn("Cannot parse lastUpdate for db '" + dbKey + "' in indexConfigFile '" + _configFile.getPath() + "'." , e);
                }
            }
        }        
        return null;
    }
    
    private void setDBConfig(String dbkey, LuceneConfiguration config) {
        Element dbElement = fetchDBElement(dbkey);
        Element configElement = dbElement.element("configuration");
        if (config.isEnabled()) {
            configElement.addAttribute("enabled", "true");
        } else {
            configElement.addAttribute("enabled", "false");
        }       
        LuceneIndexItemRule.saveRules(configElement, config.getItemRules());
        LuceneIndexFileRule.saveRules(configElement, config.getFileRules());
    }    
    
    protected void finalize() throws Throwable {       
        writeConfigDoc();
    }    
    
    public boolean isNewConfiguration() {
        return _newConfiguration;
    }
}
