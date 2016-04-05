/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.dom4j.Attribute;
import org.dom4j.Document;
import org.dom4j.DocumentFactory;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Node;
import org.dom4j.QName;
import org.dom4j.Text;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.net.IPv4Restriction;
import de.innovationgate.wga.common.beans.DesignConfiguration;
import de.innovationgate.wga.common.beans.LuceneConfiguration;
import de.innovationgate.wga.common.beans.LuceneIndexFileRule;
import de.innovationgate.wga.common.beans.LuceneIndexItemRule;

/**
 * Utility class for manipulation the WGA configuration file wga.xml
 *
 */
public class WGAXML {
    
    /**
     * Dummy content db title to use in Manager when no title is configured
     */
    public static final String DUMMY_CONTENTDB_TITLE = "(Leave empty to retrieve from database itself)";    
    
    /**
     * String representing personalisation mode "automatic" in wga.xml
     */
    public static final String PERS_MODE_AUTO = "auto";
    /**
     * String representing personalisation mode "login based" in wga.xml
     */
    public static final String PERS_MODE_LOGIN = "login";
    /**
     * String representing personalisation mode "custom / TMLScript based" in wga.xml
     */
    public static final String PERS_MODE_CUSTOM = "custom";
    
    /**
     * String representing personalisation statistics mode "off" in wga.xml
     */
    public static final String PERS_STATISTICS_OFF = "off";
    /**
     * String representing personalisation statistics mode "session based" in wga.xml
     */
    public static final String PERS_STATISTICS_SESSION = "session";
    /**
     * String representing personalisation statistics mode "hit based" in wga.xml
     */
    public static final String PERS_STATISTICS_HIT = "hit";
    
    /**
     * String representing domain login mode "user based" in wga.xml
     */
    public static final String DOMAIN_LOGINMODE_USERLOGINS = "user";
    /**
     * String representing domain login mode "master login" in wga.xml
     */
    public static final String DOMAIN_LOGINMODE_MASTERLOGIN = "master";
    
    /**
     * String representing design synchronisation as design provider in wga.xml
     */
    public static final String DESIGNPROVIDER_SYNC = "sync";
    /**
     * String representing design sharing as design provider in wga.xml
     */
    public static final String DESIGNPROVIDER_DB = "db";
    
    /**
     * String representing virtual design synchronisation in wga.xml
     */
    public static final String DESIGNSYNC_MODE_VIRTUAL = "virtual";
    /**
     * String representing full design synchronisation in wga.xml
     */
    public static final String DESIGNSYNC_MODE_FULL = "full";
    /**
     * String representing direct file system design providing in wga.xml
     */
    public static final String DESIGNSYNC_MODE_DIRECT = "direct";
    
    /**
     * String representing anonymous cross login mode for design sharing in wga.xml
     */
    public static final String DESIGNSHARING_MODE_ANONYMOUS = "anonymous";
    /**
     * String representing anonymous cross login mode for design sharing in wga.xml
     */
    public static final String DESIGNSHARING_MODE_PARALLEL = "parallel";
    
    
    /**
     * add a default domain element to the given parentElement (<domains>)
     * @param parent
     * @param domainName
     * @return the created domain element
     */
    public static Element addDomain(Element parent, String domainName) {
        Element domain = DocumentFactory.getInstance().createElement("domain");
        domain.addAttribute("name", domainName);
        domain.addAttribute("loginattempts", "5");
        Element login = domain.addElement("login");
        login.addAttribute("mode", "user");
        login.addAttribute("username", "");
        login.addAttribute("password", "");
        domain.addElement("defaultdboptions");
        domain.addElement("defaultpublisheroptions");
        Element errorPage = domain.addElement("errorpage");
        errorPage.addAttribute("enabled", "false");
        parent.add(domain);
        return domain;
    }

    
    /**
     * set the given information on the given domainElement
     * @param domainElement
     * @param domainName
     * @param loginMode valid values - (WGAXML.DOMAIN_LOGINMODE_USERLOGINS, WGAXML.DOMAIN_LOGINMODE_MASTERLOGIN)
     * @param masterLoginUsername
     * @param masterLoginPassword
     * @return the updated domainElement
     */
    public static Element setBasicDomainInformation(Element domainElement, String domainName, String loginMode, String masterLoginUsername, String masterLoginPassword) {
        domainElement.addAttribute("name", domainName);
        
        Element login = (Element) domainElement.selectSingleNode("login");
        if (login == null) {
            login = (Element) DocumentFactory.getInstance().createElement("login");
            domainElement.add(login);
        }        
        login.addAttribute("mode", loginMode);                
        login.addAttribute("username", masterLoginUsername);
        login.addAttribute("password", Base64.encode(masterLoginPassword.getBytes()));
        
        return domainElement;
    }
    
    /**
     * Tests if a domain of the given name already exists in wga.xml
     * @param wgaXMLElement The document element of wga.xml
     * @param domainName The name of the domain
     * @return true if a domain of that name is already defined
     */
    public static boolean domainExists(Element wgaXMLElement, String domainName) {
        Element domain = (Element) wgaXMLElement.selectSingleNode("/wga/domains/domain[@name='" + domainName + "']");
        if (domain != null) {
            return true;
        }
        return false;
    }
    
    /**
     * get all defined domainElements in wga.xml
     * @param wgaXMLElement - an element of wga.xml
     * @return domain elements sorted by name
     */
    public static List getDomainElements(Element wgaXMLElement) {
        List domains =  wgaXMLElement.selectNodes("/wga/domains/domain");
        DocumentHelper.sort(domains, "@name");
        return domains;
    }
    
    /**
     * add a default database element to the given parentElement (<contentdbs>)
     * @param parent
     * @param implClass
     * @return the created contentdb-element
     */
    public static Element addContentDB(Element parent, String implClass) {
        DocumentFactory factory = DocumentFactory.getInstance();
        
        Element database = factory.createElement("contentdb");
        database.addAttribute("enabled", "true");
        database.addAttribute("lazyconnect", "false");
        
        Element elem = factory.createElement("type");
        database.add(elem);
        elem.add(factory.createText(implClass));

        elem = factory.createElement("dbkey");
        database.add(elem);
        elem.add(factory.createText("(Enter database key here)"));
        
        elem = factory.createElement("dbpath");
        database.add(elem);
        elem.add(factory.createText("(Enter database path here)"));
        
        elem = factory.createElement("title");
        database.add(elem);

        elem = factory.createElement("domain");
        database.add(elem);
        elem.add(factory.createText("default"));
        
        elem = factory.createElement("design");
        database.add(elem);
        elem.addAttribute("provider", "none");
        elem.addAttribute("mode", "");
        elem.addAttribute("key", "");
        
        elem = factory.createElement("dboptions");
        database.add(elem);
        createDefaultDbOptions(elem);
        
        elem = factory.createElement("publisheroptions");
        database.add(elem);
        createDefaultPublisherOptions(elem);
                
        database.add(factory.createElement("storedqueries"));
        database.add(factory.createElement("fieldmappings"));
        Element cache = database.addElement("cache");
        cache.addAttribute("path", "");
        cache.addAttribute("type", "de.innovationgate.wgpublisher.cache.WGACacheHSQLDB");
        cache.addAttribute("maxpages", "3000");
        
        //lucene config per db
        Element luceneDBConfig = factory.createElement("lucene");
        luceneDBConfig.addAttribute("enabled", "false");
        luceneDBConfig.addElement("itemrules");
        LuceneIndexItemRule.addDefaultRule(luceneDBConfig);
        luceneDBConfig.addElement("filerules");
        LuceneIndexFileRule.addDefaultRule(luceneDBConfig);
        database.add(luceneDBConfig);
        
        
        // Client restrictions
        Element clientrestrictions =  database.addElement("clientrestrictions");
        clientrestrictions.addAttribute("enabled", "false");
        clientrestrictions.addElement("restrictions");
        
        parent.add(database);
        return database;
    }    
    
    /**
     * set the given information on the given contentDbElement
     * @param contentDbElement
     * @param enabled
     * @param dbImplClass
     * @param dbkey
     * @param title
     * @param domain
     * @param dbPath
     * @param defaultLogin
     * @param username
     * @param password
     * @return the updated contentdbElement
     */
    public static Element setBasicContentDBInformation(Element contentDbElement, boolean enabled, boolean lazyConnect, String dbImplClass, String dbkey, String title, String domain, String dbPath, boolean defaultLogin, String username, String password) {
        
        contentDbElement.addAttribute("enabled", String.valueOf(enabled));
        contentDbElement.addAttribute("lazyconnect", String.valueOf(lazyConnect));
       
        contentDbElement.selectSingleNode("type").setText(dbImplClass);
        
        contentDbElement.selectSingleNode("dbkey").setText(dbkey.toLowerCase());
        
        contentDbElement.selectSingleNode("dbpath").setText(dbPath);
        
        Element login = (Element) contentDbElement.selectSingleNode("login");
        if (login == null) {
            login = contentDbElement.addElement("login");
        }                
        if (defaultLogin) {
            login.addAttribute("password", "");
            login.addAttribute("username", "");            
        }
        else {
            login.addAttribute("password", Base64.encode(password.getBytes()));
            login.addAttribute("username", username);
        }
        login.addAttribute("encoding", "base64");

        Element titleNode = contentDbElement.element("title");
        titleNode.clearContent();
        if (title.equals(DUMMY_CONTENTDB_TITLE) || title.trim().equals("")) {
            titleNode.addCDATA("");
        }
        else {
            titleNode.addCDATA(title);
        }

        contentDbElement.selectSingleNode("domain").setText(domain);        
                        
        return contentDbElement;
    }    
    
    /**
     * Tests if a content database of the given key is already registered
     * @param wgaXMLElement The root node of the wga.xml
     * @param dbkey The dbkey to search
     */
    public static boolean contentDBExists(Element wgaXMLElement, String dbkey) {
        Element contentDB = (Element) wgaXMLElement.selectSingleNode("/wga/contentdbs/contentdb[dbkey='" + dbkey.toLowerCase() + "']");
        if (contentDB != null) {
            return true;
        }
        return false;
    }    
    
    /**
     * copy a whole contentDB definition and inserts it as new db with the given key
     * @param contentDBElement The element to copy
     * @param newDBKey The new database key for the copy
     * @return new created contentDBElement
     */
    public static Element copyContentDB(Element contentDBElement, String newDBKey) {
        Element copy = (Element) contentDBElement.clone();
        copy.selectSingleNode("dbkey").setText(newDBKey.toLowerCase());      
        Element contentdbs = contentDBElement.getParent();
        contentdbs.add(copy);
        return copy;
    }
    
    /**
     * Creates default DB options that are included on every new database
     * @param data The database element
     * @return The database element
     */
    public static Element createDefaultDbOptions(Element data) {
        
        String dbType = data.selectSingleNode("../type").getStringValue();
        Map defaultOptions = new HashMap();
        
        if (dbType.startsWith("de.innovationgate.webgate.api.domino")) {
            defaultOptions.put("EnableRTFWebLoader", "false");
            defaultOptions.put("DominoURL", "(Enter URL to Domino HTTP here)");
        }

        if (dbType.startsWith("de.innovationgate.webgate.api.query.rss")) {
            defaultOptions.put("CacheLatency", "10");
        }
        
        if (dbType.startsWith("de.innovationgate.webgate.api.xfiles")) {
            defaultOptions.put("UpdateInterval", "10");
        }
        
        if (dbType.startsWith("de.innovationgate.webgate.api.query.jdbc") || dbType.startsWith("de.innovationgate.webgate.api.jdbc.custom")) {
            defaultOptions.put("Driver", "");
        }
        
        createDefaultOptions(data, defaultOptions);
        
        return data;
    }    
    
    /**
     * Creates default publisher options that are included on every new database
     * @param data The database element
     * @return The database element
     */
    public static Element createDefaultPublisherOptions(Element data) {
        
        String dbType = data.selectSingleNode("../type").getStringValue();
        boolean isQueryImplementation = dbType.startsWith("de.innovationgate.webgate.api.query");
        //boolean isDominoImplementation = (dbType.indexOf(".domino.") != -1);
        
        Map defaultOptions = new HashMap();
        if (isQueryImplementation) {
            defaultOptions.put("AllowBrowsing", "false");
        }
        else {
            defaultOptions.put("AllowBrowsing", "true");                    
        }
        createDefaultOptions(data, defaultOptions);
        
        return data;
        

    }    
    
    /**
     * Creates a list of options at a database element.
     * If an option is already defined it will be overwritten with the value specified.
     * @param data The database element
     * @param defaultOptions Map of options. Map keys are option names, map values are option values.
     */
    private static void createDefaultOptions(Element data, Map defaultOptions) {
        Iterator options = defaultOptions.keySet().iterator();
        String optionName;
        String optionValue;
        Element option;
        
        while (options.hasNext()) {
            optionName = (String) options.next();
            optionValue = (String) defaultOptions.get(optionName);
            
            option = (Element) data.selectSingleNode("option[@name='" + optionName + "']");
            if (option == null) {
                option = data.addElement("option");
                option.addAttribute("name", optionName);
            }
            WGUtils.getOrCreateAttribute(option, "value", optionValue);
        
        }
    }    
    
    /**
     * add a default persDbElement to the given parentElement (<personalisationdbs>)
     * @param parent
     * @param implClass
     * @return the created persDbElement
     */
    public static Element addPersDB(Element parent, String implClass) {
        DocumentFactory factory = DocumentFactory.getInstance();
        
        Element database = factory.createElement("personalisationdb");
        database.addAttribute("enabled", "true");
        database.addAttribute("lazyconnect", "false");
        
        Element elem = factory.createElement("type");
        database.add(elem);
        elem.add(factory.createText(implClass));
        
        elem = factory.createElement("dbpath");
        database.add(elem);
        elem.add(factory.createText("(Enter database path here)"));

        elem = factory.createElement("domain");
        database.add(elem);
        elem.add(factory.createText("default"));
        
        elem = factory.createElement("persconfig");
        database.add(elem);
        elem.addAttribute("mode", "auto");
        
        elem = factory.createElement("dboptions");
        database.add(elem);
        createDefaultDbOptions(elem);
        
        elem = factory.createElement("publisheroptions");
        database.add(elem);
        createDefaultPublisherOptions(elem);
                
        database.add(factory.createElement("userclasses"));
        
        parent.add(database);
        return database;
    }    
    
    /**
     * set the given information on the given persDbElement
     * @param persDbElement
     * @param enabled
     * @param lazyConnect 
     * @param dbImplClass
     * @param dbPath
     * @param mode - valid values (WGAXML.PERS_MODE_AUTO, WGAXML.PERS_MODE_LOGIN, WGAXML.PERS_MODE_CUSTOM)
     * @param statistics - valid values (WGAXML.PERS_STATISTICS_OFF, WGAXML.PERS_STATISTICS_SESSION, WGAXML.PERS_STATISTICS_HIT)
     * @param defaultLogin
     * @param username
     * @param password
     * @param domain
     * @return the updated persDbElement
     */
    public static Element setBasicPersDBInformation(Element persDbElement, boolean enabled, boolean lazyConnect, String dbImplClass, String dbPath, String mode, String statistics, boolean defaultLogin, String username, String password, String domain) {
        
        persDbElement.addAttribute("enabled", String.valueOf(enabled));
        persDbElement.addAttribute("lazyconnect", String.valueOf(lazyConnect));
        
        persDbElement.selectSingleNode("type").setText(dbImplClass);

        persDbElement.selectSingleNode("dbpath").setText(dbPath);
        
        Element persconfig = (Element) persDbElement.selectSingleNode("persconfig");
        persconfig.addAttribute("mode", mode);
        persconfig.addAttribute("statistics", statistics);
        
        Element login = (Element) persDbElement.selectSingleNode("login");
        if (login == null) {
            login = (Element) DocumentFactory.getInstance().createElement("login");
            persDbElement.add(login);
        }
        if (defaultLogin) {
            login.addAttribute("password", "");
            login.addAttribute("username", "");            
        } else {
            login.addAttribute("password", Base64.encode(password.getBytes()));
            login.addAttribute("username", username);            
        }
        login.addAttribute("encoding", "base64");
                        

        persDbElement.selectSingleNode("domain").setText(domain);
                    
        return persDbElement;
    }    
    
    
    /**
     * set the given designSyncConfiguration on the given contentDbElement
     * @param dbElement database element to receive the design sync configuration
     * @param config Design sync configuration bean
     * @return Design Configuration DOM Element
     */
    public static Element createDesignConfig(Element dbElement, DesignConfiguration config) {
        Element designsync = dbElement.element("design");
        designsync.addAttribute("provider", config.getProvider());
        designsync.addAttribute("key", config.getKey());
        designsync.addAttribute("mode", config.getMode());
        designsync.addAttribute("lookupvariants", String.valueOf(config.isLookupDesignVariants()));
        designsync.addAttribute("autoupdate", String.valueOf(config.isAutoUpdate()));
        designsync.setText(config.getDetailInfo());
        
        return dbElement;
    }
    
    /**
     * set the given designSyncConfiguration on the given contentDbElement
     * @param contentDbElement database element to receive the design sync configuration
     * @param provider The design provider
     * @param key Provider information "key"
     * @param mode Provider information "mode"
     * @param detailInfo Detail provider information
     * @return Design Configuration DOM Element
     */
    public static Element createDesignConfig(Element contentDbElement, String provider, String key, String mode, String detailInfo) {
        return createDesignConfig(contentDbElement, new DesignConfiguration(provider, key, mode, detailInfo));
    }
    
    /**
     * set the given luceneConfiguration on the given contentDbElement
     * @param contentDbElement
     * @param enabled
     * @param indexItemRules - list of de.innovationgate.wga.common.beans.LuceneIndexItemRule
     * @return the updated contentDbElement
     */
    public static Element createLuceneConfig(Element contentDbElement, boolean enabled, List indexItemRules) {
        LuceneConfiguration config = new LuceneConfiguration();
        config.setEnabled(enabled);
        config.setItemRules(indexItemRules);
        return createLuceneConfig(contentDbElement, config);
    }    
    
    /**
     * set the given luceneConfiguration on the given contentDbElement
     * @param contentDbElement
     * @param config
     * @return the updated contentDbElement
     */
    public static Element createLuceneConfig(Element contentDbElement, LuceneConfiguration config) {
        Element lucene = contentDbElement.element("lucene");
        if (config.isEnabled()) {
            lucene.addAttribute("enabled", "true");
        } else {
            lucene.addAttribute("enabled", "false");
        }
        LuceneIndexItemRule.saveRules(lucene, config.getItemRules());
        LuceneIndexFileRule.saveRules(lucene, config.getFileRules());
        
        return contentDbElement;        
    }
    
    /**
     * set the given clientRestrictions on the given contentDbElement
     * @param contentDbElement
     * @param enabled
     * @param restrictions - list of de.innovationgate.utils.net.IPv4Restriction
     * @return the updated contentDbElement
     */
    public static Element createClientRestrictions(Element contentDbElement, boolean enabled, List restrictions) {
        Element clientRestrictions = contentDbElement.element("clientrestrictions");
        clientRestrictions.addAttribute("enabled", new Boolean(enabled).toString());
        IPv4Restriction.saveRestrictions(clientRestrictions, restrictions);
        
        return contentDbElement;
    }    
    
    /**
     * creates default file handler mappings on the given filehandlermappings-element
     * @param fileHandlerMappingElement
     * @return the updated filehandlermappings-element
     */
    public static Element createDefaultFileHandlerMappings(Element fileHandlerMappingElement) {
    	// none - this is done by lucene file handler plugin
    	return fileHandlerMappingElement;
    }
    
    private static void removeDefaultFileHandlerMappings(Element fileHandlerMappingElement) {   	
    	// remove old default file handler 
    	Iterator mappings = fileHandlerMappingElement.elementIterator("filehandlermapping");
    	while (mappings.hasNext()) {
    		Element mapping = (Element) mappings.next();
    		String extension = mapping.attributeValue("extension", null);
    		String className = mapping.attributeValue("class", null);
    		if (extension != null && className != null) { 
    			if (extension.equalsIgnoreCase("pdf") && 
    					className.equals("de.innovationgate.wgpublisher.lucene.analysis.PDFFileHandler")) {
    				mappings.remove();
    			} else if (extension.equalsIgnoreCase("xml") && 
    					className.equals("de.innovationgate.wgpublisher.lucene.analysis.XMLFileHandler")) {
    				mappings.remove();
    			} else if (extension.equalsIgnoreCase("txt") &&
    					className.equals("de.innovationgate.wgpublisher.lucene.analysis.TXTFileHandler")) {
    				mappings.remove();
    			} else if (extension.equalsIgnoreCase("doc") && 
    					className.equals("de.innovationgate.wgpublisher.lucene.analysis.DOCFileHandler")) {
    				mappings.remove();
    			}   			    			
    		}
    	}   
    }

    /**
     * Automatically sorts design providers in wga.xml to be the first to be connected.
     * By that way the situation that a design consumer is connected earlier than its provider will be avoided
     * @param doc The wga.xml document
     */
    public static void pullupDesignProviders(Document doc) {
        
        // Collect keys of used provider dbs
        Element contentdbs = (Element) doc.selectSingleNode("/wga/contentdbs");
        Iterator providerKeysElems = contentdbs.selectNodes("contentdb/design[@provider='db']/@key").iterator();
        Set providerKeys = new LinkedHashSet();
        while (providerKeysElems.hasNext()) {
            Attribute providerKeyAttr = (Attribute) providerKeysElems.next();
            String key = providerKeyAttr.getValue();
            providerKeys.add(key);
        }
        
        // Move them up
        Iterator keysIt = providerKeys.iterator();
        int insertIdx = 0;
        while (keysIt.hasNext()) {
            String key = (String) keysIt.next();
            Element contentdb = (Element) contentdbs.selectSingleNode("contentdb[dbkey='" + key + "']");
            if (contentdb != null) {
                contentdbs.content().remove(contentdb);
                contentdbs.content().add(insertIdx, contentdb);
                insertIdx++;
            }
        }
    }

    /**
     * Performs normalization on the wga.xml by creating mandatory elements and attributes and doing some
     * additional validations, like converting obsolete structures, defining yet undefined domains etc.
     * @param doc The wga.xml
     */
    public static void normalize(Document doc) {
    
        // Remove obsolete namespace
        String ns = "urn:de.innovationgate.webgate.api.query.domino.WGDatabaseImpl";
        Iterator nodes = doc.selectNodes("//*[namespace-uri(.)='" + ns + "']").iterator();
        Element element;
        while (nodes.hasNext()) {
            element = (Element) nodes.next();
            element.setQName(QName.get(element.getName()));
        }
    
        // Build necessary elements
        Element wga = (Element) doc.selectSingleNode("wga");
    
        // Licenses
        Element licenses = WGUtils.getOrCreateElement(wga, "licenses");
        Iterator licenseTags = licenses.elements("authorlicense").iterator();
        while (licenseTags.hasNext()) {
            Element licenseTag = (Element) licenseTags.next();
            //WGUtils.getOrCreateAttribute(licenseTag, "type", "WGA.Client");
            // B0000486E
            licenseTag.addAttribute("type", "WGA.Client");
        }
    
        // administrators
        WGUtils.getOrCreateElement(wga, "administrators");
    
        // configuration
        Element configuration = WGUtils.getOrCreateElement(wga, "configuration");
        Element defaultdb = WGUtils.getOrCreateElement(configuration, "defaultdb");
        WGUtils.getOrCreateAttribute(defaultdb, "key", "");
        WGUtils.getOrCreateAttribute(defaultdb, "favicon", "");
        WGUtils.getOrCreateAttribute(defaultdb, "datacache", "10000");
        WGUtils.getOrCreateAttribute(defaultdb, "staticexpiration", "10");
    
        Element features = WGUtils.getOrCreateElement(configuration, "features");
        WGUtils.getOrCreateAttribute(features, "bi", "true");
        WGUtils.getOrCreateAttribute(features, "adminpage", "true");
        WGUtils.getOrCreateAttribute(features, "manager", "true");
        WGUtils.getOrCreateAttribute(features, "startpage", "true");
        WGUtils.getOrCreateAttribute(features, "webdav", "true");
        WGUtils.getOrCreateAttribute(features, "webservice", "true");
        WGUtils.getOrCreateAttribute(features, "adminport", "");
        WGUtils.getOrCreateAttribute(features, "authoringport", "");
        WGUtils.getOrCreateAttribute(features, "clusterport", "");
        
    
        Element warnings = WGUtils.getOrCreateElement(configuration, "warnings");
        WGUtils.getOrCreateAttribute(warnings, "enabled", "true");
        WGUtils.getOrCreateAttribute(warnings, "consoleOutput", "false");
        WGUtils.getOrCreateAttribute(warnings, "pageOutput", "true");
    
        Element tml = WGUtils.getOrCreateElement(configuration, "tml");
        WGUtils.getOrCreateAttribute(tml, "characterEncoding", "");
        WGUtils.getOrCreateAttribute(tml, "linkEncoding", "UTF-8");
        Element tmlheader = WGUtils.getOrCreateElement(tml, "tmlheader");
        WGUtils.getOrCreateAttribute(tmlheader, "buffer", "8kb");
    
        Element authoringconfig = WGUtils.getOrCreateElement(configuration, "authoringconfig");
        WGUtils.getOrCreateAttribute(authoringconfig, "dbfile", "");
    
        Element applog = WGUtils.getOrCreateElement(configuration, "applog");
        WGUtils.getOrCreateAttribute(applog, "level", "INFO");
        WGUtils.getOrCreateAttribute(applog, "logserver", "false");
        
        Element compression = WGUtils.getOrCreateElement(configuration, "compression");
        WGUtils.getOrCreateAttribute(compression, "enabled", "false");
        
        Element listeners = WGUtils.getOrCreateElement(configuration, "listeners");
    
        Element lucene = WGUtils.getOrCreateElement(configuration, "lucene");
        WGUtils.getOrCreateAttribute(lucene, "dir", "");
        WGUtils.getOrCreateAttribute(lucene, "enabled", "false");
        WGUtils.getOrCreateAttribute(lucene, "booleanQueryMaxClauseCount", "1024");
        WGUtils.getOrCreateAttribute(lucene, "maxDocsPerDBSession", "50");
        
        // read old lucene enabled dbs
        Attribute dbs = WGUtils.getOrCreateAttribute(lucene, "dbs", "");
        List oldLuceneEnabledDBKeys = WGUtils.deserializeCollection(dbs.getText(), ",");        
        // remove old attribute for lucene enabled dbs
        lucene.remove(dbs);
        
        Element persoconfig = WGUtils.getOrCreateElement(configuration, "personalisation");
        
        // Element for TestCore - config
        Element testcore = WGUtils.getOrCreateElement(configuration, "testcore");
        WGUtils.getOrCreateAttribute(testcore, "dir", "");
        WGUtils.getOrCreateAttribute(testcore, "enabled", "false");
        
        Element design = WGUtils.getOrCreateElement(configuration, "designsync");
        WGUtils.getOrCreateAttribute(design, "fileEncoding", "");
        WGUtils.getOrCreateAttribute(design, "interval", "1");
        WGUtils.getOrCreateAttribute(design, "throttling", "false");
        WGUtils.getOrCreateAttribute(design, "throttlingactivation", "10");
        
        Element jdbcDrivers = WGUtils.getOrCreateElement(configuration, "jdbcdrivers");
        
        WGUtils.getOrCreateElement(configuration, "defaultdboptions");
        WGUtils.getOrCreateElement(configuration, "defaultpublisheroptions");
        
        Element mailConfig = WGUtils.getOrCreateElement(configuration, "mailconfig");
        WGUtils.getOrCreateAttribute(mailConfig, "mailHost", "");
        WGUtils.getOrCreateAttribute(mailConfig, "mailUser", "");
        WGUtils.getOrCreateAttribute(mailConfig, "mailPassword", "");
        WGUtils.getOrCreateAttribute(mailConfig, "mailFrom", "");
        WGUtils.getOrCreateAttribute(mailConfig, "mailTo", "");
        WGUtils.getOrCreateAttribute(mailConfig, "mailWGARootURL", "");
        WGUtils.getOrCreateAttribute(mailConfig, "useAsDefaultForWF", "false");
        WGUtils.getOrCreateAttribute(mailConfig, "enableAdminNotifications", "true");        
        
        // Mappings
        Element mappings = WGUtils.getOrCreateElement(wga, "mappings");
        Attribute mappingLibraries = WGUtils.getOrCreateAttribute(mappings, "libraries", "");
    
        Element elementmappings = WGUtils.getOrCreateElement(mappings, "elementmappings");
        if (elementmappings.attribute("libraries") != null && mappingLibraries.getText().equals("")) {
            mappingLibraries.setText(elementmappings.attributeValue("libraries", ""));
            elementmappings.remove(elementmappings.attribute("libraries"));
        }
        
        List elementsToRemove = new ArrayList();
        Iterator elementmappingTags = elementmappings.selectNodes("elementmapping").iterator();
        while (elementmappingTags.hasNext()) {
            Element elementmapping = (Element) elementmappingTags.next();
            if (elementmapping.attribute("binary") != null) {
                elementmapping.remove(elementmapping.attribute("binary"));
            }
            // remove old FOP implementation reference (F000040EE)
            String implClass = elementmapping.attributeValue("class", null);
            if (implClass != null && implClass.equals("de.innovationgate.wgpublisher.webtml.elements.FOP")) {
            	elementsToRemove.add(elementmapping);
            }
        }        
        Iterator toRemove = elementsToRemove.iterator();
        while (toRemove.hasNext()) {
        	Element elementmapping = (Element) toRemove.next();
        	elementmappings.remove(elementmapping);
        }
    
        Element mediamappings = WGUtils.getOrCreateElement(mappings, "mediamappings");
        Iterator mediamappingTags = mediamappings.selectNodes("mediamapping").iterator();
        while (mediamappingTags.hasNext()) {
            Element mediamapping = (Element) mediamappingTags.next();
            WGUtils.getOrCreateAttribute(mediamapping, "binary", "false");
            WGUtils.getOrCreateAttribute(mediamapping, "httplogin", "false");
        }
    
        WGUtils.getOrCreateElement(mappings, "encodermappings");
        WGUtils.getOrCreateElement(mappings, "syncmappings");
    
        Element analyzermappings = WGUtils.getOrCreateElement(mappings, "analyzermappings");
        WGUtils.getOrCreateAttribute(analyzermappings, "defaultAnalyzerClass", "de.innovationgate.wgpublisher.lucene.analysis.StandardAnalyzer");
    

       	removeDefaultFileHandlerMappings(WGUtils.getOrCreateElement(mappings, "filehandlermappings"));

        
        WGUtils.getOrCreateElement(mappings, "filtermappings");
        
        Element scheduler = WGUtils.getOrCreateElement(wga, "scheduler");
        WGUtils.getOrCreateAttribute(scheduler, "loggingdir", "");
    
        // Domains
        Element domains = WGUtils.getOrCreateElement(wga, "domains");
        Iterator domainsIt = domains.elementIterator("domain");
        while (domainsIt.hasNext()) {
            Element domain = (Element) domainsIt.next();
            WGUtils.getOrCreateAttribute(domain, "name", "");
            WGUtils.getOrCreateAttribute(domain, "loginattempts", "5");
            WGUtils.getOrCreateAttribute(domain, "defaultmanager", "");
            Element login = WGUtils.getOrCreateElement(domain, "login");
            WGUtils.getOrCreateAttribute(login, "mode", "user");
            WGUtils.getOrCreateAttribute(login, "username", "");
            WGUtils.getOrCreateAttribute(login, "password", "");
            Element errorpage = WGUtils.getOrCreateElement(domain, "errorpage");
            WGUtils.getOrCreateAttribute(errorpage, "enabled", "false");
            WGUtils.getOrCreateElement(domain, "defaultdboptions");
            WGUtils.getOrCreateElement(domain, "defaultpublisheroptions");
        }
    
        // content dbs
        Element contentdbs = WGUtils.getOrCreateElement(wga, "contentdbs");
        Iterator contentdbTags = contentdbs.selectNodes("contentdb").iterator();
        Set usedDomains = new HashSet();
        while (contentdbTags.hasNext()) {
            Element contentdb = (Element) contentdbTags.next();
            WGUtils.getOrCreateAttribute(contentdb, "enabled", "true");
            WGUtils.getOrCreateAttribute(contentdb, "lazyconnect", "false");
    
            Element type = WGUtils.getOrCreateElement(contentdb, "type");
            String typeName = type.getStringValue();
            if (typeName.equals("de.innovationgate.webgate.api.domino.local.WGDatabaseImpl")) {
                type.setText("de.innovationgate.webgate.api.domino.WGDatabaseImpl");
            }
            
            boolean isFullContentStore = false;
            DbType dbType = DbType.getByImplClass(DbType.GENTYPE_CONTENT, typeName);
            if (dbType != null) {
                isFullContentStore = dbType.isFullContentStore();
            }
            
            //lowercase dbkey
            Element dbkey = WGUtils.getOrCreateElement(contentdb, "dbkey");
            dbkey.setText(dbkey.getText().trim().toLowerCase());
            
            WGUtils.getOrCreateElement(contentdb, "title");
            Element domain = WGUtils.getOrCreateElement(contentdb, "domain");
            String domainStr = domain.getTextTrim();
            if (domainStr.equals("")) {
                domainStr = "masterloginonly";
                domain.setText("masterloginonly");
            }
            usedDomains.add(domainStr);
            WGUtils.getOrCreateElement(contentdb, "login");
    
            Element dboptions = WGUtils.getOrCreateElement(contentdb, "dboptions");
            Iterator options = dboptions.selectNodes("option").iterator();
            Element option;
            String optionName;
            while (options.hasNext()) {
                option = (Element) options.next();
                optionName = option.attributeValue("name");
                if (optionName.indexOf(":") != -1) {
                    option.addAttribute("name", optionName.substring(optionName.indexOf(":") + 1));
                }
            }
    
            WGUtils.getOrCreateElement(contentdb, "publisheroptions");
            WGUtils.getOrCreateElement(contentdb, "storedqueries");
            WGUtils.getOrCreateElement(contentdb, "fieldmappings");
            
            if (isFullContentStore) {
                WGUtils.getOrCreateElement(contentdb, "shares");
            }
            else {
                if (contentdb.element("shares") != null) {
                    contentdb.remove(contentdb.element("shares"));
                }
            }
            
            Element cache = WGUtils.getOrCreateElement(contentdb, "cache");
            WGUtils.getOrCreateAttribute(cache, "type", "de.innovationgate.wgpublisher.cache.WGACacheHSQLDB");
            WGUtils.getOrCreateAttribute(cache, "path", "");
            WGUtils.getOrCreateAttribute(cache, "maxpages", "5000");
            
            // Design - Migrate old designsync element
            Element designsync = contentdb.element("designsync");
            design = contentdb.element("design");
            if (designsync != null && design == null) {
                design = contentdb.addElement("design");
                if (designsync.attributeValue("enabled", "false").equals("true")) {
                    design.addAttribute("provider", "sync");
                }
                else {
                    design.addAttribute("provider", "none");
                }
                design.addAttribute("mode", designsync.attributeValue("mode", ""));
                design.addAttribute("key", designsync.attributeValue("key", ""));
                design.setText(designsync.getText());
            }
            else {
                design = WGUtils.getOrCreateElement(contentdb, "design");
                WGUtils.getOrCreateAttribute(design, "provider", "none");
                WGUtils.getOrCreateAttribute(design, "mode", "");
                WGUtils.getOrCreateAttribute(design, "key", "");
            }
            
            // create default lucene config for old enabled dbs
            if (oldLuceneEnabledDBKeys.contains(dbkey.getText().toLowerCase())) {
                Element luceneDBConfig = WGUtils.getOrCreateElement(contentdb, "lucene");
                WGUtils.getOrCreateAttribute(luceneDBConfig, "enabled", "true");
                WGUtils.getOrCreateElement(luceneDBConfig, "itemrules");
                // create defaultrule
                LuceneIndexItemRule.addDefaultRule(luceneDBConfig);
            }
            
            //lucene config per db
            Element luceneDBConfig = WGUtils.getOrCreateElement(contentdb, "lucene");
            WGUtils.getOrCreateAttribute(luceneDBConfig, "enabled", "false");
            WGUtils.getOrCreateElement(luceneDBConfig, "itemrules");
            //check for default rule
            ArrayList rules = (ArrayList)LuceneIndexItemRule.getRules(luceneDBConfig);
            if (rules.size() > 0) {
                //check if last rule is defaultrule
                LuceneIndexItemRule checkDefaultRule = (LuceneIndexItemRule) rules.get(rules.size()-1);
                if (!checkDefaultRule.getItemExpression().equals(LuceneIndexItemRule.EXPRESSION_WILDCARD)) {
                    //last rule is no defaultRule, create defaultRule
                    LuceneIndexItemRule.addDefaultRule(luceneDBConfig);
                }
            } else {
                //no rules present, create defaultRule
                LuceneIndexItemRule.addDefaultRule(luceneDBConfig);
            }
            // lucene file rules
            WGUtils.getOrCreateElement(luceneDBConfig, "filerules");
            //check for default filerule
            rules = (ArrayList)LuceneIndexFileRule.getRules(luceneDBConfig);
            if (rules.size() > 0) {
                //check if last rule is defaultrule
                LuceneIndexFileRule checkDefaultRule = (LuceneIndexFileRule) rules.get(rules.size()-1);
                if (!checkDefaultRule.isDefaultRule()) {
                    //last rule is no defaultRule, create defaultRule
                    LuceneIndexFileRule.addDefaultRule(luceneDBConfig);
                }
            } else {
                //no rules present, create defaultRule
                LuceneIndexFileRule.addDefaultRule(luceneDBConfig);
            }
            
            
            // client restrictions
            Element clientRestrictions = WGUtils.getOrCreateElement(contentdb, "clientrestrictions");
            WGUtils.getOrCreateAttribute(clientRestrictions, "enabled", "false");
            WGUtils.getOrCreateElement(clientRestrictions, "restrictions");
        }
        
        // Personalisation dbs
        Element persodbs = WGUtils.getOrCreateElement(wga, "personalisationdbs");
        Iterator persodbTags = persodbs.selectNodes("personalisationdb").iterator();
        while (persodbTags.hasNext()) {
            Element persodb = (Element) persodbTags.next();
            WGUtils.getOrCreateAttribute(persodb, "enabled", "true");
            WGUtils.getOrCreateAttribute(persodb, "lazyconnect", "false");
    
            Element type = WGUtils.getOrCreateElement(persodb, "type");
            if (type.getStringValue().equals("de.innovationgate.webgate.api.domino.local.WGDatabaseImpl")) {
                type.setText("de.innovationgate.webgate.api.domino.WGDatabaseImpl");
            }
                       
            Element domain = WGUtils.getOrCreateElement(persodb, "domain");
            String domainStr = domain.getTextTrim();
            if (domainStr.equals("")) {
                domainStr = "masterloginonly";
                domain.setText("masterloginonly");
            }
            usedDomains.add(domainStr);
            WGUtils.getOrCreateElement(persodb, "login");
            
            Element persConfig = WGUtils.getOrCreateElement(persodb, "persconfig");
            WGUtils.getOrCreateAttribute(persConfig, "mode", "auto");
            WGUtils.getOrCreateAttribute(persConfig, "statistics", "off");
    
            Element dboptions = WGUtils.getOrCreateElement(persodb, "dboptions");
            Iterator options = dboptions.selectNodes("option").iterator();
            Element option;
            String optionName;
            while (options.hasNext()) {
                option = (Element) options.next();
                optionName = option.attributeValue("name");
                if (optionName.indexOf(":") != -1) {
                    option.addAttribute("name", optionName.substring(optionName.indexOf(":") + 1));
                }
            }
    
            WGUtils.getOrCreateElement(persodb, "publisheroptions");
        }
        
        //  **** Post-Processings **** 
        
        // Turn stored queries into CDATA-Sections
        List queries = doc.selectNodes("/wga/contentdbs/contentdb/storedqueries/storedquery/query");
        for (Iterator iter = queries.iterator(); iter.hasNext();) {
            Element query = (Element) iter.next();
            Node text = query.selectSingleNode("text()");
            if (text != null && text instanceof Text) {
                query.addCDATA(text.getText());
                query.remove(text);
            }
        }
        
        // Create domains from database definitions
        Iterator usedDomainsIt = usedDomains.iterator();
        String usedDomain;
        while (usedDomainsIt.hasNext()) {
            usedDomain = (String) usedDomainsIt.next();
            Element domain = (Element) domains.selectSingleNode("domain[@name='" + usedDomain + "']");
            if (domain == null) {
                domain = domains.addElement("domain");
                domain.addAttribute("name", usedDomain);
                Element login = domain.addElement("login");
                if (usedDomain.equals("masterloginonly")) {
                    login.addAttribute("mode", "master");
                }
                else {
                    login.addAttribute("mode", "user");
                }
                login.addAttribute("username", "");
                login.addAttribute("password", "");
                Element errorPage = domain.addElement("errorpage");
                errorPage.addAttribute("enabled", "false");
                Element defDBOptions = domain.addElement("defaultdboptions");
                Element defPublisherOptions = domain.addElement("defaultpublisheroptions");
            }
        }
        
        // Reorder content dbs, so design providers are first
        pullupDesignProviders(doc);
    }
    
    /**
     * Adds or replaces a single option. The option is created if it does not yet exist.
     * If it exists its value will be replaced.
     * @param parent The parent element for options
     * @param name The name of the option
     * @param value The value of the option
     * @return The added or replaced option element
     */
    public static Element addOrReplaceOption(Element parent, String name, String value) {
        
        Element optionElement = (Element) parent.selectSingleNode("option[@name='" + name + "']");
        if (optionElement == null) {
            optionElement = parent.addElement("option");
            optionElement.addAttribute("name", name);
        }
        optionElement.addAttribute("value", value);
        return optionElement;
    }
    
    /**
     * Remoevs a single option.
     * @param parent The parent element for options
     * @param name The name of the option
     * @return true, if the element existed and was removed, false if it did not exist
     */
    public static boolean removeOption(Element parent, String name) {
        Element optionElement = (Element) parent.selectSingleNode("option[@name='" + name + "']");
        if (optionElement != null) {
            optionElement.getParent().remove(optionElement);
            return true;
        }
        else {
            return false;
        }
    }
    
    
}
