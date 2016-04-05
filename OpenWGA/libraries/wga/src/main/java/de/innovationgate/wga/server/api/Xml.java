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

package de.innovationgate.wga.server.api;

import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.jxpath.JXPathContext;
import org.dom4j.Attribute;
import org.dom4j.Branch;
import org.dom4j.CDATA;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Node;
import org.dom4j.Text;
import org.dom4j.XPath;
import org.dom4j.io.SAXReader;
import org.xml.sax.SAXException;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.CodeCompletion;

/**
 * Object "Xml" collects functionalities to load and parse Xml documents.
 * OpenwGA uses the Java XML Parser Dom4J to parse and process XML documents.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Xml {

    @SuppressWarnings("unused")
    private WGA _wga;

    protected Xml(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Creates a new empty DOM document
     */
    public Document create() throws WGException {
        return DocumentHelper.createDocument();
    }
    
    /**
     * Parses XML text and returns a DOM document object for it
     * @param xml The xml
     * @param systemId An URL which should be used to resolve relative references in the XML text, like a DOCTYPE reference.
     * @return DOM document of parsed XML data 
     * @throws DocumentException
     */
    public Document parse(String xml, String systemId) throws WGException, DocumentException {
        // Cutoff eventual content in prolog which may be a problem for the parser (like BOMs, see B00005006)
        int beginTagIndex = xml.indexOf("<");
        if (beginTagIndex != -1) {
            xml = xml.substring(beginTagIndex);
        }
        
        if (systemId != null) {
            SAXReader reader = new SAXReader();
            return reader.read(new StringReader(xml), systemId);
        }
        else {
            return DocumentHelper.parseText(xml);
        }
    }
    
    /**
     * Parses XML text and returns a DOM document object for it
     * @param xml The xml
     * @return DOM document of parsed XML data 
     * @throws DocumentException
     */
    public Document parse(String xml) throws WGException, DocumentException {
        return parse(xml, null);
    }


    /**
     * Executes an XPath expression on some XML text or JavaBean
     * This function always returns single values. If the xpath expression matches multiple values it will only return the first one. To retrieve lists of values use {@link #xpathList(Object, String)}.
     * The given object to parse as XML is either a dom4j branch object (mostly document or element), a String containing XML text or a JavaBean. In the last case this function uses JXPath functionality to find a bean property value.
     * This uses the Apache library JXPath under the hood. See their documentation for details how XPath is used to browser JavaBeans.
     * @param object Object to inspect
     * @param xpath XPath expression
     * @return Returned value
     * @throws DocumentException
     */
    public Object xpath(Object object, String xpath) throws WGException, DocumentException {
        return xpath(object, xpath, null);
    }
    
    /**
     * Executes an XPath expression on some XML text or JavaBean
     * This function always returns single values. If the xpath expression matches multiple values it will only return the first one. To retrieve lists of values use {@link #xpathList(Object, String)}.
     * The given object to parse as XML is either a dom4j branch object (mostly document or element), a String containing XML text or a JavaBean. In the last case this function uses JXPath functionality to find a bean property value.
     * This uses the Apache library JXPath under the hood. See their documentation for details how XPath is used to browser JavaBeans.
     * @param object Object to inspect
     * @param xpath XPath expression
     * @param ns Map of namespace prefix declarations used in the XPath. Keys are prefixes, values are namespace URIs.
     * @return Returned value
     * @throws DocumentException
     */
    public Object xpath(Object object, String xpath, Map<String,String> ns) throws WGException, DocumentException {
        Object result;
        if (object instanceof String || object instanceof Branch) {
            Branch branch = retrieveBranch(object);
            XPath xpathObj = createXPath(xpath, branch, ns);
            result = xpathObj.evaluate(branch);
        }
        
        // Do JXPath on Bean
        else {
            JXPathContext jxContext = JXPathContext.newContext(object);
            jxContext.setLenient(true);
            result = jxContext.getValue(xpath);
        }
        return convertXMLObjects(result, false);
    }

    private Branch retrieveBranch(Object object) throws DocumentException {
        Branch branch;
        if (object instanceof String) {
            branch = DocumentHelper.parseText((String) object);
        }
        else {
            branch = (Branch) object;
        }
        return branch;
    }

    private XPath createXPath(String xpath, Branch branch, Map<String, String> ns) {
        XPath xpathObj = branch.createXPath(xpath);
        if (ns != null) {
            xpathObj.setNamespaceURIs(ns);
        }
        return xpathObj;
    }

    /**
     * Executes an XPath expression on some XML text or JavaBean
     * This function always returns lists. If the xpath expression matches only a single values it will return it as single element in a list. If you only want to retrieve single values use xpath().
     * The given object to parse as XML is either a dom4j branch object (mostly document or element), a String containing XML text or a JavaBean. In the last case this function uses JXPath functionality to find a bean property value.
     * This uses the Apache library JXPath under the hood. See their documentation for details how XPath is used to browser JavaBeans.
     * @param object Object to inspect
     * @param xpath XPath expression
     * @return Returned value
     * @throws DocumentException
     */
    public Object xpathList(Object object, String xpath) throws WGException, DocumentException {
        return xpathList(object, xpath, null);
    }
    
    /**
     * Executes an XPath expression on some XML text or JavaBean
     * This function always returns lists. If the xpath expression matches only a single values it will return it as single element in a list. If you only want to retrieve single values use xpath().
     * The given object to parse as XML is either a dom4j branch object (mostly document or element), a String containing XML text or a JavaBean. In the last case this function uses JXPath functionality to find a bean property value.
     * This uses the Apache library JXPath under the hood. See their documentation for details how XPath is used to browser JavaBeans.
     * @param object Object to inspect
     * @param xpath XPath expression
     * @param ns Map of namespace prefix declarations used in the XPath. Keys are prefixes, values are namespace URIs.
     * @return Returned value
     * @throws DocumentException
     */
    @SuppressWarnings("unchecked")
    public Object xpathList(Object object, String xpath, Map<String,String> ns) throws WGException, DocumentException {
        List<Object> results;
        if (object instanceof String || object instanceof Branch) {
            Branch branch = retrieveBranch(object);
            XPath xpathObj = createXPath(xpath, branch, ns);
            results = xpathObj.selectNodes(branch);
        }
        
        // Do JXPath on Bean
        else {
            JXPathContext jxContext = JXPathContext.newContext(object);
            jxContext.setLenient(true);
            results = jxContext.selectNodes(xpath);
        }
        return convertXMLObjects(results, true);
    }
    
    /**
     * Loads an XML document from a URL and parses it as DOM document
     * @param url URL to the XML document
     * @return DOM document
     * @throws UnsupportedEncodingException
     * @throws WGAPIException
     * @throws IOException
     * @throws HttpException
     * @throws SAXException
     * @throws DocumentException
     */
    public Document load(String url) throws WGException, UnsupportedEncodingException, IOException, HttpException, SAXException, DocumentException {
        return load(null, url);
    }
    
    /**
     * Loads an XML document from a URL and parses it as DOM document
     * @param client  HTTP client object to load the resource on the URL, used to specify various settings regarding this operation. If no client object is passed a default one is used.
     * @param url URL to the XML document
     * @return DOM document
     * @throws UnsupportedEncodingException
     * @throws WGAPIException
     * @throws IOException
     * @throws HttpException
     * @throws SAXException
     * @throws DocumentException
     */
    public Document load(HttpClient client, String url) throws UnsupportedEncodingException, WGException, IOException, HttpException, SAXException, DocumentException {
        
        if (client == null) {
            client = WGFactory.getHttpClientFactory().createHttpClient();
        }
        
        GetMethod homePageMethod = new GetMethod(url);
        int status = client.executeMethod(homePageMethod);
        
        if (homePageMethod.getStatusCode() != 200) {
            throw new HttpException("HTTP Status Code is " + homePageMethod.getStatusCode());
        }
        
        SAXReader reader = new SAXReader();
        return reader.read(homePageMethod.getResponseBodyAsStream(), url);
        
        
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private Object convertXMLObjects(Object result, boolean descent) {
        
        if (result instanceof List && descent) {
            List list = (List) result;
            for (int idx=0; idx < list.size(); idx++) {
                list.set(idx,convertXMLObjects(list.get(idx), false));
            }
        }
        if (result instanceof Text || result instanceof CDATA || result instanceof Attribute) {
            return ((Node) result).getText();
        }
        else {
            return result;
        }
    }
    
    
}