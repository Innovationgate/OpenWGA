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
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.html.dom.HTMLDocumentImpl;
import org.cyberneko.html.parsers.DOMFragmentParser;
import org.cyberneko.html.parsers.DOMParser;
import org.dom4j.Document;
import org.dom4j.io.DOMReader;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.html.HTMLDocument;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.CodeCompletion;

/**
 * Collects functionalties to load and parse HTML documents.
 * The HTML parser NekoHTML is used for parsing HTML documents as a DOM tree. It is tolerant against most "common errors" done on HTML documents and may parse a wide range of HTML structures.
 * Note: NekoHTML converts all HTML tags name to uppercase even if they were specified as lowercase in the source code. This is important when querying the DOM via XPath.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Html {

    @SuppressWarnings("unused")
    private WGA _wga;

    protected Html(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Loads a HTML page from a URL and parses it as DOM document
     * The HTML parser NekoHTML is responsible for parsing the HTML document as a DOM tree. It is tolerant against most "common errors" done on HTML documents and may parse a wide range of HTML structures.
     * @param url The URL to load the HTML document from
     * @return A DOM document
     * @throws UnsupportedEncodingException
     * @throws WGAPIException
     * @throws IOException
     * @throws HttpException
     * @throws SAXException
     */
    public Document load(String url) throws UnsupportedEncodingException, WGException, IOException, HttpException, SAXException {
        return load(null, url);
    }

    /**
     * Loads a HTML page from a URL and parses it as DOM document
     * @param client  HTTP client object to load the resource on the URL, used to specify various settings regarding this operation. If no client object is passed a default one is used.
     * @param url The URL to load the HTML document from
     * @return A DOM document
     * @throws UnsupportedEncodingException
     * @throws WGAPIException
     * @throws IOException
     * @throws HttpException
     * @throws SAXException
     */
    public Document load(HttpClient client, String url) throws UnsupportedEncodingException, WGException, IOException, HttpException, SAXException {
        
        if (client == null) {
            client = WGFactory.getHttpClientFactory().createHttpClient();
        }
        
        GetMethod homePageMethod = new GetMethod(url);
        int status = client.executeMethod(homePageMethod);
        
        if (homePageMethod.getStatusCode() != 200) {
            throw new HttpException("HTTP Status Code is " + homePageMethod.getStatusCode());
        }
        
        DOMParser parser = new DOMParser();
        parser.setFeature("http://xml.org/sax/features/namespaces", false);
        
        InputSource inputSource;
        if (homePageMethod.getResponseCharSet() != null) {
            Reader reader = new InputStreamReader(homePageMethod.getResponseBodyAsStream(), homePageMethod.getResponseCharSet());
            inputSource = new InputSource(reader);
        }
        else {
            inputSource = new InputSource(homePageMethod.getResponseBodyAsStream());    
        }
        
        parser.parse(inputSource);
        org.w3c.dom.Document w3cDoc = parser.getDocument();
        
        DOMReader xmlReader = new DOMReader();
        return xmlReader.read(w3cDoc);
        
    }
    
    /**
     * Parses HTML text and returns it as DOM document object. The returned document represents a whole HTML document. Omitted elements are automatically added.
     * The HTML parser NekoHTML is responsible for parsing the HTML document as a DOM tree. It is tolerant against most "common errors" done on HTML documents and may parse a wide range of HTML structures. 
     * Note: NekoHTML converts all HTML tags name to uppercase even if they were specified as lowercase in the source code. This is important when querying the DOM via XPath.
     * @param html
     * @return The DOM document of the parsed HTML
     * @throws SAXException
     * @throws IOException
     */
    public Document parse(String html) throws WGException, SAXException, IOException {
        
        DOMParser parser = new DOMParser();
        parser.parse(new InputSource(new StringReader(html)));
        org.w3c.dom.Document w3cDoc = parser.getDocument();
        
        DOMReader xmlReader = new DOMReader();
        return xmlReader.read(w3cDoc);
        
    }
    
    /**
     * Parses HTML text and returns it as DOM document object. The returned document represents only the parsed fragment.
     * The HTML parser NekoHTML is responsible for parsing the HTML document as a DOM tree. It is tolerant against most "common errors" done on HTML documents and may parse a wide range of HTML structures. 
     * Note: NekoHTML converts all HTML tags name to uppercase even if they were specified as lowercase in the source code. This is important when querying the DOM via XPath.
     * @param html
     * @return The DOM document of the parsed HTML
     * @throws SAXException
     * @throws IOException
     */
    public Document parseFragment(String html) throws WGException, SAXException, IOException {
        
        HTMLDocument document = new HTMLDocumentImpl();
        DOMFragmentParser parser = new DOMFragmentParser();
        DocumentFragment frag = document.createDocumentFragment();
        parser.parse(new InputSource(new StringReader(html)), frag);
        document.appendChild(frag);
        DOMReader xmlReader = new DOMReader();
        return xmlReader.read(document);
        
    }
    
}