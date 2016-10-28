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
/*
 * Created on 17.12.2008 from oliver
 *
 */
package de.innovationgate.igutils.htmldiff;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.Locale;

import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;

import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.io.HTMLWriter;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXContentHandler;
import org.outerj.daisy.diff.HtmlCleaner;
import org.outerj.daisy.diff.helper.NekoHtmlParser;
import org.outerj.daisy.diff.html.HTMLDiffer;
import org.outerj.daisy.diff.html.HtmlSaxDiffOutput;
import org.outerj.daisy.diff.html.TextNodeComparator;
import org.outerj.daisy.diff.html.dom.DomTreeBuilder;
import org.xml.sax.InputSource;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.utils.ElementImpl;
import de.innovationgate.wgpublisher.webtml.utils.ElementImplContext;

public class HTMLDiff implements ElementImpl {

    public boolean afterBody(ElementImplContext context) throws WGAPIException {
        return false;
    }

    public boolean beforeBody(ElementImplContext context) throws WGAPIException {
        return false;
    }

    public void begin(ElementImplContext context) throws WGAPIException {
    }

    public void end(ElementImplContext context) throws WGAPIException {

        try {
            String code1 = (String) context.getOptions().get("code1");
            String code2 = (String) context.getOptions().get("code2");
            String cleanStr = (String) context.getOptions().get("clean");
            boolean clean = (cleanStr != null ? WGUtils.stringToBoolean(cleanStr) : true);
            
            
            if (code1 == null || code2 == null) {
                context.getTMLContext().addwarning("Input code 1 or 2 is null", true);
                return;
            }
            
            // Preparation of parsing objects
            SAXTransformerFactory tf = (SAXTransformerFactory) TransformerFactory.newInstance();
            Locale locale = context.getTMLContext().getPreferredLanguageLocale();
            
            InputSource code1Source = new InputSource(new StringReader(code1));
            InputSource code2Source = new InputSource(new StringReader(code2));
            
            // "Clean" the source code so the XML functionalities can use it
            DomTreeBuilder code1dom = new DomTreeBuilder();
            DomTreeBuilder code2dom = new DomTreeBuilder();
            NekoHtmlParser nekoParser = new NekoHtmlParser();
            if (clean) {
                HtmlCleaner cleaner = new HtmlCleaner();
                cleaner.cleanAndParse(code1Source, code1dom);
                cleaner.cleanAndParse(code2Source, code2dom);
            }
            else {
                nekoParser.parse(code1Source, code1dom);
                nekoParser.parse(code2Source, code2dom);
            }
            
            // Do the diff first into a string, which is XML format
            // this breaks some HTML artefacts for browsers (<div/> <script src=""/>)
            TextNodeComparator leftComparator = new TextNodeComparator(code1dom,locale);
            TextNodeComparator rightComparator = new TextNodeComparator(code2dom,locale);
            TransformerHandler contentHandler = tf.newTransformerHandler();
            StringWriter writer = new StringWriter();
            contentHandler.setResult(new StreamResult(writer));
            HtmlSaxDiffOutput output = new HtmlSaxDiffOutput(contentHandler, "diff");
            
            HTMLDiffer differ = new HTMLDiffer(output);
            differ.diff(leftComparator, rightComparator);
            String resultXML = writer.toString();
            
            // Read result xml with html parser again into to dom4j document
            // (we cannot fetch xml directly with xml parser since it is only a fragment)
            SAXContentHandler dom4jHandler = new SAXContentHandler();
            nekoParser.parse(new InputSource(new StringReader(resultXML)), dom4jHandler);
            Document resultDocument = dom4jHandler.getDocument();  
            
            // Put out XML document with HTML style settings. Use only "body" content if possible
            Element body = (Element) resultDocument.selectSingleNode("/html/body");
            
            OutputFormat outputFormat = new OutputFormat();
            outputFormat.setExpandEmptyElements(true);
            outputFormat.setIndent(true);
            outputFormat.setSuppressDeclaration(true);
            outputFormat.setXHTML(false);
            StringWriter resultWriter = new StringWriter();
            HTMLWriter htmlWriter = new HTMLWriter(resultWriter, outputFormat);
            if (body != null) {
                htmlWriter.write(body.content());
            }
            else {
                htmlWriter.write(resultDocument);
            }
            
            // Set the string writer result as element result
            context.clearResult();
            context.appendResult(resultWriter.toString());
            
        }
        catch (Exception e) {
            context.getLog().error("Exception diffing html", e);
            context.getTMLContext().addwarning("Exception diffing html: " + e.getClass().getName() + " - " + e.getMessage(), true);
        }
        
        
    }

    public Object tagInfo(String name) {
        return null;
    }

}
