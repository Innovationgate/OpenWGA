/*******************************************************************************
 * Copyright (c) 2009, 2010 Innovation Gate GmbH.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Innovation Gate GmbH - initial API and implementation
 ******************************************************************************/
package de.innovationgate.ant;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class Tml2Tld {

	private File tmlFile;
	private File targetFile;
	private Document tld;

	private Map<String, NodeList> attributeGroups = new HashMap<String, NodeList>();

	public Tml2Tld(File tmlFile, File targetFile) {
		this.tmlFile = tmlFile;
		this.targetFile = targetFile;
		targetFile.getParentFile().mkdirs();
	}

	public void run() throws Exception {
		DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		Document tml = builder.parse(tmlFile);
		this.tld = builder.newDocument();
		
		Element taglib = createElement("taglib");
		taglib.setAttribute("xmlns", "http://java.sun.com/xml/ns/j2ee");
		taglib.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		taglib.setAttribute("xsi:schemaLocation", "http://java.sun.com/xml/ns/j2ee web-jsptaglibrary_2_0.xsd");
		tld.appendChild(taglib);
		
		appendElement(taglib, "tlibversion", "4.0");
		appendElement(taglib, "jspversion", "2.0");
		appendElement(taglib, "shortname", "WebTML");
		
		NodeList groups = tml.getElementsByTagName("attributeGroup");
		for(int i = 0; i < groups.getLength(); i++) {
			Element group = (Element) groups.item(i);
			String name = childText(group, "name");
			attributeGroups.put(name, group.getElementsByTagName("attribute"));
		}
		
		NodeList tags = tml.getElementsByTagName("tag");
		for(int i = 0; i < tags.getLength(); i++) {
			Element tmlTag = (Element) tags.item(i);
			String name = childText(tmlTag, "name");
			copyTag(name, tmlTag, appendElement(taglib, "tag"));
			Element aliases = (Element) tmlTag.getElementsByTagName("aliases").item(0);
			NodeList aliasNames = aliases.getElementsByTagName("string");
			for(int j = 0; j < aliasNames.getLength(); j++) {
				String aliasName = aliasNames.item(j).getTextContent();
				copyTag(aliasName, tmlTag, appendElement(taglib, "tag"));
			}
		}
		
		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.transform(new DOMSource(tld), new StreamResult(targetFile));
	}
	
	private void copyTag(String tagName, Element tmlTag, Element tldTag) {
		appendElement(tldTag, "name", tagName);
		appendElement(tldTag, "tagclass", childText(tmlTag, "tagclass"));
		appendElement(tldTag, "bodycontent", childText(tmlTag, "bodycontent"));
		appendElement(tldTag, "info", childText(tmlTag, "description"));
		appendElement(tldTag, "dynamic-attributes", "true");
		
		NodeList attributes = tmlTag.getElementsByTagName("attribute");
		for(int i = 0; i < attributes.getLength(); i++) {
			Element attr = (Element) attributes.item(i);
			copyAttribute(attr, appendElement(tldTag, "attribute"));
		}
		NodeList groups = ((Element) tmlTag.getElementsByTagName("attributeGroups").item(0)).getElementsByTagName("string");
		for(int i = 0; i < groups.getLength(); i++) {
			String name = groups.item(i).getTextContent();
			NodeList attributes2 = this.attributeGroups.get(name);
			for(int j = 0; j < attributes2.getLength(); j++) {
				Element attr = (Element) attributes2.item(j);
				copyAttribute(attr, appendElement(tldTag, "attribute"));
			}			
		}
		Element sorucelineAttr = appendElement(tldTag, "attribute");
		appendElement(sorucelineAttr, "name", "sourceline");
		appendElement(sorucelineAttr, "required", "false");
		appendElement(sorucelineAttr, "rtexprvalue", "false");
		
	}
	
	private void copyAttribute(Element tmlAttr, Element tldAttr) {
		String attrName = childText(tmlAttr, "name");
		appendElement(tldAttr, "name", attrName);
		appendElement(tldAttr, "required", childText(tmlAttr, "required"));
		appendElement(tldAttr, "rtexprvalue", "true");
	}
	
	private Element createElement(String tagName) {
		return tld.createElement(tagName);
	}
	
	private Element createElement(String tagName, String text) {
		Element e = createElement(tagName);
		e.setTextContent(text);
		return e;
	}

	private Element appendElement(Element parent, String tagName) {
		Element e = createElement(tagName);
		parent.appendChild(e);
		return e;
	}
	
	private void appendElement(Element parent, String tagName, String text) {
		Element e = createElement(tagName, text);
		parent.appendChild(e);
	}
	
	private String childText(Element e, String childName) {
		return e.getElementsByTagName(childName).item(0).getTextContent();
	}

}
