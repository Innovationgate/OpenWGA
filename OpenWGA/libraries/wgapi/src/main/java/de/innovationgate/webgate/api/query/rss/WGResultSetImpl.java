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
package de.innovationgate.webgate.api.query.rss;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGResultSetCore;

public class WGResultSetImpl implements WGResultSetCore {
	
	public static final String NS_RSS1_NETSCAPE = "http://my.netscape.com/rdf/simple/0.9/";
	public static final String NS_RSS1_PURL = "http://purl.org/rss/1.0/";
	
	
	private String instantiation;
	private WGDatabase db;
	private org.dom4j.Document doc;
	private List itemsList = null;
	private Element channel = null;
	private boolean isRSS1 = false;
	
	public WGResultSetImpl(WGDatabase db, org.dom4j.Document doc) {
		this.instantiation = new Long(System.currentTimeMillis()).toString();
		this.db = db;
		this.doc = doc;
		
		if (this.doc.getRootElement().getName().equalsIgnoreCase("RDF")) {
			isRSS1 = true;
		}
	}
	/**
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList()
	 */
	public List getContentList() {

		Iterator itemElements = this.getItemsList().iterator();
		List items = new ArrayList();
		int counter = 0;
		String key;
		while (itemElements.hasNext()) {
			key = instantiation + "_" + counter++;
			items.add(new WGDocumentImpl(db, key, this.getChannel(), (Element) itemElements.next()));
		}
		return items;


	}

	/**
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList(int, int)
	 */
	public List getContentList(int start, int length) {
		
		Iterator itemElements = this.getItemsList().subList(start, start + length).iterator();
		List items = new ArrayList();
		int counter = start;
		String key;
		while (itemElements.hasNext()) {
		key = instantiation + "_" + counter++;
			items.add(createDocumentImpl((Element) itemElements.next(), key));
		}
		return items;

	}
	public WGDocumentCore createDocumentImpl(Element element, String key) {
		if (isRSS1) {
			return new WGDocumentImplRSS1(db, key, this.getChannel(), element);
		}
		else {
			return new WGDocumentImpl(db, key, this.getChannel(), element);
		}
	}

	/**
	 * @see de.innovationgate.webgate.api.WGResultSetCore#results()
	 */
	public int results() {
		return this.getItemsList().size();
	}
	
	private List getItemsList() {
		
		if (this.itemsList == null) {
			
			if (isRSS1) {
				this.itemsList = this.doc.getRootElement().elements(new QName("item", Namespace.get(NS_RSS1_PURL)));
				if (this.itemsList.size() == 0) {
					this.itemsList = this.doc.getRootElement().elements(new QName("item", Namespace.get(NS_RSS1_NETSCAPE)));
				}
			}
			else {
				this.itemsList = this.doc.selectNodes("//item");
			}
		}
		return this.itemsList;
		
	}
	
	private Element getChannel() {
		
		if (this.channel == null) {
			if (isRSS1) {
				this.channel = (Element) this.doc.getRootElement().element(new QName("channel", Namespace.get(NS_RSS1_PURL)));
				if (this.channel == null) {
					this.channel = (Element) this.doc.getRootElement().element(new QName("channel", Namespace.get(NS_RSS1_NETSCAPE)));
				}
			}
			else {
				this.channel =  (Element) this.doc.selectSingleNode("//channel");
			}
		}
		return this.channel;
	}
    public boolean isReturnsKeys() {
        return false;
    }
    public List getColumnNames() {
        return null;
    }
    public boolean isLimitingResults() {
        return false;
    }

}
