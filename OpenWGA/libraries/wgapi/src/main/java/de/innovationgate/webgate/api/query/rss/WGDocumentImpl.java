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

import java.io.InputStream;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dom4j.Element;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;

public class WGDocumentImpl extends de.innovationgate.webgate.api.fake.WGFakeDocument {
	
	private String key;
	private Element item;
	private Element channel;
	private DateFormat DATE_FORMATTER = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z");
	
	public WGDocumentImpl(WGDatabase db, String key, Element channel, Element item) {
		super(db, WGDocument.TYPE_CONTENT);
		this.key = key;
		this.channel = channel;
		this.item = item;
	}

	public int getType() {
		return WGDocument.TYPE_CONTENT;
	}

	public Object getFastAccessKey() {
		return null;
	}

	public boolean isDeleted() {
		return false;
	}

	public boolean isTemporary() {
		return true;
	}

	public boolean hasItem(String strName) {
		
		if (strName.equalsIgnoreCase("description")) {
			return true;
		}
		else {
			return false;
		}
	}

	public Object getItemValue(String strName) {

		if (strName.equals("description")) {
			return this.item.elementText("description");
		}
		else {
			return "";
		}

	}

	public Object getMetaData(String type) throws WGSystemException, WGIllegalArgumentException, WGBackendException {

		if (type.equals(WGContent.META_TITLE)) {
			return this.item.elementText("title");
		}
		else if (type.equals(WGContent.META_VIRTUAL_LINK)) {
			return this.item.elementText("link");
		}
		else if (type.equals(WGContent.META_VIRTUAL_LINK_TYPE)) {
			return WGContent.VIRTUALLINKTYPE_EXTERNAL;
		}
		else if (type.equals(WGDocument.META_CREATED) || type.equals(WGDocument.META_LASTMODIFIED)) {
			try {
				return this.DATE_FORMATTER.parse(this.item.elementText("pubDate"));
			}
			catch (ParseException e) {
				return new Date();
			}
		}
		else if (type.equals(WGContent.META_AUTHOR)) {
			return this.item.elementText("author");
		}
		else if (type.equals(WGContent.META_STRUCTENTRY)) {
			String key = this.item.elementText("guid");
			if (key == null || key.equals("")) {
				key = this.key;
			}
			return key;
		}
		else if (type.equals(WGContent.META_KEYWORDS)) {
			return this.item.elements("category");
		}
		else if (type.equals(WGContent.META_LANGUAGE)) {
			String lang =  this.channel.elementText("language");
			if (lang == null || lang.equals("")) {
				lang = "uni";
			}
			else if (lang.indexOf("-") != -1) {
				lang = lang.substring(0, lang.indexOf("-"));
			}
			return lang;
		}
		else {
			return super.getMetaData(type); 
		}

	}

	public List getFileNames() {
		return new ArrayList();
	}

	public InputStream getFileData(String strFile) {
		return null;
	}

	public int getFileSize(String strFile) {
		return 0;
	}

	public Date getCreated() {
		try {
			return this.DATE_FORMATTER.parse(this.item.elementText("pubDate"));
		}
		catch (ParseException e) {
			return new Date();
		}
	}

	public Date getLastModified() {
		return null;
	}

	public boolean setItemValue(String strName, Object value) {
		return false;
	}

	public boolean setMetaData(String strName, Object value) {
		return false;
	}

	public WGDatabaseRevision save(java.util.Date lastModified) {
		return null;
	}

	public Object evaluateExpression(String expression) throws WGExpressionException {
		return null;
	}

	public void setWGDocument(WGDocument doc) {}

	public List getItemNames() {
		return null;
	}

	public boolean removeItem(String Name) {
		return false;
	}

}
