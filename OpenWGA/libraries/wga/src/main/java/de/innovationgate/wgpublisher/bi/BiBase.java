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
package de.innovationgate.wgpublisher.bi;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Vector;

import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;

public abstract class BiBase {

	public static final String LS = System.getProperty("line.separator");
	public static final String URL_EDITOR_TYPE_RTF_LINK = "RTF_LINK";
	public static final String URL_EDITOR_TYPE_RTF_IMAGE = "RTF_IMAGE";
	public static final String URL_EDITOR_TYPE_TML_IMAGE = "TML_IMAGE";
	public static final String URL_EDITOR_TYPE_TML_ITEM = "TML_ITEM";
	public static final String URL_EDITOR_TYPE_VIRTUAL_LINK = "VIRTUAL_LINK";
	
	
	private static de.innovationgate.wgpublisher.jsputils.JspHelper helper;
	
	public static String encode(String sourceText, String encoding) throws URIException{		
		String result = URIUtil.encodeWithinQuery(sourceText, encoding);
		return result; 		
	}
	/*
	private static class ContentComparatorByStatus implements Comparator{
		
		public int compare(Object o1, Object o2){
			if ( o1 instanceof WGContent && o2 instanceof WGContent ){
				
				WGContent content1 = (WGContent) o1;
				WGContent content2 = (WGContent) o2;

				String status1 = content1.getStatus();
				String status2 = content2.getStatus();
				
				if ( status1 != null && status2 != null ) {
					if(status1.equals(status2)){
						return -1; //if equal -> no order
					}
					else{
						if(status1.equals("w")) return -1;
						if(status2.equals("w")) return 1;
						if(status1.equals("a")) return 1;
						if(status2.equals("a")) return -1;
						if(status1.equals("g")) return -1;
						if(status2.equals("g")) return 1;						
					}
					return -1;
				}
				else {
					throw new IllegalArgumentException();
				}
			} else throw new IllegalArgumentException();
		}
		
		public boolean equals(Object o1){
			return false;
		}
	};
	*/
	public static de.innovationgate.wgpublisher.jsputils.JspHelper getHelper(javax.servlet.jsp.PageContext pageContext) {
		
		//pageContext.getSession().setAttribute(de.innovationgate.wgpublisher.WGPDispatcher.ATTRIB_BROWSERINTERFACE,new Boolean(true));
			
		Object oTmp = pageContext.getRequest().getAttribute("usedRequest");
						
		if (oTmp==null) {
			pageContext.getRequest().setAttribute("usedRequest","yes");
			helper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
			
		}
		
		return helper;
	}
	
	public static String getBrowserLanguageKey(javax.servlet.jsp.PageContext pageContext){		
		Enumeration enumLocales = pageContext.getRequest().getLocales();
		Vector vecLocales = new Vector();
		vecLocales.clear();
		String language = null;
		
		while (enumLocales.hasMoreElements()) vecLocales.add((Locale) enumLocales.nextElement());
		for (int i = 0; i < vecLocales.size(); i++) {
				Locale currentLocale = (Locale) vecLocales.get(i);
				language = currentLocale.getLanguage().toUpperCase();
				if( language.equalsIgnoreCase("DE") || language.equalsIgnoreCase("EN") ) break;
		}
		if( language == null || language.equals("") ) language = "EN";
		return language;	
	}
	
	public static WGDatabase getDB(javax.servlet.jsp.PageContext pageContext, String dbAlias) throws WGException {
		de.innovationgate.wgpublisher.jsputils.JspHelper helper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
		return helper.openDatabase(dbAlias);
	}
			
	public static String getUsername(javax.servlet.jsp.PageContext pageContext, String dbKey, String format) throws WGException {
		
		Map hm = WGACore.getSessionLogins(pageContext.getSession());
		
		WGDatabase db = getDB(pageContext, dbKey);
		
		if(hm==null) return "";
		if(hm.isEmpty()) return "";
		if(dbKey.equals("")) return "";							

			
		String fullUsername = db.getSessionContext().getUser();
		
		WGFactory.getInstance().closeSessions();
		
		if(format!=null){
			if(format.equals("")){
				return fullUsername;
			}
			if(format.equals("CN")){
				int pos2 = fullUsername.indexOf("/");
				if(pos2 != -1){
					return fullUsername.substring(3,pos2);
				}
				else{
					return	fullUsername;
				}
			}
			return fullUsername;
		}
		else{
			return fullUsername;
		}
		
	}
	
	public static String getPassword(javax.servlet.jsp.PageContext pageContext, String dbKey) throws WGException  {
		
		Map hm = WGACore.getSessionLogins(pageContext.getSession());
		WGDatabase db = getDB(pageContext, dbKey);	
		
		String domain = (String) db.getAttribute(WGACore.DBATTRIB_DOMAIN);
		if(hm==null) return "";
		if(hm.isEmpty()) return "";
					
		DBLoginInfo li = (DBLoginInfo) hm.get(domain);
		return li.getPassword(); 
		
	}
	
}

















