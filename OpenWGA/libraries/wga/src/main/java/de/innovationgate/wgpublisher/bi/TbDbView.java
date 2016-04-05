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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

//import de.innovationgate.license.LicenseManager;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.log.WGALoggerWrapper;

// Comment ow3
// Comment RF

public abstract class TbDbView extends BiBase{
	
	private static class dbComparator implements Comparator{
		
		public int compare(Object o1, Object o2){
			if ( o1 instanceof WGDatabase && o2 instanceof WGDatabase ) {
			
				WGDatabase wg1 = (WGDatabase) o1;
				WGDatabase wg2 = (WGDatabase) o2;
								
				String title1 = wg1.getTitle();
				String title2 = wg2.getTitle();
			
				if ( title1 != null && title2 != null ) {
					return 				
					  title1.compareTo( title2 );
				}				
				return -1;
			} else throw new IllegalArgumentException();			
		}
	
		public boolean equals(Object o1){
			return false;
		}
	};
	
	public static String getViewList(javax.servlet.jsp.PageContext pageContext) throws WGException {
		String dbKey = pageContext.getRequest().getParameter("dbKey");
		//String newBI = pageContext.getRequest().getParameter("NewBI");
		if(dbKey == null) return "";
		WGDatabase database = getDB(pageContext,dbKey);
				
		//String username = database.getSessionContext().getUser();
		boolean isAuthor = (database.getSessionContext().getAccessLevel() > WGDatabase.ACCESSLEVEL_NOACCESS);
		boolean isEditor = (database.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_EDITOR);
		//boolean isDesigner = (database.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_EDITOR_DESIGNER);
		StringBuffer html = new StringBuffer("");		
		
		/*
		int majVer = de.innovationgate.wgpublisher.WGACore.WGAPUBLISHER_MAJOR_VERSION;
		int minVer = de.innovationgate.wgpublisher.WGACore.WGAPUBLISHER_MINOR_VERSION;
		*/
		
		if(isAuthor){ 
			if(isEditor){
				
			}
			
			
			//------------ Special Labels -----------------------------------
			
			de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
			java.util.PropertyResourceBundle sl = jspHelper.getLabels("toolbars");
			
			html.append("<option class=\"standardForm\" value=\"Siteexplorer\">" + sl.getString("siteexplorer") + "</options>");
			//html.append("<option class=\"standardForm\" value=\"search\">" + sl.getString("search") + "</options>");
			//if( newBI == null ) 
			//html.append("<option class=\"standardForm\" value=\"myworkspace\">My Workspace</options>");
			html.append("<option class=\"standardForm\" value=\"fileattachments\">" + sl.getString("filecontainer") + "</options>");	
				
			/*
			if( isDesigner && (( majVer==2 && minVer>2 ) || ( majVer > 2 )) ){			
				html.append("<option class=\"standardForm\" value=\"doctypes\">" + sl.getString("contenttypes") + "</options>");
				html.append("<option class=\"standardForm\" value=\"workflows\">" + sl.getString("workflows") + "</options>");
				html.append("<option class=\"standardForm\" value=\"webtml\">" + sl.getString("webtmls") + "</options>");
				html.append("<option class=\"standardForm\" value=\"cssjavascript\">" + sl.getString("cssjs") + "</options>");
				html.append("<option class=\"standardForm\" value=\"areas\">" + sl.getString("areas") + "</options>");
				html.append("<option class=\"standardForm\" value=\"languages\">" + sl.getString("languages") + "</options>");
			}
			*/
			WGFactory.getInstance().closeSessions();	
			return html.toString();
		}
		else{
			WGFactory.getInstance().closeSessions();
			return "<option>Access violation";
		}
	}
	
	
	public static String getDBList(javax.servlet.jsp.PageContext pageContext) throws WGException {
					
		String dbKey = pageContext.getRequest().getParameter("dbKey");		
		
		de.innovationgate.wgpublisher.jsputils.JspHelper helper = getHelper(pageContext);
		
		if(dbKey == null) return "<option>Missing Parameter dbKey</option>";
		if(helper == null) return "<option>Appliction was not initialized</option>";
		if(helper.getDatabaseKeys() == null) return "<option>No DBs</option>";
		if(helper.getDatabaseKeys().size()== 0) return "<option>No DBs</option>";
	
		WGDatabase database = helper.openDatabase(dbKey);		
		
		if(database == null) return "<option>Database with dbKey " + dbKey + " not found</option>";
		
		String domain = (String)database.getAttribute(de.innovationgate.wgpublisher.WGACore.DBATTRIB_DOMAIN);
		WGSessionContext sc = database.getSessionContext();
		String username = sc.getUser();
		String password = sc.getPassword();
		
		Iterator dbs = null;
		WGACore wgaCore = helper.getCore();
		List allDBs = new ArrayList(wgaCore.getContentdbs().values());
				
		Collections.sort(allDBs , new dbComparator());
		WGFactory.getInstance().closeSessions();
	
		dbs = allDBs.iterator();
	
		StringBuffer html = new StringBuffer("");
		String dbk = "";					
		
		while (dbs.hasNext()) {			
			boolean hasAccess = false;		
			database = (WGDatabase) dbs.next();
			
			if( database.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)	){
					
				database.openSession(username, password);
				
				if( database.isSessionOpen() ){			
					hasAccess = true; //wgaCore.isAuthor(database, pageContext.getRequest().getRemoteAddr(), LicenseManager.LICENSENAME_AUTHOR_BI);
				}		
			
				String dbDom = (String)database.getAttribute(de.innovationgate.wgpublisher.WGACore.DBATTRIB_DOMAIN);				
				if( dbDom != null && dbDom.equalsIgnoreCase(domain) ){
					dbk = (String) database.getAttribute(WGACore.DBATTRIB_DBKEY);
					if( database.hasFeature(WGDatabase.FEATURE_EDITABLE) 
						&& database.hasFeature(WGDatabase.FEATURE_HIERARCHICAL) 
						&& database.getRoles().contains(WGDatabase.ROLE_DESIGN)){				
						
							html.append("<option class=\"standardForm\" value=\"");
							html.append("?dbKey=");
							html.append(dbk);
						
							String sLogger = WGACore.DBATTRIB_LOGGER;
							html.append("&domain=");
							html.append(database.getAttribute(WGACore.DBATTRIB_DOMAIN));
						
							//html.append("&dominoURL=");
							//html.append(database.getCreationOptions().get(de.innovationgate.webgate.api.domino.WGDatabaseImpl.COPTION_DOMINO_URL));
							if(hasAccess) {
								html.append("&access=1");
							}
							else{							
								html.append("&access=0");			
							}
		
							html.append("\"");
							if(database.getAttribute(WGACore.DBATTRIB_DBKEY).equals(dbKey)){	
								html.append(" selected");	
							}
							html.append(">");
						
							if(hasAccess) {
								html.append(database.getTitle());
								//html.append(username);
								html.append(" (");	
								html.append(dbk);
								html.append(")");							
							}
							else{
								html.append(dbk);								
							}
							html.append("</option>");	
							html.append(LS);	
					
					}
				}
			}		
		}

		WGFactory.getInstance().closeSessions();
		return html.toString();
	}
	/*
	public static String getNamesNsfUrl(javax.servlet.jsp.PageContext pageContext) throws WGUnavailableException{
		
		WGDatabase database=null;
		Iterator dbs = null;
		
		de.innovationgate.wgpublisher.jsputils.JspHelper helper = getHelper(pageContext);
		
		if(helper==null) return "No helper";
		if(helper.getDatabaseKeys() == null) return "No DBs";
		if(helper.getDatabaseKeys().size()== 0) return "No DBs";
				
		dbs = helper.getDatabaseKeys().iterator();
		String dominoURL = null;
		if (dbs.hasNext()){			
			String dbKey = dbs.next().toString();				
			database = helper.openDatabase( dbKey );							
			while(!database.getTypeName().equals(de.innovationgate.webgate.api.domino.WGDatabaseImpl.DBTYPE)){				
				if (dbs.hasNext()){		
					dbKey = dbs.next().toString();	
					database = helper.openDatabase( dbKey );
				}
				else return "Error: No Contentstore found!\n\n";
			}
			String errorMSG = "Error: Can't get names.nsf. Please check database-option 'dominoURL' in wga.xml.\nDatabase key: " + dbKey + "\n\n";
			dominoURL = errorMSG;
			String tmp = (String) database.getCreationOptions().get(de.innovationgate.webgate.api.domino.WGDatabaseImpl.COPTION_DOMINO_URL);
			if( tmp != null && !tmp.equals("") && tmp.indexOf("//") != -1 ){
				int protocolPos = tmp.indexOf("//");
				int hostPos = tmp.indexOf("/",protocolPos+2);			
				dominoURL = tmp.substring(0,hostPos);			
				if(dominoURL == null) dominoURL = errorMSG;								
			}
			
		}
		return dominoURL;
	}
	*/
}

