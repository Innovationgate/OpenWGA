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
package de.innovationgate.wgpublisher.jsputils;

import java.io.IOException;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAError;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.Root;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Object to be used inside JSP scriptlets in TML modules, that can be used to retrieve WGA data.
 */
public class JspHelper {
	
	private PageContext _pageContext;
	
	/**
	 * Constructor, taking the page context.
	 */
	public JspHelper(PageContext pageContext) {
		this._pageContext = pageContext;
	}
	
	/**
	 * Returns the database keys of available Databases in WGA
	 */
	@SuppressWarnings("unchecked")
    public Collection<String> getDatabaseKeys() {
		return ((Map<String,WGDatabase>) this._pageContext.getServletContext().getAttribute(WGACore.ATTRIB_CONTENTDBS)).keySet();
	}
	
	/**
	 * Returns the dispatcher servlet of WGA
	 */
	public WGPDispatcher getDispatcher() {
		if (this._pageContext.getServletContext() != null) {
			return (de.innovationgate.wgpublisher.WGPDispatcher) this._pageContext.getServletContext().getAttribute(WGACore.ATTRIB_DISPATCHER);
		}
		else {
			return null;
		}
	}
	
	/**
	 * Returns the WGA core object
	 */
	public WGACore getCore() {
		if (this._pageContext.getServletContext() != null) {
			return (de.innovationgate.wgpublisher.WGACore) this._pageContext.getServletContext().getAttribute(WGACore.ATTRIB_CORE);
		}
		else {
			return null;
		}		
	}
	
	/**
	 * Opens the database with the given key. 
	 * The login used is an existing login for the database's domain or anonymous if none yet exists.
	 * @param key The database key
	 * @return The database object
	 * @throws WGUnavailableException
	 */
	public WGDatabase openDatabase(String key) throws WGException {
		return getCore().openContentDB(key.toLowerCase(), (HttpServletRequest) _pageContext.getRequest());
	}
    
    /**
     * Opens the given database object with the domain login of the current user, or anonymous if there is none.
     * @param db The database to open
     * @return The WGAPI database object
     * @throws WGException
     */
    public WGDatabase openDatabase(WGDatabase db) throws WGException {
        return getCore().openContentDB(db, (HttpServletRequest) _pageContext.getRequest(), false);
    }
    
    /**
     * Fetches a database by the given key, without opening it. There will be no session open unless some
     * earlier code has opened the database.
     * @param key The database key
     * @return WGDatabase
     * @throws WGException
     */
    public WGDatabase getDatabase(String key) throws WGException {
        return (WGDatabase) getCore().getContentdbs().get(key.toLowerCase());
    }
	
	/**
	 * Returns the complete URL to this request
	 */
	public String getRequestPath() {
		return (String)  WGPDispatcher.getCompleteRequestURL((javax.servlet.http.HttpServletRequest) this._pageContext.getRequest());
	}
	
	/**
	 * Returns the path to the WGAPublisher on this server, excluding any additional path information. 
	 */
	public String getPublisherPath() {
		
		WGPDispatcher disp = this.getDispatcher();
		
		javax.servlet.http.HttpServletRequest sr = (javax.servlet.http.HttpServletRequest) this._pageContext.getRequest();
		
		String path = WGPDispatcher.getPublisherURL(sr, true);
		
		return path;

	}
	
	/**
	 * Retrieves a TMLContext object for the context of a specified WebTML tag.
	 * @param id The ID of the WebTML tag.
	 */
	public TMLContext getContextFromTag(String id) {
		
		@SuppressWarnings("unchecked")
        Map<String,BaseTagStatus> tags = (Map<String,BaseTagStatus>) _pageContext.getRequest().getAttribute("TagIds"); // Get a map of tags, keyed by their tag ids
		BaseTagStatus tag = tags.get(id);
		return tag.tmlContext;
		
	}
	
	/**
	 * Returns a TMLContext object for the main context of the current request.
	 */
	public TMLContext getMainContext() {
		Root.Status rootTag = (Root.Status) _pageContext.getRequest().getAttribute(WGACore.ATTRIB_ROOT_TAG);
		return rootTag.tmlContext;
	}
	
	/**
	 * Returns labels for a given language name
	 * @param baseName The language name
	 * @return Labels of that language as PropertyResourceBundle
	 */
	public PropertyResourceBundle getLabels(String baseName){
		PropertyResourceBundle l = null;
		try{
			l = (PropertyResourceBundle)ResourceBundle.getBundle("de.innovationgate.wgpublisher.labels." + baseName, _pageContext.getRequest().getLocale(),this.getClass().getClassLoader());
		}
		catch(MissingResourceException e){	
			System.out.println( e.getMessage() );
		}
		
		return l;
	}
	
	public boolean isBrowserInterface() {
        
        if (!getCore().isAuthoringPort(_pageContext.getRequest().getLocalPort())) {
            return false;
        }
        
	    Boolean bi = (Boolean) this._pageContext.getSession().getAttribute(WGACore.ATTRIB_BROWSERINTERFACE);
		if (bi == null || bi.booleanValue() == false) {
			return false;
		}
		
        WGContent content = (WGContent) _pageContext.getRequest().getAttribute(WGACore.ATTRIB_MAINCONTEXT);
        if (content != null) {
            WGDatabase db = content.getDatabase();
            String startPageEnabled = (String) db.getAttribute(WGACore.DBATTRIB_STARTPAGE);
            if (startPageEnabled != null && startPageEnabled.equals("false")) {
                return false;
            }
        }
        
        return true;
	}
	
	public boolean isAdminLoggedIn() {
	    
	    return getDispatcher().isAdminLoggedIn((HttpServletRequest) _pageContext.getRequest());
	    
	}
    
    public WGAError getWGAError() {
        return (WGAError) _pageContext.getRequest().getAttribute(de.innovationgate.wgpublisher.WGACore.ATTRIB_WGAERROR);
    }
    
    public String shortDateFormat(Date date) {
        return DateFormat.getDateInstance(DateFormat.SHORT, _pageContext.getRequest().getLocale()).format(date);
    }
		
    public String fullDateFormat(Date date) {
        return DateFormat.getDateInstance(DateFormat.FULL, _pageContext.getRequest().getLocale()).format(date);
    }		

    public String longDateFormat(Date date) {
        return DateFormat.getDateInstance(DateFormat.LONG, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String mediumDateFormat(Date date) {
        return DateFormat.getDateInstance(DateFormat.MEDIUM, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String shortDateTimeFormat(Date date) {
        return DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String fullTimeFormat(Date date) {
        return DateFormat.getTimeInstance(DateFormat.FULL, _pageContext.getRequest().getLocale()).format(date);
    }       

    public String longTimeFormat(Date date) {
        return DateFormat.getTimeInstance(DateFormat.LONG, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String mediumTimeFormat(Date date) {
        return DateFormat.getTimeInstance(DateFormat.MEDIUM, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String shortTimeFormat(Date date) {
        return DateFormat.getTimeInstance(DateFormat.SHORT, _pageContext.getRequest().getLocale()).format(date);
    }
        
    public String fullDateTimeFormat(Date date) {
        return DateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.FULL, _pageContext.getRequest().getLocale()).format(date);
    }       

    public String longDateTimeFormat(Date date) {
        return DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String mediumDateTimeFormat(Date date) {
        return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM, _pageContext.getRequest().getLocale()).format(date);
    }
    
    public String numberFormat(Number num) {
        return NumberFormat.getNumberInstance(_pageContext.getRequest().getLocale()).format(num.doubleValue());
    }
    
    public void setContentType(String contentType) {
        String cType = contentType;
        _pageContext.getResponse().setContentType(cType);
    }
    
    /**
     * Writes content information script block based on the request path.
     * This is meant to be used on WGA error pages to provide minimal information about the requested content to WGA Content Manager.
     * @throws IOException
     */
    public String getContentInfoScript() throws IOException {
        
        WGPRequestPath requestPath = (WGPRequestPath) _pageContext.getRequest().getAttribute(WGACore.ATTRIB_REQUESTPATH);
        if (requestPath == null) {
            return "";
        }
       
        String quote = "\"";
        String dbKeyStr = quote + requestPath.getDatabaseKey() + quote;
        String contentKeyStr = "null";
        String structKeyStr = "null";
        String titleStr = "null";
       
        // Try to retrieve content key to fill content specific information
        try {
            WGContent content = (WGContent) _pageContext.getRequest().getAttribute(WGACore.ATTRIB_MAINCONTEXT);
            if (content != null) {
                dbKeyStr = quote + content.getDatabase().getDbReference() + quote;
                contentKeyStr = quote + content.getContentKey().toString() + quote;
                structKeyStr = quote + content.getContentKey().getStructKey() + quote;
                titleStr = quote + content.getTitle() + quote;
            }
        }
        catch (Exception e) {
            // Fail silently. Better to display info of the originating error than to show info of internal error in error handling
        }
        
        StringBuffer writer = new StringBuffer();
        writer.append("<script>");
        writer.append("WGA = {");
        writer.append("contentinfo: {");
        writer.append("dbkey:" + dbKeyStr + ",");
        writer.append("contentkey:" + contentKeyStr + ",");
        writer.append("structkey:" + structKeyStr + ",");
        writer.append("title: " + titleStr);
        writer.append("}");
        writer.append("}");
        
        writer.append("\nwindow.parent.CM && window.parent.CM.pageLoaded(WGA.contentinfo);");
        
        writer.append("</script>");
        
        return writer.toString();
    }

    /**
     * Serves the JSP page context again
     */
    public PageContext getPageContext() {
        return _pageContext;
    }
}

