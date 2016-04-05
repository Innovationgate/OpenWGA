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
package de.innovationgate.wgpublisher.webtml.actions;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLInvalidActionLinkException;

public class TMLActionLink {

	private String _sequenceId = null;

	/**
	 * Sets the sequence.
	 * @param sequence The sequence to set
	 */
	public void setSequenceId(String sequence) {
		this._sequenceId = sequence;
	}

	/**
	 * Returns the sequence.
	 * @return String
	 */
	public String getSequenceId() {
		return _sequenceId;
	}

	public static final String LINK_DIVIDER = "||";

	private Integer _actionKey = null;

	private String _contextPath = null;
	private String _dbKey = null;
	private String _portletKey = null;
	
	private Map<String,Object> _namedParams = new HashMap<String,Object>();
	private List<Object> _unnamedParams = new ArrayList<Object>();
	
	private String _defaultAction = null;
    
    // sessionID this actionLink was rendered for
    // this is to ensure an action can only be called out of the session it was rendered in
    private String _sessionID;
    
    // mode - added within F00004242
    private String _mode = null;
    public static final String MODE_AJAX_NO_PORTLET_REFRESH = "AjaxNoPortletRefresh";
    
    // portletmode to set after action is done F00004292
    private String _portletmode = null;
    
    // absolute portlet context to set after action is done
    private String _portletContextPath = null;
    private String _webtmlScope = null;
    private DesignResourceReference _definitionModule = null;

    public static final String EMPTY_PARAM = "##NULL##";
	
	public TMLActionLink (HttpSession session) {
	    if (session != null) {
	        _sessionID = session.getId();
	    }
	}

	public static TMLActionLink read(HttpServletRequest req, String link, WGACore core) throws IOException, TMLInvalidActionLinkException {
	    
	    // Strip of additional action link data, necessary only for the client side
	    List<String> linkElements = WGUtils.deserializeCollection(link, "/");
	    int slashPos = link.lastIndexOf("/");
	    if (slashPos != -1) {
	        link = linkElements.get(linkElements.size() - 1);
	    }
	    
	    // Way for clientside JavaScript to trigger portlet reloads without an action link
	    if (link.equals("$refresh")) {
	        TMLActionLink actionLink = new TMLActionLink(req.getSession());
	        actionLink._defaultAction = "refresh";
	        actionLink._sessionID = req.getSession().getId();
	        actionLink._contextPath = null;
	        if (linkElements.size() >= 2) {
	            actionLink._portletKey = linkElements.get(linkElements.size() - 2);
	        }
	        return actionLink;
	    }
	    
        //decrypt, unzip and read action link
	    byte[] decrypted;
        try {
            decrypted = core.getSymmetricEncryptionEngine().decryptBase64Web(link);
        }
        catch (GeneralSecurityException e) {
            return null;
        }
        String decodedLink = WGUtils.unzipString(decrypted);
        if (decodedLink == null) {
            return null;
        }
        
        try {
            return (TMLActionLink) core.getLibraryXStream().fromXML(decodedLink);
        }
        catch (Exception e) {
            return null;
        }
		
	}
	/**
	 * Returns the actionKey.
	 * @return Integer
	 */
	public Integer getActionKey() {
		return _actionKey;
	}
	
	public Integer getActionKeyInteger() {
		return Integer.valueOf(_actionKey);
	}

	/**
	 * Sets the actionKey.
	 * @param actionKey The actionKey to set
	 */
	public void setActionKey(Integer actionKey) {
		this._actionKey = actionKey;
	}

	/**
	 * Returns the contentKey.
	 * @return String
	 */
	public String getContextPath() {
		return _contextPath;
	}

	/**
	 * Sets the contentKey.
	 * @param contentKey The contentKey to set
	 */
	public void setContextPath(String contentKey) {
		this._contextPath = contentKey;
	}

	

	public String getEncodedString(WGACore core) {

	    // Prevent serialisation for invalid or insecur action links
	    if ((_actionKey == null && _defaultAction == null) || _sequenceId == null || _sessionID == null) {
        	return null;
        }

	    // To XML
        String encodedString = core.getLibraryXStream().toXML(this);
                    
        // Zip String
        byte[] zipped = WGUtils.zipString(encodedString.toString());
        if (zipped != null) {
    
            // Encrypt
            try {
                return core.getSymmetricEncryptionEngine().encryptBase64Web(zipped);
            }
            catch (Exception e) {
                core.getLog().error("Unable to encrypt actionlink." , e);
                return null;
            }
        }
        else {
           core.getLog().error("unable to zip actionlink");
           return null;
        }
        
	}

	/**
	 * Returns the dbKey.
	 * @return String
	 */
	public String getDbKey() {
		return _dbKey;
	}

	/**
	 * Sets the dbKey.
	 * @param dbKey The dbKey to set
	 */
	public void setDbKey(String dbKey) {
		this._dbKey = dbKey;
	}

	/**
	 * Returns the portletKey.
	 * @return String
	 */
	public String getPortletKey() {
		return _portletKey;
	}

	/**
	 * Sets the portletKey.
	 * @param portletKey The portletKey to set
	 */
	public void setPortletKey(String portletKey) {
		this._portletKey = portletKey;
	}

	public boolean isDefaultAction() {
		return (_defaultAction != null);
	}

	/**
	 * @return
	 */
	public String getDefaultAction() {
		return _defaultAction;
	}

	/**
	 * @param string
	 */
	public void setDefaultAction(String string) {
		_defaultAction = string;
	}

    public String getSessionID() {
        return _sessionID;
    }

    public String getMode() {
        return _mode;
    }

    public void setMode(String mode) {
        _mode = mode;
    }

	public String getPortletmode() {
		return _portletmode;
	}

	public void setPortletmode(String portletmode) {
		_portletmode = portletmode;
	}
	
	public String getPortletContextPath() {
		return _portletContextPath;
	}

	public void setPortletContextPath(TMLContext baseContext, String contextpath) throws WGAPIException {
	    if (TMLPortlet.PORTLETCONTEXT_NONE.equals(contextpath)) {
	        _portletContextPath = contextpath;
	    }
	    else if (baseContext.getpath().equals(contextpath)) {
	        _portletContextPath = baseContext.getpath() +  "/" + contextpath;
	    }
	    else {
	        _portletContextPath = contextpath;
	    }
		
	}
	
	public void setPortletContextPath(String contextpath) throws WGAPIException {
	    _portletContextPath = contextpath;
	}

	public String getJavascriptLink(WGACore core, String formID, Version versionCompliance) throws UnsupportedEncodingException {
	    
	    if (formID == null) {
	        formID = EMPTY_PARAM;
	    }
	    
	    String versionComplianceStr = EMPTY_PARAM;
	    if (versionCompliance != null) {
	        versionComplianceStr = versionCompliance.getMajorVersion() + "." + versionCompliance.getMinorVersion();
	    }
	    
	    return versionComplianceStr + "/" + formID + "/" + _portletKey + "/" + getEncodedString(core);
	    
	}

    public String getWebtmlScope() {
        return _webtmlScope;
    }

    public void setWebtmlScope(String webtmlScope) {
        _webtmlScope = webtmlScope;
    }

    public Map<String, Object> getNamedParams() {
        return _namedParams;
    }

    public List<Object> getUnnamedParams() {
        return _unnamedParams;
    }

    public DesignResourceReference getDefinitionModule() {
        return _definitionModule;
    }

    public void setDefinitionModule(DesignResourceReference definitionModule) {
        _definitionModule = definitionModule;
    }
}

