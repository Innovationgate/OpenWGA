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
package de.innovationgate.webgate.api;

/**
 * Thrown whenever a user tries to start an unauthorized action (e.g. create a content document).
 */
public class WGAuthorisationException extends WGAPIException {
    
    
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final String ERRORCODE_OP_DENIED_BY_AREA = "area-settings-denies-edit";
    public static final String ERRORCODE_OP_DENIED_BY_PAGE = "struct-settings-denies-edit";
    public static final String ERRORCODE_OP_DENIED_BY_CONTENTTYPE = "pagetype-settings-denies-edit";
    public static final String ERRORCODE_OP_DENIED_BY_LANGUAGE = "language-settings-denies-edit";
    public static final String ERRORCODE_OP_DENIED_BY_INVISIBLE_CONTENT = "content-with-no-readaccess";
    public static final String ERRORCODE_OP_DENIED_BY_PAGERIGHTSFILTER = "pagerightsfilter-denies-edit";
    
    public static final String ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS = "you-are-no-author";
    public static final String ERRORCODE_OP_NEEDS_WORKFLOW_APPROVER_RIGHTS = "you-are-no-approver";
    public static final String ERRORCODE_OP_NEEDS_WORKFLOW_ADMIN_RIGHTS = "you-are-no-workflow-admin";
    public static final String ERRORCODE_OP_NEEDS_DESIGNER_RIGHTS = "you-are-no-designer";
    public static final String ERRORCODE_OP_NEEDS_CHIEF_EDITOR_RIGHTS = "you-are-no-chief-editor";
    public static final String ERRORCODE_OP_NEEDS_MOVEPAGE_RIGHTS = "move-page-denied";
    
    public static final String ERRORCODE_OP_NEEDS_EDITOR_LEVEL = "database.you-need-to-be-editor";
    public static final String ERRORCODE_OP_NEEDS_AUTHOR_LEVEL = "database.you-need-to-be-author";
    public static final String ERRORCODE_OP_NEEDS_MANAGER_LEVEL = "database.you-need-to-be-manager";
    private WGDocument _causeDocument = null;
    
    
	
	/**
     * Constructor for WGAuthorisationException.
     * @param msg
     */ 
    public WGAuthorisationException(String msg, String code) {
        super(msg,code);
    }
	
	public WGAuthorisationException(String msg, String code, WGDocument cause) {
        super(msg, code, cause.getDocumentKeyObj());
        _causeDocument = cause;
    }
	

	/**
	 * @deprecated Only for backward compatibility. Use {@link #getCauseDocumentKey()} instead.
	 */
	public WGStructEntry getEntry() {
	    if (_causeDocument instanceof WGStructEntry) {
	        return (WGStructEntry) _causeDocument;
	    }
	    else {
	        return null;
	    }
	}



    /**
     * Returns the document (if any) which denies the requested operation
     * @deprecated Better use {@link #getCauseDocumentKey()}, because the cause document may not be visible for the current user
     */
    public WGDocument getCauseDocument() {
    	return _causeDocument;
    }

    public WGAuthorisationException(String msg, WGDocument cause) {
    	super(msg, cause.getDocumentKeyObj());
    }

    /**
     * Constructor for WGAuthorisationException.
     * @param msg
     */	
    public WGAuthorisationException(String msg) {
    	super(msg);
    }

}
