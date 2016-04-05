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
 * Root class for all exceptions thrown by the WGAPI.
 */
public class WGAPIException extends WGException {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _errorCode = null;
    private WGDocumentKey _causeDocumentKey = null;

	/**
	 * Returns An error code identifying the error type. Usable to lookup localized error messages or react on individual error types.
	 */
	public String getErrorCode() {
        return _errorCode;
    }

    public WGAPIException(String msg) {
		super(msg);
	}
    
    public WGAPIException(Throwable cause) {
        super(cause);
    }
    
    public WGAPIException(String msg, Throwable cause) {
        super(msg, cause);
    }
    
    public WGAPIException(String msg, WGDocumentKey causeDocument) {
        super(msg);
        _causeDocumentKey = causeDocument;
    }

    
    public WGAPIException(String msg, String code) {
        super(msg);
        _errorCode = code;
    }
    
    public WGAPIException(String msg, String code, WGDocumentKey causeDocument) {
        super(msg);
        _errorCode = code;
        _causeDocumentKey = causeDocument;
    }

    public WGDocumentKey getCauseDocumentKey() {
        return _causeDocumentKey;
    }


}

