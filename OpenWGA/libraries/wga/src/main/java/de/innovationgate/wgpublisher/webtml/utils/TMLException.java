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
package de.innovationgate.wgpublisher.webtml.utils;

import de.innovationgate.webgate.api.WGException;

/**
 * Thrown for problems in executing WebTML tag code
 */
public class TMLException extends WGException {
	

    private static final long serialVersionUID = 1L;
    private Throwable _rootCause;
	private boolean _cancelTag;
	
	public TMLException(String message, Throwable rootCause, boolean cancelTag) {
		super(message, rootCause);
		_rootCause = rootCause;
		_cancelTag = cancelTag;
	}
	
    public TMLException(String message) {
        this(message, null, false);
    }
    
    public TMLException(String message, boolean cancelTag) {
		this(message, null, cancelTag);
	}

	/**
	 * Gets the rootCause
	 * @return Returns a Throwable
	 */
	public Throwable getRootCause() {
		return _rootCause;
	}


	/**
	 * @see java.lang.Throwable#getMessage()
	 */
	public String getMessage() {
		if (this._rootCause != null) {
			return super.getMessage() + ": " + this._rootCause.getClass().getName() + " - " + this._rootCause.getMessage();
		}
		else {
			return super.getMessage();
		}
	}

	/**
	 * @see java.lang.Throwable#getLocalizedMessage()
	 */
	public String getLocalizedMessage() {
		if (_rootCause != null) {
			return super.getLocalizedMessage() + ": " + this._rootCause.getLocalizedMessage();
		}
		else {
			return super.getLocalizedMessage();
		}
	}

    /**
     * Returns if the WebTML tag that threw the exception cancelled its functionality
     */
    public boolean isCancelTag() {
        return _cancelTag;
    }

}

