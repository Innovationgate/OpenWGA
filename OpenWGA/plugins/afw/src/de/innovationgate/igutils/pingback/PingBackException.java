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
package de.innovationgate.igutils.pingback;

/**
 * thrown by PingBackClient on errors
 * @author tbinias
 *
 */
public class PingBackException extends Exception {
	
	public static final int ERROR_GENERIC = 0;
	public static final int ERROR_SOURCE_URI_DOES_NOT_EXIST = 16;
	public static final int ERROR_SOURCE_URI_HAS_NO_LINK = 17;
	public static final int ERROR_TARGET_URI_DOES_NOT_EXIST = 32;
	public static final int ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET = 33;
	public static final int ERROR_PINGBACK_ALREADY_REGISTERED = 48;
	public static final int ERROR_ACCESS_DENIED = 49;
	public static final int ERROR_UPSTREAM_SERVER_COMMUNICATION_ERROR = 50;

	private static final long serialVersionUID = 1L;
	
	private int _errorCode = ERROR_GENERIC;
	
	public PingBackException(int errorCode, String message){
		super(message);
		_errorCode = errorCode;
	}
	
	public  PingBackException(int errorCode, String message, Throwable cause) {
		super(message, cause);
	}

	public int getErrorCode() {
		return _errorCode;
	}

	public void setErrorCode(int errorCode) {
		_errorCode = errorCode;
	}
	
	
}
