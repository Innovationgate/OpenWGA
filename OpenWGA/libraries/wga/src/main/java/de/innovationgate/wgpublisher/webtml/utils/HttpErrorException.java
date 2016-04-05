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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import de.innovationgate.utils.WGUtils;

public class HttpErrorException extends Exception {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private int code;
	private String message;
    private String dbHint;

	public HttpErrorException(int code, String message, String dbHint) {
		super(message);
		this.code = code;
		this.message = message;
        this.dbHint = dbHint;
	}
    
    public HttpErrorException(HttpServletRequest req) {
        Integer codeInt = (Integer) req.getAttribute("javax.servlet.error.status_code");
        if (codeInt != null) {
            code = codeInt.intValue();
        }
        else {
            code = 200;
        }
        
        message = (String) req.getAttribute("javax.servlet.error.message");
        if (WGUtils.isEmpty(message)) {
            message = getDefaultMessage(code);
        }
        
        
    }

    public static String getDefaultMessage(int code) {

        switch (code) {
            case 403: return "Forbidden resource";
            case 404: return "Resource not found";
            case 500: return "Internal server error";
            default: return "HTTP error " + code;
        }
        
        
    }

    public String getLogMessage(HttpServletRequest req) {
        return "HTTP Error " + getCode() + " for client " + req.getRemoteAddr() + " calling URL \"" + WGUtils.reduce(req.getRequestURI(), 80) + "\", message: " + getMessage();
    }


	/**
	 * Returns the code.
	 * @return int
	 */
	public int getCode() {
		return code;
	}

	/**
	 * Sets the code.
	 * @param code The code to set
	 */
	public void setCode(int code) {
		this.code = code;
	}



    /**
     * @return Returns the message.
     */
    public String getMessage() {
        return message;
    }



    /**
     * @param message The message to set.
     */
    public void setMessage(String message) {
        this.message = message;
    }



    /**
     * @return Returns the dbHint.
     */
    public String getDbHint() {
        return dbHint;
    }



    /**
     * @param dbHint The dbHint to set.
     */
    public void setDbHint(String dbHint) {
        this.dbHint = dbHint;
    }

}
