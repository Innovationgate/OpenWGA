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
 * Thrown when a native expression caused an error while evaluating
 */
public class WGExpressionException extends WGAPIException {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String expression = null;
	private Throwable cause = null;
	private String nativeStackTrace = null;

	public WGExpressionException(String message, String expression, Throwable cause, String nativeStackTrace) {
		super(message);
		this.expression = expression;
		this.cause = cause;
		this.nativeStackTrace = nativeStackTrace;
	}
	
	public WGExpressionException(String message, String expression, Throwable cause) {
        this(message, expression, cause, null);
	    
	}

	public WGExpressionException(String message, String expression) {
		this(message, expression, null, null);
	}
		
	/**
	 * Source code of the expression
	 * @return Returns a String
	 */
	public String getExpression() {
		return expression;
	}

	/**
	 * May contain a throwable, that was the root error
	 * @return Throwable
	 */
	public Throwable getCause() {
		return cause;
	}

    public String getNativeStackTrace() {
        return nativeStackTrace;
    }

}



