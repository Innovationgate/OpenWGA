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
package de.innovationgate.wgpublisher.expressions.tmlscript;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.utils.JavaScriptEncodingFormatter;

/**
 * Java Representation of an explicitly thrown TMLScript error. Used to transport these exceptions and their data to the outside (b.c. the native class
 * JavaScriptException is not known outside the isolated class loader).
 */
public class TMLScriptException extends WGException {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private int lineNumber;
    private int columnNumber;
    private String lineSource;
    private Object errorValue;
    private Throwable jsException;

	public TMLScriptException(String string) {
		super(string);
	}
	
	public TMLScriptException(String string, Throwable cause) {
		super(string, cause);
	}
    
    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getLineSource() {
        return lineSource;
    }

    public void setLineSource(String lineSource) {
        this.lineSource = lineSource;
    }

    public int getColumnNumber() {
        return columnNumber;
    }

    public void setColumnNumber(int columnNumber) {
        this.columnNumber = columnNumber;
    }

    public Object getErrorValue() {
        return errorValue;
    }

    public void setErrorValue(Object errorValue) {
        this.errorValue = errorValue;
    }

    public Throwable getJsException() {
        return jsException;
    }

    public void setJsException(Throwable jsException) {
        this.jsException = jsException;
    }
	
}
