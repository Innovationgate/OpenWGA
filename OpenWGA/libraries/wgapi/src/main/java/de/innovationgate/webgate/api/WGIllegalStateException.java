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
 * checked exception corresponding to IllegalStateException. Thrown whenever an operation must fail because
 * some involved resource is in wrong state. 
 *
 */
public class WGIllegalStateException extends WGAPIException {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final String ERRORCODE_DOCTYPE_READONLY = WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY;
    public static final String ERRORCODE_SCHEMA_DOCUMENT_IN_USE = WGIllegalStateException.ERRORCODE_SCHEMA_DOCUMENT_IN_USE;
    public static final String ERRORCODE_CONTENT_DRAFT_EXISTS = "draft-exists";

    public WGIllegalStateException(String msg) {
        super(msg);
    }
    
    public WGIllegalStateException(String msg, String code) {
        super(msg, code);
    }

    public WGIllegalStateException(String msg, Throwable cause) {
        super(msg, cause);
    }

}
