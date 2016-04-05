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
 * Thrown when a query results in an error, e.g. because of invalid query syntax
 */
public class WGQueryException extends WGAPIException {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String _error;

	private String _query;

	public WGQueryException(String query, String error, Throwable cause) {
		super("Error executing query {" + query + "} : " + error, cause);
		_query = query;
		_error = error;
	}
    
    public WGQueryException(String query, String error) {
            this(query, error, null);
    }
    


	/**
	 * Returns the error that was encountered
	 */
	public String getError() {
		return _error;
	}

	/**
	 * Returns the query that was executed
	 */
	public String getQuery() {
		return _query;
	}

}

