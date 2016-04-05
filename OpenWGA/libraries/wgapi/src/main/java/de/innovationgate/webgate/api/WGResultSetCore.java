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

import java.util.List;

/**
 * Represents the result set of a database query holding contents. To save resources, the implementation of this interface should generate only those document cores for the contents demanded.
 */
public interface WGResultSetCore {
	/**
	 * Returns the number of contents in this result set.
	 * @return long
	 * @throws WGBackendException 
	 */
	public int results() throws WGBackendException;
	/**
	 * Called to retrieve a subset of content of this result set.
	 * @param start Starting index of contents to be retrieved. First index is 1.
	 * @param length Number of contents to be retrieved
	 * @return List of the requested contents. Partial or empty list, if the requested position/amount exceeded the result sets size.
	 * @throws WGAPIException 
	 */
	public List getContentList(int start, int length) throws WGAPIException;
	/**
	 * Called to retrieve a list of all content in this result set.
	 * @return List of contents. Empty list if this result set holds no content.
	 * @throws WGAPIException 
	 */
	public List getContentList() throws WGAPIException;
    
    /**
     * Determines, if this result set core only returns WGContentKey-Objects, to allow the WGAPI
     * to rely on cached document objects. If false, this result set core is expected to return
     * WGDocumentCore objects.
     */
    public boolean isReturnsKeys();
    
    /**
     * Indicates if the query type does honor result size limits, give via {@link WGDatabase#QUERYOPTION_MAXRESULTS}. False means that the result set does not limit itself, and the limit must be enforced by WGA.
     */
    public boolean isLimitingResults();
    
    /**
     * Returns the name of result row columns, if this query serves multi column results
     * Result set cores not supporting this type of results should return null here
     */
    public List getColumnNames() throws WGAPIException;

}
