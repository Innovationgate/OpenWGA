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

import java.util.Iterator;

import de.innovationgate.utils.SkippingIterator;


/**
 * Generic interface for result sets that are returned by the WGAPI.
 * The WGResultSet implementation should:
 * - Filter out contents based on the fields "validfrom" and "validto" if the query
 *   option "enhanced" is either omitted or set to true
 * - Filter out contents based on the field "visible", if the query option "enhanced"
 *   is either omitted or set to true
 */
public interface WGResultSet {
	
	/**
	 * Number of results that are in this results set.
	 * @throws WGBackendException 
     */
	public int results() throws WGBackendException;
	
	
	/**
	 * Indicates if the result set contains any results
	 * @throws WGBackendException
	 */
	public boolean hasResults() throws WGBackendException;
	
	/**
	 * Called to retrieve a subset of content of this result set.
	 * @param start Starting index of contents to be retrieved. First index is 1.
	 * @param length Number of contents to be retrieved
	 * @return List of the requested contents. Partial or empty list, if the requested position/amount exceeded the result sets size.
	 * @throws WGAPIException 
	 */
	public WGContentList getContentList(int start, int length) throws WGAPIException;
	/**
	 * Called to retrieve a list of all content in this result set.
	 * @return List of contents. Empty list if this result set holds no content.
	 * @throws WGAPIException 
	 */
	public WGContentList getContentList() throws WGAPIException;
	
	
	/**
	 * Returns an iterator iterating over the results
	 * @throws WGAPIException
	 */
	public SkippingIterator<WGContent> getResultIterator() throws WGAPIException;
	
	/**
	 * Call to limit the results to return to the given count. The resultset will only return results to this number.
	 * If the available results are fewer than this number this call has no effect.
	 * @param limit Maximum number of results to return.
	 * @throws WGBackendException 
	 */
	public void limitResults(int limit) throws WGBackendException;
	
	/**
	 * This method may return the execution time of the backend query in milliseconds. Returns 0 if not supported.
	 */
	public long getExecutionTime();
	
	/**
     * Indicates if the query type does honor result size limits, give via {@link WGDatabase#QUERYOPTION_MAXRESULTS}. False means that the result set does not limit itself, and the limit must be enforced by WGA (for example via {@link #limitResults(int)}
     */
	public boolean isLimitingResultsInBackend();

}
