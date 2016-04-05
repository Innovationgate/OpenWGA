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
package de.innovationgate.webgate.api.templates;

import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.LinkedMap;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGSessionContext;

/**
 * Template that can be used to quickly implement query-only datasources.
 * 
 * Mandatory methods to implement:
 * <ul>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#find find}</li>
 * 
 * Optional methods to overwrite:
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#init init}</li>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#login login}</li>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#login logout}</li>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#destroy destroy}</li>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#getTitle getTitle}</li>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#getServerName getServerName}</li>
 * <li>{@link de.innovationgate.webgate.api.templates.QueryableSource#getTypeName getTypeName}</li>
 * </ul>
 * 
 */
public abstract class QueryableSource extends SimpleContentSource {
	
	/**
     * A temporary key for all results of queryable source
	 */
	public static class QueryableSourceKey  implements TemporaryKey, Comparable {
		private Integer _number;

		/**
		 * Constructor taking an arbitrary number that must be unique in the current result set.
		 * @param number
		 */
		public QueryableSourceKey(int number) {
			_number = new Integer(number);
		}

		/* (Kein Javadoc)
		 * @see java.lang.Comparable#compareTo(java.lang.Object)
		 */
		public int compareTo(Object o) {
			QueryableSourceKey otherKey = (QueryableSourceKey) o;
			return getNumber().compareTo(otherKey.getNumber());
		}
		/**
		 * Returns the arbitrary number of this key
		 */
		public Integer getNumber() {
			return _number;
		}

		/* (Kein Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object obj) {
			
			if (!(obj instanceof QueryableSourceKey)) {
				return false;
			}
			
			QueryableSourceKey otherKey = (QueryableSourceKey) obj;
			return getNumber().equals(otherKey.getNumber());
			
			
		}

		/* (Kein Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		public int hashCode() {
			return getNumber().hashCode();
		}

		/* (Kein Javadoc)
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			return super.toString();
		}

	}
	
	private WGDatabase _db;
	private String _path;

	protected WGSessionContext getSessionContext() {
		return getDatabase().getSessionContext();
	}
	
	protected String getPath() {
		return _path;
	}
	
	protected WGDatabase getDatabase() {
		return _db;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#login(java.lang.String, java.lang.String)
	 */
	public int login(String user, String pwd) throws WGAPIException {
		return WGDatabase.ACCESSLEVEL_READER;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#logout()
	 */
	public void logout() throws WGAPIException {}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTitle()
	 */
	public String getTitle() throws WGBackendException {
		return "Queryable source: " + getClass().getName() + " on path " + _path;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
	 */
	public String getTypeName() {
		return "custom/queryablesource";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getCreated()
	 */
	public Date getCreated() throws WGBackendException {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#convertToKey(java.lang.String, java.lang.String)
	 */
	public Object convertToKey(String key, String folder) throws WGAPIException {
		return key;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getContent(java.lang.String, java.lang.Object)
	 */
	public Object getContent(String folder, Object key) throws WGAPIException {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#insertContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean insertContent(String folder, Object key, Object bean) throws WGAPIException {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#updateContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean updateContent(String folder, Object key, Object bean) throws WGAPIException {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#createContent(java.lang.String)
	 */
	public Object createContent(String folder) throws WGAPIException {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#find(java.lang.String, java.lang.String, java.util.Map)
	 */
	public Map find(String type, String query, Map parameters) throws WGAPIException {
		List results = find(query);
		if (results == null) {
			return null;
		}
		LinkedMap resultMap = new LinkedMap();
		int keyNr = 0;
		Iterator resultsIt = results.iterator();
		Object result;
		while (resultsIt.hasNext()) {
			result = resultsIt.next();
			resultMap.put(new QueryableSourceKey(++keyNr), result);
		}
		return resultMap;
		
	}
	
	/**
	 * Abstract method to implement query behaviour.
	 * @param query The specified query.
	 * @return A list of JavaBeans or java.util.Map objects that contain content data
	 */
	public abstract List find(String query) throws WGAPIException;

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getLastModified()
	 */
	public Date getLastModified() throws WGAPIException {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#removeContent(java.lang.String, java.lang.Object)
	 */
	public void removeContent(String folder, Object key) throws WGAPIException {}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#init(de.innovationgate.webgate.api.WGDatabase, java.lang.String)
	 */
	public ContentSourceSpecs init(WGDatabase db, String path) throws WGAPIException {
		_db = db;
		_path = path;
		
		ContentSourceSpecs specs = new ContentSourceSpecs();
		specs.setBrowseable(false);
		specs.setCalculatesKeys(false);
		specs.setDynamicFolders(false);
		specs.setLowerCaseItems(false);
		specs.setMaintainsLastChanged(false);
		specs.setQueryable(true);
		specs.setUseMasterLogin(false);
		specs.setWritable(false);
		return specs;
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#destroy()
	 */
	public void destroy() {}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getFolders()
	 */
	public String[] getFolders() throws WGAPIException {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#browse(java.lang.String)
	 */
	public Map browse(String folder) throws WGAPIException {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
	 */
	public String getServerName() throws WGAPIException {
		return "(unknown)";
	}

}
