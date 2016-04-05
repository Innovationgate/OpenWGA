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
package de.innovationgate.webgate.api.jdbc;

import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.webgate.api.WGACLCore;
import de.innovationgate.webgate.api.WGACLEntry;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGUpdateLog;

public class ACLImpl implements WGACLCore {	private WGDatabaseImpl _parent;

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.ACL#createEntry(java.lang.String, int)
	 */
	public WGACLEntry createEntry(String name, int type, int accessLevel) throws WGAPIException {
	
		try {
			ACLEntry entry = new ACLEntry(name, type, accessLevel, "");
			Session session = _parent.getSession();
			_parent.createLogEntry(session, WGUpdateLog.TYPE_UPDATE, "$aclentry/" + entry.getName(), entry.getId());
			session.save(entry);
			_parent.commitHibernateTransaction();
			_parent.getDb().getUserCache().clear();
			if (_parent._saveIsolationActive) {
			    session.evict(entry);
			}
			return entry;
		}
		catch (Exception e) {
            _parent.rollbackHibernateTransaction(true);
			throw new WGBackendException("Error creating ACL entry", e);
		}
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.ACL#getAllEntries()
	 */
	public List getAllEntries() throws WGAPIException {
		
		try {
			Session session = _parent.getSession();
			List<ACLEntry> entries = session.createQuery("from ACLEntry as aclentry order by aclentry.type asc, aclentry.name asc").list();
			if (_parent._saveIsolationActive) {
    			for (ACLEntry entry :  entries) {
    			    session.evict(entry);
    			}
			}
			return entries;
		}
		catch (HibernateException e) {
			throw new WGBackendException("Error retrieving all ACL entries", e);
		}
		
	}
	
	protected ACLImpl(WGDatabaseImpl parent) {
		_parent = parent;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGACL#remove(de.innovationgate.webgate.api.WGACLEntry)
	 */
	public void remove(WGACLEntry entry) throws WGAPIException  {
	
		try {
			Session session = _parent.getSession();
			ACLEntry entryImpl = (ACLEntry) entry;
			_parent.createLogEntry(session, WGUpdateLog.TYPE_DELETE, "$aclentry/" + entry.getName(), entryImpl.getId());
			session.delete(entry);
			_parent.commitHibernateTransaction();
			_parent.getDb().getUserCache().clear();
		}
		catch (Exception e) {
			_parent.rollbackHibernateTransaction(true);
			throw new WGBackendException("Error removing ACL entry", e);
		}
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGACL#save(de.innovationgate.webgate.api.WGACLEntry)
	 */
	public void save(WGACLEntry entry) throws WGAPIException {
	
	
		try {
			Session session = _parent.getSession();
			ACLEntry entryImpl = (ACLEntry) entry;
			_parent.createLogEntry(session, WGUpdateLog.TYPE_UPDATE, WGDocument.TYPENAME_ACLENTRY + "/" + entry.getName(), entryImpl.getId());
			session.update(entry);
			_parent.commitHibernateTransaction();
			_parent.getDb().getUserCache().clear();
		}
		catch (Exception e) {
			_parent.rollbackHibernateTransaction(true);
			throw new WGBackendException("Error saving ACL entry", e);
		}
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGACL#getEntry(java.lang.String)
	 */
	public WGACLEntry getEntry(String name) throws WGAPIException {
		try {
			Session session = _parent.getSession();
            Query query = session.createQuery("from ACLEntry where name=:name");
            query.setParameter("name", name);
			List results = query.list();
			if (results.size() >= 1) {
				ACLEntry entry =  (ACLEntry) results.get(0);
	            if (_parent._saveIsolationActive) {
	                session.evict(entry);
				}
				return entry;
			}
			else {
				return null;
			}
		}
		catch (HibernateException e) {
			throw new WGBackendException("Error retrieving ACL entry with name '" + name + "'.", e);			
		}

	}

}
