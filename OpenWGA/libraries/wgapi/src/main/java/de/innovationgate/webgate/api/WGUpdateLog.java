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

import java.util.Date;

/**
 * Object representing a modification of a document that took place, which may be an update or a deletion.
 */
public class WGUpdateLog {
    
    @Override
    public String toString() {
        return "WGUpdateLog [user=" + _user + ", date=" + _date + ", documentKey=" + _documentKey + ", type=" + _type + ", revision=" + _revision + ", task=" + _task + "]";
    }

    /**
     * An update log for a document creation or update
     */
    public static final int TYPE_UPDATE = 1;
    /**
     * An update log for a document deletion
     */
    public static final int TYPE_DELETE = 2;
    /**
     * An update log for a struct entry movage. 
     * This log type is issued additionally to the UPDATE log of the struct entry which reflects
     * that the struct data has been changed in the movage operation. 
     */
    public static final int TYPE_STRUCTMOVE = 3;
    
	private String _user;
	private Date _date;
	private String _documentKey;
    private int _type;
    private WGDatabaseRevision _revision;
    private String _task;
	
	/**
	 * Constructor.
	 * @param date The date of modification.
	 * @param user The user that modified the document.
	 * @param dockey The document key of the modified document.
	 */
	public WGUpdateLog(int type, Date date, String user, String dockey, String task, WGDatabaseRevision revision) {
	    _type = type;
		_date = date;
		_user = user;
		_documentKey = dockey;
		_revision = revision;
		_task = task;
	}

	/**
	 * Return the date of modification.
	 */
	public Date getDate() {
		return _date;
	}

	/**
	 * Returns the key of the modified document
	 */
	public String getDocumentKey() {
		return _documentKey;
	}

	/**
	 * Returns the user that executed the modification
	 */
	public String getUser() {
		return _user;
	}

	/**
	 * Sets the date of modification
	 * @param date
	 */
	public void setDate(Date date) {
		_date = date;
	}

	/**
	 * Sets the document key of the modified document
	 * @param string
	 */
	public void setDocumentKey(String string) {
		_documentKey = string;
	}

	/**
	 * Sets the user that executed the modification
	 * @param string
	 */
	public void setUser(String string) {
		_user = string;
	}

    /**
     * Returns the type of log as constants TYPE_...
     */
    public int getType() {
        return _type;
    }
    /**
     * Sets the type of the update log.
     */
    public void setType(int type) {
        _type = type;
    }

    /**
     * Returns the revision of this modification
     */
    public WGDatabaseRevision getRevision() {
        return _revision;
    }

    /**
     * Sets the revision of the update log
     */
    protected void setRevision(WGDatabaseRevision revision) {
        _revision = revision;
    }

    /**
     * Returns the task that was set on the session doing this modification
     */
    public String getTask() {
        return _task;
    }

    /**
     * Sets the task of the update log
     */
    public void setTask(String task) {
        _task = task;
    }
}
