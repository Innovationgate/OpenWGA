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
package de.innovationgate.webgate.api.query.jdbc;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGResultSetCore;

public class WGResultSetImpl implements WGResultSetCore {
	
	protected WGDatabaseImpl db;
	private ResultSet resultSet;
	private int results = -1;

	public WGResultSetImpl(WGDatabaseImpl db, ResultSet resultSet) {
		this.db = db;
		this.resultSet = resultSet;
	}


	/**
	 * @see de.innovationgate.webgate.api.WGResultSetCore#results()
	 */
	public int results() {
		try {
			if (this.results == -1) {
			    
			    if (resultSet == null) {
                    return 0;
                }
			    
				resultSet.last();
				if (resultSet.isAfterLast() || resultSet.isBeforeFirst()) {
					this.results = 0;					
				}
				else {
					this.results = resultSet.getRow();
				}
				
			}
			return this.results;
		}
		catch (SQLException e) {
			return 0;
		}
		
	}

	/**
	 * @throws WGBackendException 
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList(long, long)
	 */
	public List getContentList(int start, int length) throws WGBackendException {
			try {
				List result = new ArrayList();
				if (resultSet == null) {
                    return result;
                }
				
				if (!resultSet.absolute(start)) {
				    return result;
				}
				
				int idx = 0;
				while (!resultSet.isAfterLast()) {
					result.add(createDocumentImpl());
					idx++;
					if (idx >= length) {
						break;
					}
					resultSet.next();
				}
				return result;
			}
			catch (SQLException e) {
				throw new WGBackendException("Error retrieving content list.", e);
			}
	}


	private WGDocumentCore createDocumentImpl() throws WGBackendException {
		return new WGDocumentImpl(this.db.db, resultSet);
	}
 
	/**
	 * @throws WGBackendException 
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList()
	 */
	public List getContentList() throws WGBackendException {
			try {
			    List result = new ArrayList();
			    if (resultSet == null) {
			        return result;
			    }
			    
			    
				resultSet.beforeFirst();
				while (resultSet.next()) {
					result.add(new WGDocumentImpl(this.db.db, resultSet));
				}
				return result;
			}
			catch (SQLException e) {
				throw new WGBackendException("Error retrieving content list.", e);
			}
	}

	/**
	 * @see java.lang.Object#finalize()
	 */
	protected void finalize() throws Throwable {
		super.finalize();
		if (this.resultSet != null) {
		    this.resultSet.close();
		}
	}


    public boolean isReturnsKeys() {
        return false;
    }


    public List getColumnNames() throws WGBackendException {
        try {
            if (resultSet == null) {
                return Collections.emptyList();
            }
            
            List<String> names = new ArrayList<String>();
            ResultSetMetaData rsMeta = resultSet.getMetaData();
            for (int idx=rsMeta.getColumnCount(); idx > 0; idx--) {
                names.add(rsMeta.getColumnLabel(idx).toLowerCase());
            }
            return names;
        }
        catch (SQLException e) {
            throw new WGBackendException("Exception fetching column names", e);
        }
    }
    
    @Override
    public boolean isLimitingResults() {
        return false;
    }

}
