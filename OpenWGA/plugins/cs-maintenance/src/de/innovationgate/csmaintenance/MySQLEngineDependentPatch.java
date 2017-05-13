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

package de.innovationgate.csmaintenance;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.jdbc.ReturningWork;
import org.hibernate.jdbc.Work;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;

public class MySQLEngineDependentPatch implements CS5Patch {
    
    private String _code; 
    
    public MySQLEngineDependentPatch(String code) {
        _code = code;
    }

    public String getPatchCode(WGDatabase db) throws WGAPIException {
       
            // Find out about the storage engine used on table "webarea"
            return ((Session) db.getNativeObject()).doReturningWork(new ReturningWork<String>() {
                
                public String execute(Connection conn) throws SQLException {
                    Statement stmt1 = null;
                    Statement stmt2 = null;
                    try {
                        // Query engine
                        stmt1 = conn.createStatement();
                        if (!stmt1.execute("SELECT ENGINE FROM information_schema.TABLES WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'webarea'")) {
                            throw new SQLException("Unable to determine storage engine for MySQL patch");
                        }
                        ResultSet set =stmt1.getResultSet();
                        set.next();
                        String engine = set.getString(1);
                        
                        // Query character set
                        stmt2 = conn.createStatement();
                        if (!stmt2.execute("SELECT CHARACTER_SET_NAME, COLLATION_NAME FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'webarea' AND COLUMN_NAME='id'")) {
                            throw new SQLException("Unable to determine storage engine for MySQL patch");
                        }
                        set = stmt2.getResultSet();
                        set.next();
                        String characterSet = set.getString(1);
                        String collationName = set.getString(2);
                        
                        String code = WGUtils.strReplace(_code, "${mysql.engine}", engine, true);
                        code = WGUtils.strReplace(code, "${mysql.charset}", characterSet, true);
                        code = WGUtils.strReplace(code, "${mysql.collation}", collationName, true);
                        
                        return code;
                        
                    }
                    catch (Exception e) {
                        throw new SQLException("Exception determining patch code", e);
                    }
                    finally {
                        if (stmt1 != null) {
                            try {
                                stmt1.close();
                            }
                            catch (SQLException e) {
                            }
                        }
                        if (stmt2 != null) {
                            try {
                                stmt2.close();
                            }
                            catch (SQLException e) {
                            }
                        }
                    }
                    
                }
            });
        
    }

}
