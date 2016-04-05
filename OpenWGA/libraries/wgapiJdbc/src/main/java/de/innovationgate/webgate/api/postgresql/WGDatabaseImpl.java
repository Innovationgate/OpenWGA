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

package de.innovationgate.webgate.api.postgresql;


import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.hibernate.HibernateException;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.dialect.PostgreSQL82Dialect;
import org.hibernate.dialect.PostgreSQLDialect;
import org.hibernate.jdbc.Work;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.jdbc.Area;
import de.innovationgate.webgate.api.jdbc.CSSJSModule;
import de.innovationgate.webgate.api.jdbc.Content;
import de.innovationgate.webgate.api.jdbc.ContentType;
import de.innovationgate.webgate.api.jdbc.FileContainer;
import de.innovationgate.webgate.api.jdbc.Language;
import de.innovationgate.webgate.api.jdbc.MainEntity;
import de.innovationgate.webgate.api.jdbc.StructEntry;
import de.innovationgate.webgate.api.jdbc.TMLModule;
import de.innovationgate.webgate.api.jdbc.UserProfile;

public class WGDatabaseImpl extends de.innovationgate.webgate.api.jdbc.WGDatabaseImpl {

    public static final String DRIVER = "org.postgresql.Driver";

    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException {
        
        // Register drivers with driver manager
        try {
            Class.forName(DRIVER);
        }
        catch (ClassNotFoundException e) {
            throw new WGInvalidDatabaseException("Necessary JDBC driver not found: " + e.getMessage());
        }
        
        // Build creations options
        Map creationOptions = db.getCreationOptions();
        
        // Hibernate configuration
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dialect", PostgreSQL82Dialect.class.getName());
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.driver_class", DRIVER);
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.validationQuery", "select 1");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.testOnBorrow", "true");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.maxWait", "30000");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.connectTimeout", "60000");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.socketTimeout", "120000");

        // enable query paging for mysql
        WGDatabase.putDefaultOption(creationOptions, COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING, "false");
                        
        
        return super.open(db, path, user, pwd, prepareOnly);
    }
    
    /* (Kein Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
     */
    public String getTypeName() {
        return "jdbc/wgacontentstore/postgresql";
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGDatabaseImpl#hasFeature(java.lang.String)
     */
    public boolean hasFeature(String feature) {

        /*if (feature.equals(WGDatabase.FEATURE_CREATEABLE)) {
            return true;
        }*/
        
        return super.hasFeature(feature);
    }

    @Override
    protected String getNativeSQLExpression(String expression) {
        
        if (NATIVESQL_BOOLEAN_TRUE.equals(expression)) {
            return "true";
        }
        
        
        return super.getNativeSQLExpression(expression);
    }
    
    @Override
    public WGDocumentImpl createDocumentImpl(MainEntity entity) {

        int type;
        if (entity instanceof Content) {
            type = WGDocument.TYPE_CONTENT;
        }
        else if (entity instanceof StructEntry) {
            type = WGDocument.TYPE_STRUCTENTRY;
        }
        else if (entity instanceof Area) {
            type = WGDocument.TYPE_AREA;
        }
        else if (entity instanceof ContentType) {
            type = WGDocument.TYPE_CONTENTTYPE;
        }
        else if (entity instanceof TMLModule) {
            type = WGDocument.TYPE_TML;
        }
        else if (entity instanceof CSSJSModule) {
            type = WGDocument.TYPE_CSSJS;
        }
        else if (entity instanceof Language) {
            type = WGDocument.TYPE_LANGUAGE;
        }
        else if (entity instanceof UserProfile) {
            type = WGDocument.TYPE_USERPROFILE;
        }
        else if (entity instanceof FileContainer) {
            type = WGDocument.TYPE_FILECONTAINER;
        }
        else {
            throw new IllegalArgumentException("The given object type is no known entity: " + entity.getClass().getName());
        }

        return new WGDocumentImpl(this, entity, type);
        
    }
    
    public boolean isBackendServiceSupported(String serviceName) {
        if (WGDatabase.BACKENDSERVICE_CLEAR_USERPROFILES.equals(serviceName) ||
                WGDatabase.BACKENDSERVICE_CLEAR_CONTENT.equals(serviceName) ||
                WGDatabase.BACKENDSERVICE_CLEAR_CONTENT_AND_SCHEMA.equals(serviceName) ||
                WGDatabase.BACKENDSERVICE_CLEAR_DATABASE.equals(serviceName)) {
            return (_ddlVersion >= WGDatabase.CSVERSION_WGA5);
        }
        else {
            return super.isBackendServiceSupported(serviceName);
        }
    }
    
    public Object callBackendService(String serviceName, final Object[] params) throws WGAPIException {
        
        try {
            if (WGDatabase.BACKENDSERVICE_CLEAR_USERPROFILES.equals(serviceName)) {
                
                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    throw new WGNotSupportedException("Backend service only supported since content store version 5: " + serviceName);
                }

                getSession().clear();
                getSession().doWork(new Work() {
                    
                    public void execute(Connection connection) throws SQLException {
                        Statement stmt = connection.createStatement();
                        List<String> tablesToTruncate = new ArrayList<String>();
                        tablesToTruncate.addAll(Arrays.asList(clearUserProfilesExtdata(stmt)));
                        truncateTables(stmt, tablesToTruncate);
                        stmt.executeBatch();
                    }
                });
                commitHibernateTransaction();
                return null;
            }
            
            else if (WGDatabase.BACKENDSERVICE_CLEAR_CONTENT.equals(serviceName)) {

                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    throw new WGNotSupportedException("Backend service only supported since content store version 5: " + serviceName);
                }

                getSession().clear();
                getSession().doWork(new Work() {
                    
                    public void execute(Connection connection) throws SQLException {
                        Statement stmt = connection.createStatement();
                        List<String> tablesToTruncate = new ArrayList<String>();
                        tablesToTruncate.addAll(Arrays.asList(clearContentExtdata(stmt)));
                        truncateTables(stmt, tablesToTruncate);
                        stmt.executeBatch();
                    }
                });
                commitHibernateTransaction();
                return null;
                
            }
            else if (WGDatabase.BACKENDSERVICE_CLEAR_CONTENT_AND_SCHEMA.equals(serviceName)) {
                
                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    throw new WGNotSupportedException("Backend service only supported since content store version 5: " + serviceName);
                }

                getSession().clear();
                getSession().doWork(new Work() {
                    public void execute(Connection connection) throws SQLException {
                        Statement stmt = connection.createStatement();
                        List<String> tablesToTruncate = new ArrayList<String>();
                        tablesToTruncate.addAll(Arrays.asList(clearContentExtdata(stmt)));
                        tablesToTruncate.addAll(Arrays.asList(clearSchemaExtdata(stmt)));
                        truncateTables(stmt, tablesToTruncate);
                        stmt.executeBatch();
                    }
                });
                commitHibernateTransaction();
                return null;

                
            }
            
            else if (WGDatabase.BACKENDSERVICE_CLEAR_DATABASE.equals(serviceName)) {

                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    throw new WGNotSupportedException("Backend service only supported since content store version 5: " + serviceName);
                }

                getSession().clear();
                getSession().doWork(new Work() {
                    public void execute(Connection connection) throws SQLException {
                        
                        @SuppressWarnings("unchecked")
                        Map<String,Object> truncateParams = (params.length > 0 ? (Map<String,Object>) params[0] : Collections.<String,Object>emptyMap()); 
                        
                        Statement stmt = connection.createStatement();
                        List<String> tablesToTruncate = new ArrayList<String>();
                        tablesToTruncate.addAll(Arrays.asList(clearUserProfilesExtdata(stmt)));
                        tablesToTruncate.addAll(Arrays.asList(clearContentExtdata(stmt)));
                        tablesToTruncate.addAll(Arrays.asList(clearDesignExtdata(stmt)));
                        tablesToTruncate.addAll(Arrays.asList(clearSchemaExtdata(stmt)));
                        truncateTables(stmt, tablesToTruncate);
                        truncateSystemTables(stmt, (Boolean) WGUtils.getValueOrDefault(truncateParams.get("truncateAcl"), true));
                        stmt.executeBatch();
                    }
                });
                commitHibernateTransaction();
                return null;
                
            }
            else {
                return super.callBackendService(serviceName, params);
            }
        }
        catch (HibernateException e) {
            Throwable cause = e.getCause();
            if (cause instanceof SQLException) {
                SQLException sqle = (SQLException) cause;
                while(sqle != null) {
                    WGFactory.getLogger().error(sqle);
                    sqle = sqle.getNextException();
              }
            }
            else {
                WGFactory.getLogger().error(e);
            }
            return null;
        }
        
    }
    
    private String[] clearUserProfilesExtdata(Statement stmt) throws SQLException {
        stmt.addBatch("delete from only extensiondata using userprofile_items as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using userprofile_portlets as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using userprofile as entity where entity.id=extensiondata.entity_id");
        
        
        return new String[] {"userprofile_items", "userprofile_portlets", "userprofile_langs", "userprofile"};
    }
    
    private String[] clearContentExtdata(Statement stmt) throws SQLException {
        stmt.addBatch("delete from only extensiondata using content_relations as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using content_items as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using content_files_meta as entity where entity.id=extensiondata.entity_id");
        if (_csVersion.getPatchLevel() >= 4) {
            stmt.addBatch("delete from only extensiondata using content_filecontents as entity where entity.id=extensiondata.entity_id");
            stmt.addBatch("delete from only extensiondata using content_filederivates as entity where entity.id=extensiondata.entity_id");
        }
        stmt.addBatch("delete from only extensiondata using content as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using structentry as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using webarea as entity where entity.id=extensiondata.entity_id");
        
        
        
        List<String> tables = new ArrayList<String>();
        tables.addAll(Arrays.asList(new String[] {"content_wfhistory", "content_relations", "content_readers", "content_keywords", "content_items", "content_ishiddenfrom", "content_files_data", "content_files_meta", "content_coauthors", "content", "structentry_readers", "structentry_published", "structentry_childeditors", "structentry_pageeditors", "structentry", "webarea_readers", "webarea_editors", "webarea"}));
        if (_csVersion.getPatchLevel() >= 4) {
            tables.addAll(Arrays.asList(new String[] {"content_filecontents_data", "content_filecontents", "content_filederivates"}));
        }
        return (String[]) tables.toArray(new String[tables.size()]);
    }
    
    private String[] clearDesignExtdata(Statement stmt) throws SQLException {
        stmt.addBatch("delete from only extensiondata using tml as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using filecontainer_files_meta as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using filecontainer as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using scripts as entity where entity.id=extensiondata.entity_id");
        
        return new String[] {"filecontainer_files_data", "filecontainer_files_meta", "filecontainer", "tml", "scripts"};
    }
    
    private String[] clearSchemaExtdata(Statement stmt) throws SQLException {
        
        stmt.addBatch("delete from only extensiondata using lang as entity where entity.id=extensiondata.entity_id");
        stmt.addBatch("delete from only extensiondata using contenttype as entity where entity.id=extensiondata.entity_id");
        
        return new String[] {"lang_editors", "lang", "contenttype_editors", "contenttype_positions", "contenttype"};
        
    }
    
    private void truncateSystemTables(Statement stmt, boolean truncateAcl) throws SQLException {

        List<String> protectedExtDataTerms = new ArrayList<String>();
        for (String extDataName : WGDatabase.PROTECTED_DATABASE_EXTDATA) {
            protectedExtDataTerms.add("not name='" + extDataName + "'");
        }
        
        stmt.addBatch("delete from extensiondata where " + WGUtils.serializeCollection(protectedExtDataTerms, " and "));
        stmt.addBatch("truncate table historylog");
        if (truncateAcl) {
            stmt.addBatch("truncate table acl");
        }
        stmt.addBatch("truncate table cs_sequences");
    }
    
    private void truncateTables(Statement stmt, List<String> tablesToTruncate) throws SQLException {
        stmt.addBatch("truncate table " + WGUtils.serializeCollection(tablesToTruncate, ", "));
    }
    
}
