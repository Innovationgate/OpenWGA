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

package de.innovationgate.webgate.api.mysql;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.hibernate.dialect.MySQL5Dialect;
import org.hibernate.jdbc.Work;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
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

    public static final String SOCKET_TIMEOUT_DEFAULT = "600000";
    public static final String CONNECT_TIMEOUT_DEFAULT = "60000";
    public static final String DRIVER = "com.mysql.cj.jdbc.Driver";

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
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dialect", MySQL5Dialect.class.getName());
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.driver_class", DRIVER);
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.validationQuery", "select 1");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.testOnBorrow", "true");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.maxWait", "30000");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.connectTimeout", CONNECT_TIMEOUT_DEFAULT);
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.socketTimeout", SOCKET_TIMEOUT_DEFAULT);

        // enable query paging for mysql
        WGDatabase.putDefaultOption(creationOptions, COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING, "false");
        
        return super.open(db, path, user, pwd, prepareOnly);
    }
    
    /* (Kein Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
     */
    public String getTypeName() {
        return "jdbc/wgacontentstore/mysql";
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGDatabaseImpl#hasFeature(java.lang.String)
     */
    public boolean hasFeature(String feature) {

        if (feature.equals(WGDatabase.FEATURE_CREATEABLE)) {
            return true;
        }
        
        return super.hasFeature(feature);
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
        
        if (WGDatabase.BACKENDSERVICE_CLEAR_USERPROFILES.equals(serviceName)) {
            
            if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                throw new WGNotSupportedException("Backend service only supported since content store version 5: " + serviceName);
            }

            getSession().clear();
            getSession().doWork(new Work() {
                
                public void execute(Connection connection) throws SQLException {
                    Statement stmt = connection.createStatement();
                    stmt.addBatch("SET FOREIGN_KEY_CHECKS=0;");
                    try {
                        truncateUserProfiles(stmt);
                        stmt.executeBatch();
                    }
                    finally {
                        try {
                            connection.createStatement().execute("SET FOREIGN_KEY_CHECKS=1;");
                        }
                        // If that doesn't work the connection is no longer reliable and has to be closed
                        catch (Exception e) {
                            connection.close();
                        }
                    }
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
                    stmt.addBatch("SET FOREIGN_KEY_CHECKS=0;");
                    try {
                        truncateContent(stmt);
                        stmt.executeBatch();
                    }
                    finally {
                        try {
                            connection.createStatement().execute("SET FOREIGN_KEY_CHECKS=1;");
                        }
                        // If that doesn't work the connection is no longer reliable and has to be closed
                        catch (Exception e) {
                            connection.close();
                        }
                    }
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
                    stmt.addBatch("SET FOREIGN_KEY_CHECKS=0;");
                    try {
                        truncateContent(stmt);
                        truncateSchema(stmt);
                        stmt.executeBatch();
                    }
                    finally {
                        try {
                            connection.createStatement().execute("SET FOREIGN_KEY_CHECKS=1;");
                        }
                        // If that doesn't work the connection is no longer reliable and has to be closed
                        catch (Exception e) {
                            connection.close();
                        }
                    }
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
                    Statement stmt = connection.createStatement();
                    stmt.addBatch("SET FOREIGN_KEY_CHECKS=0;");
                    try {
                        @SuppressWarnings("unchecked")
                        Map<String,Object> truncateParams = (params.length > 0 ? (Map<String,Object>) params[0] : Collections.<String,Object>emptyMap()); 
                        
                        truncateUserProfiles(stmt);
                        truncateContent(stmt);
                        truncateDesign(stmt);
                        truncateSchema(stmt);
                        truncateSystemTables(stmt, (Boolean) WGUtils.getValueOrDefault(truncateParams.get("truncateAcl"), true));
                        stmt.executeBatch();
                    }
                    finally {
                        try {
                            connection.createStatement().execute("SET FOREIGN_KEY_CHECKS=1;");
                        }
                        // If that doesn't work the connection is no longer reliable and has to be closed
                        catch (Exception e) {
                            connection.close();
                        }
                    }
                }
            });
            commitHibernateTransaction();
            return null;
            
        }
        else {
            return super.callBackendService(serviceName, params);
        }
        
    }
    

    private void truncateUserProfiles(Statement stmt) throws SQLException {
        stmt.addBatch("delete extdata from userprofile_items as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table userprofile_items");
        stmt.addBatch("delete extdata from userprofile_portlets as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table userprofile_portlets");
        stmt.addBatch("truncate table userprofile_langs");
        stmt.addBatch("delete extdata from userprofile as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table userprofile");
    }
    
    private void truncateContent(Statement stmt) throws SQLException {
        stmt.addBatch("truncate table content_wfhistory");
        stmt.addBatch("delete extdata from content_relations as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table content_relations");
        stmt.addBatch("truncate table content_readers");
        stmt.addBatch("truncate table content_keywords");
        stmt.addBatch("delete extdata from content_items as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table content_items");
        stmt.addBatch("truncate table content_ishiddenfrom");
        stmt.addBatch("truncate table content_files_data");
        stmt.addBatch("delete extdata from content_files_meta as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table content_files_meta");
        if (_csVersion.getPatchLevel() >= 4) {
            stmt.addBatch("truncate table content_filecontents_data");
            stmt.addBatch("delete extdata from content_filecontents as entity, extensiondata as extdata where entity.id=extdata.entity_id");
            stmt.addBatch("truncate table content_filecontents");
            stmt.addBatch("delete extdata from content_filederivates as entity, extensiondata as extdata where entity.id=extdata.entity_id");
            stmt.addBatch("truncate table content_filederivates");
        }
        stmt.addBatch("truncate table content_coauthors");
        stmt.addBatch("delete extdata from content as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table content");
        
        stmt.addBatch("truncate table structentry_readers");
        stmt.addBatch("truncate table structentry_published");
        stmt.addBatch("truncate table structentry_childeditors");
        stmt.addBatch("truncate table structentry_pageeditors");
        stmt.addBatch("delete extdata from structentry as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table structentry");
        
        stmt.addBatch("truncate table webarea_readers");
        stmt.addBatch("truncate table webarea_editors");
        stmt.addBatch("delete extdata from webarea as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table webarea");
    }
    
    private void truncateDesign(Statement stmt) throws SQLException {
        stmt.addBatch("delete extdata from tml as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table tml");
        stmt.addBatch("truncate table filecontainer_files_data");
        stmt.addBatch("delete extdata from filecontainer_files_meta as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table filecontainer_files_meta");
        stmt.addBatch("delete extdata from filecontainer as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table filecontainer");
        stmt.addBatch("delete extdata from scripts as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table scripts");
    }
    
    private void truncateSchema(Statement stmt) throws SQLException {
        stmt.addBatch("truncate table lang_editors");
        stmt.addBatch("delete extdata from lang as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table lang");
        stmt.addBatch("truncate table contenttype_editors");
        stmt.addBatch("truncate table contenttype_positions");
        stmt.addBatch("delete extdata from contenttype as entity, extensiondata as extdata where entity.id=extdata.entity_id");
        stmt.addBatch("truncate table contenttype");
        
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

    
}
