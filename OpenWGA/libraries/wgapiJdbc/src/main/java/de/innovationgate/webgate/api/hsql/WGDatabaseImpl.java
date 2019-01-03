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

package de.innovationgate.webgate.api.hsql;


import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.dialect.HSQLDialect;
import org.hibernate.jdbc.Work;

import de.innovationgate.groq.Groq;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGACLEntry;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGDatabase.ConnectAction;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionProvider;
import de.innovationgate.webgate.api.mysql.MySqlDatabaseServer;
import de.innovationgate.webgate.api.utils.ContentStoreDumpManager;

public class WGDatabaseImpl extends de.innovationgate.webgate.api.jdbc.WGDatabaseImpl {

    public class ImportPre5ConnectAction implements ConnectAction {
        
        private WGDatabase _importSource;
        
        public ImportPre5ConnectAction(WGDatabase importSource) {
            _importSource = importSource;
        }
        
        public void run(WGDatabase db) throws WGInvalidDatabaseException {
            
            WGFactory.getLogger().info("Importing original WGA Content Store into new WGA Content Store of Version 5");
            
            
            
            try {
                // All the content data and schema
                ContentStoreDumpManager importer = new ContentStoreDumpManager(_importSource, db);
                importer.importDump();

                // The ACL
                WGFactory.getLogger().info("Importing original ACL");
                Iterator aclEntries = _importSource.getACL().getAllEntries().iterator();
                while (aclEntries.hasNext()) {
                    WGACLEntry entry = (WGACLEntry) aclEntries.next();
                    WGFactory.getLogger().info("Cloning aclentry '" + entry.getName() + "'");
                    try {
                        WGACLEntry clone = db.getACL().createEntry(entry.getName(), entry.getType(), entry.getLevel());
                        clone.setFlags(entry.getFlags());
                        db.getACL().save(clone);
                    }
                    catch (WGAPIException e) {
                        WGFactory.getLogger().info("Exception cloning aclentry", e);
                    }
                }
            }
            catch (WGAPIException e) {
                throw new WGInvalidDatabaseException("Exception while importing HSQL content store to CS5 format", e);
            }
            finally {
                try {
                    _importSource.close();
                }
                catch (WGAPIException e) {
                    WGFactory.getLogger().error("Exception closing pre 5 backup for of database '" + db.getDbReference() + "'" ,e);
                }
            }
        }
        
    }

    public static final String DRIVER = "org.hsqldb.jdbcDriver";
    public static final String SYSPROPERTY_HSQL_ROOT = "de.innovationgate.wga.hsql.root";
    public static final String SYSPROPERTY_HSQL_LOGSIZE = "de.innovationgate.wga.hsql.logsize";
    public static final String COPTION_MOCKDATABASE = "MockDatabase";
    public static final String COPTION_JDBCPORT = "JDBCPort";
    public static final String COPTION_DAILY_CHECKPOINT = "DailyCheckpoint";
    private HsqlJDBCServer _hsqlServer = null;
    private boolean _dailyCheckpoint;

    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException {
        
        // Register drivers with driver manager
        Driver driver = null;
        try {
             driver = (Driver) Class.forName(DRIVER).newInstance();
        }
        catch (ClassNotFoundException e) {
            throw new WGInvalidDatabaseException("Necessary JDBC driver not found: " + e.getMessage());
        }
        catch (InstantiationException e) {
            throw new WGInvalidDatabaseException("Cannot instantiate neccessary JDBC driver", e);
        }
        catch (IllegalAccessException e) {
            throw new WGInvalidDatabaseException("Cannot instantiate neccessary JDBC driver", e);
        }
        
        Map creationOptions = db.getCreationOptions();

        // General config options
        _dailyCheckpoint = WGUtils.getBooleanMapValue(creationOptions, COPTION_DAILY_CHECKPOINT, true); 
        
        // Hibernate configuration
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dialect", HSQLDialect.class.getName());
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.driver_class", DRIVER);
        String disableBatch = System.getProperty("de.innovationgate.wga.hsql.disable_batch");
        if ("true".equals(disableBatch)) {
            WGDatabase.putDefaultOption(creationOptions, "hibernate.jdbc.batch_size", "0");
        }
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.hsqldb.default_table_type", "cached");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.shutdown", "true");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.maxConnLifetimeMillis", "-1");
        
        // If there is no external access on the database we optimize the logfile regarding autocommit statements (#00002446)
        boolean disableAutocommitLogging = false;
        if (!db.getCreationOptions().containsKey(COPTION_JDBCPORT)) {
            disableAutocommitLogging = true;
        }
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.hsqldb.disable_autocommit_logging", String.valueOf(disableAutocommitLogging));
        
        String hsqlLogSize = System.getProperty(SYSPROPERTY_HSQL_LOGSIZE);
        if (hsqlLogSize == null) {
            hsqlLogSize = String.valueOf(1);
        }
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.hsqldb.log_size", hsqlLogSize);
        

        if (user == null) {
            user = "sa";
        }
        if (pwd == null) {
            pwd = "";
        }
        
        // Build hsql path
        String hsqlPath = buildHsqlPath(db, path);
        String jdbcPath = "jdbc:hsqldb:" + hsqlPath;
        
        // Create a first connection to the database to do some tests
        
        JDBCConnectionProvider conProvider = null;
        
        boolean isInitialized = false;
        boolean migrateToCS5 = false;
        double csVersion = 0;
        try {
            conProvider = createConnectionProvider(db, DRIVER, jdbcPath, user, pwd);
            List<String> tables = conProvider.getDatabaseTables();
            
            // Look if the database is empty and we must initialize it
            String contentTable = (String) Groq.selectFrom(tables).whereEqualsIgnoreCase("content").singleValue();
            if (contentTable != null) {
                isInitialized = true;
                if (db.getCreationOptions().containsKey(WGDatabase.COPTION_CONTENT_STORE_VERSION)) {
                    
                    // Look if the desired target format is CS5 (or higher)
                    double targetCsVersion = Double.valueOf((String) db.getCreationOptions().get(WGDatabase.COPTION_CONTENT_STORE_VERSION)).doubleValue();
                    if (targetCsVersion >= WGDatabase.CSVERSION_WGA5) {
                        // Look if the actual format it is lower than CS5. If so we automatically migrate it to that format
                        csVersion = determineCSVersion(conProvider);
                        if (csVersion < WGDatabase.CSVERSION_WGA5) {
                            migrateToCS5 = true;
                            isInitialized = false;
                        }
                    }
                }
            }
        }
        catch (Exception e) {
            throw new WGInvalidDatabaseException("Unable to connect to database", e);
        }
        finally {
            if (conProvider != null) {
                try {
                    conProvider.close();
                }
                catch (JDBCConnectionException e) {
                }
            }
        }
        
        WGDatabase importSource = null;
        if (migrateToCS5) {
            try {
                importSource = prepareCSDump(db, getHsqlBaseFile(path), user, pwd, csVersion);
            }
            catch (Exception e) {
                throw new WGInvalidDatabaseException("Exception while migrating HSQL content store to CS5 format", e);
            }
        }
        
        if (isInitialized == false) {
            WGFactory.getLogger().info("Initializing empty database " + db.getDbReference() + " as a WGA Content Store");
            initializeContentStore(jdbcPath, user, pwd, null, false);
        }
        
        WGUserAccess userAccess = super.open(db, jdbcPath, user, pwd, prepareOnly);
        
        // In case of success
        if (userAccess.getAccessLevel() > WGDatabase.ACCESSLEVEL_NOACCESS) {
            
            // When the database gets connected we try to import the backed up pre5 database
            if (migrateToCS5) {
                db.onConnect(new ImportPre5ConnectAction(importSource));
            }
            
            //  start a HSQL server if a port is provided
            if (db.getCreationOptions().containsKey(COPTION_JDBCPORT)) {
                try {
                    int port = Integer.parseInt((String) db.getCreationOptions().get(COPTION_JDBCPORT));
                    _hsqlServer = new HsqlJDBCServer(hsqlPath, port);
                    WGFactory.getLogger().info("JDBC Server for HSQL database '" + db.getDbReference() + "' started on port " + _hsqlServer.getPort());
                }
                catch (Exception e) {
                    WGFactory.getLogger().error("Exception creating JDBC server for HSQL database '" + db.getDbReference() + "'", e);
                }
            }
        }
        
        return userAccess;
    }

    private WGDatabase prepareCSDump(WGDatabase db, final File baseFile, String user, String pwd, double csVersion) throws IOException, WGAPIException, URISyntaxException {

        
        WGFactory.getLogger().info("Automatic migration of HSQL Content Store '" + db.getDbReference() + "' to WGA Content Store Version 5 format");
        File dbDir = baseFile.getParentFile();
        
        // Use a directoy "pre5backup" under the normal location and copy the original DB there
        File backupDir = new File(dbDir, "pre5backup");
        if (!backupDir.exists()) {
            backupDir.mkdir();
        }
        WGFactory.getLogger().info("Backing up original WGA Content Store to location: " + backupDir.getAbsolutePath());
        
        File[] dbFiles = dbDir.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.startsWith(baseFile.getName() + ".");
            }
        });
        
        // First pass - copy all files
        for (File file : dbFiles) {
            WGUtils.copyFile(file, new File(backupDir, file.getName()));
        }
        
        // Second pass - delete orignal files so an empty database gets created in CS5 format
        for (File file : dbFiles) {
            file.delete();
        }
        
        // Open a WGDatabase to it and keep it as source database for an import. Set CS version to prevent triggering auto-migration
        Map cOptions = new java.util.HashMap();
        cOptions.put(WGDatabase.COPTION_CONTENT_STORE_VERSION, String.valueOf(csVersion));
        return WGFactory.getInstance().openDatabase(null, db.getType(), (new File(backupDir, baseFile.getName())).getAbsolutePath(), user, pwd, cOptions);
    }

    protected static String buildHsqlPath(WGDatabase db, String path) throws WGInvalidDatabaseException {
        String hsqlPath;
        if (db != null && WGUtils.nullSafeEquals(db.getCreationOptions().get(COPTION_MOCKDATABASE), "true")) {
            hsqlPath = "mem:mock_" + path; 
        }
        // Normal database
        else {
            // Build complete db path
            File dbfile = getHsqlBaseFile(path);
            
            if (dbfile.getParentFile() == null || !dbfile.getParentFile().exists()) {
                throw new WGInvalidDatabaseException("The database path '" + dbfile.getParent() + "' does not exist");
            }

            hsqlPath = "file:" + dbfile.getPath();
            
            
        }
        return hsqlPath;
    }

    protected static File getHsqlBaseFile(String path) {
        File dbfile = new File(path);
        if (!dbfile.isAbsolute()) {
            String rootPath = System.getProperty(SYSPROPERTY_HSQL_ROOT);
            if (rootPath != null) {
                dbfile = new File(rootPath, path);
            }
        }
        return dbfile;
    }
    
    /* (Kein Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
     */
    public String getTypeName() {
        return "jdbc/wgacontentstore/hsql";
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGDatabaseImpl#close()
     */
    public void close() throws WGAPIException {
        
        Session session = null;
        boolean sessionOpened = false;
        try {
            session = getSessionStatus().getSession();
            if (session == null) {
                session = _sessionBuilder.openSession();
                sessionOpened = true;
            }
            
            session.doWork(new Work() {
                public void execute(Connection connection) throws SQLException {
                    try {
                        connection.createStatement().execute("SHUTDOWN COMPACT");
                    }
                    catch (SQLException e) {
                        WGFactory.getLogger().error("Error shutting down HSQL database", e);
                    }
                }
            });
            
        }
        catch (HibernateException e) {
            WGFactory.getLogger().error("Error shutting down HSQL database", e);
        }
        finally {
            
            if (session != null && sessionOpened) {
                try {
                    session.close();
                }
                catch (HibernateException e) {
                    WGFactory.getLogger().error("Error closing HSQL shutdown session", e);
                }
            }
            
        }
        
        if (_hsqlServer != null) {
            _hsqlServer.shutdown();
            _hsqlServer = null;
        }
        
        super.close();
        
    }

    public static void initializeContentStore(String path, String user, String pwd, String script, boolean warnWhenExisting) throws WGInvalidDatabaseException {
        // Retrieve driver
        Driver driver;
        try {
            driver = (Driver) Class.forName(DRIVER).newInstance();
        }
        catch (Exception e) {
            throw new WGInvalidDatabaseException("Cannot create database bc. of exception when retrieving driver", e);
        }
    
        // Build props
        Properties props = new Properties();
        props.setProperty("hsqldb.default_table_type", "cached");
        props.put("shutdown", "true");
        props.setProperty("user", user);
        boolean disableBatch = Boolean.parseBoolean(System.getProperty("de.innovationgate.wga.hsql.disable_batch"));
        
        if (pwd != null) {
            props.setProperty("password", pwd);
        }
        
        // Connect to create
        Connection con = null;
        try {
            try {
                con = driver.connect(path, props);
            }
            catch (SQLException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception on creation", e);
            }
            
            // If script is null retrieve default script
            try {
                if (script == null) {
                    Reader read = new InputStreamReader(WGDatabaseImpl.class.getClassLoader().getResourceAsStream("de/innovationgate/webgate/api/hsql/wgacs5_hsqldb.ddl"));
                    script = WGUtils.readString(read);
                    read.close();
                }
            }
            catch (IOException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when reading initialisation script", e);
            }
        
            // Execute script
            con.setAutoCommit(false);
            Iterator statements = WGUtils.deserializeCollection(script, ";", false, new Character('\'')).iterator();
            Statement st = con.createStatement();
            while (statements.hasNext()) {
               String code = ((String) statements.next()).trim();
               if (!code.equals("")) {
                   if (!disableBatch) {
                       st.addBatch(code);
                   }
                   else {
                       st.execute(code);
                   }
               }
            }
            if (!disableBatch) {
                st.executeBatch();
            }
            con.commit();
            
        }
        catch (SQLException e) {
            throw new WGInvalidDatabaseException("Cannot create database bc. of exception when executing initialisation script", e);
        }
        finally {
            try {
                if (con != null) {
                    con.close();
                }
            }
            catch (SQLException e) {
                throw new WGInvalidDatabaseException("Exception when closing connection", e);
            }
        }
    }
    
    
    @Override
    public boolean hasFeature(String feature) {
        if (WGDatabase.FEATURE_MILLISECOND_PRECISION.equals(feature)) {
            return true;
        }
        
        if (WGDatabase.FEATURE_TRANSACTIONS.equals(feature)) {
            return false;
        }
        
        
        return super.hasFeature(feature);
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
                    truncateUserProfiles(stmt);
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
                    truncateContent(stmt);
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
                    truncateContent(stmt);
                    truncateSchema(stmt);
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
                    truncateUserProfiles(stmt);
                    truncateContent(stmt);
                    truncateDesign(stmt);
                    truncateSchema(stmt);
                    truncateSystemTables(stmt, (Boolean) WGUtils.getValueOrDefault(truncateParams.get("truncateAcl"), true));
                    //stmt.executeBatch();
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
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from userprofile_items)");
        stmt.executeUpdate("delete from userprofile_items ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from userprofile_portlets)");
        stmt.executeUpdate("delete from userprofile_portlets ");
        stmt.executeUpdate("delete from userprofile_langs ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from userprofile)");
        stmt.executeUpdate("delete from userprofile ");
    }
    
    private void truncateContent(Statement stmt) throws SQLException {
        stmt.executeUpdate("delete from content_wfhistory ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from content_relations)");
        stmt.executeUpdate("delete from content_relations ");
        stmt.executeUpdate("delete from content_readers ");
        stmt.executeUpdate("delete from content_keywords ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from content_items)");
        stmt.executeUpdate("delete from content_items ");
        stmt.executeUpdate("delete from content_ishiddenfrom ");
        stmt.executeUpdate("delete from content_files_data ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from content_files_meta)");
        stmt.executeUpdate("delete from content_files_meta ");
        if (_csVersion.getPatchLevel() >= 4) {
            stmt.executeUpdate("delete from content_filecontents_data");
            stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from content_filecontents)");
            stmt.executeUpdate("delete from content_filecontents");
            stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from content_filederivates)");
            stmt.executeUpdate("delete from content_filederivates");
        }
        stmt.executeUpdate("delete from content_coauthors ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from content)");
        stmt.executeUpdate("delete from content ");
        
        stmt.executeUpdate("delete from structentry_readers ");
        stmt.executeUpdate("delete from structentry_published ");
        stmt.executeUpdate("delete from structentry_childeditors ");
        stmt.executeUpdate("delete from structentry_pageeditors ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from structentry)");
        stmt.executeUpdate("delete from structentry ");
        
        stmt.executeUpdate("delete from webarea_readers ");
        stmt.executeUpdate("delete from webarea_editors ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from webarea)");
        stmt.executeUpdate("delete from webarea ");
    }
    
    private void truncateDesign(Statement stmt) throws SQLException {
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from tml)");
        stmt.executeUpdate("delete from tml ");
        stmt.executeUpdate("delete from filecontainer_files_data ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from filecontainer_files_meta)");
        stmt.executeUpdate("delete from filecontainer_files_meta ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from filecontainer)");
        stmt.executeUpdate("delete from filecontainer ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from scripts)");
        stmt.executeUpdate("delete from scripts ");
    }
    
    private void truncateSchema(Statement stmt) throws SQLException {
        stmt.executeUpdate("delete from lang_editors ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from lang)");
        stmt.executeUpdate("delete from lang ");
        stmt.executeUpdate("delete from contenttype_editors ");
        stmt.executeUpdate("delete from contenttype_positions ");
        stmt.executeUpdate("delete from extensiondata as extdata where extdata.entity_id in (select id from contenttype)");
        stmt.executeUpdate("delete from contenttype ");
        
    }
    
    private void truncateSystemTables(Statement stmt, boolean truncateAcl) throws SQLException {
        
        List<String> protectedExtDataTerms = new ArrayList<String>();
        for (String extDataName : WGDatabase.PROTECTED_DATABASE_EXTDATA) {
            protectedExtDataTerms.add("not name='" + extDataName + "'");
        }
        
        stmt.executeUpdate("delete from extensiondata where " + WGUtils.serializeCollection(protectedExtDataTerms, " and "));
        stmt.executeUpdate("delete from historylog ");
        if (truncateAcl) {
            stmt.executeUpdate("delete from acl ");
        }
        stmt.executeUpdate("delete from cs_sequences ");
    }
    
    @Override
    protected long dailyMaintenance(Logger log) throws WGAPIException {

       long freedMemory = super.dailyMaintenance(log);
       
       if (_dailyCheckpoint) {
           log.info("Maintenance on content store of app/plugin '" + getDb().getDbReference() + "': Clearing log and defragging data files");
           getSession().doWork(new Work() {
    
                @Override
                public void execute(Connection connection) throws SQLException {
                    connection.createStatement().executeUpdate("CHECKPOINT DEFRAG;");
                }
               
           });
       }
       
       return freedMemory;
        
    
    }
}


