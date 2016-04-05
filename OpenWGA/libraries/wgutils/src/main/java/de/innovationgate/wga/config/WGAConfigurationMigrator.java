/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.config;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.dom4j.Attribute;
import org.dom4j.Branch;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.security.SHA1HashingScheme;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.WGAXML;
import de.innovationgate.wga.common.beans.DesignConfiguration;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.model.ValidationError;

/**
 * Tool to migrate WGA4 configuration file "wga.xml" to OpenWGA5 configuration file "wgaconfig.xml"
 */
public class WGAConfigurationMigrator {

    private static final String ENHANCED_JDBC_DB_CLASS = "de.innovationgate.webgate.api.jdbc.custom.JDBCSource";
    private static final String HSQL_SERVER_CLASS = "de.innovationgate.webgate.api.hsql.HsqlDatabaseServer";
    private static final String MYSQL_SERVER_CLASS = "de.innovationgate.webgate.api.mysql.MySqlDatabaseServer";
    public static final List<String> FULL_CONTENT_STORE_IMPLEMENTATIONS = new ArrayList<String>();
    private static Set<String> _domainsWithCertAuthEnabled = new HashSet<String>();
    static {
        FULL_CONTENT_STORE_IMPLEMENTATIONS.add("de.innovationgate.webgate.api.mysql.WGDatabaseImpl");
        FULL_CONTENT_STORE_IMPLEMENTATIONS.add("de.innovationgate.webgate.api.hsql.WGDatabaseImpl");
        FULL_CONTENT_STORE_IMPLEMENTATIONS.add("de.innovationgate.webgate.api.oracle.WGDatabaseImpl");
        FULL_CONTENT_STORE_IMPLEMENTATIONS.add("de.innovationgate.webgate.api.jdbc.WGDatabaseImpl");
        FULL_CONTENT_STORE_IMPLEMENTATIONS.add("de.innovationgate.webgate.api.domino.remote.WGDatabaseImpl");
    }

    public static final Map<String, String> REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS = new HashMap<String, String>();
    private static final String QUERY_JDBC_DB_CLASS = "de.innovationgate.webgate.api.query.jdbc.WGDatabaseImpl";
    private static final String ORACLE_SERVER_CLASS = "de.innovationgate.webgate.api.oracle.OracleDatabaseServer";
    private static final String JDBC_SERVER_CLASS = "de.innovationgate.webgate.api.jdbc.JDBCDatabaseServer";
    static {
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("QueryDefault", "native");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("AllowCustomQueries", "true");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("FileCacheSize", "10");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("FileCacheThreshold", "10");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("FileExpirationMinutes", "10");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("GlobalVars", "true");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("DefaultItemEncoding", "none");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("MaxQueryResults", "500");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("ExpressionDefault", "tmlscript");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("AllowBrowsing", "true");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("HomePage", "");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("LoginPage", "");
        REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.put("DefaultMediaKey", "html");
    }

    public static MigrationResult createFromWGAXML(InputStream wgaXML, String configPath) throws DocumentException, IOException, NoSuchAlgorithmException {
        MigrationResult migrationResult = new MigrationResult();
        migrationResult.logInfo("Starting migration of 'wga.xml'.");

        SAXReader reader = new SAXReader();
        Document doc = reader.read(wgaXML);
        WGAXML.normalize(doc);

        WGAConfiguration config = new WGAConfiguration();
        config.createDefaultResources();
        
        // Add a file system design source that will register all design directories that are not at default location
        DesignSource fsDesignSource = new DesignSource("de.innovationgate.wgpublisher.design.fs.FileSystemDesignSource");
        fsDesignSource.setDescription("Migrated design directories that are not in the default folder");
        fsDesignSource.setTitle("Migrated design directories");
        String dir = System.getProperty(WGAConfiguration.SYSPROP_DESIGN_ROOT);
        if (dir == null) {
            dir =  WGAConfiguration.DEFAULT_DESIGNROOT;
        }
        fsDesignSource.getOptions().put("Path", dir);
        config.getDesignConfiguration().getDesignSources().add(fsDesignSource);

        Element root = doc.getRootElement();

        Iterator administrators = root.element("administrators").elementIterator("administrator");
        while (administrators.hasNext()) {
            Element adminElement = (Element) administrators.next();
            String name = adminElement.attributeValue("name");
            String password = adminElement.attributeValue("password");
            if (!adminElement.attributeValue("encode", "").equals("hash")) {
                password = WGUtils.hashPassword(password);
            }

            Administrator admin = new Administrator(name, password, SHA1HashingScheme.NAME);
            migrationResult.logInfo("Migrating admin login '" + name + "'.");
            config.add(admin);
        }

        migrationResult.logInfo("Migrating general configuration.");
        Element configuration = root.element("configuration");

        Element defaultDB = configuration.element("defaultdb");
        config.setDefaultDatabase(defaultDB.attributeValue("key"));
        config.setFavicon(defaultDB.attributeValue("favicon"));
        config.setCacheExpirationForStaticResources(Integer.parseInt(defaultDB.attributeValue("staticexpiration")));
        config.setUsePermanentRedirect(Boolean.parseBoolean(defaultDB.attributeValue("permanentredirect", "false")));

        Element warnings = configuration.element("warnings");
        config.setWarningsEnabled(Boolean.parseBoolean(warnings.attributeValue("enabled")));
        config.setWarningsOutputOnConsole(Boolean.parseBoolean(warnings.attributeValue("consoleOuput")));
        config.setWarningsOutputViaTML(Boolean.parseBoolean(warnings.attributeValue("pageOutput")) == true ? Constants.WARNINGS_TML_AS_HTML : Constants.WARNINGS_TML_OFF);

        Element tml = configuration.element("tml");
        String enc = tml.attributeValue("characterEncoding", null);
        if (enc != null && !enc.trim().equals("")) {
        	config.setCharacterEncoding(enc);
        }
        Element tmlHeader = tml.element("tmlheader");
        if (tmlHeader != null) {
            String tmlBufferStr = tmlHeader.attributeValue("buffer").toLowerCase();
            if (tmlBufferStr.indexOf("kb") != -1) {
                tmlBufferStr = tmlBufferStr.substring(0, tmlBufferStr.indexOf("kb"));
            }
            try {
                int tmlBuffer = Integer.parseInt(tmlBufferStr);
                config.setTmlBuffer(tmlBuffer);
            }
            catch (NumberFormatException e) {
                migrationResult.logError("Unable to parse WebTML output buffer size: " + tmlBufferStr + ". Falling back to default: " + WGAConfiguration.SERVEROPTIONDEFAULT_WEBTML_OUTPUT_BUFFER);
            }
            config.setTmlHeader(tmlHeader.getText());
        }

        Element features = configuration.element("features");
        /* Deprecated in WGA5
        config.setAuthoringApplicationsEnabled(Boolean.parseBoolean(features.attributeValue("bi")));
        
        config.setStartPageEnabled(Boolean.parseBoolean(features.attributeValue("startpage")));
        */
        config.setAdminPageEnabled(Boolean.parseBoolean(features.attributeValue("adminpage")));
        config.setWebservicesEnabled(Boolean.parseBoolean(features.attributeValue("webservice")));
        config.clearAdminToolsPortRestrictions();
        String port = features.attributeValue("adminport");
        if (port != null && !port.trim().equals("")) {
            config.addAdminToolsPortRestriction(Integer.parseInt(port));
        }
        config.clearAuthoringDesignAccessPortRestrictions();
        port = features.attributeValue("authoringport");
        if (port != null && !port.trim().equals("")) {
            config.addAuthoringDesignAccessPortRestriction(Integer.parseInt(port));
        }

        Element applog = configuration.element("applog");
        config.setApplicationLogLevel(applog.attributeValue("level"));
        config.setApplicationLogDirectory(applog.attributeValue("dir", null));

        Iterator listeners = configuration.element("listeners").elementIterator("listener");
        while (listeners.hasNext()) {
            Element listener = (Element) listeners.next();
            config.getCoreEventListeners().add(listener.attributeValue("class"));
        }

        // personalisation agent exclusions
        Element personalisation = configuration.element("personalisation");
        Iterator agentExclusions = personalisation.elementIterator("agentexclusion");
        while (agentExclusions.hasNext()) {
            Element agentExclusion = (Element) agentExclusions.next();
            config.getPersonalisationConfiguration().getPersonalisationAgentExclusions().add(agentExclusion.attributeValue("name"));
        }

        migrationResult.logInfo("Migrating global lucene configuration.");
        Element lucene = configuration.element("lucene");
        config.getLuceneManagerConfiguration().setEnabled(Boolean.parseBoolean(lucene.attributeValue("enabled")));   
        config.getLuceneManagerConfiguration().setPath(System.getProperty(WGAConfiguration.SYSPROP_LUCENE_ROOT, lucene.attributeValue("dir")));
        config.getLuceneManagerConfiguration().setMaxBooleanClauseCount(Integer.parseInt(lucene.attributeValue("booleanQueryMaxClauseCount")));
        config.getLuceneManagerConfiguration().setMaxDocsPerDBSession(Integer.parseInt(lucene.attributeValue("maxDocsPerDBSession")));
        if (WGUtils.isEmpty(config.getLuceneManagerConfiguration().getPath())) {
            File lucenePath = new File(configPath, "lucene");
            if (!lucenePath.exists()) {
                lucenePath.mkdir();
            }
            config.getLuceneManagerConfiguration().setPath(lucenePath.getAbsolutePath());
        }

        migrationResult.logInfo("Migrating global designsync configuration.");
        Element designSync = configuration.element("designsync");
        config.getDesignConfiguration().setDefaultEncoding(designSync.attributeValue("fileEncoding"));
        config.getDesignConfiguration().setPollingInterval(Integer.parseInt(designSync.attributeValue("interval")));
        config.getDesignConfiguration().setThrottlingEnabled(Boolean.parseBoolean(designSync.attributeValue("throttling")));
        config.getDesignConfiguration().setThrottlingPeriodMinutes(Integer.parseInt(designSync.attributeValue("throttlingactivation")));
        Iterator fileExclusions = designSync.elementIterator("fileexclusion");
        while (fileExclusions.hasNext()) {
            Element fileExclusion = (Element) fileExclusions.next();
            config.getDesignConfiguration().getFileExclusions().add(fileExclusion.attributeValue("name"));
        }

        migrationResult.logInfo("Migrating global database options.");
        Iterator defaultDBOptions = configuration.element("defaultdboptions").elementIterator("option");
        while (defaultDBOptions.hasNext()) {
            Element option = (Element) defaultDBOptions.next();
            config.getGlobalDatabaseOptions().put(option.attributeValue("name"), option.attributeValue("value"));
        }
        migrationResult.logInfo("Migrating global publishing options.");
        Iterator defaultPublisherOptions = configuration.element("defaultpublisheroptions").elementIterator("option");
        while (defaultPublisherOptions.hasNext()) {
            Element option = (Element) defaultPublisherOptions.next();
            config.getGlobalPublisherOptions().put(option.attributeValue("name"), option.attributeValue("value"));
        }

        migrationResult.logInfo("Migrating global mail configuration.");
        Element mailconfig = configuration.element("mailconfig");
        config.getMailConfiguration().setServer(mailconfig.attributeValue("mailHost"));
        config.getMailConfiguration().setUser(mailconfig.attributeValue("mailUser"));
        config.getMailConfiguration().setPassword(mailconfig.attributeValue("mailPassword"));
        config.getMailConfiguration().setFromAddress(mailconfig.attributeValue("mailFrom"));
        config.getMailConfiguration().setToAddress(mailconfig.attributeValue("mailTo"));
        config.getMailConfiguration().setEnableAdminNotifications(Boolean.parseBoolean(mailconfig.attributeValue("enableAdminNotifications")));
        config.setRootURL(mailconfig.attributeValue("mailWGARootURL"));

        Element domains = root.element("domains");
        migrateDomains(migrationResult, config, domains);

        // create dbservers & content dbs
        migrationResult.logInfo("Starting migration of content dbs ...");
        int mysqlServerCount = 0;
        int hsqlServerCount = 0;
        int notesServerCount = 0;
        int oracleServerCount = 0;
        int jdbcServerCount = 0;
        int dummyServerCount = 0;
        Map<String, DatabaseServer> dbServersByUniqueID = new HashMap<String, DatabaseServer>();
        
        Iterator dbPaths = root.selectNodes("//contentdb/dbpath").iterator();
        while (dbPaths.hasNext()) {
            Element dbPathElement = (Element) dbPaths.next();
            String dbType = dbPathElement.getParent().elementTextTrim("type");
            String path = dbPathElement.getTextTrim();
            String user = determineUser(dbPathElement.getParent());
            String password = determinePassword(dbPathElement.getParent());

            // migration for mysql dbs
            if (path.startsWith("jdbc:mysql")) {
                mysqlServerCount = createMySqlServerAndDB(configPath, migrationResult, config, mysqlServerCount, dbServersByUniqueID, dbPathElement, path, user, password, false, fsDesignSource);
            }

            else if (dbType.contains("domino.remote")) {
                notesServerCount = createDominoServerAndDB(configPath, migrationResult, config, notesServerCount, dbServersByUniqueID, dbPathElement, path, user, password, false, fsDesignSource);
            }

            else if (dbType.contains("de.innovationgate.webgate.api.hsql") || path.startsWith("jdbc:hsqldb:")) {
                hsqlServerCount = createHSQLServerAndDB(configPath, migrationResult, config, hsqlServerCount, dbServersByUniqueID, dbPathElement, path, user, password, false, fsDesignSource);
            }
            else if (path.startsWith("jdbc:oracle:")) {
                oracleServerCount = createOracleServerAndDB(configPath, migrationResult, config, oracleServerCount, dbServersByUniqueID, dbPathElement, path, user, password, false, fsDesignSource);
            }
            else if (dbType.contains(".jdbc.")) {
                jdbcServerCount = createJDBCServerAndDB(configPath, migrationResult, config, jdbcServerCount, dbServersByUniqueID, dbPathElement, path, user, password, false, fsDesignSource);
            }
            
            else {
                // migrate other dbs with same user/password combination to
                // "other sources" server
                DatabaseServer server = new DatabaseServer("de.innovationgate.webgate.api.servers.OtherSourcesDatabaseServer");
                server.setUid(WGAConfiguration.SINGLETON_SERVER_PREFIX + "de.innovationgate.webgate.api.servers.OtherSourcesDatabaseServer");
                addContentDB(config, dbPathElement.getParent(), server, migrationResult, configPath, fsDesignSource, path);
            }
        }

        // migrate personalisation dbs
        migrationResult.logInfo("Starting migration of personalisation dbs ...");
        Iterator persDBPaths = root.selectNodes("//personalisationdb/dbpath").iterator();
        while (persDBPaths.hasNext()) {
            Element dbPathElement = (Element) persDBPaths.next();
            String dbType = dbPathElement.getParent().elementTextTrim("type");
            String domain = dbPathElement.getParent().elementTextTrim("domain");
            String path = dbPathElement.getTextTrim();
            String user = determineUser(dbPathElement.getParent());
            String password = determinePassword(dbPathElement.getParent());
            // migration for mysql dbs
            if (path.startsWith("jdbc:mysql")) {
                mysqlServerCount = createMySqlServerAndDB(configPath, migrationResult, config, mysqlServerCount, dbServersByUniqueID, dbPathElement, path, user, password, true, fsDesignSource);
            }
            else if (dbType.contains("domino.remote")) {
                mysqlServerCount = createDominoServerAndDB(configPath, migrationResult, config, notesServerCount, dbServersByUniqueID, dbPathElement, path, user, password, true, fsDesignSource);
            }
            else if (dbType.contains("de.innovationgate.webgate.api.hsql")) {
                hsqlServerCount = createHSQLServerAndDB(configPath, migrationResult, config, hsqlServerCount, dbServersByUniqueID, dbPathElement, path, user, password, true, fsDesignSource);
            }
            else if (path.startsWith("jdbc:oracle:")) {
                oracleServerCount = createOracleServerAndDB(configPath, migrationResult, config, oracleServerCount, dbServersByUniqueID, dbPathElement, path, user, password, false, fsDesignSource);
            }
            else {
                // migrate other dbs to "other sources" server
                // migrate other dbs with same user/password combination to
                // "other sources" server
                DatabaseServer server = new DatabaseServer("de.innovationgate.webgate.api.servers.OtherSourcesDatabaseServer");
                server.setUid(WGAConfiguration.SINGLETON_SERVER_PREFIX + "de.innovationgate.webgate.api.servers.OtherSourcesDatabaseServer");
                addPersonalisationDB(config, dbPathElement.getParent(), server, migrationResult, null);
            }
        }

        // migrate first found access log
        migrationResult.logWarning("Accessloggers will not be migrated.");
                
        // migrate libraries
        migrationResult.logInfo("Migrating library mappings");
        String libraries = root.element("mappings").attributeValue("libraries", null);
        if (libraries != null && !libraries.trim().equals("")) {
        	config.getServerOptions().put(WGAConfiguration.SERVEROPTION_LIBRARIES, libraries);
        }
        
        // migrate mappings
        migrationResult.logInfo("Migrating filter mappings.");
        Iterator filterMappings = root.element("mappings").element("filtermappings").elementIterator("filtermapping");
        while (filterMappings.hasNext()) {
            Element filterMappingElement = (Element) filterMappings.next();
            FilterMapping mapping = new FilterMapping(filterMappingElement.attributeValue("filtername"), filterMappingElement.attributeValue("class"));
            Iterator patterns = filterMappingElement.elementIterator("filterurlpattern");
            while (patterns.hasNext()) {
                Element pattern = (Element) patterns.next();
                mapping.getUrlPatterns().add(pattern.attributeValue("urlpattern"));
            }
            Iterator params = filterMappingElement.elementIterator("filterinitparam");
            while (params.hasNext()) {
                Element param = (Element) params.next();
                mapping.getInitParameters().put(param.attributeValue("name"), param.attributeValue("value"));
            }
        }

        // migrate jobs
        migrationResult.logInfo("Migrating configured jobs & schedules.");
        Element scheduler = (Element) root.element("scheduler");
        if (scheduler != null) {
            config.getSchedulerConfiguration().setLoggingDir(scheduler.attributeValue("loggingdir", null));
            Iterator jobs = scheduler.elementIterator("job");
            while (jobs.hasNext()) {
                Element jobElement = (Element) jobs.next();
                Job job = new Job(jobElement.attributeValue("name"));
                String desc = jobElement.attributeValue("description", null);
                if (desc != null && !desc.trim().equalsIgnoreCase("(Enter description here)")) {
                    job.setDescription(desc);
                }
                Iterator options = jobElement.element("joboptions").elementIterator("option");
                Element option;
                while (options.hasNext()) {
                    option = (Element) options.next();
                    String value = option.attributeValue("value");
                    if (value != null && !value.trim().equalsIgnoreCase("(database containing the script module)") && !value.trim().equalsIgnoreCase("(Script module to execute)")) {
                        job.getOptions().put(option.attributeValue("name"), option.attributeValue("value"));
                    }
                }
                Iterator tasks = jobElement.element("tasks").elementIterator("task");
                while (tasks.hasNext()) {
                    Element taskElem = (Element) tasks.next();
                    Task task = new Task(taskElem.attributeValue("class"));
                    Iterator taskOptions = taskElem.attributeIterator();
                    while (taskOptions.hasNext()) {
                        Attribute attribute = (Attribute) taskOptions.next();
                        if (!attribute.getName().equals("name") && !attribute.getName().equals("class")) {
                            String value = attribute.getValue();
                            if (value != null && !value.trim().equalsIgnoreCase("(database containing the script module)") && !value.trim().equalsIgnoreCase("(Script module to execute)")) {
                                task.getOptions().put(attribute.getName(), attribute.getValue());
                            }
                        }
                    }
                    job.getTasks().add(task);
                }
                Iterator schedules = jobElement.element("schedules").elementIterator("schedule");
                SimpleDateFormat df = new SimpleDateFormat("dd.MM.yyyy hh:mm");
                while (schedules.hasNext()) {
                    Element scheduleElement = (Element) schedules.next();
                    Schedule schedule = new Schedule(scheduleElement.attributeValue("type"), scheduleElement.getText());
                    schedule.setEnabled(Boolean.parseBoolean(scheduleElement.attributeValue("enabled", "true")));
                    String sStarting = scheduleElement.attributeValue("starting", null);
                    if (sStarting != null && !sStarting.trim().equals("")) {
                        try {
                            schedule.setStartDate(df.parse(sStarting));
                        }
                        catch (ParseException e) {
                            migrationResult.logError("Unable to parse start date of job '" + job.getName() + "'.", e);
                        }
                    }
                    String sEnding = scheduleElement.attributeValue("ending", null);
                    if (sEnding != null && !sEnding.trim().equals("")) {
                        try {
                            schedule.setEndDate(df.parse(sEnding));
                        }
                        catch (ParseException e) {
                            migrationResult.logError("Unable to parse end date of job '" + job.getName() + "'.", e);
                        }
                    }
                    job.getSchedules().add(schedule);
                }

                config.getSchedulerConfiguration().getJobs().add(job);
            }
        }

        List<ValidationError> errors = config.validate();
        if (!errors.isEmpty()) {
            migrationResult.logError("Migration failed:");
            Iterator<ValidationError> it = errors.iterator();
            while (it.hasNext()) {
                migrationResult.logError(it.next().getMessage());
            }
        }
        else {

            // write the config once to apply simple-xml-api validation
            try {
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                WGAConfiguration.write(config, out);
                out.close();
                migrationResult.setConfig((WGAConfiguration)WGAConfiguration.read(new ByteArrayInputStream(out.toByteArray())));

            }
            catch (Exception e) {
                migrationResult.logError("Unable to serialize or deserialize configuration", e);
                if (e instanceof ConfigValidationException) {
                    errors = ((ConfigValidationException) e).getValidationErrors();
                    if (errors != null) {
                        Iterator<ValidationError> it = errors.iterator();
                        while (it.hasNext()) {
                            migrationResult.logError(it.next().getMessage());
                        }
                    }
                }
            }
            migrationResult.logInfo("Migrating of 'wga.xml' finished.");
        }

        return migrationResult;
    }

    private static void migrateDomains(MigrationResult migrationResult, WGAConfiguration config, Element domains) {
        Iterator domainIt = domains.elementIterator("domain");
        while (domainIt.hasNext()) {
            Element domain = (Element) domainIt.next();
            String domainName = domain.attributeValue("name");
            migrationResult.logInfo("Migrating domain '" + domainName + "'.");
            Domain domainConfig = null;
            if (domainName.equalsIgnoreCase("default")) {
                domainConfig = config.getDefaultDomain();
            }
            else {
                domainConfig = new Domain();
                domainConfig.setName(domainName);
            }
            domainConfig.setMaximumLoginAttempts(Integer.parseInt(domain.attributeValue("loginattempts")));
            domainConfig.setDefaultManager(domain.attributeValue("defaultmanager"));

            Iterator dbOptions = domain.element("defaultdboptions").elementIterator();
            boolean globalMailConfigured = config.getMailConfiguration().isConfigured();
            boolean globalMailHasBeenYetBeenConfigured = false;
            Map<String, String> authOptions = new HashMap<String, String>();
            while (dbOptions.hasNext()) {
                Element option = (Element) dbOptions.next();
                String name = option.attributeValue("name");
                String value = option.attributeValue("value");
                // collect authentication related options
                if (name.startsWith("auth.") || name.startsWith("jndi.") || name.equals("CertAuth") || name.equals("CRL") || name.startsWith("multi.")) {
                    authOptions.put(name, value);
                }
                else if (name.startsWith("workflow.") && !globalMailConfigured) {
                    // migrate mail related options
                    if (name.equals("workflow.mail.host")) {
                        globalMailHasBeenYetBeenConfigured = true;
                        config.getMailConfiguration().setServer(value);
                    }
                    else if (name.equals("workflow.mail.from")) {
                        config.getMailConfiguration().setFromAddress(value);
                    }
                    else if (name.equals("workflow.mail.rooturl.domain")) {
                        config.setRootURL(value);
                    }
                    else if (name.equals("workflow.mail.user")) {
                        config.getMailConfiguration().setUser(value);
                    }
                    else if (name.equals("workflow.mail.password")) {
                        config.getMailConfiguration().setPassword(value);
                    }
                    else if (name.equals("workflow.mail.transport.protocol") && value != null && value.equalsIgnoreCase("smtp")) {
                        // ignore this - smtp is default
                    }
                    else if (name.startsWith("workflow.mail.")) {
                        config.getMailConfiguration().getOptions().put(name.substring(9), value);
                    }
                }
                else if (name.startsWith("workflow.")) {
                    // global mailing has been configured so these options can
                    // be skipped
                    migrationResult.logInfo("skipping MailConfiguration related option '" + name + "' - value '" + value + "' on domain '" + domainConfig.toString()
                            + "' - global mail config will be used instead.");
                }
                else {
                    migrationResult.logWarning("DBOption '" + name + "' - value '" + value + "' on domain '" + domainConfig.toString() + "' has not been migrated.");
                }
            }

            if (globalMailHasBeenYetBeenConfigured) {
                migrationResult.logInfo("Global mailing was not configured yet, it has been configured from options on domain '" + domainConfig.toString() + "'.");
            }

            // migrate authentication related options
            String preConfig = authOptions.remove("auth.preconfig");
            if (preConfig != null) {
                List params = WGUtils.deserializeCollection(preConfig, ";");
                String type = (String) params.get(0);
                if (type.equals("ldap")) {
                    buildLDAPOptions(authOptions, params);
                }
                else if (type.equals("file")) {
                    buildFileOptions(authOptions, params);
                }
                else if (type.equals("domino")) {
                    buildDominoOptions(authOptions, params);
                    migrationResult.logWarning("Domino authentication on domain '" + domainName + "' will need to be configured manually after the migration. Choose a Domino Server as authentication source in admin client");
                }
                else if (type.equals("cs")) {
                    buildContentStoreAuthOptions(authOptions, params, domainConfig);
                }
                else if (type.equals("plugin")) {
                    buildPluginAuthOptions(authOptions, params);
                }
            }
            String authModule = authOptions.remove("auth.module");
            if (authModule != null) {
            	if (authModule.equals("multi")) {
            		domainConfig.createAuthenticationSource("de.innovationgate.webgate.api.auth.multi.MultiAuthModule");
            	} else {
            		domainConfig.createAuthenticationSource(authModule);
            	}
                domainConfig.getAuthenticationSource().getOptions().putAll(authOptions);
            }

            Iterator pubOptions = domain.element("defaultpublisheroptions").elementIterator();
            while (pubOptions.hasNext()) {
                Element option = (Element) pubOptions.next();
                String name = option.attributeValue("name");
                String value = option.attributeValue("value");
                migrationResult.logWarning("PublisherOption '" + name + "' - value '" + value + "' on domain '" + domainConfig.toString() + "' has not been migrated.");
            }

            // migrate first configured errorpage if not yet configured
            Element errorpage = domain.element("errorpage");
            boolean enabled = Boolean.parseBoolean(errorpage.attributeValue("enabled"));
            String code = errorpage.getText();
            if (code != null && !code.trim().equals("")) {
                if (config.getCustomErrorPage() == null || config.getCustomErrorPage().trim().equals("")) {
                    migrationResult.logInfo("Using errorpage of domain '" + domainName + "' as global errorpage.");
                    config.setCustomErrorPageEnabled(enabled);
                    config.setCustomErrorPage(code);
                }
                else {
                    migrationResult.logWarning("Second errorpage defined on domain '" + domainConfig.toString()
                            + "' has not been migrated bc. an errorpage has already been migrated from another domain.");
                }
            }

            if (!domainConfig.equals(config.getDefaultDomain())) {
                config.add(domainConfig);
            }
        }
    }

    private static int createHSQLServerAndDB(String configPath, MigrationResult migrationResult, WGAConfiguration config, int hsqlServerCount, Map<String, DatabaseServer> dbServersByUniqueID,
            Element dbPathElement, String path, String user, String password, boolean persDB, DesignSource fsDesignSource) {
        String serverPath = null;
        String dbPath = path;
        String search = path.replaceAll("\\\\", "/");
        if (search.contains("/")) {
            int lastSlashPos = search.lastIndexOf("/");
            serverPath = path.substring(0, lastSlashPos);
            dbPath = path.substring(lastSlashPos + 1);
        }
        DatabaseServer server = null;
        String id = null;
        if (serverPath != null) {
            id = serverPath + user + password;
        }
        else {
            id = "hsql" + user + password;
        }
        
        server = dbServersByUniqueID.get(id);
        if (server == null) {
            server = new DatabaseServer(HSQL_SERVER_CLASS);
            server.setTitle("hsql_" + hsqlServerCount);
            if (serverPath == null) {
                serverPath = "";
            }
            server.getOptions().put(DatabaseServer.OPTION_PATH, serverPath);
            hsqlServerCount += 1;
            config.add(server);
            dbServersByUniqueID.put(id, server);
        }
            
        if (persDB) {
            addPersonalisationDB(config, dbPathElement.getParent(), server, migrationResult, dbPath);   
        }
        else {
            addContentDB(config, dbPathElement.getParent(), server, migrationResult, configPath, fsDesignSource, dbPath);
        }
        
        return hsqlServerCount;

    }
    
    private static int createOracleServerAndDB(String configPath, MigrationResult migrationResult, WGAConfiguration config, int oracleServerCount, Map<String, DatabaseServer> dbServersByUniqueID,
            Element dbPathElement, String path, String user, String password, boolean persDB, DesignSource fsDesignSource) {
        
        DatabaseServer server = null;
        String id = null;

        id = "oracle" + user + password;
        server = dbServersByUniqueID.get(id);
        if (server == null) {
            server = new DatabaseServer(ORACLE_SERVER_CLASS);
            server.setTitle("oracle_" + oracleServerCount);
            if (user != null) {
                server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_USER, user);
            }
            if (password != null) {
                server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD, "[" + Base64.ENCODING_KEY +"] " + password);
            }

            oracleServerCount += 1;
            config.add(server);
            dbServersByUniqueID.put(id, server);
        }
            
        if (persDB) {
            addPersonalisationDB(config, dbPathElement.getParent(), server, migrationResult, path);   
        }
        else {
            addContentDB(config, dbPathElement.getParent(), server, migrationResult, configPath, fsDesignSource, path);
        }
        
        return oracleServerCount;

    }
    
    private static int createJDBCServerAndDB(String configPath, MigrationResult migrationResult, WGAConfiguration config, int jdbcServerCount, Map<String, DatabaseServer> dbServersByUniqueID,
            Element dbPathElement, String path, String user, String password, boolean persDB, DesignSource fsDesignSource) {
        
        DatabaseServer server = null;
        String id = null;

        id = "jdbc" + user + password;
        server = dbServersByUniqueID.get(id);
        if (server == null) {
            server = new DatabaseServer(JDBC_SERVER_CLASS);
            server.setTitle("jdbc_" + jdbcServerCount);
            jdbcServerCount += 1;
            config.add(server);
            if (user != null) {
                server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_USER, user);
            }
            if (password != null) {
                server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD, "[" + Base64.ENCODING_KEY +"] " + password);
            }
            dbServersByUniqueID.put(id, server);
        }
            
        if (persDB) {
            addPersonalisationDB(config, dbPathElement.getParent(), server, migrationResult, path);   
        }
        else {
            addContentDB(config, dbPathElement.getParent(), server, migrationResult, configPath, fsDesignSource, path);
        }
        
        return jdbcServerCount;

    }

    private static int createDominoServerAndDB(String configPath, MigrationResult migrationResult, WGAConfiguration config, int notesServerCount, Map<String, DatabaseServer> dbServersByUniqueID,
            Element dbPathElement, String path, String user, String password, boolean persDB, DesignSource fsDesignSource) {
        String[] pathTokens = path.split(";");
        if (pathTokens != null && pathTokens.length == 2) {
            String serverAddress = pathTokens[0];
            String dbPath = pathTokens[1];

            String id = serverAddress + user + password;
            DatabaseServer server = dbServersByUniqueID.get(id);
            if (server == null) {
                server = new DatabaseServer("de.innovationgate.webgate.api.domino.DominoDatabaseServer");
                server.setTitle("domino_" + notesServerCount);
                
                String serverPort = null;
                String serverPath = serverAddress;
                int colonIndex = serverPath.indexOf(":");
                if (colonIndex != -1) {
                    serverPort = serverPath.substring(colonIndex + 1);
                    serverPath = serverPath.substring(0, colonIndex);
                }

                server.getOptions().put(DatabaseServer.OPTION_PATH, serverPath);
                if (serverPort != null) {
                    server.getOptions().put(DatabaseServer.OPTION_PORT, serverPort);
                }
                notesServerCount += 1;
                if (user != null) {
                    server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_USER, user);
                }
                if (password != null) {
                    server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD, "[" + Base64.ENCODING_KEY +"] " + password);
                }
                config.add(server);
                dbServersByUniqueID.put(id, server);
            }
            
            if (persDB) {
                addPersonalisationDB(config, dbPathElement.getParent(), server, migrationResult, path);                
            }
            else {
                addContentDB(config, dbPathElement.getParent(), server, migrationResult, configPath, fsDesignSource,
                    dbPath);
            }
            
            migrationResult.logWarning("You will most likely need to setup Domino Authentication for the domains that contain domino databases. There is no implicit domino authentication any more in WGA5.");
            
        }
        else {
            migrationResult.logError("Unable to migrate content db '" + path + "' bc. notes path are invalid.");
        }
        
        return notesServerCount;
        
    }

    private static int createMySqlServerAndDB(String configPath, MigrationResult migrationResult, WGAConfiguration config, int mysqlServerCount, Map<String, DatabaseServer> dbServersByUniqueID,
            Element dbPathElement, String path, String user, String password, boolean persDB, DesignSource fsDesignSource) {

        String dbPath = path;
        String serverPath = null;
        String serverPort = null;

        if (dbPath.startsWith("jdbc:")) {
            int hostStart = dbPath.indexOf("//") + 2;
            int hostEnd = dbPath.indexOf("/", hostStart);
            serverPath = dbPath.substring(hostStart, hostEnd);
            dbPath = dbPath.substring(hostEnd + 1);
            int colonPos = serverPath.indexOf(":");
            if (colonPos != -1) {
                serverPort = serverPath.substring(colonPos + 1);
                serverPath = serverPath.substring(0, colonPos);
            }
        }

        DatabaseServer server = null;
        String id = serverPath + serverPort + user + password;
        server = dbServersByUniqueID.get(id);
        if (server == null) {
            server = new DatabaseServer(MYSQL_SERVER_CLASS);
            server.setTitle("mysql_" + mysqlServerCount);
            if (serverPath != null) {
                server.getOptions().put(DatabaseServer.OPTION_PATH, serverPath);
            }
            if (serverPort != null) {
                server.getOptions().put(DatabaseServer.OPTION_PORT, serverPort);
            }
            mysqlServerCount += 1;
            if (user != null) {
                server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_USER, user);
            }

            if (password != null) {
                server.getOptions().put(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD, "[" + Base64.ENCODING_KEY +"] " + password);
            }
            config.add(server);
            dbServersByUniqueID.put(id, server);
        }
            
        if (persDB) {
            addPersonalisationDB(config, dbPathElement.getParent(), server, migrationResult, dbPath);
        }
        else {
            addContentDB(config, dbPathElement.getParent(), server, migrationResult, configPath, fsDesignSource, dbPath);
        }

        return mysqlServerCount;
    }

    private static void addPersonalisationDB(WGAConfiguration config, Element persDBElement, DatabaseServer server, MigrationResult migrationResult, String dbPath) {
        // default props for all content dbs (full cs && custom)
        String type = persDBElement.elementText("type");
        String domain = persDBElement.elementText("domain");
        boolean enabled = Boolean.parseBoolean(persDBElement.attributeValue("enabled"));
        boolean lazy = Boolean.parseBoolean(persDBElement.attributeValue("lazyconnect"));
        if (dbPath == null) {
	        dbPath = persDBElement.elementText("dbpath");
	        // create relative path if dbserver defines one
	        String serverPath = server.getOptions().get(DatabaseServer.OPTION_PATH);
	        if (serverPath != null) {
	            int relativeDbPathStart = dbPath.indexOf(serverPath);
	            if (relativeDbPathStart != -1) {
	                relativeDbPathStart += serverPath.length();
	                dbPath = dbPath.substring(relativeDbPathStart + 1);
	            }
	        }
        }
        PersonalisationDatabase persDB = new PersonalisationDatabase(server.getUid(), type);
        persDB.getDatabaseOptions().put(Database.OPTION_PATH, dbPath);
        migrateOptions(persDB, persDBElement, migrationResult);
        
        // lookup domain by title since domain has a uid
        Domain domainConfig = null;
        Iterator<Domain> domains = config.getDomains().iterator();
        while (domains.hasNext()) {
        	Domain tempDomainConfig = domains.next();
        	if (tempDomainConfig.getName() != null && tempDomainConfig.getName().equalsIgnoreCase(domain)) {
        		domainConfig = tempDomainConfig;
        		break;
        	}
        }
        if (domainConfig != null) {
            domainConfig.setPersonalisation(persDB);
        }
        else {
            migrationResult.logError("Unable to migrate personalisation db for domain '" + domain + "' bc. domain does not exist.");
        }
    }

    private static void buildPluginAuthOptions(Map<String, String> options, List<String> params) {
        options.put("auth.module", "de.innovationgate.wgpublisher.auth.PluginAuthModule");
        options.put("auth.cs.dbkey", params.get(1));
        
        /* Impossible for plugin auth
        if (params.size() > 2) {
            options.put("auth.cs.rootdoc.users", params.get(2));
        }
        if (params.size() > 3) {
            options.put("auth.cs.rootdoc.groups", params.get(3));
        }*/
    }

    private static void buildContentStoreAuthOptions(Map<String, String> options, List<String> params, Domain domain) {
        options.put("auth.module", "de.innovationgate.wgpublisher.auth.CSAuthModule");
        options.put("auth.cs.dbkey", params.get(1));
        options.put("auth.cs.rootdoc.users", params.get(2));
        options.put("auth.cs.rootdoc.groups", params.get(3));
        if (params.size() >= 6) {
            if (params.get(4).trim().equals("true")) {
                options.put("auth.cs.certauth", "true");
                _domainsWithCertAuthEnabled .add(domain.getUid());
            }
            
            options.put("auth.cs.crl", params.get(6));
        }
        
        
    }

    private static String determineUser(Element contentDBElement) {
        Element login = contentDBElement.element("login");
        String user = null;

        // first try retrieve user from db
        if (login != null) {
            String value = login.attributeValue("username");
            if (value != null && !value.trim().equalsIgnoreCase("")) {
                user = value;
            }
        }

        // second try retrieve user from domain
        if (user == null) {
            String domain = contentDBElement.elementText("domain");
            if (domain != null) {
                Element domainsElement = contentDBElement.getParent().getParent().element("domains");
                List domainElements = domainsElement.selectNodes("domain[@name='" + domain + "']");
                if (domainElements.size() > 0) {
                    Element domainElement = (Element) domainElements.get(0);
                    login = domainElement.element("login");
                    if (login != null) {
                        String value = login.attributeValue("username");
                        if (value != null && !value.trim().equalsIgnoreCase("")) {
                            user = value;
                        }
                    }

                }
            }
        }

        return user;
    }

    private static String determinePassword(Element contentDBElement) {
        Element login = contentDBElement.element("login");
        String password = null;

        // first try retrieve user from db
        if (login != null) {
            String value = login.attributeValue("password");
            if (value != null && !value.trim().equalsIgnoreCase("")) {
                password = value;
            }
        }

        // second try retrieve user from domain
        if (password == null) {
            String domain = contentDBElement.elementText("domain");
            if (domain != null) {
                Element domainsElement = contentDBElement.getParent().getParent().element("domains");
                List domainElements = domainsElement.selectNodes("domain[@name='" + domain + "']");
                if (domainElements.size() > 0) {
                    Element domainElement = (Element) domainElements.get(0);
                    login = domainElement.element("login");
                    if (login != null) {
                        String value = login.attributeValue("password");
                        if (value != null && !value.trim().equalsIgnoreCase("")) {
                            password = value;
                        }
                    }

                }
            }
        }

        return password;
    }

    private static void addContentDB(WGAConfiguration config, Element contentDBElement, DatabaseServer server, MigrationResult migrationResult, String configPath, DesignSource fsDesignSource,
            String dbPath) {
        // default props for all content dbs (full cs && custom)
        String type = contentDBElement.elementText("type");
        String dbkey = contentDBElement.elementText("dbkey");
        String sDomain = contentDBElement.elementText("domain");

        // find domain by old key (title)
        Domain domain = null;
        Iterator<Domain> domains = config.getDomains().iterator();
        while (domains.hasNext()) {
            domain = domains.next();
            if (domain.getName() != null && domain.getName().equalsIgnoreCase(sDomain)) {
                break;
            }
            else {
                domain = null;
            }
        }
        if (domain == null) {
            migrationResult.logWarning("Domain '" + sDomain + "' not found. Adding content database '" + dbkey + "' to default domain.");
            domain = config.getDefaultDomain();
        }

        String title = contentDBElement.elementText("title");
        boolean enabled = Boolean.parseBoolean(contentDBElement.attributeValue("enabled"));
        boolean lazy = Boolean.parseBoolean(contentDBElement.attributeValue("lazyconnect"));

        if (FULL_CONTENT_STORE_IMPLEMENTATIONS.contains(type)) {
            // migrate full cs

            // determine default language - fallback to current system locale
            String defaultLang = Locale.getDefault().getLanguage();
            List defaultLangOptions = contentDBElement.element("dboptions").selectNodes("option[@name='DefaultLanguage']");
            if (defaultLangOptions.size() > 0) {
                defaultLang = ((Element) defaultLangOptions.get(0)).attributeValue("value");
            }
            else {
                migrationResult.logWarning("DefaultLanguage was not defined on db '" + dbkey + "' using current system locale '" + defaultLang + "' as default language.");
            }

            ContentStore cs = new ContentStore(server.getUid(), domain.getUid(), type, dbkey, defaultLang);
            cs.setTitle(title);
            cs.setEnabled(enabled);
            cs.setLazyConnecting(lazy);
            cs.getDatabaseOptions().put(Database.OPTION_PATH, dbPath);

            // determine design
            migrateDesign(contentDBElement, cs, config, configPath, fsDesignSource, migrationResult);

            // options
            migrateOptions(cs, contentDBElement, migrationResult);

            migrateClientRestrictions(cs, contentDBElement);

            // stored queries are not supported anymore - log warnings
            Iterator storedQueries = contentDBElement.element("storedqueries").elementIterator();
            if (storedQueries.hasNext()) {
                migrationResult.logWarning("Database '" + dbkey + "' defines stored queries. This feature is not supported anymore and will not be migrated.");
            }

            // migrate item & meta mappings
            Iterator itemMappings = contentDBElement.element("fieldmappings").elementIterator();
            while (itemMappings.hasNext()) {
                Element mapping = (Element) itemMappings.next();
                String mappingType = null;
                if (mapping.getName().equals("itemmapping")) {
                    mappingType = FieldMapping.TYPE_ITEM;
                }
                else {
                    mappingType = FieldMapping.TYPE_META;
                }
                FieldMapping fieldMapping = new FieldMapping(mappingType, mapping.attributeValue("name"), mapping.attributeValue("expression"));
                cs.getFieldMappings().add(fieldMapping);
            }

            // migrate lucene config
            Element lucene = contentDBElement.element("lucene");
            LuceneIndexConfiguration luceneConfig = cs.getLuceneIndexConfiguration();
            luceneConfig.setEnabled(Boolean.parseBoolean(lucene.attributeValue("enabled", "false")));
            Iterator itemRules = lucene.element("itemrules").elementIterator("itemrule");
            while (itemRules.hasNext()) {
                Element itemRuleElement = (Element) itemRules.next();
                String ruleExpression = itemRuleElement.getTextTrim();
                if (!ruleExpression.equals(LuceneIndexItemRule.DEFAULT_RULE.getItemExpression())) {
                    LuceneIndexItemRule rule = new LuceneIndexItemRule(ruleExpression, itemRuleElement.attributeValue("indextype"), itemRuleElement.attributeValue("contenttype"));
                    rule.setSortable(Boolean.parseBoolean(itemRuleElement.attributeValue("sortable")));
                    rule.setBoost(Float.parseFloat(itemRuleElement.attributeValue("boost", "1.0")));
                    luceneConfig.getItemRules().add(rule);
                }
            }
            Iterator fileRules = lucene.element("filerules").elementIterator("filerule");
            while (fileRules.hasNext()) {
                Element fileRuleElement = (Element) fileRules.next();
                String ruleExpression = fileRuleElement.getTextTrim();
                if (!ruleExpression.equals(LuceneIndexFileRule.DEFAULT_RULE.getFilePattern())) {
                    LuceneIndexFileRule rule = new LuceneIndexFileRule(ruleExpression);
                    rule.setFileSizeLimit(Integer.parseInt(fileRuleElement.attributeValue("filesizelimit")));
                    rule.setIncludedInAllContent(Boolean.parseBoolean(fileRuleElement.attributeValue("includedinallcontent")));
                    rule.setBoost(Float.parseFloat(fileRuleElement.attributeValue("boost", "1.0")));
                    luceneConfig.getFileRules().add(rule);
                }
            }
            
            // Migrate WebDAV shares
            Element sharesRoot = contentDBElement.element("shares");
            if (sharesRoot != null) {
                Iterator shares = sharesRoot.elementIterator("share");
                while (shares.hasNext()) {
                    Element share = (Element) shares.next();
                    String name = share.attributeValue("name");
                    if (!WGUtils.isEmpty(name)) {
                        name = cs.getKey() + "-" + name;
                    }
                    else {
                        name = cs.getKey();
                    }
                    
                    Share newShare = new Share();
                    newShare.setName(name);
                    newShare.setImplClassName("de.innovationgate.enterprise.modules.contentshares.WebDAVContentShareModuleDefinition");
                    newShare.getOptions().put("Database", cs.getKey());
                    newShare.getOptions().put("RootType", share.attributeValue("parenttype"));
                    newShare.getOptions().put("RootName", share.attributeValue("parentname"));
                    
                    String unknownCtTreatment;
                    int oldCtTreatment = Integer.parseInt(share.attributeValue("unknownctmode"));
                    if (oldCtTreatment == 0) {
                        unknownCtTreatment = "ignore";
                    }
                    else if (oldCtTreatment == 1) {
                        unknownCtTreatment = "folder";
                    }
                    else {
                        unknownCtTreatment = "fileorfolder";
                    }
                    newShare.getOptions().put("UnknownCtTreatment", unknownCtTreatment);
                    
                    newShare.getOptions().put("FolderContentType", share.attributeValue("folderct"));
                    newShare.getOptions().put("FolderOperations", share.attributeValue("folderactions"));
                    newShare.getOptions().put("FileContentType", share.attributeValue("filect"));
                    newShare.getOptions().put("FileOperations", share.attributeValue("fileactions"));
                    
                    List options = WGUtils.deserializeCollection(share.attributeValue("options"), ",");
                    if (options.contains("versioning")) {
                        newShare.getOptions().put("Versioning", "true");
                    }
                    
                    config.getShares().add(newShare);                    
                }
            }
            


            config.add(cs);
        }
        else {
            
            // Transform custom types to new special types for database server
            if (type.equals(ENHANCED_JDBC_DB_CLASS)) {
                if (server.getImplClassName().equals(MYSQL_SERVER_CLASS)) {
                    type = "de.innovationgate.webgate.api.mysql.MySqlEnhancedQuerySource";
                }
                else if (server.getImplClassName().equals(HSQL_SERVER_CLASS)) {
                    type = "de.innovationgate.webgate.api.hsql.HsqlEnhancedQuerySource";
                }
                else if (server.getImplClassName().equals(ORACLE_SERVER_CLASS)) {
                    type = "de.innovationgate.webgate.api.oracle.OracleEnhancedQuerySource";
                }
            }
            else if (type.equals(QUERY_JDBC_DB_CLASS)) {
                if (server.getImplClassName().equals(MYSQL_SERVER_CLASS)) {
                    type = "de.innovationgate.webgate.api.mysql.MySqlQuerySource";
                }
                else if (server.getImplClassName().equals(HSQL_SERVER_CLASS)) {
                    type = "de.innovationgate.webgate.api.hsql.HsqlQuerySource";
                }
                else if (server.getImplClassName().equals(ORACLE_SERVER_CLASS)) {
                    type = "de.innovationgate.webgate.api.oracle.OracleQuerySource";
                }
            }
            
            // migrate custom db
            ContentDatabase contentDB = new ContentDatabase(server.getUid(), domain.getUid(), type, dbkey);
            contentDB.setTitle(title);
            contentDB.setEnabled(enabled);
            contentDB.setLazyConnecting(lazy);
            contentDB.getDatabaseOptions().put(Database.OPTION_PATH, dbPath);
            migrateOptions(contentDB, contentDBElement, migrationResult);
            migrateClientRestrictions(contentDB, contentDBElement);
            config.add(contentDB);
        }
    }

    private static void migrateDesign(Element contentDBElement, ContentStore cs, WGAConfiguration config, String configPath, DesignSource fsDesignSource, MigrationResult migrationResult) {
        Element designElement = contentDBElement.element("design");
        DesignConfiguration oldConfig = new DesignConfiguration(designElement);
        if (designElement != null) {

            if (oldConfig.getProvider().equals("sync")) {
                String designLocation = oldConfig.getDetailInfo();
                if (designLocation != null && !designLocation.trim().equals("")) {
                    File designDir = new File(configPath, designLocation);
                    String defaultFSDesignRoot = System.getProperty(WGAConfiguration.SYSPROP_DESIGN_ROOT);
                    File defaultFSSourceDir = null;
                    if (defaultFSDesignRoot != null && (defaultFSDesignRoot.startsWith("/") || defaultFSDesignRoot.contains(":"))) {
                    	// default fsDesignRoot is absolute
                    	defaultFSSourceDir = new File(defaultFSDesignRoot);
                    } else if (defaultFSDesignRoot != null) {
                    	// default fsDesignRoot is relative
                    	defaultFSSourceDir = new File(configPath, defaultFSDesignRoot);                    	
                    } else {
                    	// default fsDesignRoot is not set
                    	defaultFSSourceDir = new File(configPath, WGAConfiguration.DEFAULT_DESIGNROOT);
                    }
                    
                    if (!designDir.exists() && !designLocation.startsWith("/") && !designLocation.contains(":")) {
                    	// if design location is relativ
                    	// try to find designDir in default FSSource               	
                    	designDir = new File(defaultFSSourceDir, designDir.getName());
                    }
                    if (!designDir.exists()) {
                        migrationResult.logWarning("Unable to migrate design of content store '" + cs.getKey() + "'. Cannot find design directory '" + designLocation + "'.");
                        return;
                    }

                    Design design;
                    // Look if the design dir is at the proposed position under
                    // default FSSource
                    if (designDir.getParentFile().equals(defaultFSSourceDir)) {
                        design = new Design(Constants.DESIGNCOL_FILESYSTEM, designDir.getName());
                    }
                    else {
                        // The design is not at the default position. We
                        // register it as "additional dir"
                        int idx = 1;

                        String addDirName = null;
                        String otherDir = null;
                        idx = 0;
                        do {
                            idx++;
                            addDirName = designDir.getName() + idx;
                            otherDir = fsDesignSource.getOptions().get("additionaldir." + addDirName);
                        } while (otherDir != null);

                        fsDesignSource.getOptions().put("additionaldir." + addDirName, designDir.getAbsolutePath());
                        design = new Design(fsDesignSource.getUid(),  "additional:" + addDirName);
                    }

                    design.getOptions().put("sync", String.valueOf(oldConfig.getMode().equals("full")));
                    design.getOptions().put("autoupdate", String.valueOf(oldConfig.isAutoUpdate()));
                    design.getOptions().put("designkey", String.valueOf(oldConfig.getKey()));

                    cs.setDesign(design);

                }
            }
            else if (oldConfig.getProvider().equals("db")) {
                String dbDesignReference = oldConfig.getKey();
                if (dbDesignReference != null) {
                    Design design = null;
                    if (dbDesignReference.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                        String pluginInstallationKey = dbDesignReference.substring(PluginConfig.PLUGIN_DBKEY_PREFIX.length());
                        design = new Design(Constants.DESIGNCOL_PLUGIN, pluginInstallationKey);

                    }
                    else {
                        // find design provider db and migrate design config
                        // from there
                        ContentStore providerCS = (ContentStore) config.getContentDatabase(dbDesignReference);
                        if (providerCS == null) {
                            migrationResult.logError("Unable to migrate design of content store '" + cs.getKey() + "'. Design provider db '" + dbDesignReference + "' not found in wga.xml.");
                        }
                        // If the provider cs has no external design or it
                        // syncs,
                        // we must connect to it with db connection provider
                        else if (providerCS.getDesign() == null || "true".equals(providerCS.getDesign().getOptions().get("sync"))) {
                            design = new Design(Constants.DESIGNCOL_DB, dbDesignReference);
                        }
                        // Else we just connect to the same design with the same
                        // options
                        else {
                            design = new Design(providerCS.getDesign().getSource(), providerCS.getDesign().getName());
                            design.getOptions().putAll(providerCS.getDesign().getOptions());
                        }
                    }

                    if (design != null) {
                        if (design.getSource().equals(Constants.DESIGNCOL_DB)) {
                            design.getOptions().put("crossloginmode", oldConfig.getMode());
                            design.getOptions().put("variants", String.valueOf(oldConfig.isLookupDesignVariants()));
                        }
                        cs.setDesign(design);
                    }

                }
            }
        }
    }

    private static void migrateOptions(Database db, Element dbElement, MigrationResult migrationResult) {

        // Migrate database options;

        Iterator options = dbElement.element("dboptions").elementIterator("option");
        while (options.hasNext()) {
            Element option = (Element) options.next();
            String name = option.attributeValue("name");
            String value = option.attributeValue("value");
            

            if (name != null && name.trim().equalsIgnoreCase("(Enter option name here)")) {
            	continue;            	
            }
            if (value != null && value.trim().equalsIgnoreCase("(Enter option value here)")) {
            	continue;
            }
            
            if (!name.equalsIgnoreCase(("DefaultLanguage"))) {
                db.getDatabaseOptions().put(name, value);
            }
        }
        
        // Migrate publisher options;

        options = dbElement.element("publisheroptions").elementIterator("option");
        if (options.hasNext()) {
            if (db instanceof ContentDatabase) {
                ContentDatabase contentdb = (ContentDatabase) db;
                while (options.hasNext()) {
                    Element option = (Element) options.next();
                    String name = option.attributeValue("name");
                    String value = option.attributeValue("value");
        
            if (name != null && name.trim().equalsIgnoreCase("(Enter option name here)")) {
            	continue;            	
            }
            if (value != null && value.trim().equalsIgnoreCase("(Enter option value here)")) {
            	continue;
            }
            
                    String defaultValue = REMOVABLE_DEFAULT_PUBLISHER_OPTIONS_CS.get(name);
                    if (defaultValue != null && defaultValue.equals(value)) {
                        // skip this option - its default
                    }
                    else {
                        contentdb.getPublisherOptions().put(name, value);
                    }
                }
            }
            else {
                migrationResult.logWarning("Publisher options for personalisation databases are not migrated");
            }
        }

        // Set CS version to 3.0 where optimized file handling not explicitly set
        // Otherwise remove the "opt file handling" option since this is detected automatically in WGA5
        if (db.getImplClassName().equals("de.innovationgate.webgate.api.jdbc.WGDatabaseImpl") || db.getImplClassName().equals("de.innovationgate.webgate.api.mysql.WGDatabaseImpl")
                || db.getImplClassName().equals("de.innovationgate.webgate.api.hsql.WGDatabaseImpl") || db.getImplClassName().equals("de.innovationgate.webgate.api.oracle.WGDatabaseImpl")) {

            if (!db.getDatabaseOptions().containsKey("UseOptimizedFileHandling") || !Boolean.valueOf(db.getDatabaseOptions().get("UseOptimizedFileHandling"))) {
                db.getDatabaseOptions().put("ContentStoreVersion", "3.0");
            }
            
            db.getDatabaseOptions().remove("UseOptimizedFileHandling");
        }
        
        // Some special operations on the poptions of full content stores
        if (db instanceof ContentDatabase && FULL_CONTENT_STORE_IMPLEMENTATIONS.contains(db.getImplClassName())) {
            ContentDatabase contentdb = (ContentDatabase) db;
            
            // Set the option to use the unique name on content documents. New CS should prefer the unique name on the struct.
            contentdb.getPublisherOptions().put("UniqueNamesOnContents", "true");
            
            // Migrate pers mode from persdb if not already explicitly set
            if (!contentdb.getPublisherOptions().containsKey("PersMode")) {
                int persMode = Constants.PERSMODE_AUTO;
                int persStatMode = Constants.PERSSTATMODE_OFF; 
                Element persDBElement = (Element) dbElement.getDocument().selectSingleNode("/wga/personalisationdbs/personalisationdb[domain='" + contentdb.getDomain() + "']");
                if (persDBElement != null) {
                    persMode = translatePersMode(persDBElement.element("persconfig").attributeValue("mode"));
                    persStatMode = translatePersStatMode(persDBElement.element("persconfig").attributeValue("statistics"));
                }
                contentdb.getPublisherOptions().put("PersMode", String.valueOf(persMode));
                contentdb.getPublisherOptions().put("PersStatMode", String.valueOf(persStatMode));
            }
            

        }
    }

    private static int translatePersMode(String mode) {
        
        if (mode != null) {
            if (mode.equals("auto")) {
                return Constants.PERSMODE_AUTO;
            }
            else if (mode.equals("login")) {
                return Constants.PERSMODE_LOGIN;
            }
            else if (mode.equals("custom")) {
                return Constants.PERSMODE_CUSTOM;
            }
        }

        // The default
        return Constants.PERSMODE_AUTO;
        
    }
    
    private static int translatePersStatMode(String mode) {
        
        if (mode != null) {
            if (mode.equals("off")) {
                return Constants.PERSSTATMODE_OFF;
            }
            else if (mode.equals("session")) {
                return Constants.PERSSTATMODE_SESSION;
            }
            else if (mode.equals("hit")) {
                return Constants.PERSSTATMODE_HIT;
            }
        }

        // The default
        return Constants.PERSSTATMODE_OFF;
        
    }

    private static void migrateClientRestrictions(ContentDatabase db, Element dbElement) {
        Element clientRestrictions = dbElement.element("clientrestrictions");
        db.setClientRestrictionsEnabled(Boolean.parseBoolean(clientRestrictions.attributeValue("enabled")));

        Iterator restrictions = clientRestrictions.elementIterator("restriction");
        while (restrictions.hasNext()) {
            Element restrictionElement = (Element) restrictions.next();
            ClientRestriction restriction = new ClientRestriction(restrictionElement.attributeValue("type"));
            restriction.setHostIP(restrictionElement.attributeValue("hostIP", null));
            restriction.setNetwork(restrictionElement.attributeValue("network", null));
            restriction.setNetmask(restrictionElement.attributeValue("netmask", null));
            restriction.setStartIP(restrictionElement.attributeValue("startIP", null));
            restriction.setEndIP(restrictionElement.attributeValue("endIP", null));
            db.getClientRestrictions().add(restriction);
        }
    }

    private static void buildDominoOptions(Map options, List params) {
        options.put("auth.module", "de.innovationgate.webgate.api.auth.DominoAuthenticationModule");
        options.put("auth.host", params.get(1) + ":" + params.get(2));
        options.put("auth.sso", params.get(3));
    }

    private static void buildFileOptions(Map options, List params) {
        options.put("auth.module", "de.innovationgate.webgate.api.auth.FileAuthenticationModule");
        options.put("auth.file", params.get(1));
    }

    private static void buildLDAPOptions(Map options, List params) {
        options.put("auth.module", "de.innovationgate.webgate.api.auth.JndiAuthenticationModule");

        String ldapPort = (String) params.get(2);
        if (ldapPort.equals("636")) {
            options.put("jndi.path", "ldaps://" + params.get(1) + ":" + params.get(2));
        }
        else {
            options.put("jndi.path", "ldap://" + params.get(1) + ":" + params.get(2));
        }

        options.put("jndi.basenode.people", params.get(4));
        options.put("jndi.basenode.groups", params.get(5));
        options.put("jndi.servertype", params.get(3));
    }

}
