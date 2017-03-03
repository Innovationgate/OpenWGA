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

import java.beans.PropertyDescriptor;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.security.BCrypt10HashingScheme;
import de.innovationgate.utils.security.SHA1HashingScheme;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.LogLevel;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.model.ValidationError;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.types.HashingSchemeType;

/**
 * The complete OpenWGA configuration contained in csconfig.xml
 */
@Root(strict=false)
public class WGAConfiguration extends ConfigBean {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public class CachingOptionReader {
        
        private Map<String,Object> _cache = new HashMap<String, Object>();
    
        public Object readOptionValueOrDefault(Map<String,String> options, String optionName, ModuleDefinition modDef) throws OptionConversionException {
            
            String cacheKey = System.identityHashCode(options) + "//" + optionName;
            Object cachedValue = _cache.get(cacheKey);
            if (cachedValue != null) {
                return cachedValue;
            }
            
            OptionReader reader = OptionReader.create(options, modDef);
            Object value = reader.readOptionValueOrDefault(optionName);
            if (value != null) {
                _cache.put(cacheKey, value);
            }
            return value;
        }

    }
	
	public static final String SYSPROP_DESIGN_ROOT = "de.innovationgate.wga.migration.design.root";
	public static final String SYSPROP_LUCENE_ROOT = "de.innovationgate.wga.migration.lucene.root";
	public static final String SYSPROP_WARNINGS_ON_SERVER_CONSOLE = "de.innovationgate.wga.outputWarningsOnConsole";
	
	public static final String DEFAULT_DESIGNROOT = "designs";

	public static final String UID_DEFAULT_DOMAIN = "default";
	public static final String UID_EMBEDDED_DBSERVER = WGAConfiguration.SINGLETON_SERVER_PREFIX + "de.innovationgate.wgpublisher.servers.HsqlDefaultDatabaseServer";
	public static final String UID_DESIGNSOURCE_DB = Constants.DESIGNCOL_DB;
	public static final String UID_DESIGNSOURCE_FILESYSTEM = Constants.DESIGNCOL_FILESYSTEM;
	public static final String UID_DESIGNSOURCE_PLUGIN = Constants.DESIGNCOL_PLUGIN;
	
	public static final String SERVEROPTION_SERVICE_APIS_PREFIX = "Services.API.";
	
	public static final String SERVEROPTION_SERVICE_WEBSERVICES = "Services.WebServicesEnabled";
	public static final String SERVEROPTIONDEFAULT_SERVICE_WEBSERVICES = Boolean.TRUE.toString();

    public static final String SERVEROPTION_SERVICE_ADMIMPAGE = "Services.AdminPageEnabled";
	public static final String SERVEROPTIONDEFAULT_SERVICE_ADMINPAGE = Boolean.TRUE.toString();
 
    public static final String SERVEROPTION_SERVICE_STARTPAGE = "Services.StartPageEnabled";
    public static final String SERVEROPTIONDEFAULT_SERVICE_STARTPAGE = Boolean.TRUE.toString();

	public static final String SERVEROPTION_SERVICE_PROTOCOL_IMPLEMENTATION = "Services.ProtocolImplementation";
	
	public static final String SERVEROPTION_SERVICE_INTEGRATEDJMX_ENABLED = "Services.IntegratedJMX.Enabled";
	public static final String SERVEROPTION_SERVICE_INTEGRATEDJMX_SSL = "Services.IntegratedJMX.SSL";
	public static final String SERVEROPTION_SERVICE_INTEGRATEDJMX_HOST = "Services.IntegratedJMX.Host";
	public static final String SERVEROPTION_SERVICE_INTEGRATEDJMX_PORT_REGISTRY = "Services.IntegratedJMX.Port.Registry";
	public static final String SERVEROPTION_SERVICE_INTEGRATEDJMX_PORT_SERVICE = "Services.IntegratedJMX.Port.Service";
	public static final String SERVEROPTION_SERVICE_INTEGRATEDJMX_LEGACY_DBCP = "Services.IntegratedJMX.Port.LegacyDBCP";
	
	public static final String SERVEROPTION_SERVICE_WEBSOCKETS = "Services.WebSockets.Enabled";
	
	public static final int SERVEROPTIONDEFAULT_SERVICE_INTEGRATEDJMX_PORT_SERVICE = 29802;
	public static final int SERVEROPTIONDEFAULT_SERVICE_INTEGRATEDJMX_PORT_REGISTRY = 1558;
    
    public static final String SERVEROPTION_LOG_WARNINGS = "Log.WarningsEnabled";
    public static final String SERVEROPTIONDEFAULT_LOG_WARNINGS = Boolean.TRUE.toString();
    
    public static final String SERVEROPTION_LOG_WARNINGS_TML = "Log.WarningsInTML";

    public static final String SERVEROPTIONDEFAULT_LOG_WARNINGS_TML = Constants.WARNINGS_TML_AS_HTML;
    
    public static final String SERVEROPTION_LOG_WARNINGS_APPLOG = "Log.WarningsInApplog";
    public static final String SERVEROPTIONDEFAULT_LOG_WARNINGS_APPLOG = Boolean.FALSE.toString();

    public static final String SERVEROPTION_LOG_APPLOG_LEVEL = "Log.ApplogLevel";
    public static final String SERVEROPTIONDEFAULT_LOG_APPLOG_LEVEL = LogLevel.LEVEL_INFO.getIdentifier();
    
    public static final String SERVEROPTION_LOG_PERMANENTLOG_DIR = "Log.PermanentLogDirectory";
    
    public static final String SERVEROPTION_CACHE_WEBTML_SIZE = "Cache.WebTMLSize";
    public static final String SERVEROPTIONDEFAULT_CACHE_WEBTML_SIZE = "10000";
    
    public static final String SERVEROPTION_CACHE_DESIGN_SIZE = "Cache.DesignFiles.Size";
    public static final String SERVEROPTIONDEFAULT_CACHE_DESIGN_SIZE = "1000";
    
    public static final String SERVEROPTION_CACHE_DESIGN_NO_BACKGROUND_CHANGES = "Cache.DesignFiles.NoBackgroundChanges";
    public static final String SERVEROPTIONDEFAULT_CACHE_DESIGN_NO_BACKGROUND_CHANGES = "false";
    
    public static final String SERVEROPTION_CACHE_STATIC_RESOURCES_EXPIRATION = "Cache.StaticResourcesExpiration";
    public static final String SERVEROPTIONDEFAULT_CACHE_STATIC_RESOURCES_EXPIRATION = "1440";
    
    public static final String SERVEROPTION_CACHE_USERCACHE_LATENCY = "Cache.UsercacheLatency";
    public static final String SERVEROPTIONDEFAULT_CACHE_USERCACHE_LATENCY = "60";
    
    public static final String SERVEROPTION_ENCODING_OUTPUT = "Encoding.Output";
    public static final String SERVEROPTIONDEFAULT_ENCODING_OUTPUT = "UTF-8";
    
    public static final String SERVEROPTION_ENCODING_DESIGN = "Encoding.Design";
    
    public static final String SERVEROPTION_WEBTML_OUTPUT_BUFFER = "WebTML.OutputBuffer";
    public static final String SERVEROPTIONDEFAULT_WEBTML_OUTPUT_BUFFER = "8";
    
    
    public static final String SERVEROPTION_WEBTML_HEADER = "WebTML.Header";
    public static final String SERVEROPTION_RESOURCES_DEFAULTDB = "Resources.DefaultDB";
    public static final String SERVEROPTION_RESOURCES_FAVICON = "Resources.FavIcon";

    public static final String SERVEROPTION_SECURITY_PASSWORD_ENCODING = "Security.PasswordEncoding";
    public static final String SERVEROPTIONDEFAULT_SECURITY_PASSWORD_ENCODING = Base64.ENCODING_KEY;
    
    public static final String SERVEROPTION_LIBRARIES = "Libraries";
    public static final String SERVEROPTIONDEFAULT_LIBRARIES = "";
    
    public static final String SERVEROPTION_DEFAULT_WORKFLOW_ENGINE = "Resources.DefaultWorkflowEngine";
    public static final String SERVEROPTION_SERVER_NAME = "Basic.ServerName";
    public static final String SERVEROPTION_ROOT_URL = "Basic.RootURL";
    
    
    
    public static final String SERVEROPTION_EXTERNAL_FILE_SERVING_ENABLED = "ExternalFileServing.Enabled";
    public static final String SERVEROPTION_EXTERNAL_FILE_SERVING_DIRECTORY = "ExternalFileServing.Directory";
    public static final String SERVEROPTION_EXTERNAL_FILE_SERVING_THRESHOLD = "ExternalFileServing.Threshold";
    public static final String SERVEROPTION_EXTERNAL_FILE_SERVING_ROOT_URL = "ExternalFileServing.RootURL";
    
    public static final String SERVEROPTIONDEFAULT_EXTERNAL_FILE_SERVING_ENABLED = "false";
    public static final String SERVEROPTIONDEFAULT_EXTERNAL_FILE_SERVING_THRESHOLD = "5120";

    public static final String SERVEROPTION_PERMANENT_REDIRECT = "Server.UsePermanentRedirect";
    public static final String SERVEROPTIONDEFAULT_PERMANENT_REDIRECT = Boolean.FALSE.toString();
    
    private transient CachingOptionReader _cachingOptionReader = new CachingOptionReader();

	@Attribute(required=false)
	@NotNull
	private Date created = new Date();
	
	@Attribute(required=false)
	@NotNull
	private Date lastModified = new Date();
	
	
	@Attribute(required=false)
    private String wgaVersion;
	
	@Attribute(required=false)
	@NotNull
	private boolean runWizard = false;
	
	@ElementMap(entry="option", key="name", attribute=true, required=false)
	@NotNull
	@Options
	private Map<String,String> serverOptions = new LinkedHashMap<String,String>();
	
	@ElementList(required=false)
	@NotNull
	private List<Administrator> administrators = new ArrayList<Administrator>();
	
	public static final String LOG_LEVEL_DEBUG = "DEBUG";
	public static final String LOG_LEVEL_INFO = "INFO";
	public static final String LOG_LEVEL_WARNING = "WARNING";
	public static final String LOG_LEVEL_ERROR = "ERROR";
    public static final String SINGLETON_SERVER_PREFIX = "SINGLETON-";
	
	@ElementList(required=false)
	@NotNull
	private List<Integer> adminToolsPortRestrictions = new ArrayList<Integer>();
	
	@ElementList(required=false)
	@NotNull
	private List<Integer> authoringDesignAccessPortRestrictions = new ArrayList<Integer>();
	
	@Element(required=false)
	@NotNull
	private LuceneManagerConfiguration luceneManagerConfiguration = new LuceneManagerConfiguration("${wga.datadir}/#lucene");
	
	@Element(required=false)
	@NotNull
	private FileDerivateManagerConfiguration fileDerivateManagerConfiguration = new FileDerivateManagerConfiguration();
	
	@Element(required=false)
	@NotNull
	private PersonalisationConfiguration personalisationConfiguration = new PersonalisationConfiguration();

	@Element(required=false)
	@NotNull
	private DesignConfiguration designConfiguration = new DesignConfiguration();
	
	@ElementList(required=false)
	@NotNull
	private List<String> coreEventListeners = new ArrayList<String>();
	
	@ElementMap(entry="option", key="name", attribute=true, required=false)
	@NotNull
	@Options
	private Map<String, String> globalDatabaseOptions = new LinkedHashMap<String,String>();
	
	@ElementMap(entry="option", key="name", attribute=true, required=false)
	@NotNull
	@Options
	private Map<String, String> globalPublisherOptions = new LinkedHashMap<String,String>();
	
	@Element(required=false)
	private boolean customErrorPageEnabled = false;
	
	@Element(required=false, data=true)
	@NormalizeEmptyValue
	private String customErrorPage;
	
	@Element(required=false)
	@NotNull
	private MailConfiguration mailConfiguration = new MailConfiguration();
	
	@Element(required=false)
	private AccessLog accessLog;
	
	@ElementList(required=false)
	@NotNull
	private List<Domain> domains = new ArrayList<Domain>();
	
	@ElementList(required=false)
	@NotNull
	private List<DatabaseServer> databaseServers = new ArrayList<DatabaseServer>();
	
	@ElementList(required=false)
	@NotNull
	private List<ContentDatabase> contentDatabases = new ArrayList<ContentDatabase>();
		
	@ElementList(required=false)
	@NotNull
	private List<FilterMapping> filterMappings = new ArrayList<FilterMapping>();
	
	@Element(required=false)
	@NotNull
	private SchedulerConfiguration schedulerConfiguration = new SchedulerConfiguration();
	
	   
    @ElementList(required=false)
    @NotNull
    private List<Share> shares = new ArrayList<Share>();
	

    @ElementList(required=false)
    @NotNull
    private List<VirtualHost> virtualHosts = new ArrayList<VirtualHost>();
    
    @Element(required=false)
    @NotNull
    private ProxyConfiguration proxyConfiguration = new ProxyConfiguration();
    
    @Element(required=false)
    @NotNull
    private ClusterConfiguration clusterConfiguration = new ClusterConfiguration();
    
    @Element(required=false)
    @NotNull
    private HttpSessionManagerConfiguration httpSessionManagerConfiguration = new HttpSessionManagerConfiguration();

    public WGAConfiguration() {
	}
	
	public boolean isWarningsEnabled() {
		return Boolean.parseBoolean(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_LOG_WARNINGS), SERVEROPTIONDEFAULT_LOG_WARNINGS));
	}
	
   public boolean isAdminPageEnabled() {
        return Boolean.parseBoolean(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_SERVICE_ADMIMPAGE), SERVEROPTIONDEFAULT_SERVICE_ADMINPAGE));
   }
   
   public boolean isUsePermanentRedirect() {
       return Boolean.parseBoolean(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_PERMANENT_REDIRECT), SERVEROPTIONDEFAULT_PERMANENT_REDIRECT));
  }
   
   public boolean isStartPageEnabled() {
       return Boolean.parseBoolean(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_SERVICE_STARTPAGE), SERVEROPTIONDEFAULT_SERVICE_STARTPAGE));
  }

	public void setWarningsEnabled(boolean warningsEnabled) {
		serverOptions.put(SERVEROPTION_LOG_WARNINGS, Boolean.valueOf(warningsEnabled).toString());
	}
	
    public void setAdminPageEnabled(boolean enabled) {
        serverOptions.put(SERVEROPTION_SERVICE_ADMIMPAGE, Boolean.valueOf(enabled).toString());
    }

	public String getWarningsOutputViaTML() {
	    return WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_LOG_WARNINGS_TML), SERVEROPTIONDEFAULT_LOG_WARNINGS_TML);
	}

	public void setWarningsOutputViaTML(String warningsOutputViaTML) {
	    serverOptions.put(SERVEROPTION_LOG_WARNINGS_TML, warningsOutputViaTML);
	}

	public boolean isWarningsOutputOnConsole() {

	    String sysPropWarnings = System.getProperty(SYSPROP_WARNINGS_ON_SERVER_CONSOLE);
        if (sysPropWarnings != null) {
            return Boolean.parseBoolean(sysPropWarnings);
        }
	    
	    return Boolean.parseBoolean(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_LOG_WARNINGS_APPLOG), SERVEROPTIONDEFAULT_LOG_WARNINGS_APPLOG));
	}

	public void setWarningsOutputOnConsole(boolean warningsOutputOnConsole) {
	    serverOptions.put(SERVEROPTION_LOG_WARNINGS_APPLOG, Boolean.valueOf(warningsOutputOnConsole).toString());
	}

	public void setUsePermanentRedirect(boolean redirect) {
        serverOptions.put(SERVEROPTION_PERMANENT_REDIRECT, Boolean.valueOf(redirect).toString());
    }

	/**
	 * @return the dbkey of the default database
	 */
	public String getDefaultDatabase() {
	    return serverOptions.get(SERVEROPTION_RESOURCES_DEFAULTDB);
	}
	
	/**
	 * sets the dbkey of the default database
	 * @param defaultDatabase
	 */
	public void setDefaultDatabase(String defaultDatabase) {
	    serverOptions.put(SERVEROPTION_RESOURCES_DEFAULTDB, defaultDatabase);
	}

	public String getFavicon() {
	    return serverOptions.get(SERVEROPTION_RESOURCES_FAVICON);
	}

	public void setFavicon(String favicon) {
	    serverOptions.put(SERVEROPTION_RESOURCES_FAVICON, favicon);
	}

	public int getWebtmlCacheSize() {
	    return Integer.parseInt(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_CACHE_WEBTML_SIZE), SERVEROPTIONDEFAULT_CACHE_WEBTML_SIZE));
	}

	public void setWebtmlCacheSize(int webtmlCacheSize) {
	    serverOptions.put(SERVEROPTION_CACHE_WEBTML_SIZE, Integer.valueOf(webtmlCacheSize).toString());
	}

	public int getCacheExpirationForStaticResources() {
	    return Integer.parseInt(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_CACHE_STATIC_RESOURCES_EXPIRATION), SERVEROPTIONDEFAULT_CACHE_STATIC_RESOURCES_EXPIRATION));
	}

	public void setCacheExpirationForStaticResources(
			int cacheExpirationForStaticResources) {
	    serverOptions.put(SERVEROPTION_CACHE_STATIC_RESOURCES_EXPIRATION, Integer.valueOf(cacheExpirationForStaticResources).toString());
	}

	public String getCharacterEncoding() {
		return WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_ENCODING_OUTPUT), SERVEROPTIONDEFAULT_ENCODING_OUTPUT);
	}

	public void setCharacterEncoding(String characterEncoding) {
	    serverOptions.put(SERVEROPTION_ENCODING_OUTPUT, characterEncoding);
	}

	public String getApplicationLogLevel() {
	    return WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_LOG_APPLOG_LEVEL), SERVEROPTIONDEFAULT_LOG_APPLOG_LEVEL);
	}
	
	public String getWebServiceProtocolImplementation() {
        return WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_SERVICE_PROTOCOL_IMPLEMENTATION), null);
    }

	public void setApplicationLogLevel(String applicationLogLevel) {
		serverOptions.put(SERVEROPTION_LOG_APPLOG_LEVEL, applicationLogLevel);
	}

	public boolean isWebservicesEnabled() {
	    return Boolean.parseBoolean(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_SERVICE_WEBSERVICES), SERVEROPTIONDEFAULT_SERVICE_WEBSERVICES));
	}

	public void setWebservicesEnabled(boolean webservicesEnabled) {
	    serverOptions.put(SERVEROPTION_SERVICE_WEBSERVICES, Boolean.valueOf(webservicesEnabled).toString());
	}

	public boolean isCustomErrorPageEnabled() {
		return customErrorPageEnabled;
	}

	public void setCustomErrorPageEnabled(boolean enabled) {
		this.customErrorPageEnabled = enabled;
	}

	public String getCustomErrorPage() {
		return customErrorPage;
	}

	public void setCustomErrorPage(String customErrorPage) {
		this.customErrorPage = customErrorPage;
	}

	public MailConfiguration getMailConfiguration() {
		return mailConfiguration;
	}

	public List<Administrator> getAdministrators() {
		return administrators;
	}
	
	public Administrator getAdministrator(String name) {
		Iterator<Administrator> admins = administrators.iterator();
		while (admins.hasNext()) {
			Administrator admin = admins.next();
			if (admin.getUsername() != null && admin.getUsername().equalsIgnoreCase(name)) {
				return admin;
			}
		}
		return null;
	}

	public List<Integer> getAdminToolsPortRestrictions() {		
		return adminToolsPortRestrictions;
	}

	public List<Integer> getAuthoringDesignAccessPortRestrictions() {
		return authoringDesignAccessPortRestrictions;
	}

	public LuceneManagerConfiguration getLuceneManagerConfiguration() {
		return luceneManagerConfiguration;
	}

	public DesignConfiguration getDesignConfiguration() {
		return designConfiguration;
	}

	public List<String> getCoreEventListeners() {
		return coreEventListeners;
	}

	public Map<String, String> getGlobalDatabaseOptions() {
		return globalDatabaseOptions;
	}

	public Map<String, String> getGlobalPublisherOptions() {
		return globalPublisherOptions;
	}

	public List<Domain> getDomains() {
		return domains;
	}

	public Domain getDomain(String uid) {
		Iterator<Domain> domainIt = domains.iterator();
		while (domainIt.hasNext()) {
			Domain domain = domainIt.next();
			if (domain.getUid().equals(uid)) {
				return domain;
			}
		}
		return null;
	}
	
	public Domain getDomainByName(String name) {
	    
	    for (Domain d : getDomains()) {
            if (d.getName().equals(name)) {
                return d;
            }
        }
	    return null;
	    
	}
	
	public Domain getDefaultDomain() {
		return getDomain(UID_DEFAULT_DOMAIN);
	}
	

	
	public List<DatabaseServer> getDatabaseServers() {
		return databaseServers;
	}

	public List<ContentDatabase> getContentDatabases() {
		return contentDatabases;
	}
	
	public List<PersonalisationDatabase> getPersonalisationDatabases() {
		List<PersonalisationDatabase> dbs = new ArrayList<PersonalisationDatabase>();
		Iterator<Domain> domains = getDomains().iterator();
		while (domains.hasNext()) {
			Domain domain = domains.next();
			if (domain.getPersonalisation() != null) {
				dbs.add(domain.getPersonalisation());
			}
		}
		return dbs;
	}
	
	public void add(Domain domain) {
		domains.add(domain);
	}
	
	public void add(DatabaseServer server) {
		databaseServers.add(server);
	}
	
	public DatabaseServer getDatabaseServer(String uid) {
		Iterator<DatabaseServer> it = databaseServers.iterator();
		while (it.hasNext()) {
			DatabaseServer server = it.next();
			if (server.getUid().equals(uid)) {
				return server;
			}
		}
		return null;
	}
	
	public void removeDatabaseServer(String uid) {
		DatabaseServer server = getDatabaseServer(uid);
		if (server != null) {
			databaseServers.remove(server);
		}
	}
	
	public void add(ContentDatabase contentDatabase) {
		contentDatabases.add(contentDatabase);
	}
	
	public void removeContentDatabase(String key) {
		ContentDatabase db = getContentDatabase(key);
		if (db != null) {
			contentDatabases.remove(db);
		}
	}
	
	public ContentDatabase getContentDatabase(String key) {
		Iterator<ContentDatabase> it = contentDatabases.iterator();
		while (it.hasNext()) {
			ContentDatabase db = it.next();
			if (db.getKey().equalsIgnoreCase(key)) {
				return db;
			}
		}
		return null;
	}
	
	public ContentStore getContentStore(String key) {
		Iterator<ContentDatabase> it = contentDatabases.iterator();
		while (it.hasNext()) {
			ContentDatabase db = it.next();
			if (db.getKey().equalsIgnoreCase(key) && db instanceof ContentStore) {
				return (ContentStore) db;
			}
		}
		return null;
	}
	
	@SkipValidation
	public List<ContentStore> getContentStores() {
		List<ContentStore> contentStores = new ArrayList<ContentStore>();
		Iterator<ContentDatabase> it = contentDatabases.iterator();
		while (it.hasNext()) {
			ContentDatabase db = it.next();
			if (db instanceof ContentStore) {
				contentStores.add((ContentStore)db);
			}
		}
		return contentStores;
	}

	
	public void add(Administrator admin) {
		administrators.add(admin);
	}

	public void createDefaultResources() {		
		
	    // Create default domain
		if (getDefaultDomain() == null) {
			Domain defaultDomain = new Domain();
			defaultDomain.setUid(UID_DEFAULT_DOMAIN);
			defaultDomain.setName("default");
			add(defaultDomain);
			//defaultDomain.createAuthenticationSource("de.innovationgate.webgate.api.auth.FileAuthenticationModule");
		}
		
		// Create default agent exclusions
		getPersonalisationConfiguration().addDefaultAgentExclusions();
		
		// Normalize process For all old JNDI authentications: If  "easysetup" flag is missing and jndi path is configured set easysetup it to "false", else to "true"
		Iterator<Domain> domains = getDomains().iterator();
		while (domains.hasNext()) {
            Domain domain = (Domain) domains.next();
            if (domain.getAuthenticationSource() != null && domain.getAuthenticationSource().getImplClassName().equals("de.innovationgate.webgate.api.auth.JndiAuthenticationModule")) {
                AuthenticationSource authSource = domain.getAuthenticationSource();
                if (!authSource.getOptions().containsKey("jndi.setuptype")) {
                    if (authSource.getOptions().containsKey("jndi.path")) {
                        authSource.getOptions().put("jndi.setuptype", "jndipath");
                    }
                    else {
                        authSource.getOptions().put("jndi.setuptype", "host");
                    }
                }
            }
        }
		
		// Normalize process for old language behaviours. Replace them with their OpenWGA 5.1 pendants
		Iterator<ContentDatabase> dbs = getContentDatabases().iterator();
		while (dbs.hasNext()) {
            ContentDatabase db = (ContentDatabase) dbs.next();
            String langBehaviour = db.getPublisherOptions().get("LanguageBehaviour");
            if (langBehaviour != null) {
                if (langBehaviour.equals("default")) {
                    db.getPublisherOptions().put("LanguageBehaviour", "de.innovationgate.wgpublisher.lang.DynamicLanguageBehaviour");
                }
                else if (langBehaviour.equals("maincontent")) {
                    db.getPublisherOptions().put("LanguageBehaviour", "de.innovationgate.wgpublisher.lang.StaticLanguageBehaviour");
                }
                else if (langBehaviour.equals("browser")) {
                    db.getPublisherOptions().put("LanguageBehaviour", "de.innovationgate.wgpublisher.lang.OnlyDefaultLanguageBehaviour");
                }
            }
        }
		
		// Normalize process for old proprietary service API configurations
		String hashingScheme = getServerOptions().get("Security.HashingScheme");
		if (hashingScheme != null) {
		    
		    String hashingImpl = null;
		    if (hashingScheme.equals(SHA1HashingScheme.NAME)) {
		        hashingImpl = SHA1HashingScheme.class.getName();
		    }
		    else if (hashingScheme.equals(BCrypt10HashingScheme.NAME)) {
		        hashingImpl = BCrypt10HashingScheme.class.getName();
		    }
		    
		    if (hashingImpl != null) {
    		    getServerOptions().put(SERVEROPTION_SERVICE_APIS_PREFIX + HashingSchemeType.class.getName(), hashingImpl);
    		    getServerOptions().remove("Security.HashingScheme");
		    }
		    else {
		        Logger.getLogger("wga").error("Unable to migrate old server option 'Security.HashingScheme' to service api option, as the value is unexpected: " + hashingScheme);
		    }
		}
		

	}

	public static WGAConfiguration read(InputStream in) throws Exception {
		return (WGAConfiguration) new WGAConfigurationFactory().read(in);
	}
	
	public static void write(WGAConfiguration config, OutputStream out) throws Exception {
		new WGAConfigurationFactory().write(config, out);
	}
	
	public static WGAConfiguration createDefaultConfig(Version wgaVersion) {
		return (WGAConfiguration) new WGAConfigurationFactory().createDefaultConfig(wgaVersion);
	}
	
	
	public WGAConfiguration clone() throws CloneNotSupportedException {
		return (WGAConfiguration) new WGAConfigurationFactory().clone(this);
	}

	public void addAdminToolsPortRestriction(int port) {
		if (!adminToolsPortRestrictions.contains(port)) {
			adminToolsPortRestrictions.add(port);
		}
	}
	
	public void removeAdminToolsPortRestriction(int port) {
		adminToolsPortRestrictions.remove(port);
	}

	public void clearAdminToolsPortRestrictions() {
		adminToolsPortRestrictions.clear();
	}
	
	public void addAuthoringDesignAccessPortRestriction(int port) {
		if (!authoringDesignAccessPortRestrictions.contains(port)) {
			authoringDesignAccessPortRestrictions.add(port);
		}
	}

	public void removeAuthoringDesignAccessPortRestriction(int port) {
		authoringDesignAccessPortRestrictions.remove(port);
	}
	
	public void clearAuthoringDesignAccessPortRestrictions() {
		authoringDesignAccessPortRestrictions.clear();
	}

	public AccessLog getAccessLog() {
		return accessLog;
	}

	public void setAccessLog(AccessLog accessLog) {
		this.accessLog = accessLog;
	}

	public int getTmlBuffer() {
	    return Integer.parseInt(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_WEBTML_OUTPUT_BUFFER), SERVEROPTIONDEFAULT_WEBTML_OUTPUT_BUFFER));
	}

	public void setTmlBuffer(int tmlBuffer) {
	    serverOptions.put(SERVEROPTION_WEBTML_OUTPUT_BUFFER, Integer.valueOf(tmlBuffer).toString());
	}

	public String getTmlHeader() {
	    return serverOptions.get(SERVEROPTION_WEBTML_HEADER);
	}

	public void setTmlHeader(String tmlHeader) {
	    serverOptions.put(SERVEROPTION_WEBTML_HEADER, tmlHeader);
	}

	public String getApplicationLogDirectory() {
	    return serverOptions.get(SERVEROPTION_LOG_PERMANENTLOG_DIR);
	}
	
	public String getServerName() {
        String serverName = serverOptions.get(SERVEROPTION_SERVER_NAME);
        if (serverName == null) {
            try {
                serverName = InetAddress.getLocalHost().getCanonicalHostName();
            }
            catch (UnknownHostException e) {
                serverName = "(unknown)";
            }
        }
        return serverName;
    }
	
	public String getRootURL() {
	    return serverOptions.get(SERVEROPTION_ROOT_URL);
	}
	
	public void setRootURL(String url) {
	    serverOptions.put(SERVEROPTION_ROOT_URL, url);
	}

	public void setApplicationLogDirectory(String applicationLogDirectory) {
		serverOptions.put(SERVEROPTION_LOG_PERMANENTLOG_DIR, applicationLogDirectory);
	}

	public PersonalisationConfiguration getPersonalisationConfiguration() {
		return personalisationConfiguration;
	}

	public List<FilterMapping> getFilterMappings() {
		return filterMappings;
	}

	public SchedulerConfiguration getSchedulerConfiguration() {
		return schedulerConfiguration;
	}

	public void setAdministrators(List<Administrator> administrators) {
		if (administrators == null) {
			this.administrators = new ArrayList<Administrator>();
		} else {
			this.administrators = administrators;
		}
	}

	public void setAdminToolsPortRestrictions(List<Integer> adminToolsPortRestrictions) {
		if (adminToolsPortRestrictions == null) {
			this.adminToolsPortRestrictions = new ArrayList<Integer>();
		} else {
			this.adminToolsPortRestrictions = adminToolsPortRestrictions;
		}
	}

	public void setAuthoringDesignAccessPortRestrictions(List<Integer> authoringDesignAccessPortRestrictions) {
		if (authoringDesignAccessPortRestrictions == null) {
			this.authoringDesignAccessPortRestrictions = new ArrayList<Integer>();
		} else {
			this.authoringDesignAccessPortRestrictions = authoringDesignAccessPortRestrictions;
		}
	}

	public void setCoreEventListeners(List<String> coreEventListeners) {
		if (coreEventListeners == null) {
			this.coreEventListeners = new ArrayList<String>();
		} else {
			this.coreEventListeners = coreEventListeners;
		}
	}

	public void setGlobalDatabaseOptions(Map<String, String> globalDatabaseOptions) {
		if (globalDatabaseOptions == null) {
			this.globalDatabaseOptions = new HashMap<String, String>();
		} else {
			this.globalDatabaseOptions = globalDatabaseOptions;
		}
	}

	public void setGlobalPublisherOptions(Map<String, String> globalPublisherOptions) {
		if (globalPublisherOptions == null) {
			this.globalPublisherOptions = new HashMap<String, String>();
		} else {
			this.globalPublisherOptions = globalPublisherOptions;
		}
	}

	public void setDomains(List<Domain> domains) {
		if (domains == null) {
			this.domains = new ArrayList<Domain>();
		} else {
			this.domains = domains;
		}
	}

	public void setDatabaseServers(List<DatabaseServer> databaseServers) {
		if (databaseServers == null) {
			this.databaseServers = new ArrayList<DatabaseServer>();
		} else {
			this.databaseServers = databaseServers;
		}
	}

	public void setContentDatabases(List<ContentDatabase> contentDatabases) {
		if (contentDatabases == null) {
			this.contentDatabases = new ArrayList<ContentDatabase>();
		} else {
			this.contentDatabases = contentDatabases;
		}
	}

	public void setFilterMappings(List<FilterMapping> filterMappings) {
		if (filterMappings == null) {
			this.filterMappings = new ArrayList<FilterMapping>();
		} else {
			this.filterMappings = filterMappings;
		}
	}

	public Date getCreated() {
		return created;
	}

	protected void setCreated(Date created) {
		this.created = created;
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	@Override
	protected void validate(List<ValidationError> errors, boolean integrityCheckOnly) {
		super.validate(errors, integrityCheckOnly);
		
		// Check character encoding
		if (getCharacterEncoding() != null) {
    		Charset charset = null;
    		try {
                charset = Charset.forName(getCharacterEncoding());
            }
            catch (Exception e) {
            }
            if (charset == null) {
                errors.add(new ValidationError("Character encoding '" + getCharacterEncoding() + "' is not available on this system", new String[]{"characterEncoding"}, this));
            }
		}
		
		// check content dbs
		Iterator<ContentDatabase> contentDBs = getContentDatabases().iterator();
		Set<String> uniqueKeys = new HashSet<String>();
		while (contentDBs.hasNext()) {
			ContentDatabase cDB = contentDBs.next();
			checkReferences(errors, cDB);
			if (uniqueKeys.contains(cDB.getKey())) {
				errors.add(new ValidationError("Duplicate dbkey '" + cDB.getKey()  + "'.", new String[]{"contentDatabases"}, this));
			} else {
				uniqueKeys.add(cDB.getKey());
			}
		}
				
		
		// check for duplicate domain uids / names
		Iterator<Domain> domains = getDomains().iterator();
		Set<String> uids = new HashSet<String>();
		Set<String> names = new HashSet<String>();
		while (domains.hasNext()) {
			Domain domain = domains.next();
			if (uids.contains(domain.getUid())) {
				errors.add(new ValidationError("Duplicate domain uid '" + domain.getUid()  + "'.", new String[]{"domains"}, this));
			} else {
				uids.add(domain.getUid());
			}
			if (names.contains(domain.getName())) {
				errors.add(new ValidationError("Duplicate domain name '" + domain.getName()  + "'.", new String[]{"domains"}, this));
			} else {
				names.add(domain.getName());
			}
		}

		// check for duplicate dbserver uids
		Iterator<DatabaseServer> dbservers = getDatabaseServers().iterator();
		uids = new HashSet<String>();
		while (dbservers.hasNext()) {
			DatabaseServer dbServer = dbservers.next();
			if (uids.contains(dbServer.getUid())) {
				errors.add(new ValidationError("Duplicate database server uid '" + dbServer.getUid()  + "'.", new String[]{"databaseServers"}, this));
			} else {
				uids.add(dbServer.getUid());
			}
		}
		
		// check share uniqueness
        Iterator<Share> shares = getShares().iterator();
        Set<String> uniqueNames = new HashSet<String>();
        while (shares.hasNext()) {
            Share share = shares.next();
            String shareKey = share.getImplClassName() + "//" + share.getName();
            if (uniqueNames.contains(shareKey)) {
                errors.add(new ValidationError("Duplicate share name '" + share.getName()  + "' of type '" + share.getImplClassName() + "'", new String[]{"shares"}, this));
            } else {
                uniqueNames.add(shareKey);
            }
        }
		
	}

	private void checkReferences(List<ValidationError> errors, ContentDatabase cdb) {
		if (getDomain(cdb.getDomain()) == null) {
			errors.add(new ValidationError("Domain reference '" + cdb.getDomain() + "' on content database '" + cdb.getKey() + "' does not exist.", new String[]{"domain"}, cdb));
		}
		if (getDatabaseServer(cdb.getDbServer()) == null) {
		    if (!cdb.getDbServer().startsWith(SINGLETON_SERVER_PREFIX)) {
		        errors.add(new ValidationError("DatabaseServer reference '" + cdb.getDbServer() + "'on content database '" + cdb.getKey() + "' does not exist.", new String[]{"dbServer"}, cdb));
		    }
		}		
	}

	protected static boolean isDefaultResource(ConfigBean bean) {
		if (bean == null) {
			return false;
		}
		if (bean instanceof Domain) {
			return ((Domain)bean).getUid().equals(UID_DEFAULT_DOMAIN);
		} else if (bean instanceof DatabaseServer) {
			return ((DatabaseServer)bean).getUid().equals(UID_EMBEDDED_DBSERVER);
		} else if (bean instanceof DesignSource) {
			return ((DesignSource)bean).getUid().equals(UID_DESIGNSOURCE_DB) || ((DesignSource)bean).getUid().equals(UID_DESIGNSOURCE_FILESYSTEM) ||((DesignSource)bean).getUid().equals(UID_DESIGNSOURCE_FILESYSTEM);
		} else {
			return false;
		}
	}
	
	public IdentifiableConfigBean getByUid(String uid) {
	    
	    // Content databases
	    Iterator<ContentDatabase> contentDBs = getContentDatabases().iterator();
	    while (contentDBs.hasNext()) {
            ContentDatabase contentDatabase = contentDBs.next();
            if (contentDatabase.getUid().equals(uid)) {
                return contentDatabase;
            }
        }

	    // Pers databases
        Iterator<PersonalisationDatabase> persDBs = getPersonalisationDatabases().iterator();
        while (persDBs.hasNext()) {
            PersonalisationDatabase personalisationDatabase = persDBs.next();
            if (personalisationDatabase.getUid().equals(uid)) {
                return personalisationDatabase;
            }
        }
        
        // Database servers
        Iterator<DatabaseServer> servers = getDatabaseServers().iterator();
        while (servers.hasNext()) {
             DatabaseServer databaseServer = servers.next();
             if (databaseServer.getUid().equals(uid)) {
                 return databaseServer;
             }
        }
        
        // Domains
        Iterator<Domain> domains = getDomains().iterator();
        while (domains.hasNext()) {
            Domain domain = domains.next();
            if (domain.getUid().equals(uid)) {
                return domain;
            }            
        }
        
        // Jobs, Tasks, Schedules
        Iterator<Job> jobs = getSchedulerConfiguration().getJobs().iterator();
        while (jobs.hasNext()) {
            Job job = jobs.next();
            if (job.getUid().equals(uid)) {
                return job;
            }
            Iterator<Task> tasks = job.getTasks().iterator();
            while (tasks.hasNext()) {
                Task task = tasks.next();
                if (task.getUid().equals(uid)) {
                    return task;
                }
            }
            Iterator<Schedule> schedules = job.getSchedules().iterator();
            while (schedules.hasNext()) {
                Schedule schedule = schedules.next();
                if (schedule.getUid().equals(uid)) {
                    return schedule;
                }
                
            }
        }
        
        // Shares
        Iterator<Share> shares = getShares().iterator();
        while (shares.hasNext()) {
            Share share = shares.next();
            if (share.getUid().equals(uid)) {
                return share;
            }
        }
        
        // Filters
        Iterator<FilterMapping> filtersMappings = getFilterMappings().iterator();
        while (filtersMappings.hasNext()) {
            FilterMapping filterMapping = filtersMappings.next();
            if (filterMapping.getUid().equals(uid)) {
                return filterMapping;
            }
        }
        
        // VirtualHosts & resources
        Iterator<VirtualHost> hosts = getVirtualHosts().iterator();
        while (hosts.hasNext()) {
            VirtualHost host = hosts.next();
            if (host.getUid().equals(uid)) {
                return host;
            }
            Iterator<VirtualResource> resources = host.getVirtualResources().iterator();
            while (resources.hasNext()) {
                VirtualResource resource = resources.next();
                if (resource.getUid().equals(uid)) {
                    return resource;
                }
            }
        }


        
        return null;
	}
	
	public void removeEntity(String uid) {
	    IdentifiableConfigBean bean = getByUid(uid);
	    if (bean instanceof ContentDatabase) {
	        getContentDatabases().remove(bean);
	    } else if (bean instanceof PersonalisationDatabase) {
	        getPersonalisationDatabases().remove(bean);
	    } else if (bean instanceof DatabaseServer) {
	        getDatabaseServers().remove(bean);
	    } else if (bean instanceof Domain) {
	        getDomains().remove(bean);
	    } else if (bean instanceof Job) {
	        getSchedulerConfiguration().getJobs().remove(bean);
	    } else if (bean instanceof Task || bean instanceof Schedule) {
	        Iterator<Job> jobs = getSchedulerConfiguration().getJobs().iterator();
	        while (jobs.hasNext()) {
	            Job job = jobs.next();
	            Iterator<Task> tasks = job.getTasks().iterator();
	            while (tasks.hasNext()) {
	                Task task = tasks.next();
	                if (task.getUid().equals(uid)) {
	                    tasks.remove();
	                }
	            }
	            Iterator<Schedule> schedules = job.getSchedules().iterator();
	            while (schedules.hasNext()) {
	                Schedule schedule = schedules.next();
	                if (schedule.getUid().equals(uid)) {
	                    schedules.remove();
	                }
	                
	            }
	        }
	    } else if (bean instanceof Share) {
	        getShares().remove(bean);
	    } else if (bean instanceof FilterMapping) {
	        getFilterMappings().remove(bean);
	    } else if (bean instanceof VirtualHost) {
	        getVirtualHosts().remove(bean);
	    } else if (bean instanceof VirtualResource) {
	        Iterator<VirtualHost> hosts = getVirtualHosts().iterator();
	        while (hosts.hasNext()) {
	            VirtualHost host = hosts.next();
	            Iterator<VirtualResource> resources = host.getVirtualResources().iterator();
	            while (resources.hasNext()) {
	                VirtualResource resource = resources.next();
	                if (resource.getUid().equals(uid)) {
	                    resources.remove();
	                }
	            }
	        }
	    } 
	}

    public boolean hasContentDatabase(String key) {
		Iterator<ContentDatabase> it = contentDatabases.iterator();
		while (it.hasNext()) {
			ContentDatabase db = it.next();
			if (db.getKey().equalsIgnoreCase(key)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * creates a new contentstore on the wga embedded server
	 * with the given dbkey and designReference
	 * @param dbkey database key
	 * @param designReference reference in default FS_Design_Source
	 */
	public ContentStore createContentStoreOnEmbeddedServer(String dbkey, String designReference) {
		ContentStore cs = new ContentStore(WGAConfiguration.UID_EMBEDDED_DBSERVER, getDefaultDomain().getUid(), "de.innovationgate.webgate.api.hsql.WGDatabaseImpl", dbkey, "de");
		cs.getDatabaseOptions().put(ContentStore.OPTION_PATH, dbkey);
		DesignReference designRef = new DesignReference(Constants.DESIGNCOL_FILESYSTEM, designReference);
		cs.setDesign(new Design(designRef));
		add(cs);
		return cs;
	}
	




    public String getPasswordEncoding() {
        return WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_SECURITY_PASSWORD_ENCODING), SERVEROPTIONDEFAULT_SECURITY_PASSWORD_ENCODING);
    }

    public void setPasswordEncoding(String passwordEncoding) {
        serverOptions.put(SERVEROPTION_SECURITY_PASSWORD_ENCODING, passwordEncoding);
    }

    public int getUserCacheLatencyMinutes() {
        return Integer.parseInt(WGUtils.getValueOrDefault(serverOptions.get(SERVEROPTION_CACHE_USERCACHE_LATENCY), SERVEROPTIONDEFAULT_CACHE_USERCACHE_LATENCY));
    }

    public void setUserCacheLatencyMinutes(int userCacheLatencyMinutes) {
        serverOptions.put(SERVEROPTION_CACHE_USERCACHE_LATENCY, Integer.valueOf(userCacheLatencyMinutes).toString());
    }

    public Map<String, String> getServerOptions() {
        return serverOptions;
    }
    
    public List<Share> getShares() {
        return shares;
    }

    public void setShares(List<Share> shares) {
        this.shares = shares;
    }
    
    public Share getShare(String name) {
        Iterator<Share> it = shares.iterator();
        while (it.hasNext()) {
            Share share = it.next();
            if (share.getName().equalsIgnoreCase(name)) {
                return share;
            }
        }
        return null;
    }
    
    public void removeShare(String key) {
        Share share = getShare(key);
        if (share != null) {
            shares.remove(share);
        }
    }
    
    public void removeDefaultOptions(ModuleRegistry reg) throws Exception {
        visit(new RemoveDefaultOptionsVisitor(reg, this));
    }
    
    public void sortOptions(ModuleRegistry reg) throws Exception {
        visit(new SortOptionsVisitor(reg, this));
    }

    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        if (property.getName().equals("serverOptions")) {
            return new ArrayList(registry.getModulesForType("de.innovationgate.wga.modules.types.WGAServerOptionsModuleType").values());
        }
        
        return super.getOptionDefinitions(registry, property, config);
        
    }

    public CachingOptionReader getCachingOptionReader() {
        return _cachingOptionReader;
    }
        
    public List<VirtualHost> getVirtualHosts() {
        return virtualHosts;
    }

    public void setVirtualHosts(List<VirtualHost> virtualHosts) {
        this.virtualHosts = virtualHosts;
    }

    public boolean isRunWizard() {
        return runWizard;
    }

    public void setRunWizard(boolean runWizard) {
        this.runWizard = runWizard;
    }

    public ProxyConfiguration getProxyConfiguration() {
        return proxyConfiguration;
    }

    public void setProxyConfiguration(ProxyConfiguration proxyConfiguration) {
        this.proxyConfiguration = proxyConfiguration;
    }

    public ClusterConfiguration getClusterConfiguration() {
        return clusterConfiguration;
    }

    public void setClusterConfiguration(ClusterConfiguration clusterConfiguration) {
        this.clusterConfiguration = clusterConfiguration;
    }
    
    public String getWgaVersion() {
        return wgaVersion;
    }

    public void setWgaVersion(String wgaVersion) {
        this.wgaVersion = wgaVersion;
    }

    public FileDerivateManagerConfiguration getFileDerivateManagerConfiguration() {
        return fileDerivateManagerConfiguration;
    }

    public void setFileDerivateManagerConfiguration(FileDerivateManagerConfiguration fileDerivateManagerConfiguration) {
        this.fileDerivateManagerConfiguration = fileDerivateManagerConfiguration;
    }
    
    public HttpSessionManagerConfiguration getHttpSessionManagerConfiguration() {
        return httpSessionManagerConfiguration;
    }

    public void setHttpSessionManagerConfiguration(HttpSessionManagerConfiguration httpSessionManagerConfiguration) {
        this.httpSessionManagerConfiguration = httpSessionManagerConfiguration;
    }
}
