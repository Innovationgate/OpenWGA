package de.innovationgate.wga.services.rest;

import io.swagger.jaxrs.config.BeanConfig;
import io.swagger.jaxrs.listing.ApiListingResource;
import io.swagger.jaxrs.listing.SwaggerSerializers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.ContextResolver;
import javax.xml.bind.Marshaller;

import org.apache.log4j.Logger;
import org.eclipse.persistence.jaxb.MarshallerProperties;
import org.glassfish.jersey.jsonp.JsonProcessingFeature;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.moxy.json.MoxyJsonConfig;
import org.glassfish.jersey.moxy.json.MoxyJsonFeature;
import org.glassfish.jersey.moxy.xml.MoxyXmlFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabase.DatabaseAction;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.common.beans.hdbmodel.DocumentVisitor;
import de.innovationgate.wga.common.beans.hdbmodel.ModelDefinition;
import de.innovationgate.wga.common.beans.hdbmodel.Relation;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.services.rest.v1.resources.RootResource;
import de.innovationgate.wgpublisher.WGAConfigurationUpdateEvent;
import de.innovationgate.wgpublisher.WGAConfigurationUpdateListener;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

public class RestApplication extends ResourceConfig implements WGACoreEventListener, WGAConfigurationUpdateListener {
    
    public class PrepareDBProblemOccasion implements ProblemOccasion {
        
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((_dbKey == null) ? 0 : _dbKey.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            PrepareDBProblemOccasion other = (PrepareDBProblemOccasion) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (_dbKey == null) {
                if (other._dbKey != null)
                    return false;
            }
            else if (!_dbKey.equals(other._dbKey))
                return false;
            return true;
        }

        private String _dbKey;

        public PrepareDBProblemOccasion(String dbKey) {
            _dbKey = dbKey;
        }

        @Override
        public ProblemScope getDefaultScope() {
            return new DatabaseScope(_dbKey);
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return RestApplication.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return true;
        }

        private RestApplication getOuterType() {
            return RestApplication.this;
        }
        
    }
    
    public class RelationInfo {
        
        private String _name;
        public String getName() {
            return _name;
        }
        public String getOwnerClass() {
            return _ownerClass;
        }
        private String _ownerClass;
        public RelationInfo(String name, String ownerClass) {
            super();
            _name = name;
            _ownerClass = ownerClass;
        }
        
        
    }
    
    public class DatabaseInfo {
        
        private Set<String> _enabledRestAPIs = new HashSet<String>();
        private String _dbKey;
        private boolean _forceRegularLogin = false;
        
        public DatabaseInfo(String dbReference) {
            _dbKey = dbReference;
        }

        public Set<String> getEnabledRestAPIs() {
            return _enabledRestAPIs;
        }

        private Map<String,List<RelationInfo>> _relationSourcesByContentClass = new HashMap<String, List<RelationInfo>>();

        public Map<String, List<RelationInfo>> getRelationSourcesByContentClass() {
            return _relationSourcesByContentClass;
        }

        public String getDbKey() {
            return _dbKey;
        }
        
        public boolean isForceRegularLogin() {
            return _forceRegularLogin;
        }

        public void setForceRegularLogin(boolean forceRegularLogin) {
            _forceRegularLogin = forceRegularLogin;
        }
        
    }
    
    public static final int COLLECTION_PAGESIZE_DEFAULT = 10;
    public static final String COLLECTION_PAGESIZE_DEFAULT_STR = String.valueOf(COLLECTION_PAGESIZE_DEFAULT);
    
    public static final String SERVEROPTION_SERVICE_REST = "Services.Rest.Enabled";
    public static final String DBATTRIB_ENABLED_APIS = "Services.Rest.EnabledApis";
    public static final String DBATTRIB_FORCE_REGULAR_LOGIN = "Services.Rest.ForceRegularLogin";
    
    public static final String URLPARAM_SIZE = "size";
    public static final String URLPARAM_ROLE = "role";
    public static final String URLPARAM_OFFSET = "offset";
    public static final String URLPARAM_META = "meta";
    public static final String URLPARAM_ITEM = "item";
    public static final String URLPARAM_RELATION = "relation";
    public static final String URLPARAM_ENHANCER = "enhancer";
    public static final String URLPARAM_AUTOSCROLL = "autoScroll";
    public static final String URLPARAM_AUTOSCROLLTARGET = "autoScrollTarget";
    public static final Object URLPARAM_RETURNRESOURCE = "returnResource";
    
    public static final Set<String> COLLECTION_ENHANCING_URLPARAMS = new HashSet<String>();
    static {
        COLLECTION_ENHANCING_URLPARAMS.add(URLPARAM_META);
        COLLECTION_ENHANCING_URLPARAMS.add(URLPARAM_ITEM);
        COLLECTION_ENHANCING_URLPARAMS.add(URLPARAM_RELATION);
        COLLECTION_ENHANCING_URLPARAMS.add(URLPARAM_ENHANCER);
    }
    
    public static final Set<String> NONINHERITED_PAGING_URLPARAMS = new HashSet<String>();
    static {
        NONINHERITED_PAGING_URLPARAMS.add(URLPARAM_OFFSET);
        NONINHERITED_PAGING_URLPARAMS.add(URLPARAM_AUTOSCROLLTARGET);
    }
    
    
    public static final Logger LOG = Logger.getLogger("wga.services.rest");
    

    
    public MediaType getOutputMediaType(HttpHeaders headers) {
        MediaType chosenType = MediaType.APPLICATION_JSON_TYPE;
        for(MediaType type : headers.getAcceptableMediaTypes()) {
            if (type.equals(MediaType.APPLICATION_JSON_TYPE) || type.equals(MediaType.APPLICATION_XML_TYPE)) {
                chosenType =type;
                break;
            }
        }
        return chosenType;
    }
    
    private WGA _wga;
    private String _encoding;
    private Map<String,DatabaseInfo> _databaseInfo = new HashMap<String, RestApplication.DatabaseInfo>();
    private boolean _serviceEnabled;
    
    public Map<String, DatabaseInfo> getDatabaseInfo() {
        return _databaseInfo;
    }

    public String getEncoding() {
        return _encoding;
    }

    public RestApplication(WGA wga) throws WGException {
        
        // Context
        _wga = wga;
        _encoding = _wga.getCore().getCharacterEncoding();
        
        // Init Jersey and swagger
        setClassLoader(wga.server().getLibraryLoader());
        registerClasses(RootResource.class);
        registerClasses(RestExceptionMapper.class);
        registerClasses(ApiListingResource.class);
        registerClasses(SwaggerSerializers.class);
        property(ServerProperties.METAINF_SERVICES_LOOKUP_DISABLE, true);
        property(ServerProperties.FEATURE_AUTO_DISCOVERY_DISABLE, true);
        property(ServerProperties.MONITORING_STATISTICS_MBEANS_ENABLED, true);
        property(ServerProperties.MEDIA_TYPE_MAPPINGS, "json : application/json, xml : application/xml");
        property(MarshallerProperties.JSON_WRAPPER_AS_ARRAY_NAME, true);
        register(MoxyJsonFeature.class);
        register(JsonProcessingFeature.class);
        register(MultiPartFeature.class);
        register(V1UriConnegFilter.class);
        register(new MoxyXmlFeature(Collections.<String, Object>emptyMap(), _wga.server().getLibraryLoader(), false));
        register(createMoxyJsonResolver());
        
        // Other preparations
        readServerConfig();
        for (String dbKey : _wga.server().getAppKeys()) {
            WGDatabase db = _wga.db(dbKey, false);
            prepareDB(db);
        }
        
        // Register for events
        _wga.getCore().addEventListener(this);
        
        // Start swagger
        BeanConfig cfg = new BeanConfig();
        cfg.setVersion("1.0");
        cfg.setSchemes(new String[] {"http", "https"});
        URLBuilder urlBuilder = wga.urlBuilder(_wga.server().getBaseURL());
        cfg.setHost(urlBuilder.getHost() + (urlBuilder.getPort() != -1 ? ":" + urlBuilder.getPort() : ""));
        cfg.setBasePath("/services/rest/");
        cfg.setResourcePackage("de.innovationgate.wga.services.rest.v1.resources,de.innovationgate.wga.services.rest.v1.resources.cms,de.innovationgate.wga.services.rest.v1.resources.custom,de.innovationgate.wga.services.rest.v1.resources.hdbmodel,de.innovationgate.wga.services.rest.v1.resources.query");
        //cfg.setScan(true);
        
        
    }
    
    protected void prepareDB(WGDatabase db) throws WGException {

        ProblemOccasion occ = new PrepareDBProblemOccasion(db.getDbReference());
        _wga.server().getProblemRegistry().clearProblemOccasion(occ);
        
        // Look if APIs are enabled
        Database database = _wga.database(db);
        @SuppressWarnings("unchecked")
        List<String> enabledAPIs = (List<String>) database.getPublisherOption(RestApplication.DBATTRIB_ENABLED_APIS);
        if (enabledAPIs == null || enabledAPIs.size() == 0) {
            return;
        }
        
        if (!_serviceEnabled) {
            _wga.server().getProblemRegistry().addProblem(Problem.create(occ, "ServiceProblem.DisabledButNeeded", ProblemSeverity.HIGH));
        }

        final DatabaseInfo dbInfo = new DatabaseInfo(db.getDbReference());
        dbInfo.getEnabledRestAPIs().addAll(enabledAPIs);
        dbInfo.setForceRegularLogin((Boolean) database.getPublisherOption(RestApplication.DBATTRIB_FORCE_REGULAR_LOGIN)); 
        
        // Preprocess HDBModel on connect
        db.onConnect(new DatabaseAction() {
            
            @Override
            public void run(WGDatabase db) throws Exception {
        
                HDBModel hdbModel = HDBModel.getModel(db);
                if (hdbModel != null) {
                    ModelDefinition def = hdbModel.getDefinition();
                    def.accept(new DocumentVisitor() {
                        
                        @Override
                        public void visit(Document document) {
                            if (document instanceof Content) {
                                Content content = (Content) document;
                                for (Relation rel : content.getRelations()) {
                                    List<RelationInfo> relations = dbInfo.getRelationSourcesByContentClass().get(rel.getTargetClass());
                                    if (relations == null) {
                                        relations = new ArrayList<RelationInfo>();
                                        dbInfo.getRelationSourcesByContentClass().put(rel.getTargetClass(), relations);
                                    }
                                    relations.add(new RelationInfo(rel.getName(), content.getContentClass()));
                                }
                            }
                            
                        }
                    });
                }
        
            }
            
        });
        
        _databaseInfo.put(db.getDbReference(), dbInfo);
        
        
    }

    public ContextResolver<MoxyJsonConfig> createMoxyJsonResolver() {
        final MoxyJsonConfig moxyJsonConfig = new MoxyJsonConfig();
        Map<String, String> namespacePrefixMapper = new HashMap<String, String>(1);
        namespacePrefixMapper.put("http://www.w3.org/2001/XMLSchema-instance", "xsi");
        moxyJsonConfig.setNamespacePrefixMapper(namespacePrefixMapper).setNamespaceSeparator(':');
        moxyJsonConfig.setFormattedOutput(true);
        
        Map<String,Object> props = new HashMap<String, Object>();
        props.put(Marshaller.JAXB_ENCODING, _encoding);
        moxyJsonConfig.setMarshallerProperties(props);
        
        return moxyJsonConfig.resolver();
    }
    
    public WGA getWGA() {
        return _wga;
    }
    
    public WGACore getCore() {
        return _wga.getCore();
    }

    @Override
    public void contentStoreConnected(WGACoreEvent event) {
        event.getDatabase().onConnect(new DatabaseAction() {
            
            @Override
            public void run(WGDatabase db) throws Exception {
                RestApplication.this.prepareDB(db);
            }
            
        });
    }

    @Override
    public void contentStoreDisconnected(WGACoreEvent event) {
    }

    @Override
    public void startupPreConnect(WGACoreEvent event) {
    }

    @Override
    public void startupPostConnect(WGACoreEvent event) {
    }

    @Override
    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    @Override
    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    @Override
    public void configurationUpdated(WGAConfigurationUpdateEvent event) {
        readServerConfig();
    }

    private void readServerConfig() {
        _serviceEnabled = (Boolean) _wga.getCore().getServicesServerOptionReader().readOptionValueOrDefault(SERVEROPTION_SERVICE_REST);
    }

    public Boolean getEnabled() {
        return _serviceEnabled;
    }

    public boolean isServiceEnabled() {
        return _serviceEnabled;
    }
    
    
    

}
