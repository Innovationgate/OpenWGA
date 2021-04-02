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

package de.innovationgate.wgpublisher.auth;

import java.io.File;
import java.io.FileInputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.log4j.Logger;

import de.innovationgate.utils.AnyTypeGroupMembershipResolver;
import de.innovationgate.utils.GroupMembershipResolver;
import de.innovationgate.utils.GroupResolvingException;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentEvent;
import de.innovationgate.webgate.api.WGContentEventListener;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGPageVisitor;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGStructEntryList;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.auth.AuthenticationException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.webgate.api.auth.CertAuthCapableAuthModule;
import de.innovationgate.webgate.api.auth.ConfigurationException;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.QueryResult;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.DomainScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.problems.SimpleProblemOccasion;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class CSAuthModule implements CoreAwareAuthModule, CertAuthCapableAuthModule, WGACoreEventListener, WGContentEventListener {
	
    public static final int COLLECT_LAZY_CACHESIZE_DEFAULT = 1000;

    public static final Logger LOG = Logger.getLogger("wga.api.auth.cs");
    
	public static final String DEFAULT_GROUPSROOT = "authgroups";
    public static final String DEFAULT_USERSROOT = "authusers";
    /**
     * The database to take auth information from is yet unknown
     */
    public static final int STATUS_AUTHDB_UNKNOWN = 0;
    
    
    /**
     * The database to take auth information from is known, prepared but not yet connected,
     * so the auth module didn't yet collect login data
     */
    public static final int STATUS_AUTHDB_PREPARED = 1;
    
    /**
     * The database to take auth information from is known and the login data has been fetched
     */
    public static final int STATUS_READY = 2;
    
    public class LazyLoginLoader implements Callable<Login> {
        
        private String _userName;

        public LazyLoginLoader(String userName) {
            _userName = userName;
        }

        @Override
        public Login call() {
            
            try {
                WGDatabase db = (WGDatabase) _core.getContentdbs().get(_dbkey);
                if (db == null) {
                    _core.getLog().error("Cannot load login information for user '" + _userName + "' from db '" + _dbkey + "' because the db is not connected");
                    return null;
                }
                
                db.openSession();
                db.getSessionContext().setTask("CS Authentication Module - Login lazy loading task");
                
                if (_scriptCollect != null) {
                    return doCustomLazyload(db, _userName);
                }
                else {
                    Map<String,Object> params = new HashMap<String, Object>();
                    params.put("username", _userName);
                    
                    App app = WGA.get(_core).app(db);
                    List<String> queryTerms = new ArrayList<String>();
                    queryTerms.add("content.items['" + _usernameItem.toLowerCase() + "'].text = :username");
                    for (String itemName : _aliasItemNames) {
                        queryTerms.add("content.items['" + itemName.toLowerCase() + "'].text = :username");
                    }
                    
                    String queryString;
                    if (_loginContentClass != null) {
                        queryString = "content.contentclass=:contentclass AND (" + WGUtils.serializeCollection(queryTerms, " OR ") + ")";
                        params.put("contentclass", _loginContentClass);
                    }
                    else {
                        queryString = WGUtils.serializeCollection(queryTerms, " OR ");
                    }
                    
                    QueryResult qs = app.query("hql", queryString, null, params);
                    WGContent con = qs.getFirstResultContent();
                    if (con != null) {
                        Login login = buildLoginFromDocument(con);
                        CSLazyGroupMembershipResolver groupResolver = new CSLazyGroupMembershipResolver(app);
                        Set<WGContent> groupDocs = new HashSet<WGContent>();
                        groupDocs.addAll(groupResolver.resolveMembership(con));
                        List<String> groups = new ArrayList<String>();
                        for (WGContent groupDoc : groupDocs) {
                            groups.add(groupDoc.getItemText(_groupnameItem));
                        }
                        
                        login.setGroups(groups);
                        return login;
                    }
                    else {
                        return null;
                    }
                }
                    
                
            }
            catch (Throwable t) {
                _core.getLog().error("Exception loading login information for user '" + _userName + "' from db '" + _dbkey + "'", t);
                return null;
            }
            finally {
                WGFactory.getInstance().closeSessions();
            }

            
        }
        
    }
    
    public static class AuthCollectorRunOccasion implements ProblemOccasion {
        
        private String _dbKey;

        public AuthCollectorRunOccasion(String dbKey) {
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
            return CSAuthModule.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return true;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
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
            AuthCollectorRunOccasion other = (AuthCollectorRunOccasion) obj;
            if (_dbKey == null) {
                if (other._dbKey != null)
                    return false;
            }
            else if (!_dbKey.equals(other._dbKey))
                return false;
            return true;
        }
        
    }
    
    public class GroupMembership {
        
        private String _name;
        private Set<String>  _groups;
        
        public GroupMembership(String name) {
            _name = name;
            _groups = new HashSet<String>();
            
        }

        /**
         * @return Returns the members.
         */
        public Set<String> getGroups() {
            return _groups;
        }
        
        public void addGroup(String group) {
            _groups.add(group);
        }

        /**
         * @return Returns the name.
         */
        public String getName() {
            return _name;
        }
        
        
    }
    
    public class CSPreloadGroupMembershipResolver extends GroupMembershipResolver {

        private Map<String,GroupMembership> _groupMemberships;

        public CSPreloadGroupMembershipResolver(Map<String,GroupMembership> groupMemberships) {
            _groupMemberships = groupMemberships;
        }
        
        public Set<String> resolveDirectMembership(String member) throws GroupResolvingException {
            
            GroupMembership membership = _groupMemberships.get(member.toLowerCase());
            if (membership != null) {
                return membership.getGroups();
            }
            else {
                return new HashSet<String>();
            }
            
        }
        
    }

    public Map<String,Group> _groupInformation = new HashMap<String,Group>();

    public class AuthCollector implements Runnable {

        public void run() {

            synchronized (this) {
            
                AuthCollectorRunOccasion occ = new AuthCollectorRunOccasion(_dbkey);
                _core.getProblemRegistry().clearProblemOccasion(occ);
                
                try {
                    
                    
                    _currentCollectorThread = Thread.currentThread();
                    WGDatabase db = (WGDatabase) _core.getContentdbs().get(_dbkey);
                    if (db == null) {
                        _core.getLog().error("Cannot collect authentications from db '" + _dbkey + "' because the db is not connected");
                        _core.getProblemRegistry().addProblem(Problem.create(occ, "csAuthProblem.dbNotConnected", ProblemSeverity.HIGH));
                        return;
                    }
                    
                    db.openSession();
                    db.getSessionContext().setTask("CS Authentication Module - Login collection task");
                    
                    // In "lazy" mode we only clear the logins cache
                    if (_collectMode.equals(COLLECTMODE_LAZY)) {
                        _loginInformationCache.flushAll();
                    }
                    // Collect logins with custom script
                    else if (_scriptCollect != null) {
                        doCustomCollect(db);
                    }
                    // Collect logins with default functionalities
                    else {
                        doDefaultCollect(db);
                    }
                    
                    // Notify listeners
                    synchronized (_listeners) {
                        Iterator<AuthenticationSourceListener> listeners = _listeners.iterator();
                        while (listeners.hasNext()) {
                            listeners.next().authenticationDataChanged();
                        }
                    }
                    
                }
                catch (Problem p) {
                    _core.getLog().error("Cannot collect authentications from db '" + _dbkey + "'", p);
                    _core.getProblemRegistry().addProblem(p);
                }
                catch (WGUnavailableException e) {
                    _core.getLog().error("Cannot collect authentications from db '" + _dbkey + "' because the db is currently unavailable");
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "csAuthProblem.dbUnavailable", ProblemSeverity.HIGH));
                }
                catch (Exception e) {
                    _core.getLog().error("Error collecting authentications from db' " + _dbkey + "'", e);
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "csAuthProblem.exception", ProblemSeverity.HIGH));
                    
                }
                finally {
                    _currentCollectorThread = null;                
                    WGFactory.getInstance().closeSessions();
                }
                
            }

        }

    }

    public class CSLazyGroupMembershipResolver extends AnyTypeGroupMembershipResolver<WGContent> {
    
        private App _app;

        public CSLazyGroupMembershipResolver(App app) {
            _app = app;
        }
        
        public Set<WGContent> resolveDirectMembership(WGContent member) throws GroupResolvingException {
            
            try {
                Map<String,Object> params = new HashMap<String, Object>();
                params.put("member", member);
                
                Set<WGContent> groups = new HashSet<WGContent>(); 
                String queryString = ":member in (select rel.target from content.relations as rel where rel.group=:groupname)";
                params.put("groupname", _membersItem.toLowerCase());
                if (_groupContentClass != null) {
                    queryString = "content.contentclass=:contentclass AND (" + queryString + ")";
                    params.put("contentclass", _groupContentClass);
                }
                for (Context cx :  _app.query("hql", queryString, null, params)) {
                    groups.add(cx.content());
                }
                return groups;
                
            }
            catch (WGException e) {
                throw new GroupResolvingException("Exception lazy resolving group on database '" + _app.getDbKey() + "' for member name '" + member + "'", e);
            }
            
        }
        
    }

    public class LazyLoginSearcher implements Callable<List<CSUserGroupInfo>> {
        
        private String _prefix;
    
        public LazyLoginSearcher(String prefix) {
            _prefix = prefix;
        }
    
        @Override
        public List<CSUserGroupInfo> call() {
            
            try {
                App app = WGA.get(_core).app(_dbkey);
                if (app == null) {
                    _core.getLog().error("Cannot query login information from db '" + _dbkey + "' because the db is not connected");
                    return null;
                }
                
                if (!app.isLuceneIndexed()) {
                    _core.getLog().error("Cannot query login information from db '" + _dbkey + "' because the db is not fulltext indexed");
                }
                
                app.db().getSessionContext().setTask("CS Authentication Module - Login lazy mode searching task");
                List<String> terms = new ArrayList<String>();
                terms.add(_usernameItem + ":" + _prefix + "*");
                terms.add(_groupnameItem + ":" + _prefix + "*");
                for (String alias : _aliasItemNames) {
                    terms.add(alias + ":" + _prefix + "*");
                }
                
                QueryResult qs = app.query("lucene", WGUtils.serializeCollection(terms, " OR "));
                List<CSUserGroupInfo> results = new ArrayList<CSUserGroupInfo>();
                for (Context result : qs) {
                    CSUserGroupInfo userGroupInfo = null;
                    if (isLoginDefinition(result.content())) {
                        Login login = buildLoginFromDocument(result.content());
                        userGroupInfo = buildUserInfo(login);
                    }
                    else if (isGroupDefinition(result.content())) {
                        userGroupInfo = buildGroupInfo((String) result.item(_groupnameItem));
                    }
                    if (userGroupInfo != null) {
                        results.add(userGroupInfo);
                    }
                }
                
                return results;
                    
                
            }
            catch (Throwable t) {
                _core.getLog().error("Exception loading login information for user '" + _prefix + "' from db '" + _dbkey + "'", t);
                return null;
            }
            finally {
                WGFactory.getInstance().closeSessions();
            }
    
            
        }
        
    }

    public static final String COPTION_DBKEY = "auth.cs.dbkey";

    public static final String COPTION_ROOTDOC_USERS = "auth.cs.rootdoc.users";
    public static final String COPTION_ROOTDOC_GROUPS = "auth.cs.rootdoc.groups";
    
    public static final String COPTION_COLLECT_MODE = "auth.cs.collect.mode";
    
    public static final String COPTION_COLLECT_LAZY_CACHESIZE = "auth.cs.collect.lazy.cachesize";

    public static final String COPTION_SCRIPT_COLLECT = "auth.cs.script.collect";

    public static final String COPTION_ITEM_USERNAME = "auth.cs.item.username";
    public static final String COPTION_ITEM_GROUPNAME = "auth.cs.item.groupname";
    public static final String COPTION_ITEM_GROUPMEMBERS = "auth.cs.item.groupmembers";
    
    public static final String COPTION_ITEM_PASSWORD = "auth.cs.item.password";
    public static final String COPTION_ITEM_ALIASES = "auth.cs.item.aliases";
    public static final String COPTION_ALIAS_ITEMS = "auth.cs.aliasitems";
    public static final String COPTION_ITEM_EMAIL = "auth.cs.item.email";
    public static final String COPTION_ITEM_ENABLED = "auth.cs.item.enabled";
    
    public static final String COPTION_LABELED_NAMES = "auth.cs.labelednames";
    
    public static final String COPTION_COLLECT_CONDITION = "auth.cs.collect.condition";
    
    public static final String COPTION_LOGIN_CONTENTCLASS = "auth.cs.login.contentclass";
    public static final String COPTION_GROUP_CONTENTCLASS = "auth.cs.group.contentclass";

    public static final String DEFAULTITEM_EMAIL = "EMail";

    public static final String DEFAULTITEM_USERALIASES = "UserAliases";

    public static final String DEFAULTITEM_PASSWORD = "Password";

    public static final String DEFAULTITEM_USERNAME = "UserName";
    public static final String DEFAULTITEM_GROUPNAME = "GroupName";
    public static final String DEFAULTITEM_MEMBERS = "Members";
    public static final String DEFAULTITEM_ENABLED = "Enabled";
    
    public static final String COLLECTMODE_PRELOAD = "preload";
    public static final String COLLECTMODE_LAZY = "lazy";

    private WGACore _core;

    private Map<String,Login> _loginInformation = new HashMap<String, Login>();
    private Cache _loginInformationCache;

    private String _dbkey;

    private String _userRootDoc = DEFAULT_USERSROOT;
    private volatile int _status = STATUS_AUTHDB_UNKNOWN;
    private String _groupsRootDoc = DEFAULT_GROUPSROOT;
    private WGDatabase _registeredDB = null;

    private String _scriptCollect;

    private String _usernameItem = DEFAULTITEM_USERNAME;

    private String _passwordItem = DEFAULTITEM_PASSWORD;

    private String _aliasesItem = DEFAULTITEM_USERALIASES;

    private String _emailItem = DEFAULTITEM_EMAIL;
    
    private String _enabledItem = DEFAULTITEM_ENABLED;

    private String _membersItem = DEFAULTITEM_MEMBERS;

    private String _groupnameItem = DEFAULTITEM_GROUPNAME;
    
    private String _collectCondition = null;
    
    private boolean _internalConfiguration = false;
    
    private Set<String> _labeledNames = new HashSet<String>();

    
    private List<AuthenticationSourceListener> _listeners = new ArrayList<AuthenticationSourceListener>();


    private Thread _currentCollectorThread = null;

    private X509Certificate _currentCA;

    private long _currentCALastModified;

    private String _certCA = null;

    private String _certCRL = null;

    private long _currentCRLLastModified;

    private X509CRL _currentCRL;

    private boolean _certAuth = false;

    private String _commonNameExpression;

    private Object _collectMode = COLLECTMODE_PRELOAD;

    private ExecutorService _lazyLoadingPool = null;

    private int _collectLazyCachesize = COLLECT_LAZY_CACHESIZE_DEFAULT;

    private String _loginContentClass = null;

    private String _groupContentClass = null;

    private String _consumerDomain;

    private String _consumerDb;

    private AuthCollector _authCollector;

    private List<String> _aliasItemNames;

    public static final String CSAUTH_PROPERTIES_FILE = "csauth.properties";

    
    /**
     * Enable certificate authentication
     */
    public static final String COPTION_CERTAUTH = "auth.cs.certauth";
    
    /**
     * Filename of the CA to be used to verify clientcertificates
     */
    public static final String COPTION_CA = "auth.cs.ca";

    /**
     * Filename of the CRL for the given CA
     */
    public static final String COPTION_CRL = "auth.cs.crl";

    
    /**
     * Determines a TMLScript expression by which to calculate the common name
     */
    public static final String COPTION_COMMONNAME_EXPRESSION = "auth.cs.commonname.expression";
    

    public void init(Map<String,String> params, WGDatabase db) throws ConfigurationException {

        if (db != null) {
            _consumerDb = db.getDbReference();
        }
        else {
            _consumerDomain = (String) params.get(WGADomain.AUTHOPTION_DOMAIN);
        }
            
        _dbkey = ((String) params.get(COPTION_DBKEY)).toLowerCase();
        
        configure(params);

        if (_dbkey == null) {
            throw new ConfigurationException("The dbkey of the authentication db is not specified");
        }

        if (_scriptCollect == null && _userRootDoc == null) {
            throw new ConfigurationException("Either a collection script or a root document for login documents must be specified");
        }
        
        _authCollector = new AuthCollector();
        
        registerWithTargetDatabase(_core.getContentdbs().get(_dbkey));
    }

    private void configure(Map<String,String> params) {
        
        _userRootDoc = WGUtils.getValueOrDefault((String) params.get(COPTION_ROOTDOC_USERS), _userRootDoc);
        _groupsRootDoc = WGUtils.getValueOrDefault((String) params.get(COPTION_ROOTDOC_GROUPS), _groupsRootDoc);
        _scriptCollect = WGUtils.getValueOrDefault((String) params.get(COPTION_SCRIPT_COLLECT), _scriptCollect);
        _collectMode  = WGUtils.getValueOrDefault((String) params.get(COPTION_COLLECT_MODE), _collectMode);
        _collectCondition = WGUtils.getValueOrDefault((String) params.get(COPTION_COLLECT_CONDITION), _collectCondition);
        _collectLazyCachesize = Integer.parseInt(WGUtils.getValueOrDefault((String) params.get(COPTION_COLLECT_LAZY_CACHESIZE), String.valueOf(_collectLazyCachesize)));
        
        _usernameItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_USERNAME), _usernameItem);        
        _passwordItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_PASSWORD), _passwordItem);
        _aliasesItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_ALIASES), _aliasesItem);
        _emailItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_EMAIL), _emailItem);
        _enabledItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_ENABLED), _enabledItem);
        _groupnameItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_GROUPNAME), _groupnameItem);
        _membersItem = WGUtils.getValueOrDefault((String) params.get(COPTION_ITEM_GROUPMEMBERS), _membersItem);
        _aliasItemNames = WGUtils.deserializeCollection(WGUtils.getValueOrDefault(params.get(COPTION_ALIAS_ITEMS), ""), ",", true);
        _aliasItemNames.remove("");
        
        _loginContentClass = WGUtils.getValueOrDefault((String) params.get(COPTION_LOGIN_CONTENTCLASS), _loginContentClass);
        _groupContentClass = WGUtils.getValueOrDefault((String) params.get(COPTION_GROUP_CONTENTCLASS), _groupContentClass);
        
        if (params.containsKey(COPTION_CERTAUTH)) {
            _certAuth = Boolean.parseBoolean((String) params.get(COPTION_CERTAUTH));
        }
        
        _certCA = WGUtils.getValueOrDefault((String) params.get(COPTION_CA), _certCA);
        _certCRL = WGUtils.getValueOrDefault((String) params.get(COPTION_CRL), _certCRL);

        if (params.containsKey(COPTION_LABELED_NAMES)) {
            _labeledNames.addAll(WGUtils.deserializeCollection((String) params.get(COPTION_LABELED_NAMES), ","));
        }
        
         _commonNameExpression = WGUtils.getValueOrDefault((String) params.get(COPTION_COMMONNAME_EXPRESSION), _commonNameExpression);
    }

    private synchronized void registerWithTargetDatabase(final WGDatabase dbTarget) {
        
        if (dbTarget == null || !dbTarget.getDbReference().equals(_dbkey) || _registeredDB != null) {
            return;
        }
         
        _registeredDB = dbTarget;
            
        // Prepare configuration and status on connect
        dbTarget.onConnect(new WGDatabase.ConnectAction() {
            
            @Override
            public void run(WGDatabase db) throws Exception {
                WGA wga = WGA.get(_core);
                ProblemScope scope = (_consumerDb != null ? new DatabaseScope(_consumerDb) : new DomainScope(_consumerDomain));
                ProblemOccasion occ = SimpleProblemOccasion.createAdministrativeOccasion("csAuthRegistration", scope, CSAuthModule.class, true);
                try {
                    wga.server().getProblemRegistry().clearProblemOccasion(occ);

                    
                    // Try to read configuration embedded to target db
                    readConfigurationProperties(dbTarget, occ);
                    
                    // Prepare lazy mode
                    if (_collectMode.equals(COLLECTMODE_LAZY)) {
                        _loginInformationCache = CacheFactory.createCache("CSAuthUserCache_" + UIDGenerator.generateUID(), _collectLazyCachesize, null);
                        _lazyLoadingPool = Executors.newCachedThreadPool();
                    }
                    
                    dbTarget.addContentEventListener(CSAuthModule.this);
                    
                }
                catch (Exception e) {
                    wga.server().getProblemRegistry().addProblem(Problem.create(occ, "exception", ProblemSeverity.HIGH, Problem.var("authcskey", _dbkey)));
                }     
            }
        });
        
        // If not yet connected, we will wait for our first login to collect
        if (!dbTarget.isConnected()) {
            _status = STATUS_AUTHDB_PREPARED;
            return;
        }
    
        // Collect logins first time
        try {
            runAuthCollector().join();
        }
        catch (InterruptedException e) {
        }

    }

    private synchronized void unregisterFromTargetDatabase(WGDatabase dbTarget) {
        if (dbTarget != null && dbTarget.equals(_registeredDB)) {
            dbTarget.removeContentEventListener(this);
            closeLazyCollectResources();
            _registeredDB = null;
        }
    }

    public AuthenticationSession login(X509Certificate cert) throws AuthenticationException {
        
        if (_status == STATUS_AUTHDB_UNKNOWN) {
            throw new AuthenticationException("Authentication was unable to collect data for valid logins yet. Please ensure that source database '" + _dbkey + "' is enabled and can be connected.");
        }
        
        if (_status == STATUS_AUTHDB_PREPARED) {
            try {
                runAuthCollector().join();
            }
            catch (InterruptedException e) {
            }
        }
        
        try {
            String user = cert.getSubjectDN().toString();
            Login login = getLogin(user);
            
            if (login != null) {
                return login;
            } 
            else {
                LOG.warn("Failed login for '" + user + "': Unknown user (" + getAuthenticationSource() + ")");
                return null;
            }
        }
        catch (Exception e) {
            throw new AuthenticationException("Exception on login", e);
        }
        
    }
    
    public AuthenticationSession login(String user, Object credentials) throws AuthenticationException {
        
        if (_status == STATUS_AUTHDB_UNKNOWN) {
            throw new AuthenticationException("Authentication was unable to collect data for valid logins yet. Please ensure that source database '" + _dbkey + "' is enabled and can be connected.");
        }
        
        if (_status == STATUS_AUTHDB_PREPARED) {
            try {
                runAuthCollector().join();
            }
            catch (InterruptedException e) {
            }
        }
        
        try {
            String password  = String.valueOf(credentials);
            Login login = (Login) getLogin(user);
            if (login != null) {
                
                if (login.getHashedPassword() != null && login.getHashedPassword().check(password, _core.getModuleRegistry())) {
                    return login;
                }
                else {
                    LOG.warn("Failed login for '" + user + "': Wrong password (" + getAuthenticationSource() + ")");
                }
            }
            else {
                LOG.warn("Failed login for '" + user + "': Unknown user (" + getAuthenticationSource() + ")");
            }
            return null;
        }
        catch (Exception e) {
            throw new AuthenticationException("Exception on login", e);
        }
       
    }
    
    private CSUserGroupInfo buildUserInfo(Login login){

		CSUserGroupInfo newUserEntry = new CSUserGroupInfo();
		newUserEntry.setIsUser(true);
		newUserEntry.setIsGroup(false);
		newUserEntry.setFullQualifiedName(login.getDistinguishedName());
		newUserEntry.setAliasNames(login.getAliases());
		newUserEntry.getAttributes().put("mail", login.getMailAddress());
        newUserEntry.getAttributes().put("groups", login.getGroups());
        newUserEntry.getAttributes().put("documentKey", login.getDocumentkey());
		if (login.getLabeledNames() != null) {
		    newUserEntry.getAttributes().putAll(login.getLabeledNames());
		    newUserEntry.getLabeledNames().putAll(login.getLabeledNames());
		}
		
		return newUserEntry;
    		
    } 

    private Login getLogin(String user) throws CacheException {
        Login login = null;
        if (_collectMode.equals(COLLECTMODE_LAZY)) {
            login = (Login) _loginInformationCache.read(user);
            if (login == null) {
                login = lazyLoadLogin(user);
                if (login != null && login.isCacheable()) {
                    writeLoginToCache(login);
                }
            }
        }
        else {
            login = _loginInformation.get(user.toLowerCase());
        }
        return login;
    }
    


    private void writeLoginToCache(Login login) throws CacheException {
        _loginInformationCache.write(login.getDistinguishedName(), login);
        for (String alias : login.getAliases()) {
            _loginInformationCache.write(alias, login);
        }
    }

    private Login lazyLoadLogin(String user) {

        LazyLoginLoader loader = new LazyLoginLoader(user);
        try {
            return _lazyLoadingPool.submit(loader).get();
        }
        catch (Exception e) {
            _core.getLog().error("Exception running lazy loading task to load user name '" + user + "' from content store '" + _dbkey + "'", e);
            return null;
        }
    }

    public String getEMailAddress(String user) {

        try {
            Object loginObj = getLogin(user);
            if (loginObj != null && loginObj instanceof Login) {
                return ((Login) loginObj).getMailAddress();
            }
            else {
                return null;
            }
        }
        catch (CacheException e) {
            throw new RuntimeException("Exception retrieving mail address for user '" + user + "'", e);
        }

    }



    public void clearCache() {
    }

    public String getAuthenticationSource() {
    	return "Contentstore-Authentication against app '" + _dbkey + "'";
    }

    public void setCore(WGACore core) {
        _core = core;
        core.addEventListener(this);

    }

    public void contentStoreConnected(WGACoreEvent event) {
        registerWithTargetDatabase(event.getDatabase());
    }

    public void contentStoreDisconnected(WGACoreEvent event) {
        unregisterFromTargetDatabase(event.getDatabase());
    }



    private Thread runAuthCollector() {
        Thread collectorThread = new Thread(_authCollector);
        collectorThread.start();
        _status = STATUS_READY;
        return collectorThread;
    }

    public boolean isTemporary() {
        return false;
    }

    public void contentCreated(WGContentEvent contentEvent) {
    }

    public boolean contentSaved(WGContentEvent contentEvent) {
        return true;
    }

    public void contentHasBeenSaved(WGContentEvent event) {
        
        if (event.getDatabase().getSessionContext().isTransactionActive()) {
            return;
        }
        
        runAuthCollectorByEvent(event);
    }

    public void contentHasBeenDeleted(WGContentEvent event) {
        
        if (event.getDatabase().getSessionContext().isTransactionActive()) {
            return;
        }
        
        runAuthCollector();
    }
    
    public void runAuthCollectorByEvent(WGContentEvent event) {
        
        if (_collectCondition != null) {
            
            try {
                WGContent content = event.getContent();
                if (content == null) {
                    content = (WGContent) event.getDatabase().getDocumentByKey(event.getDocumentKey());
                }
            
                if (content != null) {
                    TMLContext context = new TMLContext(event.getContent(), _core, null, null);
                    Map<String,Object> objects = new HashMap<String,Object>();
        
                    // Execute script
                    ExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
                    ExpressionResult result = engine.evaluateExpression(_collectCondition, context, ExpressionEngine.TYPE_EXPRESSION, objects);
                    if (result.isError()) {
                        _core.getLog().error("Error auth collector condition script", result.getException());
                        return;
                    }
                    else if (result.isFalse()) {
                        return;
                    }
                }
            }
            catch (WGAPIException e) {
                _core.getLog().error("Error runAuthCollectorByEvent.", e);
                return;
            }
        }
        
        runAuthCollector();
    }

    public boolean isPoolable() {
        return true;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#addAuthenticationSourceListener(de.innovationgate.webgate.api.auth.AuthenticationSourceListener)
     */
    public void addAuthenticationSourceListener(AuthenticationSourceListener listener) {
        synchronized (_listeners) {
            _listeners.add(listener);
        }
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#removeAuthenticationSourceListener(de.innovationgate.webgate.api.auth.AuthenticationSourceListener)
     */
    public void removeAuthenticationSourceListener(AuthenticationSourceListener listener) {
        synchronized (_listeners) {
            _listeners.remove(listener);
        }
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#getAllowedCredentialClasses()
     */
    public Class<?>[] getAllowedCredentialClasses() {
        return new Class[] {String.class};
    }


    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#isQueryable(java.lang.String)
     */
    public boolean isQueryable(String queryType) {
        if(queryType.equals(QUERY_USERS_AND_GROUPS)) {
        	return true;
        }
        else if (queryType.equals(QUERY_USER_DN)) {
            return true;
        }
        
    	return false;
    }

    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#query(java.lang.Object, java.lang.String)
     */
    public Object query(Object query, String queryType) throws WGQueryException {
        
        try {
            if(queryType.equals(QUERY_USERS_AND_GROUPS)) {
            	return findUsersAndGroups((String) query);
            }
            else if (queryType.equals(QUERY_USER_DN)) {
                return fetchUserByDN((String) query);
            }
            throw new WGQueryException(String.valueOf(query), "Unsupported query type '" + queryType + "'");
        }
        catch (Throwable t) {
            throw new WGQueryException(String.valueOf(query), "Exception executing query of type '" + queryType + "'", t);
        }
    }  
    
    private CSUserGroupInfo fetchUserByDN(String query) throws CacheException {

        Login login = getLogin(query.toLowerCase());
        if (login != null && login.getDistinguishedName().equalsIgnoreCase(query)) {
            return buildUserInfo(login);
        }
        else {
            return null;
        }
        
    }

    /**
     * retrieves all users and groups with the given prefix in nameAttributes
     * @param prefix to search for
     * @return a list of de.innovationgate.webgate.api.auth.UserGroupInfo
     * @throws ExecutionException 
     * @throws InterruptedException 
     */
    private List<CSUserGroupInfo> findUsersAndGroups(String prefix) throws InterruptedException, ExecutionException {
    	if (_collectMode.equals(COLLECTMODE_PRELOAD)) {
    	    return findPreloadUsersAndGroups(prefix);
    	}
    	else {
    	    return findLazyUsersAndGroups(prefix);
    	}
    }

    private List<CSUserGroupInfo> findLazyUsersAndGroups(String prefix) throws InterruptedException, ExecutionException {
        LazyLoginSearcher searcher = new LazyLoginSearcher(prefix);
        return _lazyLoadingPool.submit(searcher).get();
    }

    private List<CSUserGroupInfo> findPreloadUsersAndGroups(String prefix) {
        List<CSUserGroupInfo> results = new ArrayList<CSUserGroupInfo>();
        Set<String> foundUsers = new HashSet<String>();
        String current;
        Iterator<String> it = _loginInformation.keySet().iterator();
		while(it.hasNext()){
			current = (String) it.next();
			if(current.indexOf(prefix)!=-1){
			    Login login = _loginInformation.get(current);
			    if (!foundUsers.contains(login.getDistinguishedName())) {
    				results.add(buildUserInfo(login));
    				foundUsers.add(login.getDistinguishedName());
			    }
			}
		}
		
		it = _groupInformation.keySet().iterator();
		while (it.hasNext()) {
		    current = (String) it.next();
		    if(current.indexOf(prefix)!=-1){
		        results.add(buildGroupInfo(current));
		    }
		}
		return results;
    }

    private CSUserGroupInfo buildGroupInfo(String groupName) {
        
        CSUserGroupInfo newUserEntry = new CSUserGroupInfo();
        newUserEntry.setIsUser(false);
        newUserEntry.setIsGroup(true);
        newUserEntry.setFullQualifiedName(groupName);
        return newUserEntry;
    }

    /**
     * @return Returns the status.
     */
    public int getStatus() {
        return _status;
    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {
    }

    public void startupPreConnect(WGACoreEvent event) {
    }
    
    public void destroy() {

        closeLazyCollectResources();
        
        if (_core != null) {
            _core.removeEventListener(this);
            _core = null;
        }
        
        if (_registeredDB != null) {
            _registeredDB.removeContentEventListener(this);
            _registeredDB = null;
        }
        
    }

    public void closeLazyCollectResources() {
        if (_lazyLoadingPool != null) {
            _lazyLoadingPool.shutdown();
        }
        if (_loginInformationCache != null) {
            try {
                _loginInformationCache.destroy();
            }
            catch (CacheException e) {
                _core.getLog().error("Exception closing login cache", e);
            }
        }
    }

    public boolean isGeneratesSessionToken() {
        return false;
    }

    /**
     * returns the unique name of the root document for users
     * @return
     */
	public String getUserRootDoc() {
		return _userRootDoc;
	}
	
	/**
	 * returns the unique name of the root document for groups
	 * @return
	 */
	public String getGroupsRootDoc() {
		return _groupsRootDoc;
	}
	
	/**
	 * returns the dbkey of the authentication source
	 * @return
	 */
	public String getDbkey() {
		return _dbkey;
	}
	
	/**
	 * returns the script code for custom collection
	 * @return
	 */
	public String getScriptCollect() {
		return _scriptCollect;
	}

	/**
	 * returns the username item
	 * @return
	 */
	public String getUsernameItem() {
		return _usernameItem;
	}

	/**
	 * returns the password item name
	 * @return
	 */
	public String getPasswordItem() {
		return _passwordItem;
	}

	/**
	 * returns the alias item name
	 * @return
	 */
	public String getAliasesItem() {
		return _aliasesItem;
	}
	
	/**
	 * returns the email item name
	 * @return
	 */
	public String getEmailItem() {
		return _emailItem;
	}
	
	/**
	 * returns the enabled item name
	 * @return
	 */
	public String getEnabledItem() {
		return _enabledItem;
	}
	
	/**
	 * returns the members item name
	 * @return
	 */
	public String getMembersItem() {
		return _membersItem;
	}
	
	/**
	 * returns the groupname item
	 * @return
	 */
	public String getGroupnameItem() {
		return _groupnameItem;
	}
	
	/**
	 * returns the collect condition
	 * @return
	 */
	public String getCollectCondition() {
		return _collectCondition;
	}

    public Thread getCurrentCollectorThread() {
        return _currentCollectorThread;
    }

    public void contentHasBeenMoved(WGContentEvent event) {
        
        if (event.getDatabase().getSessionContext().isTransactionActive()) {
            return;
        }
        
        runAuthCollectorByEvent(event);
    }
    
    private void loadCA(File caFile) throws AuthenticationException {
        try {
            CertificateFactory certificatefactory = CertificateFactory.getInstance("X.509");
            FileInputStream fin = new FileInputStream(caFile);
            _currentCA = (X509Certificate) certificatefactory.generateCertificate(fin);
            _currentCALastModified = caFile.lastModified();
            fin.close();
        }
        catch (Exception e) {
            String message = "Could not load CA '" + caFile.getPath() + "'";
            WGFactory.getLogger().error(message, e);
            throw new AuthenticationException(message, e);
        }

        // verify crl is signed by ca
        try {
            getCRL().verify(_currentCA.getPublicKey());
        }
        catch (Exception e) {
            String message = "CRL '" + caFile.getPath() + "' could not be verified against given CA.";
            WGFactory.getLogger().error(message, e);
            throw new AuthenticationException(message, e);
        }
    }
    
    /**
     * Returns the certificate authority for certificate authentication
     * 
     * @throws AuthenticationException
     */
    public X509Certificate getCA() throws AuthenticationException {
        if (_certCA == null) {
            throw new AuthenticationException("CA is not configured properly for auth source: " + getAuthenticationSource());
        }
        File caFile = _core.getWGAFile(_certCA);
        if (caFile.exists()) {
            // check if CA is already loaded
            if (_currentCA != null) {
                // check if CA has been changed
                if (caFile.lastModified() != _currentCALastModified) {
                    // reload ca
                    loadCA(caFile);
                }
            }
            else {
                // load ca
                loadCA(caFile);
            }
            return _currentCA;
        }
        else {
            String message = "Could not find CA '" + _certCA + "'. No such file.";
            WGFactory.getLogger().error(message);
            throw new AuthenticationException(message);
        }
    }
    
    private void loadCRL(File crlFile) throws AuthenticationException {
        try {
            CertificateFactory certificatefactory = CertificateFactory.getInstance("X.509");
            FileInputStream fin = new FileInputStream(crlFile);
            _currentCRL = (X509CRL) certificatefactory.generateCRL(fin);
            _currentCRLLastModified = crlFile.lastModified();
            fin.close();
        }
        catch (Exception e) {
            String message = "Could not load CRL '" + crlFile.getPath() + "'";
            WGFactory.getLogger().error(message, e);
            throw new AuthenticationException(message, e);
        }

        // verify crl is signed by expected ca
        try {
            _currentCRL.verify(getCA().getPublicKey());
        }
        catch (Exception e) {
            String message = "CRL '" + crlFile.getPath() + "' could not be verified against given CA.";
            WGFactory.getLogger().error(message, e);
            throw new AuthenticationException(message, e);
        }
    }
    
    /**
     * Returns the certificate revoke list for certificate authentication
     * 
     * @throws AuthenticationException
     */
    public X509CRL getCRL() throws AuthenticationException {
        if (_certCRL == null) {
            throw new AuthenticationException("CRL is not configured properly for auth source: " + getAuthenticationSource());
        }
        File crlFile = _core.getWGAFile(_certCRL);
        if (crlFile.exists()) {
            // check if CRL is already loaded
            if (_certCRL != null) {
                // check if CRL has been changed
                if (crlFile.lastModified() != _currentCRLLastModified) {
                    // reload crl
                    loadCRL(crlFile);
                }
            }
            else {
                // load crl
                loadCRL(crlFile);
            }
            return _currentCRL;
        }
        else {
            String message = "Could not find CRL '" + _certCRL + "'. No such file.";
            WGFactory.getLogger().error(message);
            throw new AuthenticationException(message);
        }
    }

    public boolean isCertAuthEnabled() {
        return _certAuth;
    }

    public void contentStatusChanged(WGContentEvent event) {
    }
    
    private void fetchDefaultGroupsFromDocument(Map<String,GroupMembership> groupMemberships, WGContent content, Map<String,Group> groupInformation) throws WGAPIException {
        
        String groupname = content.getItemText(_groupnameItem);
        groupInformation.put(groupname.toLowerCase(), new Group(groupname, content.getDocumentKey()));
        
        // Members item
        @SuppressWarnings("unchecked")
        List<String> members = (List<String>) content.getItemValueList(_membersItem);
        if (members == null) {
            members = new ArrayList<String>();
        }
        
        Iterator<String> membersIt = members.iterator();
        String member;
        while (membersIt.hasNext()) {
            member = (String) membersIt.next();
            GroupMembership membership = (GroupMembership) groupMemberships.get(member.toLowerCase());
            if (membership == null) {
                 membership = new GroupMembership(member);
                 groupMemberships.put(member.toLowerCase(), membership);
            }
            membership.addGroup(groupname);
        }
        
        
        // Member relations
        for (WGContent memberDoc : content.getRelationsOfGroup(_membersItem)) {
            String name = null;
            if (isGroupDefinition(memberDoc)) {
                name = memberDoc.getItemText(_groupnameItem);
            }
            else if (isLoginDefinition(memberDoc)) {
                name = memberDoc.getItemText(_usernameItem);
            }
            
            if (name == null) {
                continue;
            }
            
            GroupMembership membership = (GroupMembership) groupMemberships.get(name.toLowerCase());
            if (membership == null) {
                 membership = new GroupMembership(name);
                 groupMemberships.put(name.toLowerCase(), membership);
            }
            membership.addGroup(groupname);
        }
        
    }

    @SuppressWarnings("unchecked")
    private Login buildLoginFromDocument(WGContent content) throws WGAPIException {
        String password = content.getItemText(_passwordItem);
        String username = content.getItemText(_usernameItem);
        String email = content.getItemText(_emailItem);
        
        List<String> nameAliases = new ArrayList<String>();
        if (!_collectMode.equals(COLLECTMODE_LAZY)) {
            nameAliases.addAll((Collection<? extends String>) content.getItemValueList(_aliasesItem));
        }
        for (String itemName : _aliasItemNames) {
            String alias = content.getItemText(itemName);
            if (!WGUtils.isEmpty(alias)) {
                nameAliases.add(alias);
            }
        }
        
        Login login = new Login(username, password, email, content.getDocumentKey(), new HashSet<String>(nameAliases));
        
        if (_commonNameExpression != null) {
            try {
                WGA wga = WGA.get(_core);
                Context cx = wga.createTMLContext(content);
                Object cn = wga.tmlscript().runExpression(cx, _commonNameExpression);
                if (cn != null) {
                    login.addLabeledName(AuthenticationModule.USERLABEL_COMMONNAME, String.valueOf(cn));
                }
            }
            catch (Exception e) {
                _core.getLog().error("Exception creating common name from TMLScript expression", e);
            }
        }
        else{
    		String cn_item = (String) content.getDatabase().getAttribute("commonnameItem");
			if(cn_item!=null)
				login.addLabeledName(AuthenticationModule.USERLABEL_COMMONNAME, content.getItemValue(cn_item));
        }
        
        for (String labeledName : _labeledNames) {
            if (content.hasItem(labeledName)) {
                login.addLabeledName(labeledName, content.getItemValue(labeledName));
            }
        }
        
        return login;
    }

    private void putLogin(Map<String,Login> logins, Login login) {
    
        // Test if this username is already in use
        String distName = login.getDistinguishedName();
        Login oldLogin = (Login) logins.get(distName);
        if (oldLogin != null) {
            // Both names are first user names. New one wins
            if (distName.equals(oldLogin.getDistinguishedName())) {
                _core.getLog().warn(
                        "Auth db '" + _dbkey + "': First user name '" + distName + "' is multiply defined by document '" + oldLogin.getDocumentkey() + "' and '"
                                + login.getDocumentkey() + "'");
            }
    
            // New name is first user name, old is not. New wins
            else {
                _core.getLog().warn(
                        "Auth db '" + _dbkey + "': First user name '" + oldLogin.getDistinguishedName() + "' is also defined as alias by document '" + oldLogin.getDocumentkey()
                                + "'. The alias will be overwritten.");
            }
        }
        putLoginName(logins, login, distName);
    
        // Put login under aliases
        Iterator<String> aliases = login.getAliases().iterator();
        String alias;
        while (aliases.hasNext()) {
            alias = (String) aliases.next();
            if (WGUtils.isEmpty(alias)) {
                continue;
            }
    
            // Test if this name is already in use
            oldLogin = (Login) logins.get(alias);
            if (oldLogin != null) {
    
                // Old name is first user name, new is not. Old wins
                // (exiting method)
                if (oldLogin.getDistinguishedName().equals(alias)) {
                    _core.getLog().warn(
                            "Auth db '" + _dbkey + "': First user name '" + distName + "' is also defined as alias by document '" + login.getDocumentkey()
                                    + "'. The alias will be overwritten.");
                    continue;
                }
    
                // Both are aliases. New wins
                else {
                    _core.getLog().warn(
                            "Auth db '" + _dbkey + "': Alias '" +  alias + "' from document '" + login.getDocumentkey() + "' is also defined as alias by document '" + oldLogin.getDocumentkey()
                                    + "'. The alias of the second document will be overwritten.");
                }
            }
            putLoginName(logins, login, alias);
        } 
    
    }

    private void putLoginName(Map<String,Login> logins, Login login, String name) {
        logins.put(name.toLowerCase(), login);
    }

    private boolean isGroupDefinition(WGContent content) throws WGAPIException {
        
        if (_groupContentClass != null && !_groupContentClass.equals(content.getContentClass())) {
            return false;
        }
        
        if (!content.hasItem(_groupnameItem)) {
            return false;
        }
        
        if (content.hasItem(_enabledItem) && !content.getItemText(_enabledItem).equals("true")) {
            return false;
        }
        
        return true;
    }

    private boolean isLoginDefinition(WGContent content) throws WGAPIException {
        
        if (_loginContentClass != null && !_loginContentClass.equals(content.getContentClass())) {
            return false;
        }
        
        if (!content.hasItem(_usernameItem) || !content.hasItem(_passwordItem)) {
            return false;
        }
        
        if (content.hasItem(_enabledItem) && content.getItemText(_enabledItem)!=null && !content.getItemText(_enabledItem).equals("true")) {
            return false;
        }
        
        return true;
    }

    private void readConfigurationProperties(WGDatabase db, final ProblemOccasion occ) throws WGAPIException {

        MasterSessionTask readConfigTask = new MasterSessionTask(db) {

            @Override
            protected void exec(WGDatabase db) throws Throwable {
                WGA wga = WGA.get(_core);
                try {
                    Properties props = new Properties();
                    
                    WGFileContainer system = db.getFileContainer("system");
                    if (system != null && system.hasFile(CSAUTH_PROPERTIES_FILE)) {
                        props.load(system.getFileData(CSAUTH_PROPERTIES_FILE));
                        _internalConfiguration = true;
                    }
                    
                    system = db.getFileContainer("overlay:system");
                    if (system != null && system.hasFile(CSAUTH_PROPERTIES_FILE)) {
                        props.load(system.getFileData(CSAUTH_PROPERTIES_FILE));
                        _internalConfiguration = true;
                    }
                    
                    if (props.size() > 0) {
                        configure(WGUtils.propertiesToStringMap(props));
                    }
                }
                catch (Exception e) {
                    wga.server().getProblemRegistry().addProblem(Problem.create(occ, "read_config_exception", ProblemSeverity.HIGH, Problem.var("authcskey", db.getDbReference())));
                    _core.getLog().error("Exception evaluating csauth configuration", e);
                }
            }
            
        };
        readConfigTask.run();
        
    }
    


    public WGStructEntryList getGroupStructEntries(WGDatabase db, WGContent rootContent) throws WGAPIException {
        WGStructEntryList groupStructEntries = null;
        if (rootContent != null) {
            groupStructEntries = rootContent.getStructEntry().getChildEntries();
        }
    
        if (groupStructEntries == null) {
            WGArea area = db.getArea(_groupsRootDoc);
            if (area != null) {
                groupStructEntries = area.getRootEntries();
            }
        }
        return groupStructEntries;
    }

    private void doDefaultCollect(WGDatabase db) throws WGException {
    
        
        Map<String,Login> newLoginInformation = new HashMap<String,Login>();
        Map<String,Group> newGroupInformation = new HashMap<String,Group>();
        
        // collect group struct entries / if any exist
        Map<String,GroupMembership> groupMembership = new HashMap<String,GroupMembership>();
        if (_groupsRootDoc != null) {
            WGContent rootContent = db.getContentByName(_groupsRootDoc);
            WGStructEntryList groupStructEntries = getGroupStructEntries(db, rootContent);
    
            if (groupStructEntries == null) {
                Problem p = Problem.create(new AuthCollectorRunOccasion(db.getDbReference()), "csAuthProblem.noGroupsRoot", ProblemSeverity.HIGH, Problem.var("groupsroot", _groupsRootDoc));
                _core.getLog().error(p.getMessage());
                _core.getProblemRegistry().addProblem(p);
            }
            else {
                // Recurse through group docs
                Iterator<WGStructEntry> structsIt = groupStructEntries.iterator();
                while (structsIt.hasNext()) {
                    recurseGroupDocuments((WGStructEntry) structsIt.next(), groupMembership, newGroupInformation);
                }
            }
        }
    
        // Collect user struct entries {
        WGStructEntryList structEntries = null;
        WGContent rootContent = db.getContentByName(_userRootDoc);
        if (rootContent != null) {
            structEntries = rootContent.getStructEntry().getChildEntries();
        }
    
        if (structEntries == null) {
            WGArea area = db.getArea(_userRootDoc);
            if (area != null) {
                structEntries = area.getRootEntries();
            }
        }
    
        if (structEntries == null) {
            Problem p = Problem.create(new AuthCollectorRunOccasion(db.getDbReference()), "csAuthProblem.noUsersRoot", ProblemSeverity.HIGH, Problem.var("usersroot", _userRootDoc));
            _core.getLog().error(p.getMessage());
            _core.getProblemRegistry().addProblem(p);
        }
        else {
            // Recurse through user docs
            Iterator<WGStructEntry> structsIt = structEntries.iterator();
            while (structsIt.hasNext()) {
                recurseLoginDocuments(structsIt.next(), newLoginInformation);
            }
        }
        
        // Merge users with group memberships
        CSPreloadGroupMembershipResolver membershipResolver = new CSPreloadGroupMembershipResolver(groupMembership);
        Iterator<String> loginsIt = newLoginInformation.keySet().iterator();
        String loginName;
        Login login;
        while (loginsIt.hasNext()) {
            loginName = loginsIt.next();
            login = (Login) newLoginInformation.get(loginName);
            try {
                login.addGroups(membershipResolver.resolveMembership(loginName));
            }
            catch (GroupResolvingException e) {
                _core.getLog().error("Error resolving group member ship for user name '" + loginName + "'", e);
            }
        }
        
        _loginInformation = newLoginInformation;
        _groupInformation = newGroupInformation;
    
    }

    private void recurseGroupDocuments(WGStructEntry entry, Map<String,GroupMembership> groupMembership, Map<String,Group> groupInformation) throws WGAPIException {
        
        // Look for content with necessary items. Fetch logins if available
        WGContent content = entry.getReleasedContent(entry.getDatabase().getDefaultLanguage());
        if (content != null && isGroupDefinition(content)) {
            fetchDefaultGroupsFromDocument(groupMembership, content, groupInformation);
        }
    
        // Recurse thru children
        Iterator<WGStructEntry> children = entry.getChildEntries().iterator();
        while (children.hasNext()) {
            recurseGroupDocuments(children.next(), groupMembership, groupInformation);
        }
        
    }

    private void recurseLoginDocuments(WGStructEntry entry, Map<String,Login> logins) throws WGAPIException {
    
        // Look for content with necessary items. Fetch logins if available
        WGContent content = entry.getReleasedContent(entry.getDatabase().getDefaultLanguage());
        if (content != null && isLoginDefinition(content)) {
            Login login = buildLoginFromDocument(content);
            if (login != null) {
                putLogin(logins, login);
            }
        }
    
        // Recurse thru children
        Iterator<WGStructEntry> children = entry.getChildEntries().iterator();
        while (children.hasNext()) {
            recurseLoginDocuments(children.next(), logins);
        }
    
    }

    private void doCustomCollect(WGDatabase db) throws WGException {
    
        Map<String,Login> newLoginInformation = new HashMap<String,Login>();
        HashSet<String> newGroupInformation = new HashSet<String>();
    
        // Get collect script
        WGCSSJSModule mod = db.getCSSJSModule(_scriptCollect, WGScriptModule.CODETYPE_TMLSCRIPT);
        if (mod == null) {
            throw new WGException("Database '" + _dbkey + "' does not contain a TMLScript module of name '" + _scriptCollect + "'");
        }
    
        // Build a TMLScript runtime
        ExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        TMLContext context = new TMLContext(db.getDummyContent(db.getDefaultLanguage()), _core, null, null);
        Map<String,Object> objects = new HashMap<String,Object>();
        objects.put("logins", newLoginInformation);
        objects.put("groups", newGroupInformation);
    
        // Execute script
        ExpressionResult result = engine.evaluateExpression(mod.getCode(), context, ExpressionEngine.TYPE_SCRIPT, objects);
        if (result.isError()) {
            throw new WGException("Error executing collect script", result.getException());
        }
    
        _loginInformation =  newLoginInformation;
        
        for(String groupName: newGroupInformation){
        	_groupInformation.put(groupName, new Group(groupName));
        }
    
    }
    
    private Login doCustomLazyload(WGDatabase db, String userName) throws WGException {
        
        // Get load script
        WGCSSJSModule mod = db.getCSSJSModule(_scriptCollect, WGScriptModule.CODETYPE_TMLSCRIPT);
        if (mod == null) {
            throw new WGException("Database '" + _dbkey + "' does not contain a TMLScript module of name '" + _scriptCollect + "'");
        }
    
        // Build a TMLScript runtime
        ExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        TMLContext context = new TMLContext(db.getDummyContent(db.getDefaultLanguage()), _core, null, null);
        Map<String,Object> objects = new HashMap<String,Object>();
        objects.put("username", userName);
   
        // Execute script
        ExpressionResult result = engine.evaluateExpression(mod.getCode(), context, ExpressionEngine.TYPE_SCRIPT, objects);
        if (result.isError()) {
            throw new WGException("Error executing custom load script", result.getException());
        }
        else if (result.getResult()==null) {
            return null;
        }
        else if (!(result.getResult() instanceof Login)) {
            throw new WGException("Error executing custom load script: The result is not of type " + Login.class.getName() + ": " + result.getResult());
        }
        
        return (Login) result.getResult();
    
        
    
    }

    
    public void migrateMembershipsToRelations(final Logger log) throws WGException {
            
        WGDatabase db = (WGDatabase) _core.getContentdbs().get(_dbkey);
        try {
            if (db == null) {
                throw new WGIllegalStateException("Cannot migrate memberships of db '" + _dbkey + "' because the db is not connected");
            }
            
            db.openSession();
            db.getSessionContext().setTask("CS Authentication Module - Migrating membership to relations");
            
            // First do default collect so we know the documents for each user/group name
            log.info("Collecting all user/group information once");
            doDefaultCollect(db);
            
            log.info("Migrate group memberships");
            WGContent rootContent = db.getContentByName(_groupsRootDoc);
            WGStructEntryList groupStructs = getGroupStructEntries(db, rootContent);
            for (WGStructEntry groupStruct : groupStructs) {
                groupStruct.visit(new WGPageVisitor() {
    
                    @Override
                    public void visit(WGArea area) {
                    }
    
                    @Override
                    public void visit(WGStructEntry struct) {
                    }
    
                    @Override
                    public void visit(WGContent content) {
                        try {
                            if (isGroupDefinition(content)) {
                                log.warn("Migrating memberships on group '" + content.getItemText(_groupnameItem) + "'");
                                migrateMembership(log, content);
                            }
                        }
                        catch (WGAPIException e) {
                            _core.getLog().error("Exception migrating group membership of document " + content.getDocumentKey(), e);
                        }
                        
                    }
                    
                });
            }
        }
        catch (WGException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGException("Error collecting authentications from db' " + _dbkey + "'", e);
        }
        finally {
            if (db.isSessionOpen()) {
                db.closeSession();
            }
        }
       
    }

    @SuppressWarnings("unchecked")
    protected void migrateMembership(Logger log, WGContent content) throws WGAPIException {
    
        for (String member : (List<String>) content.getItemValueList(_membersItem)) {
            
            Login login = _loginInformation.get(member.toLowerCase());
            if (login != null) {
                WGContent userDoc = (WGContent) content.getDatabase().getDocumentByKey(login.getDocumentkey());
                if (userDoc != null) {
                    content.addRelationToGroup(_membersItem, userDoc);
                    continue;
                }
                else {
                    log.warn("Error migrating member '" + member + "' on group '" + content.getItemText(_groupnameItem) + "': User document of key '" + login.getDocumentkey() + "' could not be looked up.");
                }
            }
            
            Group group = _groupInformation.get(member.toLowerCase());
            if (group != null) {
                WGContent groupDoc = (WGContent) content.getDatabase().getDocumentByKey(group.getDocumentKey());
                if (groupDoc != null) {
                    content.addRelationToGroup(_membersItem, groupDoc);
                    continue;
                }
                else {
                    log.warn("Error migrating member '" + member + "' on group '" + content.getItemText(_groupnameItem) + "': Group document of key '" + login.getDocumentkey() + "' could not be looked up.");
                }
            }
            
            log.warn("Error migrating member '" + member + "' on group '" + content.getItemText(_groupnameItem) + "': No user or group of that name");
            
        }
        content.removeItem(_membersItem);
        content.saveQuiet();
        
    }

}
