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
package de.innovationgate.wgpublisher;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.reflect.Constructor;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.security.cert.X509Certificate;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.concurrent.ConcurrentHashMap;
import java.util.prefs.Preferences;
import java.util.regex.PatternSyntaxException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.activation.DataHandler;
import javax.crypto.SecretKey;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.jsp.PageContext;
import javax.xml.bind.JAXBException;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.httpclient.URI;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.log4j.Appender;
import org.apache.log4j.AsyncAppender;
import org.apache.log4j.DailyRollingFileAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.XMLWriter;
import org.quartz.SchedulerException;
import org.quartz.impl.DirectSchedulerFactory;
import org.quartz.simpl.RAMJobStore;
import org.quartz.simpl.SimpleThreadPool;
import org.quartz.spi.JobStore;
import org.quartz.spi.ThreadPool;

import biz.minaret.log4j.DatedFileAppender;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.SingleValueConverter;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.license.LicenseException;
import de.innovationgate.utils.Base64;
import de.innovationgate.utils.ClassLoaderProvider;
import de.innovationgate.utils.DESEncrypter;
import de.innovationgate.utils.DynamicClassLoadingChain;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.PatternListVerifier;
import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.TempFileInputStream;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.TransientObjectWrapper;
import de.innovationgate.utils.URLBuilder;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.utils.cache.EHCacheCore;
import de.innovationgate.utils.net.IPAddress;
import de.innovationgate.utils.net.IPRestriction;
import de.innovationgate.utils.net.IPs;
import de.innovationgate.utils.security.HashedPassword;
import de.innovationgate.utils.security.HashingException;
import de.innovationgate.utils.security.SHA1HashingScheme;
import de.innovationgate.utils.security.SymmetricEncryptionEngine;
import de.innovationgate.webgate.api.DefaultMimetypeDeterminationService;
import de.innovationgate.webgate.api.WGACL;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabase.ConnectAction;
import de.innovationgate.webgate.api.WGDatabase.DatabaseAction;
import de.innovationgate.webgate.api.WGDatabaseConnectListener;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotator;
import de.innovationgate.webgate.api.WGFileConverter;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseCoreListener;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.auth.AuthenticationException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.CertAuthCapableAuthModule;
import de.innovationgate.webgate.api.auth.FileAuthenticationModule;
import de.innovationgate.webgate.api.auth.RequestBasedAuthenticationModule;
import de.innovationgate.webgate.api.fake.WGFakeContentStore;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.webgate.api.utils.ContentStoreDumpManager;
import de.innovationgate.webgate.api.workflow.WGDefaultWorkflowEngine;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.LogLevel;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.ElementMapping;
import de.innovationgate.wga.common.beans.csconfig.v1.EncoderMapping;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.JobDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.common.beans.csconfig.v2.Shortcut;
import de.innovationgate.wga.config.Administrator;
import de.innovationgate.wga.config.ClientRestriction;
import de.innovationgate.wga.config.ClusterConfiguration;
import de.innovationgate.wga.config.ConfigValidationException;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.config.DesignConfiguration;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.config.Domain;
import de.innovationgate.wga.config.FieldMapping;
import de.innovationgate.wga.config.FilterMapping;
import de.innovationgate.wga.config.MigrationMessage;
import de.innovationgate.wga.config.MigrationResult;
import de.innovationgate.wga.config.PersonalisationConfiguration;
import de.innovationgate.wga.config.PersonalisationDatabase;
import de.innovationgate.wga.config.Share;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.config.WGAConfigurationMigrator;
import de.innovationgate.wga.model.ValidationError;
import de.innovationgate.wga.model.VersionCompliance;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleRegistryChangeListener;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.options.DefaultOptionDefinition;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionDefinition;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.options.PasswordOptionEncoder;
import de.innovationgate.wga.modules.properties.DesignSourceProperties;
import de.innovationgate.wga.modules.types.AuthenticationSourceModuleType;
import de.innovationgate.wga.modules.types.ClusterServiceModuleType;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;
import de.innovationgate.wga.modules.types.ContentDatabasePublisherOptionsModuleType;
import de.innovationgate.wga.modules.types.ContentStoreModuleType;
import de.innovationgate.wga.modules.types.ContentStorePublisherOptionsModuleType;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;
import de.innovationgate.wga.modules.types.DesignSourceModuleType;
import de.innovationgate.wga.modules.types.FileAnnotatorModuleType;
import de.innovationgate.wga.modules.types.FilterConfigModuleType;
import de.innovationgate.wga.modules.types.HTMLHeadInclusionModuleType;
import de.innovationgate.wga.modules.types.HashingSchemeType;
import de.innovationgate.wga.modules.types.LanguageBehaviourModuleType;
import de.innovationgate.wga.modules.types.PasswordEncodingType;
import de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType;
import de.innovationgate.wga.modules.types.SchedulerTaskModuleType;
import de.innovationgate.wga.modules.types.ShareModuleType;
import de.innovationgate.wga.modules.types.VirtualLinkResolverModuleType;
import de.innovationgate.wga.modules.types.WGAServerOptionsModuleType;
import de.innovationgate.wga.modules.types.WGAWebServiceModuleType;
import de.innovationgate.wga.modules.types.WebTMLElementModuleType;
import de.innovationgate.wga.modules.types.WebTMLEncoderModuleType;
import de.innovationgate.wga.modules.types.WorkflowEngineModuleType;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.DBLoginInfo.AuthType;
import de.innovationgate.wgpublisher.SystemContainerManager.ContainerInfo;
import de.innovationgate.wgpublisher.auth.BruteForceLoginBlocker;
import de.innovationgate.wgpublisher.auth.CSAuthModule;
import de.innovationgate.wgpublisher.auth.DelegatingAuthModule;
import de.innovationgate.wgpublisher.auth.DomainRedirectionAuthModule;
import de.innovationgate.wgpublisher.auth.WGAAuthModuleFactory;
import de.innovationgate.wgpublisher.cache.FileCache;
import de.innovationgate.wgpublisher.cache.PostprocessedResourcesCache;
import de.innovationgate.wgpublisher.cache.WebTMLCache;
import de.innovationgate.wgpublisher.cluster.ClusterService;
import de.innovationgate.wgpublisher.cluster.SingleNodeClusterService;
import de.innovationgate.wgpublisher.design.WGADesign;
import de.innovationgate.wgpublisher.design.WGADesignManager;
import de.innovationgate.wgpublisher.design.WGADesignRetrievalException;
import de.innovationgate.wgpublisher.design.db.DBDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.sync.DesignFileValidator;
import de.innovationgate.wgpublisher.events.EventManager;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.IsolatedJARLoader;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptAppGlobalRegistry;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobalRegistry;
import de.innovationgate.wgpublisher.files.ImageFileConverter;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;
import de.innovationgate.wgpublisher.filter.WGAFilter;
import de.innovationgate.wgpublisher.filter.WGAFilterConfig;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.lang.DynamicLanguageBehaviour;
import de.innovationgate.wgpublisher.lang.InitializableLanguageBehaviour;
import de.innovationgate.wgpublisher.lang.LanguageBehaviour;
import de.innovationgate.wgpublisher.lang.OnlyDefaultLanguageBehaviour;
import de.innovationgate.wgpublisher.lang.StaticLanguageBehaviour;
import de.innovationgate.wgpublisher.log.AppLog;
import de.innovationgate.wgpublisher.log.AppLogAppender;
import de.innovationgate.wgpublisher.log.WGALoggerWrapper;
import de.innovationgate.wgpublisher.logserver.LogServer;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.lucene.analysis.FileHandler;
import de.innovationgate.wgpublisher.mail.WGAMailConfiguration;
import de.innovationgate.wgpublisher.mail.WGAMailNotification;
import de.innovationgate.wgpublisher.mail.WGAMailService;
import de.innovationgate.wgpublisher.modules.poptions.ContentDatabasePublisherOptionsCollector;
import de.innovationgate.wgpublisher.modules.poptions.ContentStorePublisherOptionsCollector;
import de.innovationgate.wgpublisher.modules.serveroptions.CacheModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.ServicesCollector;
import de.innovationgate.wgpublisher.modules.serveroptions.VariousOptionsCollector;
import de.innovationgate.wgpublisher.plugins.InvalidPluginException;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.plugins.WGAPlugin.Configuration;
import de.innovationgate.wgpublisher.plugins.WGAPluginSet;
import de.innovationgate.wgpublisher.problems.AccessLoggingScope;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.ContentShareScope;
import de.innovationgate.wgpublisher.problems.DBServerScope;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.DomainScope;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.JobScope;
import de.innovationgate.wgpublisher.problems.PluginScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemRegistry;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.scheduler.ConfigurationException;
import de.innovationgate.wgpublisher.scheduler.JavaTask;
import de.innovationgate.wgpublisher.scheduler.Job;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.scheduler.JobSchedule;
import de.innovationgate.wgpublisher.scheduler.OptimizeLuceneIndexTask;
import de.innovationgate.wgpublisher.scheduler.PendingReleaseTask;
import de.innovationgate.wgpublisher.scheduler.Scheduler;
import de.innovationgate.wgpublisher.scheduler.ScriptTask;
import de.innovationgate.wgpublisher.scheduler.Task;
import de.innovationgate.wgpublisher.sessions.AbstractWGAHttpSessionManager;
import de.innovationgate.wgpublisher.sessions.WGAHttpSessionListener;
import de.innovationgate.wgpublisher.shares.ShareDefinition;
import de.innovationgate.wgpublisher.shares.ShareInitException;
import de.innovationgate.wgpublisher.shares.ShareProperties;
import de.innovationgate.wgpublisher.so.NoopScopeObjectContextCreator;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;
import de.innovationgate.wgpublisher.so.ScopeProvider;
import de.innovationgate.wgpublisher.test.TestCore;
import de.innovationgate.wgpublisher.url.DefaultURLBuilder;
import de.innovationgate.wgpublisher.url.RequestIndependentDefaultURLBuilder;
import de.innovationgate.wgpublisher.url.RequestIndependentURLBuilder;
import de.innovationgate.wgpublisher.url.TitlePathManager;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.vlink.ExternalVirtualLinkResolver;
import de.innovationgate.wgpublisher.vlink.VirtualLinkResolver;
import de.innovationgate.wgpublisher.vlink.VirtualLinkTarget;
import de.innovationgate.wgpublisher.websockets.IndependentWebSocketManager;
import de.innovationgate.wgpublisher.websockets.PageConnectionManager;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletState;
import de.innovationgate.wgpublisher.webtml.utils.CRLFEncoder;
import de.innovationgate.wgpublisher.webtml.utils.FlagAwareFormatter;
import de.innovationgate.wgpublisher.webtml.utils.HTMLHeadInclusion;
import de.innovationgate.wgpublisher.webtml.utils.HTMLXMLEncodingFormatter;
import de.innovationgate.wgpublisher.webtml.utils.JSONEncodingFormatter;
import de.innovationgate.wgpublisher.webtml.utils.JavaScriptEncodingFormatter;
import de.innovationgate.wgpublisher.webtml.utils.NoneEncodingFormatter;
import de.innovationgate.wgpublisher.webtml.utils.PlainTextFormatter;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextAwareFormatter;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLScriptHDBListenerFactory;
import de.innovationgate.wgpublisher.webtml.utils.URLEncodingFormatter;
import de.innovationgate.wgpublisher.webtml.utils.URLQueryEncodingFormatter;
import de.innovationgate.wgpublisher.webtml.utils.UniqueNamePartFormatter;

/**
 * Base class of the WGA runtime, containing any configuration, maintaing access
 * to any data source. Know what you are doing when you use this class!
 */
public class WGACore implements WGDatabaseConnectListener, ScopeProvider, ClassLoaderProvider {    
    
    public static final WGACore INSTANCE = new WGACore();
    
    public static final String JOBNAME_PUBLISH_PENDING_RELEASE = "Publish contents pending release";
    public static final String JOBNAME_OPTIMIZE_LUCENE_INDEX = "Optimize Lucene Index";
    
    public static final List<Class<?>> DEFAULT_SERIALIZER_NONSERIALIZABLE_TYPES = Arrays.<Class<?>>asList(new Class[] {WGA.class, WGACore.class, WGDocument.class, WGDatabase.class, WGACL.class});

    public static final BitSet URLENCODER_PATH_PART_CHARACTERS = new BitSet(256);
    static {
        URLENCODER_PATH_PART_CHARACTERS.or(URI.allowed_within_path);
        URLENCODER_PATH_PART_CHARACTERS.clear('+');
        URLENCODER_PATH_PART_CHARACTERS.clear('%');
    }
    
    private WGFileConverter _fileConverter =  new ImageFileConverter();
    
    public static class UpdateConfigOccasion implements ProblemOccasion {

        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return WGACore.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
        
        @Override
        public int hashCode() {
            return getClass().hashCode();
        }
        
        @Override
        public boolean equals(Object obj) {
            return (obj instanceof UpdateConfigOccasion);
        }
        
        
    }
    
    public static class ConnectDatabaseProblemOccasion implements ProblemOccasion {
        
        private String _dbkey;
        private DatabaseScope _scope;
        
        public ConnectDatabaseProblemOccasion(String dbkey) {
            _dbkey = dbkey;
            _scope = new DatabaseScope(dbkey);
        }
        
        @Override
        public boolean isClearedAutomatically() {
            return true;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_dbkey == null) ? 0 : _dbkey.hashCode());
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
            ConnectDatabaseProblemOccasion other = (ConnectDatabaseProblemOccasion) obj;
            if (_dbkey == null) {
                if (other._dbkey != null)
                    return false;
            }
            else if (!_dbkey.equals(other._dbkey))
                return false;
            return true;
        }

        @Override
        public ProblemScope getDefaultScope() {
            return _scope;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }
        
        @Override
        public Class<?> getDefaultRefClass() {
            return WGACore.class;
        }

    }
    
    public class URLEncoder {
        
        
        public String encodeQueryPart(String str) throws URIException {
            
            if (str != null) {
                return URIUtil.encodeWithinQuery(str, getCharacterEncoding());
            }
            else {
                return null;
            }
            
        }
        
        public String encodePathPart(String str) throws URIException {
            
            if (str != null) {
                return URIUtil.encode(str, URLENCODER_PATH_PART_CHARACTERS, getCharacterEncoding());
            }
            else {
                return null;
            }
            
        }
        
        public String encodePath(String str) throws URIException {
            
            List<String> pathElements = new ArrayList<String>(); 
            for (String elem : str.split("/")) {
                pathElements.add(encodePathPart(elem));
            }
            return  WGUtils.serializeCollection(pathElements, "/");
            
        }
        
        public String decode(String str) throws URIException {
            
            if (str != null) {
                try {
                    return WGUtils.decodeURI(str, getCharacterEncoding());
                }
                catch (UnsupportedEncodingException e) {
                    throw new URIException("Unsupported encoding: " + e.getMessage());
                }
                catch (MalformedURLException e) {
                    throw new URIException("Malformed URL: " + e.getMessage());
                }
            }
            else {
                return null;
            }
            
        }
        
    }
    
    private URLEncoder _urlEncoder = new URLEncoder();
    public URLEncoder getURLEncoder() {
        return _urlEncoder;
    }
    

    public interface OptionFetcher {
        Map<String,String> fetch(WGAConfiguration config);
    }
    
    /**
     * Map to store session logins, either transient or persistent depending on the serializability of their credentials
     * The map may return null for values that were created transiently on another cluster node.
     */
    public static class SessionLoginMap implements Map<Object,DBLoginInfo>, Serializable {

        private static final long serialVersionUID = 1L;
        
        private Map<Object,TransientObjectWrapper<DBLoginInfo>> _map = new HashMap<Object, TransientObjectWrapper<DBLoginInfo>>();
        
        
        @Override
        public DBLoginInfo put(Object key, DBLoginInfo value) {
            TransientObjectWrapper<DBLoginInfo> wrapper = wrap(value);
            
            DBLoginInfo oldValue = get(key);
            if (oldValue != null) {
                
                if (oldValue.equals(value)) {
                    return oldValue;
                }
                
                if (oldValue.getAccessFilter() != null && value.getAccessFilter() == null) {
                    value.setAccessFilter(oldValue.getAccessFilter());
                }
                for (Map.Entry<String,String> dbFilter : oldValue.getDbAccessFilters().entrySet()) {
                    if (!value.getDbAccessFilters().containsKey(dbFilter.getKey())) {
                        value.getDbAccessFilters().put(dbFilter.getKey(), dbFilter.getValue());
                    }
                }
                
            }
            
            return unwrap(_map.put(key, wrapper));
        }

        private TransientObjectWrapper<DBLoginInfo> wrap(DBLoginInfo value) {
            TransientObjectWrapper<DBLoginInfo> wrapper;
            if (value.getCredentials() == null || value.getCredentials() instanceof Serializable) {
                wrapper = new TransientObjectWrapper<DBLoginInfo>(false);
            }
            else {
                wrapper = new TransientObjectWrapper<DBLoginInfo>(true);
            }
            wrapper.set(value);
            return wrapper;
        }


        private DBLoginInfo unwrap(TransientObjectWrapper<DBLoginInfo> wrapper) {
            if (wrapper == null) {
                return null;
            }
            else {
                return wrapper.get();
            }
        }

        @Override
        public void clear() {
            _map.clear();
        }

        @Override
        public boolean containsKey(Object arg0) {
            return _map.containsKey(arg0);
        }

        @Override
        public boolean containsValue(Object arg0) {
            for (TransientObjectWrapper<DBLoginInfo> wrapper : _map.values()) {
                if (arg0.equals(wrapper.get())) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public Set<java.util.Map.Entry<Object, DBLoginInfo>> entrySet() {
            
            Set<java.util.Map.Entry<Object, DBLoginInfo>> set = new HashSet<Map.Entry<Object,DBLoginInfo>>();
            for (final Map.Entry<Object,TransientObjectWrapper<DBLoginInfo>> entry : _map.entrySet()) {
                set.add(new Map.Entry<Object, DBLoginInfo>() {
                    
                    @Override
                    public Object getKey() {
                        return entry.getKey();
                    }

                    @Override
                    public DBLoginInfo getValue() {
                        return unwrap(entry.getValue());
                    }

                    @Override
                    public DBLoginInfo setValue(DBLoginInfo value) {
                        return unwrap(entry.setValue(wrap(value))); 
                    }
                    
                    
                });
            }
            return set;
        }

        @Override
        public DBLoginInfo get(Object arg0) {
            return unwrap(_map.get(arg0));
        }

        @Override
        public boolean isEmpty() {
            return _map.isEmpty();
        }

        @Override
        public Set<Object> keySet() {
            return _map.keySet();
        }

        @Override
        public void putAll(Map<? extends Object, ? extends DBLoginInfo> map) {
            for (Object key : map.keySet()) {
                put(key, map.get(key));
            }
        }

        @Override
        public DBLoginInfo remove(Object arg0) {
            return unwrap(_map.remove(arg0));
        }

        @Override
        public int size() {
            return _map.size();
        }

        @Override
        public Collection<DBLoginInfo> values() {
            
            List<DBLoginInfo> values = new ArrayList<DBLoginInfo>();
            for (TransientObjectWrapper<DBLoginInfo> wrapper : _map.values()) {
                values.add(wrapper.get());
            }
            return values;
            
        }
        
    }

    public class ServletFilterUpdater implements ModuleRegistryChangeListener {

        @Override
        public void moduleRegistryChanged(ModuleRegistry registry, Class<? extends ModuleType> moduleType) {

            initReadFilterMappings();
            if (getFilter() != null) {
                getFilter().initFilterChain();
            }
            

        }

    }
    
    public class FileAnnotatorUpdater implements ModuleRegistryChangeListener {

        @Override
        public void moduleRegistryChanged(ModuleRegistry registry, Class<? extends ModuleType> moduleType) {

            try {
                for (WGDatabase db : getContentdbs().values()) {
                    updateFileAnnotators(db);
                }
            }
            catch (Throwable e) {
                getLog().error("Exception updating file annotators", e);
            }
            
        }
        
                
        
    }

    public class HTMLHeadInclusionUpdater extends WGACore implements ModuleRegistryChangeListener {

        @Override
        public void moduleRegistryChanged(ModuleRegistry registry, Class<? extends ModuleType> moduleType) {

            List<HTMLHeadInclusion> htmlHeadInclusions = new ArrayList<HTMLHeadInclusion>();
            for (ModuleDefinition def : registry.getModulesForType(HTMLHeadInclusionModuleType.class).values()) {
                try {
                    HTMLHeadInclusion inc = (HTMLHeadInclusion) registry.instantiate(def);
                    htmlHeadInclusions.add(inc);
                }
                catch (Throwable e) {
                    getLog().error("Exception processing HTML head inclusion " + def.getTitle(Locale.getDefault()), e);
                }
            }
            _htmlHeadInclusions = htmlHeadInclusions;

        }

    }

    public static final String EXTERNAL_PERSDBS_FOLDER = "#persdbs";
    
    
    public class WGAPublisherOptionReader implements ModuleRegistryChangeListener {
        
        private Map<String,OptionDefinition> _optionDefCache = new ConcurrentHashMap<String, OptionDefinition>();
        
        public Object readPublisherOptionOrDefault(WGDatabase db, String name) {

            Class typeClass = ContentDatabasePublisherOptionsModuleType.class;
            Class collectorClass = ContentDatabasePublisherOptionsCollector.class;
            if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                typeClass = ContentStorePublisherOptionsModuleType.class;
                collectorClass = ContentStorePublisherOptionsCollector.class;
            }
            String cacheKey = typeClass.getName() + ":" + name;
            
            // Fetch option definition
            OptionDefinition optionDef = _optionDefCache.get(cacheKey);
            if (optionDef == null) {
                
                ModuleDefinition pOptionDefs = WGACore.this.getModuleRegistry().getModuleDefinition(typeClass, collectorClass);
                optionDef = pOptionDefs.getOptionDefinitions().get(name);
                
                if (optionDef == null) {
                    optionDef = new DefaultOptionDefinition(name);
                }
                
                _optionDefCache.put(cacheKey, optionDef);
                
            }

            // Read the option value, default it if neccessary
            Object value = db.getAttribute(name);
            if (value == null && optionDef.getDefaultValue() != null) {
                value = optionDef.getDefaultValue();
            }
            
            try {
                return OptionReader.unconvertOptionValue(optionDef, value);
            }
            catch (Exception e) {
                throw new RuntimeException("Exception unconverting publisher option '" + name + "' with value '" + value + "' on database '" + db.getDbReference() + "'", e);
            }
                  
        }

        @Override
        public void moduleRegistryChanged(ModuleRegistry registry, Class<? extends ModuleType> moduleType) {
            _optionDefCache.clear();
        }
               
    }
    
    /**
     * A reader for WGA configuration options, that always takes the current WGA configuration and caches once read option values
     */
    public class WGAConfigurationOptionReader {
        

        private OptionFetcher _optionFetcher = null;
        private Class<? extends ModuleType> _typeClass;
        private Class<?> _implClass;
    
        protected WGAConfigurationOptionReader(OptionFetcher optionFetcher, Class<? extends ModuleType> typeClass, Class<?> implClass) {
            _optionFetcher = optionFetcher;
            _typeClass = typeClass;
            _implClass = implClass;
            
            Map<String,String> target = _optionFetcher.fetch(getWgaConfiguration());
            if (target == null) {
                throw new IllegalArgumentException("Option fetcher does return options map");
            }
    
        }
        
        public Object readOptionValueOrDefault(String optionName) {
            Map<String,String> options = _optionFetcher.fetch(getWgaConfiguration());
            ModuleDefinition modDef = getModuleRegistry().getModuleDefinition(_typeClass, _implClass);
            if (modDef == null) {
                getLog().error("Cannot read module definition " + _typeClass.getName() + "/" + _implClass.getName());
                return null;
            }
            
            
            try {
                return getWgaConfiguration().getCachingOptionReader().readOptionValueOrDefault(options, optionName, modDef);
            }
            catch (OptionConversionException e) {
                getLog().error("Exception reading server option '" + optionName + "'. Falling back to default", e);
                OptionDefinition optDef = modDef.getOptionDefinitions().get(optionName);
                if (optDef != null) {
                    try {
                        return OptionReader.unconvertOptionValue(optDef, optDef.getDefaultValue());
                    }
                    catch (OptionConversionException e1) {
                        getLog().error("Exception unconverting default value of server option '" + optionName + "'. Falling back to null", e);
                    }
                }

                return null;
                
            }
        }
        
    }
    
    public WGAConfigurationOptionReader getConfigOptionReader(OptionFetcher fetcher, Class<? extends ModuleType> typeClass, Class<?> implClass) {
        return new WGAConfigurationOptionReader(fetcher, typeClass, implClass);
    }
    
    public class WGAMimetypeDeterminationService extends DefaultMimetypeDeterminationService {

        public String determineByFilename(String fileName) {
            String mimeType = getServletContext().getMimeType(fileName);
            if (mimeType == null) {
                mimeType = super.determineByFilename(fileName);
            }
            return mimeType;
        }
        
    }

    public class VariableReplaceProcessor implements ReplaceProcessor {

        public int replace(String text, int from, int to, Writer out) throws IOException {
            
            // Isolate the variable
            int varEnd = text.indexOf("}", from);
            if (varEnd == -1) {
                out.write(text.substring(from, to + 1));
                return to + 1;
            }
            
            // Get the variable name
            String varName = text.substring(from + 2, varEnd).trim();
            
            // Perform replacements
            if (varName.equals("wga.cfgdir")) {
                out.write(getConfigFile().getParentFile().getAbsolutePath());
            }
            else if (varName.equals("wga.datadir")) {
                out.write(getWgaDataDir().getAbsolutePath());
            }
            else if (varName.equals("wga.devpluginsdir")) {
                String devPluginsPath = getDeveloperPluginsPath();
                if (devPluginsPath != null) {
                    out.write(devPluginsPath);
                }
                else {
                    out.write(getConfigFilePath());
                }
            }
            else if (varName.equals("wga.defaultpluginsdir")) {
                out.write( getServletContext().getRealPath("/WEB-INF/default-plugins"));
            }

            else if (varName.startsWith("sys.")) {
                out.write(System.getProperty(varName.substring(5)));
            }
            else if (varName.startsWith("env.")) {
                out.write(System.getenv(varName.substring(5)));
            }
            
            return varEnd + 1;
            
            
        }

    }
    
    public final VariableReplaceProcessor _replaceProcessorInstance = new VariableReplaceProcessor();
    
    
    
    /**
     * Domain name used for administrative logins
     */
    public static final String DOMAIN_ADMINLOGINS = "$adminlogins";

    public static final String ENCODER_CRLF = "crlf";

    public static final String ENCODER_RTFSYSTEM = "rtfsystem";

    public static final String ENCODER_RTF = "rtf";
    
    public static final String ENCODER_PLAINTEXT = "plaintext";

    private static final String ENCODER_XML = "xml";

    public static final String ENCODER_HTML = "html";
    

    
    public static final String ENCODER_URL = "url";
    public static final String ENCODER_URLQUERY = "urlquery";

    public static final String ENCODER_JAVASCRIPT = "javascript";
    
    public static final String ENCODER_JSON = "json";
    
    public static final String ENCODER_NAMEPART = "np";

    public static final String SYSPROPERTY_UNITTEST = "de.innovationgate.wga.unittest";
    
    public static final String SYSPROPERTY_UNITTEST_LOGDIR = "de.innovationgate.wga.unittest.logdir";
    
    public static final String SYSPROPERTY_JDBC_HOTPATCHES = "de.innovationgate.wga.jdbc.hotpatches";
    
    public static final String SYSPROPERTY_DEVELOPMENT_MODE = "de.innovationgate.license.DevelopmentModeEnabled";

    public static final String DBATTRIB_DEFAULTPORT_BASE = "DefaultPort";
    
    public static final String DBATTRIB_DEFAULTPORT_HTTP = "DefaultPortHTTP";
    
    public static final String DBATTRIB_DEFAULTPORT_HTTPS = "DefaultPortHTTPS";
    
    

    public static final PatternLayout LAYOUT_APPLOG = new PatternLayout("%d{dd.MM.yyyy HH:mm:ss} %p %m\n");

    public static final String SYSPROPERTY_DEFAULT_PLUGINS = "de.innovationgate.wga.defaultplugins";
    
    public static final String SYSPROPERTY_DEVELOPER_PLUGINS = "de.innovationgate.wga.devplugins";

    public static final String SYSPROPERTY_LOGPATH = "de.innovationgate.wga.logpath";
    
    public static final String DBATTRIB_PUBLISH_CONTENTFILES_WITH_DESIGNENCODING = "PublishContentFilesWithDesignEncoding";
    
    public static final boolean DBATTRIBDEFAULT_PUBLISH_CONTENTFILES_WITH_DESIGNENCODING = true;
    
    /*
     * Server options 
     */
    public static final String SERVEROPTION_SERVER_SCALINGTHRESHOLD = "Server.ScalingThreshold";
    
    public static final String SERVEROPTION_WEBTML_FILEUPLAD_MAXSIZE = "WebTML.FileUpload.MaxSize";
    
    public static final String SERVEROPTION_WEBTML_DIRECT_OUTPUT = "WebTML.DirectOutput";
    
    public static final String SERVEROPTION_SERVER_TESTSESSIONVARSERIALIZABLE = "Server.TestSessionVarSerializable";
    
    public static final String SERVEROPTION_SERVER_SESSIONTIMEOUT = "Server.SessionTimeout";
    public static final Integer SERVEROPTIONDEFAULT_SESSIONTIMEOUT = 60;

    /**
     * Server option for redirecting Logins to a specific protocol
     */
    public static final String SERVEROPTION_LOGINREDIRECTPROTOCOL = "Server.LoginRedirect.Protocol";


    /**
     * Server option for redirecting Logins to a specific host
     */
    public static final String SERVEROPTION_LOGINREDIRECTHOST = "Server.LoginRedirect.Host";

    /**
     * Server option for redirecting Logins to a specific port
     */
    public static final String SERVEROPTION_LOGINREDIRECTPORT = "Server.LoginRedirect.Port";
    
    /**
     * Server option determining the maximum size of the thread pool executing asynchronous events
     */
    public static final String SERVEROPTION_EVENTMANAGER_THREADPOOLSIZE = "Server.EventManager.ThreadPoolSize";
    
    public static final String SERVEROPTION_WEBSOCKETS_SESSION_WORKAROUND = "Server.WebSockets.SessionWorkaround";

    
    public static final String DBATTRIB_DIRECTACCESSDEFAULT = "DirectAccessDefault";
    
    /**
     * Publisher option defining the encoding of design resources - determines
     * the encoding in which file base resources are read from filesystem -
     * determines the source encoding for textual files within file containers
     */
    public static final String DBATTRIB_DESIGN_ENCODING = "DesignEncoding";
    
    /**
     * Publisher option for redirecting URLs to this DB to a different protocol
     */
    public static final String DBATTRIB_REDIRECTPROTOCOL = "RedirectProtocol";

    /**
     * Publisher option for redirecting URLs to this DB to a different host
     */
    public static final String DBATTRIB_REDIRECTHOST = "RedirectHost";

    /**
     * Publisher option for redirecting URLs to this DB to a different port
     */
    public static final String DBATTRIB_REDIRECTPORT = "RedirectPort";
    
    public static final String DBATTRIB_HDB_USE_VERSIONING = WGHierarchicalDatabase.DBATTRIB_HDB_USE_VERSIONING;
    
    
    public static final String SYSTEMLABEL_BASE_PATH = "de.innovationgate.wgpublisher.labels.";

    public static final String SERVEROPTION_DEFAULT_AUTHORING_APP = "DefaultAuthoringApp";

    
    public class ValidateDefaultLanguageAction implements ConnectAction {

        public void run(WGDatabase db) throws Exception {
            
            // Look if it is defined
            WGLanguage defaultLanguage = db.getLanguage(db.getDefaultLanguage());
            if (defaultLanguage == null || defaultLanguage.isDummy()) {
                if (db.getLanguages().size() > 0) {
                    //WGACore.this.log.warn("The default language '" + db.getDefaultLanguage() + "' is not defined in database " + db.getDbReference() + ". As other language definitions exist this may be misconfigured.");
                }
            }
            
            // Is there some root content in the default language? If so, we are satisfied.
            if (defaultLanguage != null) {
                WGContent content = db.getFirstReleasedContent(defaultLanguage.getName(), true);
                if (content != null) {
                    return;
                }
            }
            
            // No root content in default language? This may still be ok if there is no content at all. We try this with all languages.
            WGContent content = null;
            for (WGLanguage lang : db.getLanguages().values()) {
                content = db.getFirstReleasedContent(lang.getName(), true);
                if (content != null) {
                    break;
                }
            }
            if (content == null) {
                return;
            }

               
            // It seems there is content, but the currently selected default language is not available in any root doc.
            // Now we ask the db to determine a new default language
            db.determineDefaultLanguage();
            WGACore.this.log.info("Default language of database " + db.getDbReference() + " changed to '" + db.getDefaultLanguage() + "'");
            
            
        }
        
    }
    

    public class UserAgentVerifier extends PatternListVerifier {

        public UserAgentVerifier(PersonalisationConfiguration config, WGACore core) {

            Iterator<String> agentExclusions = config.getPersonalisationAgentExclusions().iterator();
            while (agentExclusions.hasNext()) {
                String patternStr = agentExclusions.next();
                try {
                    addPattern(patternStr);
                }
                catch (PatternSyntaxException e) {
                    core.getLog().error("Cannot parse user agent exclusion as regular expression: " + patternStr);
                }
            }

        }

        public boolean isValidUserAgent(String userAgent) {
            
            // We accept no empty user agent, as it cannot be verified
            if (userAgent == null) {
                return false;
            }
            
            return (verify(userAgent) == null);
        }

    }

    public static final String ATTRIB_TMLDEBUG_TRACE_RESULTS = "tmlDebugTraceResults";



    public static final String ATTRIB_CURRENTTML = "currenttml";
    public static final String ATTRIB_CURRENTTMLDB = "currenttmldb";

    public static final String ATTRIB_AJAXINFO = "ajaxInfo";
    
    public static final String ATTRIB_REQUEST_CANCELLED = "RequestCancelled";



    private Scheduler _scheduler;

    private BruteForceLoginBlocker bruteForceLoginBlocker;



    public static final String ATTRIB_TMLDEBUG_DOCUMENTS = "tmlDebugDocuments";

    public static final String ATTRIB_TMLDEBUG = "tmlDebug";
    
    public static final String ATTRIB_TMLDEBUG_DISABLE_TMLSCRIPT_OPTIMIZATION = "tmlDebugDisableTMLScriptOptimization";

    public static final String ATTRIB_REQUESTTYPE = "RequestType";

    public static final String ATTRIB_EDITDOCUMENT = "editDocument";

    public static final String DBSESSIONCONTEXT_REQUEST = "Request";

    public static final String WGAMANAGER_URL = "wgamanager-4.0.jnlp";
    
    public static final String LANGUAGEBEHAVIOUR_DEFAULT = "default";
    public static final String LANGUAGEBEHAVIOUR_MAINCONTENT = "maincontent";
    public static final String LANGUAGEBEHAVIOUR_BROWSER = "browser";

    public static final String URL_PARAM_CLEAN = "$clean";
    
    private static DynamicClassLoadingChain libraryClassLoadingChain;

    private EventManager _eventManager;

    public static final String SESSION_COOKIESET = "CookieSet:";

    public static final String DBATTRIB_SESSIONCOOKIE = "SessionCookie";

    public static final String pubKey = "MIIBtzCCASwGByqGSM44BAEwggEfAoGBAP1/U4EddRIpUt9KnC7s5Of2EbdSPO9EAMMeP4C2USZpRV1AIlH7WT2NWPq/xfW6MPbLm1Vs14E7gB00b/JmYLdrmVClpJ+f6AR7ECLCT7up1/63xhv4O1fnxqimFQ8E+4P208UewwI1VBNaFpEy9nXzrith1yrv8iIDGZ3RSAHHAhUAl2BQjxUjC8yykrmCouuEC/BYHPUCgYEA9+GghdabPd7LvKtcNrhXuXmUr7v6OuqC+VdMCz0HgmdRWVeOutRZT+ZxBxCBgLRJFnEj6EwoFhO3zwkyjMim4TwWeotUfI0o4KOuHiuzpnWRbqN/C/ohNWLx+2J6ASQ7zKTxvqhRkImog9/hWuWfBpKLZl6Ae1UlZAFMO/7PSSoDgYQAAoGAMfEHZxC9JGRuMSI9fuKewoha0wsywr6Yi/F6k+YERZKD8GIPdzpWqcD8kQ0FZqxaI7T+1xH0oRbnr1aVsyZSfRICuhWUcoJrThsuuYBFmjMtl8XDW9saa0VDQirxpH5ee3JqCHJurp6ik0XtqI25NBVLiJZQ1aJt6Znt4FPEosw=";

	private static final String SYSPROP_SKIP_LOCAL_ADMIN_LOGINS = "de.innovationgate.wga.skipLocalAdminLogins";
	
	private static final String SYSPROP_WARNINGS_ON_SERVER_CONSOLE = "de.innovationgate.wga.outputWarningsOnConsole";

    /**
     * checks if a login-retry is allowed - user can be redirected to login page
     * 
     * @param db
     * @param session
     * @return
     */
    public boolean allowLoginRetry(WGDatabase db, HttpSession session) {

        boolean dbCertAuth = db.certAuthEnabled();
        boolean domCertAuth = certAuthEnabledForDomain((String) db.getAttribute(WGACore.DBATTRIB_DOMAIN));

        if (!dbCertAuth && domCertAuth) {
            // we have a configuration conflict
            log.warn("The domain '" + db.getAttribute(WGACore.DBATTRIB_DOMAIN) + "' is enabled for certificate authentication, but db '" + db.getDbReference()
                    + "' is not configured to support certificates. All dbs in a domain must support the same user credentials.");
        }

        if (dbCertAuth || domCertAuth) {
            // on certificate authentication a login retry makes no sense
            // user should not be redirected to login page
            return false;
        }
        else {
            DBLoginInfo loginInfo = getSessionLogins(session).get(db.getAttribute(WGACore.DBATTRIB_DOMAIN));
            return (loginInfo == null || loginInfo.isAnonymous());
        }

    }

    public String getDeveloperPluginsPath() {
        String customDefaultPlugins = System.getProperty(SYSPROPERTY_DEVELOPER_PLUGINS);
        if (customDefaultPlugins != null) {
            File pluginsFolder = getWGAFile(customDefaultPlugins);
            return pluginsFolder.getAbsolutePath();
        }
        else {
            return null;
        }
    }

    public boolean isAuthor(WGDatabase db, String ip, String type) {

        if (db == null) {
            return false;
        }

        if (!db.isSessionOpen()) {
            return false;
        }

        // Check db access rights
        if (db.getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_AUTHOR) {
            return false;
        }
        
        return true;
    }

    public boolean isAdminLogin(String name, String password) {
    	return isAdminLogin(name, password, null);        
    }
    
    public boolean isAdminLogin(String name, String password, HttpServletRequest request) {
    	// check if we should allow passwordless admin login from localhost
    	if (request != null && isLocalRequest(request) && Boolean.parseBoolean(System.getProperty(SYSPROP_SKIP_LOCAL_ADMIN_LOGINS, "false"))) {
    		return true;
    	}
    	
        if (name == null || password == null) {
            return false;
        }

        Administrator admin = (Administrator) _wgaConfiguration.getAdministrator(name);
        boolean loggedIn = getBruteForceLoginBlocker().adminLogin(name, password, admin, request);
        if (request != null) {
            if (!loggedIn) {
            	log.warn("Failed admin login attempt for account '" + name + "' from IP '" + request.getAttribute(WGAFilter.REQATTRIB_ORIGINAL_IP) + "'.");
            }
        }
        return loggedIn;
    }

    public boolean doAdminLogin(String name, String password, HttpServletRequest request) {
    	if (isAdminLogin(name, password, request)) {
    		request.getSession().setAttribute(WGACore.SESSION_ADMINNAME, name);
    		request.getSession().setAttribute(WGACore.SESSION_ADMINPASSWORD, password);
    		return true;
    	} else {
    		request.getSession().removeAttribute(WGACore.SESSION_ADMINNAME);
    		request.getSession().removeAttribute(WGACore.SESSION_ADMINPASSWORD);
    		return false;
    	}
    }

    public void doAdminLogout(HttpServletRequest request) {
   		request.getSession().removeAttribute(WGACore.SESSION_ADMINNAME);
   		request.getSession().removeAttribute(WGACore.SESSION_ADMINPASSWORD);
    }
    
    public String getElementClassForName(String name) throws TMLException {
        String className = this.systemElements.get(name.toLowerCase());
        if (className == null) {
            className = this.customElements.get(name.toLowerCase());
        }
        
        
        if (className == null) {
            ModuleDefinition modDef = getModuleRegistry().getModuleDefinitionByKey(WebTMLElementModuleType.class, name.toLowerCase());
            if (modDef != null) {
                try {
                    modDef.testDependencies();
                }
                catch (ModuleDependencyException e) {
                    throw new TMLException("WebTML element '" + name + "' not available bc. of missing dependency: " + e.getMessage(), true);
                }
                className = modDef.getImplementationClass().getName();
            }
        }
         
        return className;
    }

    public String getConfigFilePath() {
        try {
            return this.getConfigFile().getPath();
        }
        catch (Exception e) {
            this.log.error("Error retrieving config file path", e);
            return "(Error retrieving config file path)";
        }

    }

    







    public MediaKey getMediaKey(String key) {

        MediaKey mediaKey = this.systemMediaKeys.get(key);
        if (mediaKey == null) {
           mediaKey = this.customMediaKeys.get(key);
        }
        return mediaKey;
    }
    
     public Set<String> getMediaKeys() {
        
        Set<String> set = new HashSet<String>();
        set.addAll(systemMediaKeys.keySet());
        set.addAll(customMediaKeys.keySet());
        return set;
        
    }

    public boolean isDeploymentKeyDefined(String layoutKey) {
        return this._deployer.getLayoutMappings().containsKey(layoutKey);
    }



    public static SessionLoginMap getSessionLogins(javax.servlet.http.HttpSession session) {

        SessionLoginMap logins = (SessionLoginMap) session.getAttribute(WGPDispatcher.SESSION_LOGINS);
        if (logins == null) {
            logins = new SessionLoginMap();
            session.setAttribute(WGPDispatcher.SESSION_LOGINS, logins);
        }
        return logins;

    }

    public WGDatabase openContentDB(String key, javax.servlet.http.HttpServletRequest request) throws WGException {
        return openContentDB(key, request, false);
    }

    public WGDatabase openContentDB(String key, javax.servlet.http.HttpServletRequest request, boolean useMaster) throws WGException {

        WGDatabase db = this.contentdbs.get(key);
        if (db != null) {
            return openContentDB(db, request, useMaster);
        }
        else {
            return null;
        }

    }

    public WGDatabase openContentDB(WGDatabase db, javax.servlet.http.HttpServletRequest request, boolean useMaster) throws WGException {
        return openContentDB(db, request, (request != null ? request.getSession() : null), useMaster, null);
    }

    public WGDatabase openContentDB(WGDatabase db, HttpServletRequest request, HttpSession session, boolean useMaster, WGDatabase hintDB) throws WGException {

        // If already open, and useMaster==isMaster, just return
        if (db.isSessionOpen()) {
            if (db.getSessionContext().isMasterSession() == useMaster) {
                return db;
            }
        }
        
        // If db is not ready throw an exception
        if (!db.isReady()) {
            throw new WGUnavailableException(db);
        }

        // Force master login
        if (useMaster) {
            db.openSession();
            return prepareDB(db, request);
        }

        // check client access to this db
        if (request != null && !this.isClientPermitted(db, request)) {
            throw new ClientAccessException("Client '" + request.getRemoteAddr() + "' is not permitted to access app '" + db.getDbReference() + "'.");
        }
        
        // Load config and login info from domain
        WGADomain domainConfig = getDomainForDatabase(db);
        DBLoginInfo sessionLoginInfo = null;
        String accessFilter = null;
        if (session != null) {
            sessionLoginInfo = WGACore.getSessionLogins(session).get(domainConfig.getName());
            if (sessionLoginInfo != null) {
                if (sessionLoginInfo.getDbAccessFilters().containsKey(db.getDbReference()))  {
                    accessFilter = sessionLoginInfo.getDbAccessFilters().get(db.getDbReference());
                }
                else {
                    accessFilter = sessionLoginInfo.getAccessFilter();
                }
            }
        }
        
        // Check if regular or request based login is forced by some filter
        Boolean forceRegular = (request != null ? (Boolean) request.getAttribute(ATTRIB_FORCEREGULARLOGIN) : null);
        if (forceRegular == null) {
            forceRegular = Boolean.FALSE;
        }
        Boolean forceRequestBased = (request != null ? (Boolean) request.getAttribute(ATTRIB_FORCEREQUESTBASELOGIN) : null);
        if (forceRequestBased == null) {
            forceRequestBased = Boolean.FALSE;
        }
        
        // Load HTTP basic auth information
        DBLoginInfo httpLoginInfo =(request != null ? (DBLoginInfo) request.getAttribute(WGPRequestPath.REQATTRIB_HTTPLOGIN) : null);

        // Certificate based login
        if (request != null && !forceRegular.booleanValue() && db.certAuthEnabled()) {
            return openContentDBCertAuth(db, request, accessFilter);
        } 
        
        // Requestinfo based login
        else if (request != null && (!forceRegular || forceRequestBased) && db.getAuthenticationModule() != null && db.getAuthenticationModule() instanceof RequestBasedAuthenticationModule) {
            return openContentDBRequestBased(db, request, accessFilter);
        }
        
        // HTTP basic authentication login
        else if (httpLoginInfo != null) {
            getBruteForceLoginBlocker().login(db, httpLoginInfo.getUserName(), httpLoginInfo.getCredentials(), accessFilter, request);
            if (db.isSessionOpen() && session != null) {
                updateLoginInfo(db, request, DBLoginInfo.AuthType.PASSWORD);
            }
            return prepareDB(db, request);
        }

        // do standard login process
        else {

            // Try to login via session token
            if (request != null && db.hasFeature(WGDatabase.FEATURE_SESSIONTOKEN)) {
                String cookieName = (String) db.getAttribute(WGACore.DBATTRIB_SESSIONCOOKIE);
                if (cookieName != null) {
                    Cookie[] cookies = request.getCookies();
                    if (cookies != null) { // Can actually happen, especially
                                            // with
                        // non-browser http clients
                        Cookie tokenCookie = null;
                        for (int idx = 0; idx < cookies.length; idx++) {
                            if (cookies[idx].getName().equals(cookieName)) {
                                tokenCookie = cookies[idx];
                                break;
                            }
                        }
                        if (tokenCookie != null) {
                            db.openSession(WGDatabase.SESSIONTOKEN_USER, tokenCookie.getValue(), accessFilter, request);
                            if (db.isSessionOpen()) {
                                // CONSIDERED HARMFUL: Session tokens may
                                // expire. Safer to always retrieve "fresh" from
                                // cookie
                                // loginInfo = new
                                // DBLoginInfo(WGDatabase.SESSIONTOKEN_USER,
                                // tokenCookie.getValue());
                                // this.getSessionLogins(session).put(domainConfig.getName(),
                                // loginInfo);
                                return prepareDB(db, request);
                            }
                        }
                    }
                }
            }

            // If no session available, log in by hint or anonymous
            if (session == null) {
             
                if (hintDB != null && hintDB.isSessionOpen()) {
                    if (hintDB.getSessionContext().isMasterSession()) {
                        db.openSession();
                    }
                    else {
                        db.openSession(hintDB.getSessionContext().getUser(), hintDB.getSessionContext().getPassword(), accessFilter, request);
                    }
                    if (db.isSessionOpen()) {
                        return prepareDB(db, request);
                    }
                    
                }

                db.openSession(WGDatabase.ANONYMOUS_USER, null, null, request);
                return prepareDB(db, request);
            }

            // Try to login by previously stored domain-specific login
            if (sessionLoginInfo != null && !WGDatabase.ANONYMOUS_USER.equals(sessionLoginInfo.getUserName())) {
                int accessLevel = db.openSession(sessionLoginInfo.getUserName(), sessionLoginInfo.getCredentials(), accessFilter, request);
                if (accessLevel > WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
                    return prepareDB(db, request);
                }
                else {
                    if (domainConfig.getAuthModule() != null) {
                        getLog().warn(
                                "User '" + sessionLoginInfo.getUserName() + "' could not login to database '" + db.getAttribute(DBATTRIB_DBKEY)
                                        + "' although she/he could login to the domain authentication. Is the domain '" + db.getAttribute(DBATTRIB_DOMAIN) + "' misconfigured?");
                    }
                    else {
                        getLog().warn(
                                "User '" + sessionLoginInfo.getUserName() + "' could not login to database '" + db.getAttribute(DBATTRIB_DBKEY)
                                        + "' although another db in the same domain permitted it. Is the domain '" + db.getAttribute(DBATTRIB_DOMAIN) + "' misconfigured?");
                    }
                    // Misconfigured domains will no longer result in dropped
                    // logins
                    // this.getSessionLogins(session).remove(domain);
                }
            }

            // Anonymous login, if nothing else applies. CANNOT BE STORED, bc.
            // Sessionlogins may suddenly be available without notice
            db.openSession(WGDatabase.ANONYMOUS_USER, null, accessFilter, request);
            return prepareDB(db, request);
        }
    }

    /**
     * opens a content db based upon request.getRemoteUser and request.getUserPrincipal()
     * if request.getRemoteUser is 'null' and request.getUserPrincipal()!=null, WGDatabase.UNKNOWN_REMOTE_USER is given to the authmodule
     * @param db
     * @param request
     * @return
     * @throws WGAPIException
     * @throws ClientAccessException 
     */
    private WGDatabase openContentDBRequestBased(WGDatabase db, HttpServletRequest request, String accessFilter) throws WGException {
    	if (request == null) {
            return prepareDB(db, request);
        }           
        
        String user = request.getRemoteUser();
        Principal credentials = request.getUserPrincipal();

        if (user == null) {
        	user = credentials==null ? WGDatabase.ANONYMOUS_USER : WGDatabase.UNKNOWN_REMOTE_USER; 
        }
        
      	db.openSession(user, credentials, accessFilter, request);
        
        if (db.isSessionOpen()) {
            updateLoginInfo(db, request, DBLoginInfo.AuthType.REQUEST);
        }
        
        return prepareDB(db, request);
	}

    private WGDatabase openContentDBCertAuth(WGDatabase db, HttpServletRequest request, String accessFilter) throws WGException {

        // retrieve certificate chain
        X509Certificate certChain[] = (X509Certificate[]) request.getAttribute("javax.servlet.request.X509Certificate");
        if (certChain == null || certChain.length < 1) {
            getLog().warn("Failed login for client " + request.getRemoteAddr() + " to database " + db.getDbReference() + ": No client certificate chain provided");
            return prepareDB(db, request);
        }

        // retrieve client cert
        X509Certificate clientCert = certChain[0];
        if (clientCert == null) {
            getLog().warn("Failed login for client " + request.getRemoteAddr() + " to database " + db.getDbReference() + ": No client certificate provided");
            return prepareDB(db, request);
        }

        
        db.openSession(clientCert, accessFilter);
        if (db.isSessionOpen()) {
            updateLoginInfo(db, request, DBLoginInfo.AuthType.CERTIFICATE);
        }
        return prepareDB(db, request);
    }

    private void updateLoginInfo(WGDatabase db, HttpServletRequest request, AuthType authType) throws WGException {
        SessionLoginMap sessionLoginsMap = WGACore.getSessionLogins(request.getSession());
        DBLoginInfo oldLoginInfo = sessionLoginsMap.get(db.getAttribute(DBATTRIB_DOMAIN));
        DBLoginInfo loginInfo;
        if (oldLoginInfo != null) {
            loginInfo = new DBLoginInfo(db.getSessionContext().getUser(), db.getSessionContext().getCredentials(), authType, oldLoginInfo.getAccessFilter(), oldLoginInfo.getDbAccessFilters());
        }
        else {
            loginInfo = new DBLoginInfo(db.getSessionContext().getUser(), db.getSessionContext().getCredentials(), authType);
        }
        
        if (oldLoginInfo == null || !oldLoginInfo.equals(loginInfo)) {
            sessionLoginsMap.put(db.getAttribute(DBATTRIB_DOMAIN), loginInfo);
            if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                WGA.get(request, null, this).app(db).createEvent("auth=login")
                .param("userName", loginInfo.getUserName())
                .param("authType", DBLoginInfo.AuthType.CERTIFICATE)
                .fireOnSession();
            }
        }
    }

    /**
     * checks if domain is enabled for certificate authentication per definition
     * a domain is enabled for certauth if - the authmodule configured on
     * domainlevel has certauth enabled - or one db in this domain has an
     * certauth-enabled authmodule
     * 
     * @param domainName
     * @return true/false
     */
    public boolean certAuthEnabledForDomain(String domainName) {
    	
        WGADomain domain = getDomains(domainName);
        if (domain == null) {
            return false;
        }
        
        if (domain.getConfig().getAuthenticationSource() instanceof CertAuthCapableAuthModule) {
            CertAuthCapableAuthModule certAuthModule = (CertAuthCapableAuthModule) domain.getConfig().getAuthenticationSource();
            return certAuthModule.isCertAuthEnabled();
        }
        
        return false;
        
        
    }

    /**
     * returns a list with all dbs in the given domain
     * 
     * @param domain
     * @return
     */
    public List<WGDatabase> getDatabasesForDomain(String domain) {
        Iterator<WGDatabase> dbs = this.contentdbs.values().iterator();
        ArrayList<WGDatabase> domDbs = new ArrayList<WGDatabase>();
        while (dbs.hasNext()) {
            WGDatabase db = dbs.next();
            if (domain.equals(db.getAttribute(WGACore.DBATTRIB_DOMAIN))) {
                domDbs.add(db);
            }
        }
        return domDbs;
    }

    /**
     * returns all databases available within this request (for this user)
     * 
     * @param request
     * @return a list of all databases which can be accessed by this request
     */
    public List<WGDatabase> openContentDBs(javax.servlet.http.HttpServletRequest request) {
        ArrayList<WGDatabase> dbs = new ArrayList<WGDatabase>();
        Iterator<String> dbKeys = contentdbs.keySet().iterator();
        while (dbKeys.hasNext()) {
            String dbKey = dbKeys.next();
            WGDatabase db;
            try {
                db = openContentDB(dbKey, request);
                if (db.isSessionOpen()) {
                    dbs.add(db);
                }
            }
            catch (WGException e) {
                // ignore db
            }
        }
        return dbs;
    }

    /**
     * returns all databases available within this request (for this user) which
     * contains to domain
     * 
     * @param request
     * @param domain
     * @return a list of all databases which can be accessed by this request and
     *         contains to domain
     */
    public List<WGDatabase> openContentDBs(javax.servlet.http.HttpServletRequest request, String domain) {
        ArrayList<WGDatabase> dbs = new ArrayList<WGDatabase>();
        Iterator<String> dbKeys = contentdbs.keySet().iterator();
        while (dbKeys.hasNext()) {
            String dbKey = dbKeys.next();
            try {
                WGDatabase db = openContentDB(dbKey, request);
                if (db.isSessionOpen()) {
                    String dbDomain = (String) db.getAttribute(WGACore.DBATTRIB_DOMAIN);
                    if (dbDomain.equals(domain)) {
                        dbs.add(db);
                    }
                }
            }
            catch (WGException e) {
                // ignore db
            }
        }
        return dbs;
    }

    public WGDatabase prepareDB(WGDatabase db, HttpServletRequest request) {

        if (db.isSessionOpen()) {
            
            // Check for system file container update if the db's design provider does not notify
            if (db.getDesignProvider() != null && !db.getDesignProvider().isNotifying()) {
                try {
                    getSystemContainerManager().checkForUpdates(db);
                }
                catch (Exception e) {
                    getLog().error("Exception updating system file container", e);
                }
            }
            
            // Set some session context attributes
            if (request != null) {
                db.getSessionContext().setAttribute(WGACore.DBSESSIONCONTEXT_REQUEST, request);
                db.getSessionContext().setTask(
                        "WGA Server " + request.getMethod() + " Request: " + request.getRequestURL().toString());
            }
        }

        return db;
    }

    public WGDatabase openPersonalisationDB(String domain) throws WGAPIException {

        WGADomain domainCfg = getDomains(domain);
        if (domainCfg == null) {
            return null;
        }
        
        
        WGDatabase db = this.personalisationdbs.get(domainCfg.getUID());
        if (db == null) {
            return null;
        }

        
        if (!db.isSessionOpen()) {
            db.openSession();
        }

        return db;

    }

    private File licenseFile;


    private Map<String, Class> _customFormatters = Collections.synchronizedMap(new HashMap<String, Class>());
    private Map<String, Class> _systemFormatters = Collections.synchronizedMap(new HashMap<String, Class>());

    public boolean isMediaKeyDefined(String mediaKey) {
        return this.systemMediaKeys.containsKey(mediaKey) || this.customMediaKeys.containsKey(mediaKey);
    }

    // private String DBATTRIB_LOGGER = ATTRIB_BASE + "Logger";

    public void removePersonalisationDB(String domain) {
        
        WGDatabase db = this.personalisationdbs.remove(domain);
        if (db == null) {
            return;
        }
        
        // Notify all managed DB attributes
        Iterator attNames = db.getAttributeNames().iterator();
        while (attNames.hasNext()) {
            Object att = db.getAttribute((String) attNames.next());
            if (att instanceof ManagedDBAttribute) {
                ((ManagedDBAttribute) att).close();
            }

        }
        
        // Close the database
        try {
            db.close();
            this.log.info("Removed personalisation database for domain \"" + domain + "\"");
        }
        catch (WGAPIException e) {
            this.log.error("Unable to remove personalisation database for domain \"" + domain + "\"", e);
        }
    

    }
    
    // Tool threads
    private WGPDeployer _deployer;



    private ExternalFileServingConfig _externalFileServingConfig = new ExternalFileServingConfig();

    public ExternalFileServingConfig getExternalFileServingConfig() {
        return _externalFileServingConfig;
    }

    public synchronized void removeContentDB(String key) {

        WGDatabase db = this.contentdbs.remove(key);
        if (db != null) {

            String uuid = db.getUUID();
            if (uuid != null) {
                _contentdbUuidsToKeys.remove(uuid);
            }
            
            // Deregister from wgacore objects
            _systemContainerManager.removeDatabase(db);
            _deployer.removeLayouts(db);
            _eventManager.removeDatabaseEvents(key);

            db.removeDatabaseEventListener(_eventManager);
            db.removeContentEventListener(_eventManager);
            db.removeWorkflowEventListener(_eventManager);

            // Notify all managed DB attributes
            Iterator attNames = db.getAttributeNames().iterator();
            while (attNames.hasNext()) {
                Object att = db.getAttribute((String) attNames.next());
                if (att instanceof ManagedDBAttribute) {
                    ((ManagedDBAttribute) att).close();
                }

            }

            // Clear WebTML cache
            try {
                getWebTMLCache().clearForDatabase(key);
            }
            catch (CacheException e1) {
                log.error("Exception clearing WebTML cache for closing db '" + key + "'", e1);
            }

            // Call event
            fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_CS_DISCONNECTED, db, this));
            
            // we might have to remove hdb instance
            WGHierarchicalDatabase.removeInstance(db.getDbReference());
            
            // Close an external personalisation db if there is one 
            if (db.hasAttribute(DBATTRIB_EXTERNAL_SELF_PERSONALISATION_DB)) {
                try {
                    WGDatabase persdb = (WGDatabase) db.getAttribute(DBATTRIB_EXTERNAL_SELF_PERSONALISATION_DB);
                    persdb.close();
                }
                catch (WGAPIException e) {
                    log.error("Exception closing external personalisation database for db '" + key + "'", e);
                }
            }

            // Close the database itself
            try {                              
                db.close();
                this.log.info("Closed content database for key \"" + key + "\"");
            }
            catch (WGAPIException e) {
                log.error("Unable to close content database for key \"" + key + "\"", e);
            }

            // Clear related problems
            _problemRegistry.clearProblemScope(new DatabaseScope(key));


        }

    }
    
    public void removeCustomShare(String name) {
        ShareDefinition def = getShares().get(name);
        if (def == null) {
            getLog().info("Cannot remove custom share '" + def.getName() + "'. It does not exist.");
        }
        else if (def.getOrigin() != ShareDefinition.ORIGIN_CUSTOM) {
            getLog().info("Cannot remove custom share '" + def.getName() + "'. It is not custom.");
        }
        
        getShares().remove(name);
        getLog().info("Removed custom share '" + name + "'");
    }

    public static final String DBATTRIB_LOGGER = "Logger";

    public static final String DBATTRIB_DESIGNSYNC = "designsync";

    /**
     * Sets, if this database should be regarded a db with multilanguage content
     * (true) where the language should get selected by the users preferences,
     * or if all requests for content should retrieve the database's default
     * language (false)
     */
    public static final String DBATTRIB_MULTILANGUAGE_CONTENT = "MultiLanguageContent";

    private static final String DBATTRIB_FIRSTLEVELDBOPTIONS = "FirstLevelDBOptions";
    


    public static final String DBATTRIB_FIRSTLEVELPUBLISHEROPTIONS = "FirstLevelPublisherOptions";

    public static final String DBATTRIB_FULLY_CONNECTED = "FullyConnected";

    public static final String DBATTRIB_DBGlOBALS = "TMLScriptDBGlobals";

    private static final String CACHENAME_DESIGNFILES = "DesignFiles";

    public static final String DBATTRIB_EXTERNAL_SELF_PERSONALISATION_DB = "ExternalSelfPersonalisationDB";
    
    public static final String DBATTRIB_FORCE_LABEL_LANGUAGE = "ForceLabelLanguage";
    
    public static final String DBATTRIB_FALLBACK_LABEL_LANGUAGE = "FallbackLabelLanguage";
    
    public static final String DBATTRIB_USE_NONFINAL_HT_FEATURES = "UseNonFinalHTFeatures";


    private WGDatabase retrieveContentDB(ContentDatabase config, Map<String, Serializable> dbConnectionFailures) {

        final ProblemOccasion occ = new ConnectDatabaseProblemOccasion(config.getKey());
        _problemRegistry.clearProblemOccasion(occ);
        
        try {
            final String dbType = (config instanceof ContentStore ? "Web application" : "Data source");
            
            if (config instanceof ContentStore) {
                logCategoryInfo(dbType + " " + config.getKey(), 2);
            }
            else {
                logCategoryInfo(dbType + " " + config.getKey(), 2);
            }        
            
            // Get basic information
            String strType = config.getImplClassName();        
            String strKey = config.getKey();
            
            // retrieve dbserver for db
            WGDatabaseServer server = getDatabaseServers().get(config.getDbServer());
            if (server == null) {
                throw Problem.create(occ, "databaseConnectionFailed.unknownServer", ProblemSeverity.HIGH, Problem.var("server", config.getDbServer()));
            }
             
            if (strKey.equalsIgnoreCase("start") || strKey.equalsIgnoreCase("bi") || strKey.equalsIgnoreCase("static") || strKey.equalsIgnoreCase("statictml") || strKey.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                throw Problem.create(occ, "databaseConnectionFailed.invalidDbkey", ProblemSeverity.HIGH);
            }
            
            // Look if the server is able and allowed to instantiate this db type
            Class<WGDatabaseCore> typeClass;
            try {
                Class theClass = WGFactory.getImplementationLoader().loadClass(strType);
                if (!WGDatabaseCore.class.isAssignableFrom(theClass)) {
                    throw new WGAServerException("Cannot map " + dbType + " to key \"" + strKey + "\" - Database implementation class \"" + strType + "\" is no WGDatabaseCore implementation.");
                }
                typeClass = theClass;
            }
            catch (Exception e) {
                throw Problem.create(occ, "databaseConnectionFailed.invalidImplementation", ProblemSeverity.HIGH, Problem.var("class", strType), e);
            }
            
            // Test for retrievable design for content stores
            if (config instanceof ContentStore) {
                ContentStore csConfig = (ContentStore) config;
                if (csConfig.getDesign() != null) {
                    try {
                        WGADesign design = getDesignManager().getDesignForConfig(csConfig.getDesign());
                        if (design == null) {
                            throw Problem.create(occ, "applyDesignProblem.unknownDesign", ProblemSeverity.HIGH, Problem.var("design", (new DesignReference(csConfig.getDesign()).toString())));
                        }
                    }
                    catch (Problem p) {
                        throw p;
                    }
                    catch (WGADesignRetrievalException e) {
                        String design = (new DesignReference(csConfig.getDesign())).toString();
                        if (e.getDesign() != null) {
                            design = e.getDesign();
                        }
                        throw Problem.create(occ, "applyDesignProblem.unretrievableDesign", ProblemSeverity.HIGH, Problem.var("design", design), e);
                    }
                    catch (Exception e) {
                        throw Problem.create(occ, "applyDesignProblem.invalidDesign", ProblemSeverity.HIGH, Problem.var("design", (new DesignReference(csConfig.getDesign()).toString())), e);
                    }
                }
            }
    
            // Evaluate title
            String title = config.getTitle();
    
            // Evaluate domain
            /*
            Element domainElement = (Element) databaseElement.selectSingleNode("domain");
            DomainConfiguration domainConfig = new DomainConfiguration();
    
            if (domainElement != null) {
                domainConfig = getDomainConfig(domainElement.getStringValue());
            }*/
            Domain domainCfg = getWgaConfiguration().getDomain(config.getDomain());
            WGADomain domain = getDomain(domainCfg);
    
            // Collect db options from global, server, database
            HashMap<String, String> dbOptions = new HashMap<String, String>();
            putDefaultDbOptions(dbOptions);
            dbOptions.putAll(_wgaConfiguration.getGlobalDatabaseOptions());
            dbOptions.putAll(config.getDatabaseOptions());
            
            // We collect those options that were added on database level in here
            // and add this set as database attribute
            // so we can tell them later from lower priority options
            Set<String> firstLevelDBOptions = new HashSet<String>();
            firstLevelDBOptions.addAll(config.getDatabaseOptions().keySet());
     
            // Mandatory db options
            dbOptions.put(WGDatabase.COPTION_DBREFERENCE, strKey.toLowerCase());
            
            // Optionally add hotpatches path
            String hotPatchesPath = System.getProperty(SYSPROPERTY_JDBC_HOTPATCHES);
            if (hotPatchesPath != null) {
                File hotPatchesFile = getWGAFile(hotPatchesPath);
                if (hotPatchesFile != null) {
                    dbOptions.put(WGDatabaseImpl.COPTION_HOTPATCH, hotPatchesFile.getAbsolutePath());
                }
            }
            
            // get the database object
            WGDatabase db = null;
            
            if (config.isLazyConnecting()) {
                db = server.prepareDatabase(typeClass, dbOptions);
            }
            else {
                db = server.openDatabase(typeClass, dbOptions);
            }
    
            if (db == null || (!config.isLazyConnecting() && !db.isSessionOpen())) {        	
                throw new WGAServerException("Could not open database for key " + strKey.toLowerCase() + " - Check logged messages from the application log for error details");
            }
            
            try {
    
                if (!config.isLazyConnecting()) {
                    db.getSessionContext().setTask("Initializing database in WGA");
                    log.info("Mapping " + dbType + " on path \"" + db.getPath() + "\" (" + db.getTypeName() + ") to database key \"" + strKey.toLowerCase() + "\"");
                    try {
                        if (db.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
                            int patchLevel = db.getContentStorePatchLevel();
                            log.info("Database for " + strKey.toLowerCase() + " is a WGA Content Store of version " + db.getContentStoreVersion() + (patchLevel != 0 ? " at patch level " + patchLevel : ""));
                        }
                    }
                    catch (WGAPIException e) {
                        throw new WGAServerException("Exception determining content store version of database " + strKey.toLowerCase(), e);
                    }
                }
                else {
                    log.info("Preparing " + dbType + " on path \"" + db.getPath() + "\" (" + db.getTypeName() + ") as database for key \"" + strKey.toLowerCase() + "\"");
                }
        
                if (title != null) {
                    db.setTitle(title);
                }
        
                // Inject the default language if configured, else trigger determination
                if (config instanceof ContentStore) {
                    ContentStore csConfig = (ContentStore) config;
                    if (!WGUtils.isEmpty(csConfig.getDefaultLanguage())) {
                        db.setDefaultLanguage(csConfig.getDefaultLanguage());
                    }
                    else {
                        db.onConnect(new DatabaseAction() {
                            @Override
                            public void run(WGDatabase db) throws Exception {
                                try {
                                    db.determineDefaultLanguage();
                                }
                                catch (WGAPIException e) {
                                    getLog().error("Exception determining default language for " + dbType + " " + db.getDbReference(), e);
                                            _problemRegistry.addProblem(Problem.create(occ, "databaseDefaultLanguageProblem.invalidDefaultLanguage", ProblemSeverity.LOW, e));
                                }
                            }
                        });
                    }
                };
                
                // Set mandatory database attributes
                initializeDBAttributes(db, strKey, domain.getName(), firstLevelDBOptions);
                
                // Inject domain authentication module
                if (domain.getAuthModule() != null) {
                    try {
                        db.setAuthenticationModule(new DomainRedirectionAuthModule(this, domain.getName()));
                    }
                    catch (Exception e) {
                        throw Problem.create(occ, "databaseConnectionFailed.invalidAuth", ProblemSeverity.HIGH, e);
                    }
                }
        
                // Configure design provider, if neccessary
                if (config instanceof ContentStore) {
                	ContentStore csConfig = (ContentStore) config;
                	if (csConfig.getDesign() != null) {
                		getDesignManager().applyDesign(db, csConfig, occ);	
                	}
                }
                
                // Determine if ACL is empty
                // If so, eventually add domain default manager
                boolean aclEmpty = false;
                try {
                    if (db.isConnected() && db.isSessionOpen() && db.hasFeature(WGDatabase.FEATURE_ACL_MANAGEABLE) && db.getACL().getAllEntries().size() == 0) {
                        aclEmpty = true;
                    }
                }
                catch (WGBackendException e1) {
                    getLog().error("Error retrieving ACL state of " + dbType + " '" + db.getDbReference() + "'", e1);
                }
                
                if (aclEmpty && domain.getConfig().getDefaultManager() != null) {
                    try {
                        getLog().info("Adding default manager '" + domain.getConfig().getDefaultManager() + "' to ACL of '" + strKey + "'");
                        db.getACL().createUserEntry(domain.getConfig().getDefaultManager(), WGDatabase.ACCESSLEVEL_MANAGER);
                    }
                    catch (WGAPIException e) {
                        getLog().error("Exception on adding default manager to ACL of '" + strKey + "'", e);
                    }
                }
                
                // Process system container
                SystemContainerManager.SystemContainerContext scContext = null;
                try {
                    scContext = _systemContainerManager.addDatabase(db, aclEmpty);
                }
                catch (Problem p) {
                    throw p;
                }
                catch (InvalidCSConfigVersionException e) {
                    throw Problem.create(occ, "databaseConnectionFailed.incompatibleDesign", ProblemSeverity.HIGH, Problem.var("targetversion", e.getTargetVersion()), e);
                }
                catch (Exception e) {
                    throw Problem.create(occ, "databaseConnectionFailed.invalidDesign", ProblemSeverity.HIGH, e);
                }
                        
                // Merge publisher options from global, server, design, database
                Map<String, String> publisherOptions = new HashMap<String, String>();
                publisherOptions.putAll(_wgaConfiguration.getGlobalPublisherOptions());
                if (scContext != null) {
                    scContext.putPublisherOptions(publisherOptions);
                }
                publisherOptions.putAll(config.getPublisherOptions());
                
                // We collect those options that were added on database level in here
                // and add this set as database attribute
                // so we can tell them later from lower priority options
                Set<String> firstLevelPublisherOptions = new HashSet<String>();
                firstLevelPublisherOptions.addAll(config.getPublisherOptions().keySet());
                db.setAttribute(DBATTRIB_FIRSTLEVELPUBLISHEROPTIONS, firstLevelPublisherOptions);
        
                // Publisher options initialisation, which is equal for content dbs and plugins
                processPublisherOptions(db, publisherOptions);
                
                // check if db is empty before hdb script runs
                boolean isEmptyDB = false;
        		try {
        			isEmptyDB = (db.isConnected() && db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && db.isContentEmpty());
        		} catch (WGAPIException e) {
        			this.log.error("Unable to check if database '" + db.getDbReference() + "' is empty.", e);			
        		}             
        
                // Validate default language definition
                if (!isEmptyDB && db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                    db.onConnect(new ValidateDefaultLanguageAction());
                }
                
                // Eventually prepare "external self-personalisation"
                if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && domain.getConfig().getPersonalisation() == null && !db.hasFeature(WGDatabase.FEATURE_SELF_PERSONALIZABLE)) {
                    try {
                        createExternalSelfPersonalisation(db);
                    }
                    catch (WGAPIException e) {
                        this.log.error("Exception creating external personalisation database for db '" + db.getDbReference() + "'. Profiles will not be available.", e);
                        _problemRegistry.addProblem(Problem.create(occ, "databaseConnectionProblem.invalidExternalPersDb", ProblemSeverity.LOW, e));
                    }
                }
                
                // Add listeners for events and register embedded event scripts
                db.addDatabaseEventListener(_eventManager);
                db.addContentEventListener(_eventManager);
                db.addWorkflowEventListener(_eventManager);
                if (db.isConnected()) {
                    _eventManager.updateDatabaseEvents(db);
                }
                
                // Add file annotators
                updateFileAnnotators(db);
                
                // add File Converter
                db.setFileConverter(_fileConverter);
                
                // Do system container initialisations
                if (scContext != null) {
                    scContext.performInitialisation(new Boolean(isEmptyDB));
                    // Revalidate the default language if the database was empty before initialisation, might have new definitions now
                    if (isEmptyDB) {
                        db.onConnect(new ValidateDefaultLanguageAction());
                    }
                }
                
                // Mark this database as fully connected
                db.setAttribute(DBATTRIB_FULLY_CONNECTED, "true");
        
                
                if (_externalFileServingConfig.isEnabled() && db.getBooleanAttribute(DBATTRIB_EXTERNAL_FILE_SERVING_ENABLED, false)) {
                    getLog().info("External file serving enabled for database '" + db.getDbReference() + "'.");
                }
                
                return db;
            }
            catch (Throwable e) {
                // Cleanup if anything after initial connecting went wrong which makes using this db impossible
                if (db.isReady()) {
                    try {
                        db.close();
                    }
                    catch (Exception e2) {
                        // Swallowing this exception as anything went wrong before anyway
                    }
                }
                throw e;
            }
        
        }
        catch (Problem problem) {
            _problemRegistry.addProblem(problem);
            dbConnectionFailures.put(config.getKey(), problem.getMessage());
            log.error(problem.getMessage(), problem.getCause());
            return null;
            
        }
        catch (Throwable e) {
            Problem problem;
            if (e.getCause() instanceof LicenseException) {
                problem = Problem.create(occ, "databaseConnectionFailed.licenseError", ProblemSeverity.HIGH, e);
            }
            else {
                problem = Problem.create(occ, "databaseConnectionFailed.exception", ProblemSeverity.HIGH, e);
            }
            _problemRegistry.addProblem(problem);
            dbConnectionFailures.put(config.getKey(), e.getMessage());
            log.error("An unexpected exception occurred connecting database " + config.getKey(), e);
            return null;
        }
        
    }

    private void putDefaultDbOptions(Map<String, String> dbOptions) {
        dbOptions.put(WGDatabase.COPTION_USERCACHELATENCY, String.valueOf(_wgaConfiguration.getUserCacheLatencyMinutes()));
        dbOptions.put(WGDatabase.COPTION_LEGACY_DBCP_MONITORING, String.valueOf("true".equals(_wgaConfiguration.getServerOptions().get(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_LEGACY_DBCP))));
    }



    private void createExternalSelfPersonalisation(WGDatabase db) throws WGAPIException {
        
        File dbDir = new File(getWgaDataDir(), EXTERNAL_PERSDBS_FOLDER);
        if (!dbDir.exists()) {
            dbDir.mkdir();
        }
        
        Map<String, String> dbOptions = new HashMap<String, String>();
        dbOptions.put(WGDatabase.COPTION_DBREFERENCE, db.getDbReference() + "$pers");
        dbOptions.put(WGDatabase.COPTION_READERPROFILECREATION, "true");
        dbOptions.put(WGDatabase.COPTION_USERCACHELATENCY, String.valueOf(_wgaConfiguration.getUserCacheLatencyMinutes()));
        dbOptions.put(WGDatabase.COPTION_WORKFLOWENGINE, WGDefaultWorkflowEngine.class.getName()); // We do not want any real workflow here

        
        WGDatabase persdb = WGFactory.getInstance().openDatabase(null, de.innovationgate.webgate.api.hsql.WGDatabaseImpl.class.getName(), dbDir.getPath() + "/" + db.getDbReference(), "sa", "", dbOptions);
        db.setAttribute(DBATTRIB_EXTERNAL_SELF_PERSONALISATION_DB, persdb);
        
    }

    private WGADomain getDomain(Domain domain) {    	
		return getDomains(domain.getName());
	}

	private void initializeDBAttributes(WGDatabase db, String dbKey, String domainName, Set firstLevelDBOptions) {
        db.setAttribute(DBATTRIB_DBKEY, dbKey.toLowerCase());
        db.setAttribute(DBATTRIB_DOMAIN, domainName);
        db.setAttribute(DBATTRIB_FIRSTLEVELDBOPTIONS, firstLevelDBOptions);
        db.setAttribute(DBATTRIB_DBGlOBALS, new TMLScriptAppGlobalRegistry());
        db.setAttribute(DBATTRIB_SCOPEOBJECTREGISTRY, new ScopeObjectRegistry(ObjectScope.APP, "Database " + db.getDbReference(), new NoopScopeObjectContextCreator()));
        Iterator defaultAtts = _dbDefaultAttributes.entrySet().iterator();
        while (defaultAtts.hasNext()) {
            Map.Entry entry = (Map.Entry) defaultAtts.next();
            db.setAttribute((String) entry.getKey(), (String) entry.getValue());
        }
    }





    public File getWGAFile(String fileName) {

        // Path variables
        fileName = replaceWGAPathVariables(fileName);

        // Dir links
        File file = WGUtils.resolveDirLink(new File(fileName));
        
        // First try: Use as absolute path
        if (file.exists()) {
            return file;
        }

        // Second try: Find inside wgaconfig path
        File wgaConfigPath = configFile.getParentFile();
        file = new File(wgaConfigPath, fileName);
        if (file.exists()) {
            return file;
        }
        else {
            return null;
        }
    }

    public String replaceWGAPathVariables(String fileName) {
        return WGUtils.strReplace(fileName, "${", _replaceProcessorInstance, true);
    }
    
    public File getWGAFolder(String fileName) {
        return getWGAFile(fileName);
    }

    public File getOrCreateWGAFolder(String fileName) {

        // Path variables
        fileName = replaceWGAPathVariables(fileName);
        
        // Dir links
        File file = WGUtils.resolveDirLink(new File(fileName));
        
        // First try: Use as absolute path
        if (file.exists()) {
            return file;
        } else if (file.isAbsolute()) {
        	file.mkdir();
        	if (file.exists() && file.isDirectory()) {
        		return file;
        	} else {
        		return null;
        	}
        }

        // Second try: Find inside wgaconfig path
        File wgaConfigPath = configFile.getParentFile();
        file = new File(wgaConfigPath, fileName);
        if (file.exists()) {
            return file;
        }
        else {
        	file.mkdir();
        	if (file.exists() && file.isDirectory()) {
        		return file;
        	} else {
        		return null;
        	}
        }
    }
    

    public WGDatabase retrievePersonalisationDB(Domain domainConfig) throws WGAServerException {
    	PersonalisationDatabase config = domainConfig.getPersonalisation();
    	
        // Get basic information
        String strType = config.getImplClassName();

        // Look if impl class is reachable
        Class<WGDatabaseCore> typeClass;
        try {
            Class theClass = Class.forName(strType, true, WGFactory.getImplementationLoader());
            if (!WGDatabaseCore.class.isAssignableFrom(theClass)) {
                throw new WGAServerException("Cannot connect personalisation database for domain \"" + domainConfig.toString() + "\" - Database implementation class \"" + strType + "\" is no WGDatabaseCore implementation.");
            }
            typeClass = theClass;
        }
        catch (ClassNotFoundException e) {
            throw new WGAServerException("Cannot attach personalisation database to domain \"" + domainConfig.toString() + "\" - Database implementation class \"" + strType + "\" not available.");
        }
        catch (NoClassDefFoundError e) {
            throw new WGAServerException("Cannot attach personalisation database to domain \"" + domainConfig.toString() + "\" - Database implementation class or dependent class for type \"" + strType + "\" not found: \""
                    + e.getMessage());
        }
        catch (VerifyError e) {
            log.error("Cannot attach personalisation database to domain \"" + domainConfig.toString() + "\" - Verify error: " + e.getMessage());
            return null;
        }

        // Retrieve server
        WGDatabaseServer server = getDatabaseServers().get(config.getDbServer());
        if (server == null) {
            throw Problem.create(new UpdateConfigOccasion(), new DomainScope(domainConfig.getName()), "updateConfigProblem.domainPersServerUnknown", ProblemSeverity.HIGH, Problem.var("serverid", config.getDbServer()));
        }
        
        // Merge db options from global, server, database
        HashMap<String, String> dbOptions = new HashMap<String, String>();
        putDefaultDbOptions(dbOptions);
        dbOptions.putAll(_wgaConfiguration.getGlobalDatabaseOptions());
        dbOptions.putAll(server.getOptions());
        dbOptions.putAll(config.getDatabaseOptions());
        
        // Mandatory db options
        dbOptions.put(WGDatabase.COPTION_DBREFERENCE , "personalisation_" + domainConfig.getUid());

        // get the database object
        WGDatabase db = null;
        try {
            if (config.isLazyConnecting()) {
                db = server.prepareDatabase(typeClass, dbOptions);
            }
            else {
                db = server.openDatabase(typeClass, dbOptions);
            }
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Could not connect to database", e);
        }
        catch (ModuleDependencyException e) {
            throw new WGAServerException("Could not connect to database because of missing dependency: " + e.getMessage());
        }

        if (db == null) {
            throw new WGAServerException("Could not open personalisation database for domain '" + domainConfig.getName() + " - Check logged messages above for error details");
        }
        else if (!db.getRoles().contains(WGDatabase.ROLE_USERPROFILES)) {
            throw new WGAServerException("Could not open personalisation database \for domain '" + domainConfig.getName() + " - This type does not deliver user profiles");
        }

        if (config.isLazyConnecting()) {
            log.info("Preparing personalisation database on path \"" + db.getPath() + "\" for domain \"" + domainConfig.getName() + "\"");
            db.addDatabaseConnectListener(this);
        }
        else {
            log.info("Attaching personalisation database on path \"" + db.getPath() + "\" to domain \"" + domainConfig.getName() + "\"");
            try {
                log.info("Personalisation database of domain " + domainConfig.getName() + " is content store version " + db.getContentStoreVersion());
            }
            catch (WGAPIException e) {
                throw new WGAServerException("Exception determining content store version of personalisation database for domain " + domainConfig.getName(), e);
            }
        }

        // Set domain
        db.setAttribute(WGACore.DBATTRIB_DOMAIN, domainConfig.getUid());

        // Mark this database as fully connected
        db.setAttribute(DBATTRIB_FULLY_CONNECTED, "true");
        
        return db;
    }

    private ServletContext _context;

    protected int tmlBuffer = 8;

    protected String tmlHeader = "";

    

    private Map<String, ShareDefinition> _shares = new HashMap<String, ShareDefinition>();


    
    public synchronized void updateContentDBs(boolean obsoleteBogusParam) {
        updateContentDBs();
    }
    
    /**
     * 
     * @return set containing dbkeys which where initial connected in this
     *         configUpdate
     */
    public synchronized void updateContentDBs() {
        
        logCategoryInfo("Web applications and data sources", 1);
        
        // contains dbkeys that where initial connected in this configUpdate
        HashSet<String> newConnectedDBKeys = new HashSet<String>();

        WGFactory.getInstance().closeSessions();

        //Element databaseRoot = (Element) this.configDocument.getRootElement().selectSingleNode("contentdbs");
        //List databaseElements = databaseRoot.selectNodes("contentdb");
        //Element databaseElement;
        ContentDatabase databaseConfig;
        WGDatabase db;
        Set<String> currentDBs = new HashSet<String>();

        // db connection errors mapped by dbkey
        Map<String, Serializable> dbConnectionFailures = new HashMap<String, Serializable>();
        
        List<ContentDatabase> databases = _wgaConfiguration.getContentDatabases();
        
        // Map new databases and update current databases
        for (int idxDB = 0; idxDB < databases.size(); idxDB++) {

            databaseConfig = databases.get(idxDB);
            String dbKey = databaseConfig.getKey();

            // ignore disabled dbs
            ConnectDatabaseProblemOccasion occ = new ConnectDatabaseProblemOccasion(databaseConfig.getKey());
            _problemRegistry.clearProblemOccasion(occ);
            if (!databaseConfig.isEnabled()) {
                continue;
            }
            DatabaseServer serverConfig = (DatabaseServer) _wgaConfiguration.getByUid(databaseConfig.getDbServer());
            if (serverConfig != null && !serverConfig.isEnabled()) {
                continue;
            }

            // Get or create database object
            boolean isNewDB = false;
            if (this.contentdbs.containsKey(dbKey) == false) {
                try {
                    db = retrieveContentDB(databaseConfig, dbConnectionFailures);
                    if (db == null) {
                        continue;
                    }
    
                    this.contentdbs.put(dbKey, db);
                    String uuid = db.getUUID();
                    if (uuid != null) {
                        this._contentdbUuidsToKeys.put(uuid, dbKey);
                    }
    
                    isNewDB = true;
                    newConnectedDBKeys.add(dbKey);
                }
                catch (Throwable e) {
                    getLog().error("Exception connecting database " + dbKey, e);
                    if (this.contentdbs.containsKey(dbKey)) {
                        this.contentdbs.remove(dbKey);
                    }
                    _problemRegistry.addProblem(Problem.create(occ, "databaseConnectionFailed.exception", ProblemSeverity.HIGH, e));
                    continue;
                }
            }
            else {
                db = this.contentdbs.get(dbKey);
            }
            currentDBs.add(dbKey);

            updateFieldMappings(db, databaseConfig.getFieldMappings());
            
            // Update client restrictions
            if (databaseConfig.isClientRestrictionsEnabled()) {
                
                    List<IPRestriction> restrictions = new ArrayList<>();
                    boolean errornousRestrictions = false;
                    for (ClientRestriction clientRes : databaseConfig.getClientRestrictions()) {
                        try {
                            IPRestriction ipRes = IPs.parseRestriction(clientRes);
                            restrictions.add(ipRes);
                        }
                        catch (Exception e) {
                          getLog().error("Exception parsing client restriction on database " + dbKey + ": " + clientRes.toString(), e);
                          _problemRegistry.addProblem(Problem.create(occ, "databaseConnectionProblem.invalidClientRestriction", ProblemSeverity.HIGH, Problem.var("restriction", clientRes.toString()), e));
                          errornousRestrictions = true;
                        }
                    }
                    
                    db.setAttribute(DBATTRIB_CLIENTRESTRICTIONS, restrictions);
                
            } else {
                // if not enabled remove clientRestrictions from db
                db.removeAttribute(DBATTRIB_CLIENTRESTRICTIONS);
            }

            // Operations only for newly connected databases
            if (isNewDB) {
                performNewDBOperations(db);
            }
        }

        // Unmap removed databases
        Set<String> removedDBs = new HashSet<String>(this.contentdbs.keySet());
        removedDBs.removeAll(currentDBs);
        Iterator<String> removedIt = removedDBs.iterator();
        while (removedIt.hasNext()) {
            String key = removedIt.next();
            if (!key.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                removeContentDB(key);
            }
        }

        // notify LuceneManager
        if (luceneManager != null) {
            luceneManager.configurationHasChanged(newConnectedDBKeys);
        }
        
        // inform admin of connection errors
        if (!dbConnectionFailures.isEmpty()) {
        	WGAMailNotification notification = new WGAMailNotification(WGAMailNotification.TYPE_DATABASE_INITIALISATION_ERRORS);
        	notification.setSubject("Some databases could not be initialized successfully");
        	notification.append("<b>The following databases have reported errors during initialisation:</b>");
        	Iterator<String> dbkeys = dbConnectionFailures.keySet().iterator();
        	while (dbkeys.hasNext()) {
        		String dbkey = dbkeys.next();
        		notification.append("<br><br><b>Database:</b> " + dbkey + "<br>");
        		Object failure = dbConnectionFailures.get(dbkey);
        		if (failure instanceof String) {        		
        			notification.append((String)failure);        			
        		} else if (failure instanceof Throwable) {
        			Throwable throwable = (Throwable) failure;
        			if (throwable != null) {   				
        				notification.append(throwable);
        			} 
        			
        		}
        	}        
        	notification.setAttachLogfile(true);
        	send(notification);
        }
    }

    private void updateShares() {
        
        logCategoryInfo("Shares", 1);
        
        // Add all shares from configuration
        Iterator<Share> shareConfigs = getWgaConfiguration().getShares().iterator();
        Map<String, ShareDefinition> newShares = new HashMap<String, ShareDefinition>();
        while (shareConfigs.hasNext()) {
            Share shareConfig = shareConfigs.next();
            if (!shareConfig.isEnabled()) {
                continue;
            }
            
            ModuleDefinition shareModDefinition = getModuleRegistry().getModuleDefinition(ShareModuleType.class, shareConfig.getImplClassName());
            if (shareModDefinition == null) {
                getLog().error("Unknown content share type '" + shareConfig.getImplClassName() + "'");
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new ContentShareScope(shareConfig.getName()), "updateConfigProblem.shareTypeUnknown", ProblemSeverity.LOW));
                continue;
            }
            
            ShareProperties props = (ShareProperties) shareModDefinition.getProperties();
            try {
                ShareDefinition shareDefinition = props.createShareDefinition(shareConfig);
                shareDefinition.setOrigin(ShareDefinition.ORIGIN_WGACONFIG);
                getLog().info("Initializing content share '" + shareConfig.getName() + "'");
                shareDefinition.init(this);
                newShares.put(shareConfig.getName(), shareDefinition);
            }
            catch (ShareInitException e) {
                getLog().error("Unable to initialize content share '" + shareConfig.getName() + "' because of the following errors:");
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new ContentShareScope(shareConfig.getName()), "updateConfigProblem.shareInitException", ProblemSeverity.LOW, e));
                Iterator msgs = e.getDetailMessages().iterator();
                while (msgs.hasNext()) {
                    getLog().error("- " + msgs.next());
                }
            }
            catch (Exception e) {
                getLog().error("Exception initializing content share '" + shareConfig.getName() + "'", e);
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new ContentShareScope(shareConfig.getName()), "updateConfigProblem.shareInitException", ProblemSeverity.LOW, e));
            }
        }
        
        // Add custom shares that were added to the previous share configs
        if (_shares != null) {
            Iterator<Map.Entry<String,ShareDefinition>> previousConfigs = _shares.entrySet().iterator();
            while (previousConfigs.hasNext()) {
                Map.Entry<String,ShareDefinition> shareEntry = previousConfigs.next();
                ShareDefinition config = (ShareDefinition) shareEntry.getValue();
                if (config.getOrigin() == ShareDefinition.ORIGIN_CUSTOM) {
                    newShares.put(shareEntry.getKey(), config);
                }
            }
        }
        
        // Set new shares map
        _shares = newShares;

        
    }

    private void performNewDBOperations(WGDatabase db) {       

        // When lazily connected: register for database connection event
        // to load events and show a message when it happens
        if (!db.isConnected()) {
            db.addDatabaseConnectListener(this);
        }

        // Call content store connection event
        fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_CS_CONNECTED, db, this));
    }

    protected void updateFieldMappings(WGDatabase db, List<FieldMapping> fieldMappings) {

        Map<String, String> metaMappings = null;
        Map<String, String> itemMappings = null;
        Map pluginShortcuts = null;

        // Get the mapping maps from containers
        boolean dbMappingRoot = false;
        metaMappings = (Map<String, String>) db.getAttribute(DBATTRIB_META_MAPPINGS);
        itemMappings = (Map<String, String>) db.getAttribute(DBATTRIB_ITEM_MAPPINGS);
        pluginShortcuts = (Map) db.getAttribute(DBATTRIB_PLUGIN_SHORTCUTS);
        dbMappingRoot = true;
       
        // Mappings that only apply to whole dbs
        if (dbMappingRoot == true) {
            buildDesignShortcuts(db);
         }

        if (fieldMappings != null) {
	        Iterator<FieldMapping> mappings = fieldMappings.iterator();
	        while (mappings.hasNext()) {
	        	FieldMapping mapping = mappings.next();
	        	if (mapping.getType().equals(FieldMapping.TYPE_META)) {
	        		metaMappings.put(mapping.getName(), mapping.getExpression());
	        	} else if (mapping.getType().equals(FieldMapping.TYPE_ITEM)) {
	        		itemMappings.put(mapping.getName(), mapping.getExpression());        		
	        	}
	        }
       }
    }

    protected void buildDesignShortcuts(WGDatabase db) {
        
        Map metaMappings = (Map) db.getAttribute(DBATTRIB_META_MAPPINGS);
        Map itemMappings = (Map) db.getAttribute(DBATTRIB_ITEM_MAPPINGS);
        Map pluginShortcuts = (Map) db.getAttribute(DBATTRIB_PLUGIN_SHORTCUTS);
        
        CSConfig csconfig = (CSConfig) db.getAttribute(DBATTRIB_CSCONFIG);
        if (csconfig != null && csconfig instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) {
            de.innovationgate.wga.common.beans.csconfig.v2.CSConfig v2 = (de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) csconfig;
            Iterator shortcuts = v2.getShortcuts().iterator();
            while (shortcuts.hasNext()) {
                Shortcut shortcut = (Shortcut) shortcuts.next();
                if (shortcut.getType() == Shortcut.TYPE_ITEM_MAPPING || 
                    shortcut.getType() == Shortcut.TYPE_META_MAPPING ||
                    shortcut.getType() == Shortcut.TYPE_PLUGIN) {
                    Map<String, String> mappings = (Map<String, String>) (shortcut.getType() == Shortcut.TYPE_ITEM_MAPPING ? itemMappings :
                                          shortcut.getType() == Shortcut.TYPE_META_MAPPING ? metaMappings :
                                                                                             pluginShortcuts);
                    mappings.put(shortcut.getShortcut(), shortcut.getReference());
                }
            }
        }
    }


    

    private String defaultMediaKey = null;

    private static String _buildSignature;

    private Cache _calledSequenceIds = null;
    
    private static Map<String, String> _dbDefaultAttributes = new HashMap<String, String>();
    private static Set<String> _licenseFreeDatabases = new HashSet<String>();
    static {
        
        // Init the default db attributes for content stores
        _dbDefaultAttributes.put(WGACore.DBATTRIB_QUERY_DEFAULT, "native");
        _dbDefaultAttributes.put(WGACore.DBATTRIB_EXPRESSION_DEFAULT, "tmlscript");
        _dbDefaultAttributes.put(WGACore.DBATTRIB_DEFAULT_MEDIAKEY, ENCODER_HTML);
        _dbDefaultAttributes.put(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING, "none");
        _dbDefaultAttributes.put(WGACore.DBATTRIB_MULTILANGUAGE_CONTENT, "true");
        _dbDefaultAttributes.put(WGACore.DBATTRIB_HOME_PAGE, "");
        _dbDefaultAttributes.put(WGACore.DBATTRIB_LOGIN_PAGE, "");
        
        // Init the license-free database keys
        _licenseFreeDatabases.add("plugin-management");
        
    }

    

    private File wgaTempDir = null;

    private LuceneManager luceneManager = null;

    private Map<String, Analyzer> analyzerMappings = Collections.synchronizedMap(new HashMap<String, Analyzer>());

    private static final Set<String> LUCENE_STOP_WORDS = new HashSet<String>();
    static {
        LUCENE_STOP_WORDS.add("t");
        LUCENE_STOP_WORDS.add("s");
    }
    private Analyzer defaultAnalyzer = new StandardAnalyzer(org.apache.lucene.util.Version.LUCENE_35, LUCENE_STOP_WORDS);

    // lucene filehandler mapped by file-extension e.g. 'pdf', 'txt', 'xml'
    private Map<String, FileHandler> fileHandlerMappings = Collections.synchronizedMap(new HashMap<String, FileHandler>());

    // filter mappings for custom javax.servlet.filters
    private List<WGAFilterConfig> filterMappings = new LinkedList<WGAFilterConfig>();

    private UserAgentVerifier userAgentVerifier;

    private DESEncrypter desEncrypter;

    private List<WGACoreEventListener> coreEventListeners = Collections.synchronizedList(new LinkedList<WGACoreEventListener>());
    private List<WGAConfigurationUpdateListener> configUpdateListeners = Collections.synchronizedList(new LinkedList<WGAConfigurationUpdateListener>());

    // TestCore for test facility (assertions via tml:script)
    private TestCore _testCore;

    private DesignFileValidator designFileValidator;

    private boolean _webserviceEnabled;

    /**
     * character encoding used by this wga instance used for request encoding,
     * file upload encoding, encoding of deployed tmls, etc.
     */
    private String _characterEncoding = null;

    private WGAUsageStatistics _usageStatistics;

    private Set customCoreListenerClassnames;

    private static IsolatedJARLoader baseLibraryLoader;
    
    private Map<String, HttpSession> _activeHttpSessions = new ConcurrentHashMap<String, HttpSession>();

    public Map<String, HttpSession> getActiveHttpSessions() {
        return _activeHttpSessions;
    }

    private LogServer _logServer;

	private WGAMailConfiguration _mailConfig;

    private Cache designFileCache;

    private WebTMLCache _webTMLCache = null;
    
    private boolean _webTMLCachingEnabled = true;

    public boolean isWebTMLCachingEnabled() {
        return _webTMLCachingEnabled;
    }

    public void setWebTMLCachingEnabled(boolean enabled) {
        if (enabled) {
            getLog().info("Enabling WebTML caching");
        }
        else {
            getLog().info("Disabling WebTML caching");
        }
        _webTMLCachingEnabled = enabled;
    }

    private ClusterService _clusterService;

    private FileDerivateManager fileDerivateManager;
    
    private XStream _libraryXStream;

    public FileDerivateManager getFileDerivateManager() {
        return fileDerivateManager;
    }

    @SuppressWarnings({ "deprecation", "deprecation" })
    private void initReadGeneralConfig(boolean update) throws CacheException, OptionConversionException {

        logCategoryInfo("General Configuration", 1);
        
        OptionReader cacheOptions = OptionReader.create(_wgaConfiguration.getServerOptions(), new CacheModuleDefinition());
    	
        // WebTML Cache
        int tmlCache = _wgaConfiguration.getWebtmlCacheSize();
        if (_webTMLCache == null) {
            _webTMLCache = new WebTMLCache(this, _wgaConfiguration);
        }
        else {
            if (_webTMLCache.configure(_wgaConfiguration)) {
                getLog().info("WebTML Cache configuration updated. Will not be effective until next WebTML cache reset.");
        }
        }
        
        _webserviceEnabled = _wgaConfiguration.isWebservicesEnabled();

        _characterEncoding = _wgaConfiguration.getCharacterEncoding();
        if (_characterEncoding == null) {
        	_characterEncoding = "UTF-8";
        }
        log.info("Using default output encoding '" + _characterEncoding + "'.");
        
        tmlBuffer = _wgaConfiguration.getTmlBuffer();
        if (_wgaConfiguration.getTmlHeader() != null) {
        	tmlHeader = _wgaConfiguration.getTmlHeader();
        } else {
        	tmlHeader = "";
        }

        // determine if character encoding changed
        // this is used for SC_NOT_MODIFIED in WGPDispatcher
        if (_characterEncoding != null && getLastCharacterEncoding() == null || !getLastCharacterEncoding().equals(_characterEncoding)) {
            // character encoding was not yet set or has changed           
            setLastCharacterEncoding(_characterEncoding);
        }
        
        // application log
        this.log.setLevel(org.apache.log4j.Level.toLevel(_wgaConfiguration.getApplicationLogLevel()));
        if (_wgaConfiguration.getApplicationLogDirectory() != null) {
            File loggingDir = new File(_wgaConfiguration.getApplicationLogDirectory());
            if (loggingDir.exists()) {
                try {
                    Appender fileAppender = new DailyRollingFileAppender(new PatternLayout("%d{HH:mm:ss} %p %m\n"), loggingDir.getAbsolutePath() + "/" + "wga.log", "yyyy-MM-dd");
                    this.log.addAppender(fileAppender);
                }
                catch (IOException e) {
                    this.log.error("Error creating application log file", e);
                    _problemRegistry.addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.invalidApplicationLog", ProblemSeverity.HIGH, e));
                }
            }
            else {
                this.log.error("Logging directory " + loggingDir.getAbsolutePath() + " does not exist");
                _problemRegistry.addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.missingLogDir", ProblemSeverity.HIGH, Problem.var("dir", loggingDir.getAbsolutePath())));
            }
        }
        
        // Lucene
        if (update == false && _wgaConfiguration.getLuceneManagerConfiguration().isEnabled()) {
            try {
                this.luceneManager = LuceneManager.retrieve(this, _wgaConfiguration.getLuceneManagerConfiguration());
                log.debug("Lucene option 'maxBooleanClauseCount' set to '" + this.luceneManager.getBooleanQueryMaxClauseCount() + "'.");
                log.debug("Lucene option 'maxDocsPerDBSession' set to '" + this.luceneManager.getMaxDocsPerDBSession() + "'.");
                log.info("Lucene fulltext index enabled using index directory " + this.luceneManager.getIndexDirectory().getAbsolutePath());
            }
            catch (Exception e) {
                log.error("Unable to initialize Lucene fulltext index: " + e.getClass().getName() + " - ", e);
                _problemRegistry.addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.invalidLuceneIndex", ProblemSeverity.HIGH, e));
            }
        }
        
        // File derivates
        if (update == false) {
            this.fileDerivateManager = new FileDerivateManager(this); // Will be inited later on startup
        }
        else {
            this.fileDerivateManager.init(_wgaConfiguration);
        }

        // Personalisation
        userAgentVerifier = new UserAgentVerifier(_wgaConfiguration.getPersonalisationConfiguration(), this);

        // Design
        DesignConfiguration designConfig = _wgaConfiguration.getDesignConfiguration();
        designFileValidator = new DesignFileValidator(designConfig, this);
        if (designConfig.getDefaultEncoding() == null) {
            designConfig.setDefaultEncoding(DEFAULT_FILE_ENCODING);
            getLog().info("No default design file encoding configured. Using '" + DEFAULT_FILE_ENCODING + "'.");
        }
        
        if (designFileCache == null) {
            String designFileCacheSizeStr = _wgaConfiguration.getServerOptions().get(WGAConfiguration.SERVEROPTION_CACHE_DESIGN_SIZE);
            if (designFileCacheSizeStr == null) {
                designFileCacheSizeStr = String.valueOf(designConfig.getDesignFileCacheSize());
            }
            designFileCache = CacheFactory.createCache(CACHENAME_DESIGNFILES, Integer.parseInt(designFileCacheSizeStr), null);
        }

        // Custom core listeners - Get names here, instantiate later when
        // library loader is inited       
        if (update == false) {
        	customCoreListenerClassnames = new HashSet(_wgaConfiguration.getCoreEventListeners());    
        }
        
        // init mail configuration
        _mailConfig = WGAMailConfiguration.create(_wgaConfiguration.getMailConfiguration());      
        WGFactory.setHttpClientFactory(new DefaultHttpClientFactory(_wgaConfiguration));
        
    }



    public ClusterService getClusterService() {
        return _clusterService;
    }

    private void setLastCharacterEncoding(String characterEncoding) {
        Preferences prefs = Preferences.userNodeForPackage(this.getClass());
        prefs.put("LastCharacterEncoding", characterEncoding);        
        prefs.putLong("CharacterEncodingModified", System.currentTimeMillis());
    }
    
    private String getLastCharacterEncoding() {
        Preferences prefs = Preferences.userNodeForPackage(this.getClass());
        return prefs.get("LastCharacterEncoding", null);
    }    
    
    public long getCharacterEncodingLastModified() {
        Preferences prefs = Preferences.userNodeForPackage(this.getClass());
        return prefs.getLong("CharacterEncodingModified", System.currentTimeMillis());
    }
    
    private void setLastDesignEncoding(String dbkey, String encoding) {
        try {
            Preferences prefs = Preferences.userNodeForPackage(this.getClass());
            prefs.put(createPrefKeyDesignEncoding(dbkey), encoding);        
            prefs.putLong(createPrefKeyDesignEncodingLastModified(dbkey), System.currentTimeMillis());
        } catch (Exception e) {
            log.error("Unable to set lastDesignEncoding preferences.", e);
        }
    }
    
    private String createPrefKeyDesignEncoding(String dbkey) throws NoSuchAlgorithmException {
        return "LastDesignEncoding_" + WGUtils.hashPassword(dbkey.toLowerCase());
    }
    
    private String createPrefKeyDesignEncodingLastModified(String dbkey) throws NoSuchAlgorithmException {
        return "DesignEncodingModified_" + WGUtils.hashPassword(dbkey.toLowerCase());
    }
    
    
    private String getLastDesignEncoding(String dbkey) {
        Preferences prefs = Preferences.userNodeForPackage(this.getClass());
        try {
            return prefs.get(createPrefKeyDesignEncoding(dbkey), null);
        }
        catch (Exception e) {
            log.error("Unable to retrieve preferences for 'LastDesignEncoding'", e);
            return null;
        }
    }    
    
    public long getDesignEncodingLastModified(String dbkey) {
        Preferences prefs = Preferences.userNodeForPackage(this.getClass());
        try {
            return prefs.getLong(createPrefKeyDesignEncodingLastModified(dbkey), System.currentTimeMillis());
        }
        catch (Exception e) {
            log.error("Unable to retrieve preferences for 'DesignEncodingModified'", e);
            return System.currentTimeMillis();
        }
    }

    private void initReadMappings() {
        
        logCategoryInfo("Mappings", 1);

        //Element mappingsRoot = (Element) this.configDocument.getRootElement().selectSingleNode("mappings");
    	
        // get mapping class loader
        buildLibraryLoaders();

        // Default media mappings
        this.systemMediaKeys.put(ENCODER_HTML, new MediaKey(ENCODER_HTML, "text/html", false, false));
        this.systemMediaKeys.put(ENCODER_XML, new MediaKey(ENCODER_XML, "text/xml", false, true));
        this.systemMediaKeys.put("wml", new MediaKey("wml", "text/wml", false, false));
        this.systemMediaKeys.put("pdf", new MediaKey("pdf", "application/pdf", true, true));
        
        defaultAnalyzer = new StandardAnalyzer(org.apache.lucene.util.Version.LUCENE_35, LUCENE_STOP_WORDS);
        
        log.info("Registering defaultAnalyzer to '" + defaultAnalyzer.getClass().getName() + "'");
        
    }

    private synchronized void initReadFilterMappings() {
        
        logCategoryInfo("Filter mappings", 1);
        
        List<WGAFilterConfig> newFilterMappings = new LinkedList<WGAFilterConfig>();
        
        // read filter mappings from registry
        Iterator filters = getModuleRegistry().getModulesForType(FilterConfigModuleType.class).values().iterator();
        while (filters.hasNext()) {
            ModuleDefinition modDef = (ModuleDefinition) filters.next();
            try {
                modDef.testDependencies();
                FilterMapping mapping = (FilterMapping) modDef.getProperties();
                WGAFilterConfig config = WGAFilterConfig.createFromMapping(mapping);
                if (config != null) {
                    log.info("Adding filter '" + config.getFilterName() + "'");
                    newFilterMappings.add(config);
                }
            }
            catch (ModuleDependencyException e) {
                // Fail silently here. Filters may not yet be ready at this time (#00001870). We notify about filters with missing dependencies later.
            }
        }
        
        // read filter mappings from config
        Iterator<FilterMapping> mappings = _wgaConfiguration.getFilterMappings().iterator();
        while (mappings.hasNext()) {
        	WGAFilterConfig config = WGAFilterConfig.createFromMapping(mappings.next());
        	if (config != null) {
        		log.info("Adding filter '" + config.getFilterName() + "'");
            	newFilterMappings.add(config);
        	}
        }        
        this.filterMappings = newFilterMappings;
    }
    
    public void addAnalyzerMapping(String language, String analyzerClassName) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
    	log.info("Registering analyzer mapping for language code '" + language + "' to '" + analyzerClassName + "'");
    	Class analyzerClass = getLibraryLoader().loadClass(analyzerClassName);
        Analyzer analyzer = (Analyzer) analyzerClass.newInstance();
    	analyzerMappings.put(language.toLowerCase(), analyzer);
    }

    public void addAnalyzerMapping(String language, Analyzer analyzer){
    	log.info("Registering analyzer mapping for language code '" + language + "' to '" + analyzer.getClass().getName() + "'");
    	analyzerMappings.put(language.toLowerCase(), analyzer);
    }
    
    public void removeAnalyzerMapping(String language){
    	log.info("Unregistering analyzer mapping for language code '" + language);
    	analyzerMappings.remove(language.toLowerCase());
    }

    public void removeAllAnalyzerMappings(){
    	log.info("Unregistering all language specific analyzer mappings");
    	analyzerMappings.clear();
    }

    public void addFileHandlerMapping(String extension, String handlerClassName) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
    	log.info("Registering filehandler for extension '" + extension + "' - '" + handlerClassName + "'.");
    	Class fileHandlerClass = getLibraryLoader().loadClass(handlerClassName);
        FileHandler handler = (FileHandler) fileHandlerClass.newInstance();
        fileHandlerMappings.put(extension.toLowerCase(), handler);
	}
    
    

	private boolean addElementMapping(String name, String theClass, boolean system) {
        
        if (system) {
            this.systemElements.put(name, theClass);
            return true;
        }
        else if (systemElements.containsKey(name)) {
            getLog().warn("Cannot add WebTML element '" + name + "' because the name is already used in WGA configuration");
            return false;
        }
        else {
           this.customElements.put(name, theClass);
           return true;
        }
        
        
    }

    private boolean addEncoderMapping(String name, String theClass, boolean system) {
        try {
            name = name.toLowerCase();
            Class formatterClass = getLibraryLoader().loadClass(theClass);
            Object instance = formatterClass.newInstance();
            if (instance instanceof ObjectFormatter) {
                if (system) {
                    _systemFormatters.put(name, formatterClass);
                }
                else {
                    if (_systemFormatters.containsKey(name)) {
                        log.warn("Cannot add encoding formatter '" + name + "' because the name is already used by a formatter in WGA configuration");
                        return false;
                    }
                    _customFormatters.put(name, formatterClass);
                }
                return true;
            }
            else {
                log.error("Encoding formatter '" + theClass + "' does not implement interface " + ObjectFormatter.class.getName());
            }
        }
        catch (InstantiationException e) {
            log.error("Error instantiating encoding formatter '" + theClass + "':" + e.getMessage());
        }
        catch (IllegalAccessException e) {
            log.error("Access error instantiating encoding formatter '" + theClass + "':" + e.getMessage());
        }
        catch (ClassNotFoundException e) {
            log.error("Encoding formatter class '" + theClass + "' could not be found");
        }
        catch (NoClassDefFoundError e) {
            log.error("Encoding formatter class '" + theClass + "' could not be loaded", e);
        }
        
        return false;
    }

    private void buildLibraryLoaders() {
        
        // Build jars list
        List<URL> jarsList = new ArrayList<URL>();
        	
    	String libraries =  _wgaConfiguration.getServerOptions().get(WGAConfiguration.SERVEROPTION_LIBRARIES);
    	if (libraries != null && !libraries.trim().equals("")) {
		    StringTokenizer tokens = new StringTokenizer(libraries, ";");
		    String token;
		    while (tokens.hasMoreTokens()) {
		
		        token = tokens.nextToken();
		        
		        if (token.endsWith("/*")) {
		            File libDir = getWGAFile(token.substring(0, token.length() - 2));
		            if (libDir != null && libDir.exists() && libDir.isDirectory()) {
		                for (File libFile : libDir.listFiles()) {
		                    if (libFile.getName().endsWith(".jar")) {
		                        addLibraryFile(jarsList, libFile);
		                    }
		                }
		            }
		            else {
		                log.warn("Library entry is not a valid directory: " + token);
		            }
		        }
		        else {
                    File libFile = getWGAFile(token);
    		        addLibraryFile(jarsList, libFile);
		        }
		    }
		}
    	
    	// add all jars from <configdir>/libs
    	File baseLibDir = getBaseLibraryDir();
	    if (baseLibDir != null && baseLibDir.exists() && baseLibDir.canRead()) {
	    	File[] additionalJarFiles = baseLibDir.listFiles(new FilenameFilter() {
	
				public boolean accept(File dir, String name) {
					return name.toLowerCase().endsWith(".jar");
				}
	    		
	    	});
	    	
	    	for (File jarFile : additionalJarFiles) {
	    		try {	    			
					jarsList.add(jarFile.toURL());
					log.info("Registering library '" + jarFile.getAbsolutePath() + "'.");
				} catch (MalformedURLException e) {
					this.log.warn("Registering library '" + jarFile.getAbsolutePath() + "' failed.", e);
				}
	    	}
    	}
	    
	    
        // Build URL array from jars list
        URL[] loaderURLs = new URL[jarsList.size()];
        for (int idx = 0; idx < jarsList.size(); idx++) {
            loaderURLs[idx] = jarsList.get(idx);
        }
        
        baseLibraryLoader = new IsolatedJARLoader(loaderURLs, getClass().getClassLoader());
        getLog().info("Creating WGA java library loader");
        libraryClassLoadingChain = new DynamicClassLoadingChain(baseLibraryLoader);
        WGFactory.setImplementationLoader(libraryClassLoadingChain);
        updateLibraryLoader();
        _libraryXStream = new XStream(new Dom4JDriver());
        _libraryXStream.setClassLoader(libraryClassLoadingChain);
        
    }

    private void addLibraryFile(List<URL> jarsList, File libFile) {
        try {
            if (libFile.exists()) {
                jarsList.add(new URL(libFile.toURI().toString()));
                log.info("Registering class library URL " + libFile.toURI().toString());
            }
            else {
                log.warn("Library entry is not a valid file: " + libFile.getAbsolutePath());
            }

        }
        catch (MalformedURLException e) {
            log.warn("Exception adding class Library URL for: " + libFile.getAbsolutePath(), e);
        }
    }
    
    public XStream getLibraryXStream() {
        return _libraryXStream;
    }

    public File getBaseLibraryDir() {    	
    	File dir = new File(getWgaDataDir(),"lib");
    	if (!dir.exists()) {
    		dir.mkdirs();
    	}
		return dir;
	}
    
    
    
    
    public boolean isLocalRequest(HttpServletRequest request) {
        
        try {
            // if servername is not loopback --> return false
            if (!InetAddress.getByName(request.getServerName()).isLoopbackAddress()) {
                return false;
            }
        }
        catch (UnknownHostException e1) {
            // might happen if localhost cannot be looked up
            // additional check for sure
            if (!request.getServerName().toLowerCase().equals("localhost")) {
                return false;
            }
        }
            
        try {
            // if remote address is not loopback -> return false
            if (!InetAddress.getByName(request.getRemoteAddr()).isLoopbackAddress()) {
                return false;
            }
        }
        catch (UnknownHostException e1) {
            // unparsable remote address - should not happen
            return false;
        }
        
        return true;
        
    }    

    public void contextInitialized(ServletContextEvent arg0) {

        try {
            _context = arg0.getServletContext();

            // General inits
            this.instanceActiveSince = new Date();
            arg0.getServletContext().setAttribute(ATTRIB_CORE, this);
            WGFactory.setAuthModuleFactory(new WGAAuthModuleFactory(this));
            WGFactory.setMimetypeDeterminationService(new WGAMimetypeDeterminationService());

            // Create temp dir
            File mainTempDir = new File(System.getProperty("java.io.tmpdir"));
            this.wgaTempDir = new File(mainTempDir, ".wgatemp");
            if (wgaTempDir.exists()) {
                killTempDir(wgaTempDir);
            }
            wgaTempDir.mkdir();
            WGFactory.setTempDir(wgaTempDir);
            TemporaryFile.prepareDirectory(wgaTempDir);

            // Creating logger
            initLoggingFile();
            String debug = System.getProperty(SYSPROPERTY_DEBUG);
            if (debug != null) {
                for (String target : WGUtils.deserializeCollection(debug, ",", true)) {
                	log.info("Set log level DEBUG for Logger " + target);
                    Logger logger = Logger.getLogger(target);
                    logger.setLevel(Level.DEBUG);
                }
            }

            // Try to retrieve build properties
            _buildSignature = "NOSIGNATURE";
            try {
                InputStream buildPropIn = _context.getResourceAsStream("/WEB-INF/wgabuild.properties");
                Properties props = new Properties();
                props.load(buildPropIn);
                if (props.containsKey("signature")) {
                    _buildSignature = props.getProperty("signature");
                }
            }
            catch (RuntimeException e1) {
            }

            // Retrieving platform info
            String endDate = (new SimpleDateFormat("yyyy")).format(new Date());
            log.info(getReleaseString() + " (c) 2001-" + endDate + " Innovation Gate GmbH");
            try {
                this.servletPlatform = new Double(arg0.getServletContext().getMajorVersion() + "." + arg0.getServletContext().getMinorVersion()).doubleValue();
                this.jspPlatform = new Double(javax.servlet.jsp.JspFactory.getDefaultFactory().getEngineInfo().getSpecificationVersion()).doubleValue();
                log.info("On platform " + arg0.getServletContext().getServerInfo() + " (Servlet Engine " + this.servletPlatform + ", JSP Engine " + this.jspPlatform + ")");
            }
            catch (Exception ecx) {
                log.warn("Unable to retrieve platform info. Assuming Servlet Engine 3.0, JSP Engine 2.2", ecx);
                this.servletPlatform = 3.0;
                this.jspPlatform = 2.2;
            }
            arg0.getServletContext().setAttribute(WGACore.ATTRIB_SERVLET_ENGINE, new Double(this.servletPlatform));
            arg0.getServletContext().setAttribute(WGACore.ATTRIB_JSP_ENGINE, new Double(this.jspPlatform));

            // Starting other non-servlet services
            this._deployer = new WGPDeployer(this);

            // Create the statistics object
            _usageStatistics = new WGAUsageStatistics(this);

            // Test some neccessary running conditions for WGA
            
            // We test for exploded WAR
            if (getServletContext().getRealPath("/") == null) {
                log.fatal("WGA is not deployed as exploded WGA archive which is absolutely mandatory! WebTML deploying and rendering will not work correctly.");
            }
            
            // Eventually overwrite wga configpath/datapath/permanent log sysvar with init parameter
            String initConfigPath = getServletContext().getInitParameter(SYSPROPERTY_CONFIGPATH);
            if (initConfigPath != null) {
                log.info("Using WGA config path from servlet init parameter: " + initConfigPath);
                System.setProperty(SYSPROPERTY_CONFIGPATH, initConfigPath);
            }
            String initDataPath = getServletContext().getInitParameter(SYSPROPERTY_DATAPATH);
            if (initDataPath != null) {
                log.info("Using WGA data path from servlet init parameter: " + initDataPath);
                System.setProperty(SYSPROPERTY_DATAPATH, initDataPath);
            }
            String initPermLog = getServletContext().getInitParameter(SYSPROPERTY_LOGPATH);
            if (initPermLog != null) {
                log.info("Using WGA permanent log path from servlet init parameter: " + initPermLog);
                System.setProperty(SYSPROPERTY_LOGPATH, initPermLog);
            }
            
            // Do startup
            fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_PRE_STARTUP, null, this));
            startup();
        }
        catch (Throwable e) {
            log.fatal("Fatal error initializing wga", e);
            e.printStackTrace();
        }

    }

    

    public ServletContext getServletContext() {
        return _context;
    }

    @SuppressWarnings("unused")
    public static String getReleaseString() {

        StringBuffer output = new StringBuffer();

        output.append(WGAVersion.WGAPUBLISHER_PRODUCT_NAME).append(" ");
        output.append(WGAVersion.WGAPUBLISHER_MAJOR_VERSION).append(".").append(WGAVersion.WGAPUBLISHER_MINOR_VERSION);

        if (WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION > 0) {
            output.append(".").append(WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION);
        }

        if (WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION > 0) {
            output.append(" Maintenance Release");
        }
        else {
            output.append(" Release");
        }

        if (WGAVersion.WGAPUBLISHER_PATCH_VERSION > 0) {
            output.append(" Patch " + WGAVersion.WGAPUBLISHER_PATCH_VERSION);
        }

        
        output.append(" (Build ").append(WGAVersion.WGAPUBLISHER_BUILD_VERSION).append(")");

        return output.toString();
    }
    
    public static String getGeneratorString() {
        
        
        
        StringBuffer output = new StringBuffer();

        output.append(WGAVersion.WGAPUBLISHER_PRODUCT_NAME).append(" ");
        output.append(WGAVersion.WGAPUBLISHER_MAJOR_VERSION).append(" ");
        if (!WGUtils.isEmpty(WGAVersion.WGAPUBLISHER_PLATFORM_NAME)) {
            output.append("\'").append(WGAVersion.WGAPUBLISHER_PLATFORM_NAME).append("\' Platform");
        }

        return output.toString();
        
        
    }
    
    public static String getPlatformString() {

        StringBuffer output = new StringBuffer();

        output.append(WGAVersion.WGAPUBLISHER_MAJOR_VERSION).append(".").append(WGAVersion.WGAPUBLISHER_MINOR_VERSION).append(" ");
        output.append(WGAVersion.WGAPUBLISHER_PLATFORM_NAME);

        return output.toString();
    }

    // Platform info
    private double servletPlatform;

    private double jspPlatform;

    private AppLogAppender transientLogAppender = null;

    private File loggingDir = null;

    private AsyncAppender baseAppender = null;

    public void initLoggingFile() {

            // Base appender
            if (this.baseAppender == null) {
                this.baseAppender = new AsyncAppender();
                this.baseAppender.setBufferSize(1000);
                this.baseAppender.setBlocking(true);
                this.log.addAppender(this.baseAppender);
            }

            // Transient log
            if (this.transientLogAppender != null) {
                this.baseAppender.removeAppender(this.transientLogAppender);
                this.transientLogAppender.close();
                this.transientLogAppender = null;
            }
            if (this.loggingDir != null && this.loggingDir.exists()) {
                WGUtils.delTree(loggingDir);
            }
            
            this.loggingDir = new File(getWgaTempDir(), "applog");
            this.transientLogAppender = new AppLogAppender(loggingDir.getAbsolutePath(), "wga-", ".log");
            transientLogAppender.setName("wga.transientLog");
            transientLogAppender.setAppend(true);
            transientLogAppender.setEncoding("UTF-8");
            transientLogAppender.setLayout(LAYOUT_APPLOG);
            transientLogAppender.setMaxFileSize(1024 * 1024 * 10);
            transientLogAppender.setFilesToKeep(10);
            this.baseAppender.addAppender(transientLogAppender);

            // Permanent log
            String permanentLogPath = System.getProperty(SYSPROPERTY_LOGPATH);
            if (permanentLogPath != null) {
                File logPathFile = new File(permanentLogPath);
                if (!logPathFile.exists() || !logPathFile.isDirectory()) {
                    log.error("Unable to use path for permanent log '" + permanentLogPath + "' because it either does not exist or is no directory.");
                }
                else {
                    log.info("Permanent application log stored in directory " + permanentLogPath);
                    DatedFileAppender permanentFileAppender = new DatedFileAppender(permanentLogPath, "wga-", ".log");
                    permanentFileAppender.setName("wga.permanentLog");
                    permanentFileAppender.setAppend(true);
                    permanentFileAppender.setEncoding("UTF-8");
                    permanentFileAppender.setLayout(LAYOUT_APPLOG);
                    this.baseAppender.addAppender(permanentFileAppender);
                }
            }
    }

    /**
     * @return
     */
    public File getWgaTempDir() {
        return wgaTempDir;
    }

    /*
     * (Kein Javadoc)
     * 
     * @see javax.servlet.ServletContextListener#contextDestroyed(javax.servlet.ServletContextEvent)
     */
    public void contextDestroyed(ServletContextEvent arg0) {

        try {
            shutdown();
            WGFactory.getInstance().shutdown();
        }
        catch (Throwable t) {
            log.error("Error shutting down WGA", t);
        }
        finally {
            if (log != null) {
                log.removeAllAppenders();
            }
            if (transientLogAppender != null) {
                transientLogAppender.close();
            }
            if (wgaTempDir != null) {
                killTempDir(wgaTempDir);
                wgaTempDir = null;
            }
            // B000048C2 - cleanup temp. plugin resources
            if (pluginSet != null) {
            	pluginSet.clearTempResources();
            	pluginSet = null;
            }
        }

    }

    public void shutdown() {
        
        logCategoryInfo(WGAVersion.WGAPUBLISHER_PRODUCT_NAME + " starting shutdown...", 1);
        
        fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_PRE_SHUTDOWN, null, this));

        logCategoryInfo("Disabling dispatcher", 2);
        
        WGPDispatcher dispatcher = getDispatcher();
        if (dispatcher != null) {
            dispatcher.setServePages(false);
        }
        
        // Clear listeners whose actions are no longer needed on shutdown
        _moduleRegistry.clearChangeListeners();
        
        // Shutdown various platform services (db-independent)
        logCategoryInfo("Shutting down platform services", 2);
        getLog().info("Shutting down scheduler");
        try {
            _quartzScheduler.shutdown(true);
        }
        catch (SchedulerException e) {
            getLog().error("Exception shutting down WGA scheduler", e);
        }
        _quartzScheduler = null;

        if (this.timer != null) {
            this.timer.shutdown();
            this.timer = null;
        }

        getLog().info("Shutting down lucene");
        if (this.luceneManager != null) {
            this.luceneManager.destroy();
            this.luceneManager = null;
        }
        
        this.fileDerivateManager.stop();
        this.fileDerivateManager = null;
        
        getLog().info("Shutting down WebSocket connections");
        _pageConnectionManager.shutdown();
        _independentWebSocketManager.shutdown();
        
        getLog().info("Shutting down event manager");
       _eventManager.shutdown();

        if (_clusterService != null) {
            _clusterService.shutdown();
            _clusterService = null;
        }
        
        if (_externalFileMaintenanceTask != null) {
            _externalFileMaintenanceTask.stop();
            _externalFileMaintenanceTask = null;
        }
        
        // Stop the event thread
        getLog().info("Shutting down event thread");
        WGFactory.getInstance().getEventThread().stop();
        
        // fire pre disconnect event
        fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_SHUTDOWN_PRE_DISCONNECT, null, this));

        logCategoryInfo("Closing content databases", 2);
        
        // Close content databases
        Iterator<String> dbs = new HashSet<String>(this.contentdbs.keySet()).iterator();
        while (dbs.hasNext()) {
            removeContentDB(dbs.next());
        }

        logCategoryInfo("Closing personalisation databases", 2);
        
        // Close personalisation databases
        dbs = new HashSet<String>(this.personalisationdbs.keySet()).iterator();
        while (dbs.hasNext()) {
            removePersonalisationDB(dbs.next());
        }

        logCategoryInfo("Closing domains", 2);
        
        // Close domains (especially their auth modules)
        closeDomainConfigs(this.domains);
        
        logCategoryInfo("Shutting down basic services", 2);
        
        unDeployErrorPage();
        
        WGHierarchicalDatabase.removeCoreListener(_hdbCoreListener);
        
        // fire post disconnect event
        fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_SHUTDOWN_POST_DISCONNECT, null, this));                
        
        if (_httpSessionManager != null) {
            _httpSessionManager.clearListeners();
            _httpSessionManager.shutdown();
        }
        
        // Close integrated JMX
        _jmx.shutdown();
        _jmx = null;

        // Close access logger
        if (_accessLogger != null) {
            _accessLogger.close();
        }
        
        // Cleanup deployer
        _deployer.shutdown();
        
        // Cleanup WebTML cache
        try {
            _webTMLCache.close();
            _webTMLCache = null;
        }
        catch (CacheException e) {
            getLog().error("Exception shutting down WebTML cache", e);
        }
        
        // Cleanup statistics
        if (_usageStatistics != null) {
            _usageStatistics.dispose();
            _usageStatistics = null;
        }

        // Remove all core listeners
        coreEventListeners.clear();
        
        // Close expression engines and WGA classpath
        ExpressionEngineFactory.closeEngines();
        libraryClassLoadingChain = null;        
        
        
        // Clear and close caches
        WGFactory.getAuthModuleFactory().clearCache();
        try {
            designFileCache.destroy();
            designFileCache = null;
        }
        catch (CacheException e) {
            log.error("Exception closing design file cache", e);
        }

        try {
            _calledSequenceIds.destroy();
            _calledSequenceIds = null;
        }
        catch (CacheException e) {
            log.error("Exception closing action sequence id cache", e);
        }
        EHCacheCore.getCacheManager().shutdown();
        
        // Unregister HSQLDB driver
        try {
            DriverManager.deregisterDriver(DriverManager.getDriver("jdbc:hsqldb:mem:justShuttingDown"));
        }
        catch (SQLException e1) {
            getLog().error("Exception de-registering HSQL driver", e1);
        }

        // Close logserver
        if (_logServer != null) {
            try {
                _logServer.shutdown();
            }
            catch (IOException e) {
                getLog().error("Exception shutting down WGA Remote Log Server", e);
            }
            _logServer = null;
        }
        
        _problemRegistry.close();
        _problemRegistry = null;
        
        TemporaryFile.stopEviction();

        fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_POST_SHUTDOWN, null, this));
        logCategoryInfo(WGAVersion.WGAPUBLISHER_PRODUCT_NAME + " finished shutdown", 1);
    }



    // Application attributes set by this program
    public static final String ATTRIB_CORE = "Core";

    public static final String ATTRIB_DISPATCHER = "Dispatcher";



    public static final String ATTRIB_MAINCONTEXT = "TMLContext";

    public static final String ATTRIB_CONTENTDBS = "Databases";

    public static final String ATTRIB_WGPPATH = "WGPPath";

    public static final String ATTRIB_FETCHED_USERPROFILES = "FetchedUserProfiles";

    public static final String ATTRIB_SERVLET_ENGINE = "ServletEngine";

    public static final String ATTRIB_JSP_ENGINE = "JspEngine";

    public static final String ATTRIB_TAGIDS = "TagIds";

    public static final String ATTRIB_REQUESTURL = "RequestURL";

    public static final String ATTRIB_MEDIAKEY = "MediaKey";

    public static final String ATTRIB_MIMETYPE = "MimeType";

    public static final String ATTRIB_BROWSERINTERFACE = "BrowserInterface";
    
    public static final String ATTRIB_NO_CONTENT_NOTIFCATION_URL = "de.innovationgate.wgpublisher.NoContentNotificationURL";
    
    public static final String ATTRIB_VIRTUAL_CONTENT_URL = "de.innovationgate.wgpublisher.VirtualContentURL";

    public static final String ATTRIB_EXCEPTION = "Exception";

    public static final String ATTRIB_WGAERROR = "WGAError";

    public static final String ATTRIB_OUTER_DESIGN = "OuterDesign";
    public static final String ATTRIB_OUTER_DESIGN_DB = "OuterDesignDb";

    public static final String ATTRIB_ROOT_TAG = "RootTag";

    public static final String ATTRIB_LOGINERROR = "LoginError";

    public static final String ATTRIB_SERVLETRESPONSE = "OutputStream";

    public static final String ATTRIB_REDIRECT = "Redirect";

    public static final String ATTRIB_REQUESTTIME = "RequestTime";
    
    public static final String ATTRIB_SCOPEOBJECTREGISTRY = "ScopeObjectRegistry";

    public static final String SESSION_ACTIONS = "Actions";

    public static final String SESSION_SCOPEOBJECTREGISTRY_BASE = "ScopeObjectRegistry_";
    
    public static final String SESSION_ACTION_SEQUENCE = "ActionSequence";

    public static final String SESSION_ADMINNAME = "AdminName";

    public static final String SESSION_ADMINPASSWORD = "AdminPassword";
    
    public static final String SESSION_WARNED_PROFILESESSIONS = "WarnedProfileSessions";

    public static final String ATTRIB_TMLFORM = "TMLForm";
    
    public static final String ATTRIB_POSTED_TMLFORM ="PostedTMLForm";

    public static final String ATTRIB_LASTFORM = "LastForm";
    
    /**
     * Request Variable. When set to true allows OpenWGA to use regular user/password login although the auth module is request based
     * May be invalidated by using #ATTRIB_FORCEREQUESTBASELOGIN
     */
    public static final String ATTRIB_FORCEREGULARLOGIN = "ForceRegularLogin";
    
    /**
     * Request Variable. When set to true will force OpenWGA to use request based login, even when {@link #ATTRIB_FORCEREGULARLOGIN} is set to true.
     */
    public static final String ATTRIB_FORCEREQUESTBASELOGIN = "ForceRequestBasedLogin";

    public static final String ATTRIB_REQUESTPATH = "RequestPath";

    // Database attributes set by this program
    public static final String DBATTRIB_DBKEY = "DBURLPath";

    public static final String DBATTRIB_EXPRESSION_DEFAULT = "ExpressionDefault";

    public static final String DBATTRIB_QUERY_DEFAULT = "QueryDefault";

    public static final String DBATTRIB_DEFAULT_MEDIAKEY = "DefaultMediaKey";

    public static final String DBATTRIB_ALLOW_BROWSING = "AllowBrowsing";
    public static final String DBATTRIB_BROWSING_SECURITY = PublisherOption.OPTION_BROWSING_SECURITY;
    
    public static final String DBATTRIB_STARTPAGE = "StartPage";

    

    
    /**
     * Boolean publisher option that identifies a database as authoring application
     * WGA will restrict access to this database to a configured port or disable it et al
     * if authoring is disabled by configuration. 
     */
    public static final String DBATTRIB_AUTHORING_APP = "AuthoringApp";

    /**
     * Boolean publisher option that identifies a database as admininstrative application
     * WGA will restrict access to this database to a configured port and will not allow access
     * if someone is not logged in as OpenWGA admin
     */
    public static final String DBATTRIB_ADMIN_APP = "AdminApp";



    public static final String DBATTRIB_MAXQUERYRESULTS = "MaxQueryResults";



    public static final String DBATTRIB_DOMAIN = "Domain";

    public static final String DBATTRIB_STORED_QUERIES = "StoredQueries";

    public static final String DBATTRIB_META_MAPPINGS = "MetaMappings";

    public static final String DBATTRIB_ITEM_MAPPINGS = "ItemMappings";
    public static final String DBATTRIB_PLUGIN_SHORTCUTS = "PluginShortcuts";

    public static final String DBATTRIB_HOME_PAGE = "HomePage";

    public static final String DBATTRIB_LOGIN_PAGE = "LoginPage";

    public static final String DBATTRIB_PERSMODE = "PersMode";
    
    public static final String DBATTRIB_PERSMODE_OPT_IN = "PersMode.OptIn";
    
    public static final String DBATTRIB_PORTLETREGISTRYMODE = "PortletRegistryMode";

    public static final String DBATTRIB_PERSSTATMODE = "PersStatMode";

    public static final String DBATTRIB_FILEEXPIRATION_MINUTES = "FileExpirationMinutes";

    public static final String DBATTRIB_FILECACHE = "FileCache";
    
    public static final String DBATTRIB_FILECACHE_ENTRIES = "FileCacheEntries";
    
    public static final String DBATTRIB_FILECACHE_THRESHOLD = "FileCacheThreshold";
    
    public static final String DBATTRIB_PPRCACHE = "PPRCache";
    
    public static final String DBATTRIB_PPRCACHE_ENTRIES = "PPRCacheEntries";

    public static final String ATTRIB_BI_COLLECTIONS_SHOW_RELEASED_ONLY = "BICollectionsShowReleasedOnly";

    public static final String DBATTRIB_VARPROVISIONING = "VarProvisioning";
    
    public static final String DBATTRIB_TITLEPATHURL = "TitlePathURL";
    
    public static final String DBATTRIB_TITLEPATHURL_CONTENTINDEXING = "TitlePathURL.ContentIndexing";
    
    public static final String DBATTRIB_TITLEPATHURL_MIXEDLANGUAGES = "TitlePathURL.MixedLanguages";
    
    public static final String DBATTRIB_TITLEPATHURL_INCLUDEKEYS = "TitlePathURL.IncludeKeys";    
    public static final String DBATTRIB_TITLEPATHURL_USESTRUCTKEYS = "TitlePathURL.UseStructkeysAsKey";
    
    public static final String DBATTRIB_TITLEPATHURL_ALLOW_UMLAUTE = "TitlePathURL.AllowUmlaute";

    public static final String DBATTRIB_TITLEPATHMANAGER = "TitlePathManager";

    public static final String DBATTRIB_WEBTMLCACHE_SERVESTALEDATA = "WebTMLCache.ServeStaleData";

    public static final String DBATTRIB_DEFAULT_ITEM_ENCODING = "DefaultItemEncoding";

    public static final String DBATTRIB_USERCLASSES = "UserClasses";

    public static final String DBATTRIB_RESOURCEBUNDLE_MANAGER = "ResourceBundleManager";

    public static final String DBATTRIB_HTTPLOGIN = "HttpLogin";

    public static final String DBATTRIB_LUCENE_CONFIG = "LuceneConfig";

    // contains a list of IPv4Restrictions for a db
    public static final String DBATTRIB_CLIENTRESTRICTIONS = "ClientRestrictions";

    /**
     * Boolean Attrib determines if the database is allowed to use remote docs
     */
    public static final String DBATTRIB_USEREMOTECS = "UseRemoteContentStores";
    
    /**
     * Attribute to determine the language behaviour for a database, influencing
     * the way the preferred language is determined 
     */
    public static final String DBATTRIB_LANGUAGEBEHAVIOUR = "LanguageBehaviour";

    /**
     * Boolean attrib determines if contents from this db may be used for remote
     * docs
     */
    public static final String DBATTRIB_ISREMOTECS = "IsRemoteContentStore";
    
    /**
     * Configures the file annotators to be active on this database 
     */
    public static final String DBATTRIB_FILE_ANNOTATORS = "FileAnnotators";

    // System properties
    /**
     * Directory from where to load wga configuration, defaults to the user's home directory. Many other locations are used relative to this path by default.
     */
    public static final String SYSPROPERTY_CONFIGPATH = "de.innovationgate.wga.configpath";


    /**
     * Directory where WGA can store management data, defaults to CONFIPATH/wgadata
     */
    public static final String SYSPROPERTY_DATAPATH = "de.innovationgate.wga.datapath";
    
    
    /**
     * Name of the WGA configuration file, defaults to wga.xml
     */
    public static final String SYSPROPERTY_CONFIGFILE = "de.innovationgate.wga.configfile";

    /**
     * Can be used to set one log4j logging path to DEBUG level
     */
    /**
     * Can be used to set one log4j logging path to DEBUG level
     */
    public static final String SYSPROPERTY_DEBUG = "de.innovationgate.wga.debug";

    

    private static final String OLD_CONFIGFILE_NAME = "wga.xml";
    
    private static final String CONFIGFILE_NAME = "wgaconfig.xml";

    public static final java.text.DateFormat DATEFORMAT_GMT = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", new Locale("en", "US"));

    static Logger log = Logger.getLogger("wga");

    private Date instanceActiveSince;
    
    // domain configurations mapped by domainname
    private Map<String, WGADomain> domains;

    private org.quartz.Scheduler _quartzScheduler;

    private ArrayList<Object> customCoreListeners;

    private SystemContainerManager _systemContainerManager;

    private File _wgaDataDir;

	WGAConfiguration _wgaConfiguration;

    private WGADesignManager _designManager;
    
    private PersonalisationManager _persManager;

    public PersonalisationManager getPersManager() {
        return _persManager;
    }

    private ModuleRegistry _moduleRegistry = null;

    private WGALoggerWrapper _accessLogger;

	private WGHierarchicalDatabaseCoreListener _hdbCoreListener;

    private ExternalFileServingMaintenanceTask _externalFileMaintenanceTask;

    private WGAConfigurationOptionReader _servicesServerOptionReader;
    
    private JMX _jmx;

    private ProblemRegistry _problemRegistry;
    
    private XStream _defaultSerializer;

    private WGACoreTimer timer;



    private SymmetricEncryptionEngine _symmetricEncryptionEngine;

    public ProblemRegistry getProblemRegistry() {
        return _problemRegistry;
    }

    public ModuleRegistry getModuleRegistry() {
        return _moduleRegistry;
    }

    private static final String SYSPROPERTY_QUARTZ_THREADCOUNT = "de.innovationgate.wga.quartz.threadcount";

    private static final String SYSPROPERTY_QUARTZ_THREADPRIORITY = "de.innovationgate.wga.quartz.threadpriority";

    private static final String DEFAULT_CONFIG_DIR = "WGA";

    public void startup() throws ServletException {
        try {
            this.log.info("Starting up " + WGABrand.getName() + " service");
            
            // prepare HDB
            TMLScriptHDBListenerFactory listenerFactory = new TMLScriptHDBListenerFactory(this);
            WGHierarchicalDatabase.setDefaultListenerFactory(listenerFactory);
            WGHierarchicalDatabase.setDefaultStartupImpl(null);
            _hdbCoreListener = new WGHierarchicalDatabaseCoreListener() {

				public void databaseCreated(WGHierarchicalDatabase hdb) {					
					// Eventually load and initialize model
					WGDatabase db = hdb.getWrappedDB();
			        try {
			    		HDBModel.createModelObject(WGACore.this, db);            
	                }
	                catch (Exception e) {
	                    WGACore.this.log.error("Error initializing HDB model for database " + db.getDbReference(), e);
	                }
				}

				public void databaseRemoved(WGHierarchicalDatabase hdb) {					
				}
            	
            };
            WGHierarchicalDatabase.addCoreListener(_hdbCoreListener);

            String configFilePath = retrieveConfigPath();
            // init DES-Encrypter
            File desKeyFile = new File(configFilePath, "des.key");
            desEncrypter = new DESEncrypter();
            try {
                try {
                    desEncrypter.init(desKeyFile);
                    log.info("DESEncrypter initialized using keyfile '" + desKeyFile.getPath() + "'.");
                }
                catch (DESEncrypter.PersistentKeyException e) {
                    log.warn(
                            "Unable to create or restore encryption key - generating temporary key. Session replication will not work with this key. Ensure the application server has read/write access to '"
                                    + desKeyFile.getPath() + "'.", e);
                    // init with temp key
                    desEncrypter.init();
                    log.info("DESEncrypter initialized with temporary key.");
                }
            }
            catch (GeneralSecurityException e) {
                // VM does not support 'des' algorithm - should not happen in wga supported VMs
                log.error("Unable to create DESEncrypter.", e);
                throw new ServletException("wga publisher initialization failure");
            }
            
            // init symmetric encryption engine
            File keyFile = new File(configFilePath, "openwga.key");
            _symmetricEncryptionEngine = new SymmetricEncryptionEngine();
            try {
                byte[] keyBytes = null;
                if (!keyFile.exists()) {                    
                    log.info("SymmetricEncryptionEngine: Generating new key file: '" + keyFile.getAbsolutePath() + "'.");
                    SecretKey key = _symmetricEncryptionEngine.generateKey();
                    keyBytes = key.getEncoded();
                    FileOutputStream keyOut = new FileOutputStream(keyFile);
                    keyOut.write(keyBytes);
                    keyOut.close();
                } else {
                    log.info("SymmetricEncryptionEngine: Using keyfile '" + keyFile.getAbsolutePath() + "'.");
                    FileInputStream keyIn = new FileInputStream(keyFile);
                    ByteArrayOutputStream keyOut = new ByteArrayOutputStream();
                    WGUtils.inToOut(keyIn, keyOut, 128);
                    keyIn.close();
                    keyBytes = keyOut.toByteArray();
                }                
                _symmetricEncryptionEngine.init(keyBytes);
            }
            catch (Exception e) {              
                log.error("Unable to init symmetric encryption engine.", e);
                throw new ServletException("Unable to init symmetric encryption engine", e);
            }
            
            // get config xml document
            boolean configMigrated = false;
            this.configFile = retrieveConfigFile();

            if (!configFile.exists()) {
                // no new style wga configuration - check if we have to migrate an old one
            	File oldConfigFile = retrieveOldConfigFile();
            	if (oldConfigFile.exists()) {
            		migrateWGAConfiguration(oldConfigFile, configFile);
            		configMigrated = true;
            	} else {
            		// no previous old style config - create default config
            		createDefaultWGAConfiguration(configFile);
            	}
            }

            log.info("Using config file: " + configFile.getAbsolutePath());
            this.configFileLastModified = this.configFile.lastModified();
            parseConfigFile();
            adaptWGAConfigurationToVersion();
            
            initQuartz();
            _deployer.startup();
            _calledSequenceIds = CacheFactory.createCache("WGACore_calledSequenceIds", 10000, null);
            
            String dataPath = System.getProperty(SYSPROPERTY_DATAPATH);
            if (dataPath != null) {
                _wgaDataDir = new File(dataPath);
            }
            else {
                _wgaDataDir = new File(configFile.getParent(), "wgadata");
            }
            
            if (!_wgaDataDir.exists()) {
                if (!_wgaDataDir.mkdir()) {
                    log.error("Unable to create WGA data directory '" + _wgaDataDir.getPath() + "'. Some WGA functionalities that rely on this will not work!");
                    _wgaDataDir = null;
                    
                }
            }
            else if (!_wgaDataDir.isDirectory()) {
                log.error("Unable to create WGA data directory '" + _wgaDataDir.getPath() + "' because some other file uses the same name. Some WGA functionalities that rely on this will not work!");
                _wgaDataDir = null;
            }
            
            String hsqlRoot = System.getProperty(de.innovationgate.webgate.api.hsql.WGDatabaseImpl.SYSPROPERTY_HSQL_ROOT);
            if (WGUtils.isEmpty(hsqlRoot)) {
                log.info("Setting root directory for embedded HSQLDB databases to config dir: " + configFile.getParentFile().getAbsolutePath());
                System.setProperty(de.innovationgate.webgate.api.hsql.WGDatabaseImpl.SYSPROPERTY_HSQL_ROOT, configFile.getParentFile().getAbsolutePath());
            }
            else {
                log.info("Root directory for embedded HSQLDB databases is: " + hsqlRoot);
            }
            
            String authFileRoot = System.getProperty(FileAuthenticationModule.SYSPROPERTY_AUTH_FOLDER);
            if (WGUtils.isEmpty(authFileRoot)) {
                log.info("Setting root directory for XML authentication files to config dir: " + configFile.getParentFile().getAbsolutePath());
                System.setProperty(FileAuthenticationModule.SYSPROPERTY_AUTH_FOLDER, configFile.getParentFile().getAbsolutePath());
            }
            else {
                log.info("Root directory for XML authentication files is: " + authFileRoot);
            }
            
            // Prepare problem registry
            _problemRegistry = new ProblemRegistry(this);
            ProblemOccasion occ = new UpdateConfigOccasion();
            getProblemRegistry().clearProblemOccasion(occ);

            // init login bruteForceLoginBlocker
            bruteForceLoginBlocker = new BruteForceLoginBlocker(this);
            
            // retrieve general configuration
            initReadGeneralConfig(false);

            // Init some managers
            _systemContainerManager = new SystemContainerManager(this);
            _pageConnectionManager = new PageConnectionManager();
            _independentWebSocketManager = new IndependentWebSocketManager();
            _eventManager = new EventManager(this);
            _persManager = new PersonalisationManager(this);
            
            // Retrieve media key and element mappings
            initReadMappings();
            
            // Create expression engines
            ExpressionEngineFactory.createEngines(this);
            
            // Init default serializer
            initDefaultSerializer();
            
            // Initialize custom core listeners
            initCustomCoreListeners();
            
            // Read domain configurations
            Map<String, WGADomain> newDomainConfigs = initReadDomains();
            
            deployErrorPage();
            
            // Init scheduler before db connection, so system containers in dbs can add jobs
            _scheduler = new Scheduler(this);
            
            // Init module registry
            initModuleRegistry();
            
            // Init JMX
            _jmx = new JMX(this);
            
            // fire pre connect event
            fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_STARTUP_PRE_CONNECT, null, this));

            // Create server option readers
            _variousServerOptionReader = getConfigOptionReader(new OptionFetcher() {
                @Override
                public Map<String, String> fetch(WGAConfiguration config) {
                    return config.getServerOptions();
                }
            }, WGAServerOptionsModuleType.class, VariousOptionsCollector.class);
            
            _servicesServerOptionReader = getConfigOptionReader(new OptionFetcher() {
                @Override
                public Map<String, String> fetch(WGAConfiguration config) {
                    return config.getServerOptions();
                }
            }, WGAServerOptionsModuleType.class, ServicesCollector.class);
            
            // connect plugins
            updatePlugins(newDomainConfigs);            
            
            // Log most important available modules
            logCategoryInfo("Modules", 1);
            logModuleRegistry();
            
            // Some tasks that adapt options in registry to those configured in the WGA configuration
            adaptConfigurationToRegistry(configMigrated);
            
                        
            // init cluster service
            initClusterService(null);   
                        
            // init session manager
            initHttpSessionManager(null);
            
            // Init event manager
            _eventManager.reloadConfig();
            
            // Init filter mappings (which may be feeded from registry)
            initReadFilterMappings();
            
            // Init access logger - Must be after modreg so JDBC drivers from mod dependencies are already loaded
            initAccessLogger();
            
            // Init file derivate manager
            this.fileDerivateManager.init(_wgaConfiguration);
            
            // open database servers (must be before domains, to allow pers db connections)
            updateDatabaseServers();
            
            // Startup domain configurations (must be after plugin connection so that domains can use plugin-provided functionalities)
            initStartupDomains(newDomainConfigs);
            
            // Init design manager
            logCategoryInfo("Design Sources", 1);
            _designManager = new WGADesignManager(this, _wgaConfiguration.getDesignConfiguration().getDesignSources());
            
            // Init mail service
            WGFactory.setMailService(new WGAMailService(this));
            
            logCategoryInfo("Workflow engine", 1);
            Class defaultWorkflowEngine = WGFactory.getDefaultWorkflowEngine();
            ModuleDefinition wfDef = getModuleRegistry().getModuleDefinition(WorkflowEngineModuleType.class, defaultWorkflowEngine);
            if (wfDef != null) {
                getLog().info("Default workflow engine is: " + wfDef.getTitle(Locale.ENGLISH));
            }
            else {
                getLog().info("Default workflow engine is: " + defaultWorkflowEngine.getName() + " (custom unregistered engine)");
            }
            
            // open content databases
            updateContentDBs();

            // fire post connect event
            fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_STARTUP_POST_CONNECT, null, this));

            // Load scheduler jobs after db connection, so they can refer the connected dbs
            updateScheduler();

            // Init shares
            updateShares();
            
            this.getServletContext().setAttribute(WGACore.ATTRIB_CONTENTDBS, this.contentdbs);
            
            // init TestCore
            initTestCore();
            
            /*
            // Perform initial daily DB maintenance if in devmode, because devmode servers normally do not run 24 hours
            if ("true".equals(System.getProperty(SYSPROPERTY_DEVELOPMENT_MODE))) {
                performDbMaintenanceForDevmode();
            }
            */

            // notify LuceneManger
            logCategoryInfo("Lucene Fulltext Index", 1);
            if (luceneManager != null) {
                luceneManager.startup();
            }

            initExternalFileServing();
            
            // start external file serving maintenance
            _externalFileMaintenanceTask = new ExternalFileServingMaintenanceTask(this);
            _externalFileMaintenanceTask.start();
            
            // Setup integrated JMX server
            _jmx.setup();
            
            // Start timer tasks
            this.timer = new WGACoreTimer(this);
            
            // Enable daily db backend maintenance if applicable
            if (isRunSingleNodeFunctionalities()) {
                WGFactory.getInstance().setDatabaseBackendMaintenanceEnabled(true);
            }

            // Init finished
            logCategoryInfo(WGAVersion.WGAPUBLISHER_PRODUCT_NAME + " ready", 1);
            WGFactory.getInstance().closeSessions();
            fireCoreEvent(new WGACoreEvent(WGACoreEvent.TYPE_ONLINE, null, this));
            
        }
        catch (Exception exc) {
            log.fatal("Fatal error initializing WGA", exc);
            throw new ServletException("Servlet initialization failure", exc);

        }
        catch (Error err) {
            log.fatal("Fatal error initializing WGA", err);
            throw new ServletException("Servlet initialization failure", err);
        }
        finally {
            WGPDispatcher dispatcher = getDispatcher();
            if (dispatcher != null) {
                dispatcher.setServePages(true);
            }
        }
    }

    public void performDbMaintenanceForDevmode() throws WGAPIException {
        logCategoryInfo("Database maintenance", 1);
        Thread maintenance = new Thread() {

            @Override
            public void run() {

                for (WGDatabase db : getContentdbs().values()) {
                    try {
                        if (db.getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
                            continue;
                        }
                        
                        db.openSession();
                        if (db.isBackendServiceSupported(WGDatabase.BACKENDSERVICE_DAILY_MAINTENANCE)) {
                            db.getSessionContext().setBatchProcess(true);
                            db.setCachingEnabled(true);
                            db.callBackendService(WGDatabase.BACKENDSERVICE_DAILY_MAINTENANCE, null);
                        }
                    }
                    catch (Throwable e) {
                        getLog().error("Exception performing database maintenance on '" + db.getDbReference() + "'", e);
                    }
                    finally {
                        WGFactory.getInstance().closeSessions();
                    }
                }
                
            } 

        };
        maintenance.start();
        try {
            maintenance.join();
        }
        catch (InterruptedException e) {
        }
        
    }

    public void initClusterService(WGAConfiguration oldConfig) {
        if (oldConfig == null || !getWgaConfiguration().getClusterConfiguration().equals(oldConfig.getClusterConfiguration())) {
            logCategoryInfo("Cluster Service", 1);
            if (_clusterService != null) {
                try {
                    getLog().info("Shutting down current cluster service instance to apply config changes.");
                    _clusterService.shutdown();
                    _clusterService = null;
                } catch (Throwable e) {
                    getLog().error("Failed to shutdown cluster service instance '" + _clusterService.getClass().getName() + "'", e);
                }
            }
            
            // choose cluster service
            String implClass = getWgaConfiguration().getClusterConfiguration().getImplClassName();
            if (implClass != null && getWgaConfiguration().getClusterConfiguration().isEnabled()) {           
                try {
                    Class<ClusterService> clusterServiceImplClass = (Class<ClusterService>) getLibraryLoader().loadClass(implClass);
                    Constructor<ClusterService> c = clusterServiceImplClass.getDeclaredConstructor(WGACore.class);                
                    _clusterService = c.newInstance(this);
                } catch (Throwable e) {
                    getLog().error("Failed to create cluster service instance of type '" + implClass + "'", e);
                }           
            }
            
            // nothing configured or impl class not found - fallback to single node cluster
            if (_clusterService == null) {
                _clusterService = new SingleNodeClusterService(this);               
            }                        
            
            try {
                getLog().info("Starting cluster service implementation '" + _clusterService.getClass().getName() + "'");
                _clusterService.startup();
            } catch (Throwable e) {
                getLog().error("Startup of cluster service implementation '" + _clusterService.getClass().getName() + "' failed.", e);
                // fallback to single node cluster if possible
                if (!_clusterService.getClass().equals(SingleNodeClusterService.class)) {
                    getLog().info("Falling back to default implementation '" + SingleNodeClusterService.class.getName() + "'");
                    try {
                        _clusterService = new SingleNodeClusterService(this);
                        _clusterService.startup();
                    }
                    catch (Throwable e1) {
                        getLog().error("Startup of cluster service implementation '" + _clusterService.getClass().getName() + "' failed.", e1);
                    }
                }
            }
        }
    }

    private void initHttpSessionManager(WGAConfiguration oldConfig) {
        if (oldConfig == null || !getWgaConfiguration().getHttpSessionManagerConfiguration().equals(oldConfig.getHttpSessionManagerConfiguration())) {
            logCategoryInfo("HTTP session Manager", 1);
            if (_httpSessionManager != null) {
                try {
                    getLog().info("Shutting down current HTTP session manager instance to apply config changes.");
                    _httpSessionManager.clearListeners();
                    _httpSessionManager.shutdown();
                    _httpSessionManager = null;
                } catch (Throwable e) {
                    getLog().error("Failed to shutdown HTTP session manager instance  '" + _httpSessionManager.getClass().getName() + "'", e);
                    _httpSessionManager = null;
                }
            }            
            
            String implClass = getWgaConfiguration().getHttpSessionManagerConfiguration().getImplClassName();
            if (implClass != null && getWgaConfiguration().getHttpSessionManagerConfiguration().isEnabled()) {           
                try {
                    Class<AbstractWGAHttpSessionManager> managerImplClass = (Class<AbstractWGAHttpSessionManager>) getLibraryLoader().loadClass(implClass);
                    getLog().info("Initializing HTTP session manager '" + managerImplClass.getName() + "'.");
                    _httpSessionManager = managerImplClass.newInstance();   
                    _httpSessionManager.setContext(_context);
                    _httpSessionManager.addListener(new WGAHttpSessionListener());
                    _httpSessionManager.startup(getWgaConfiguration().getHttpSessionManagerConfiguration());
                } catch (Throwable e) {
                    getLog().error("Failed to create HTTP session manager instance of type '" + implClass + "'", e);
                    _httpSessionManager = null;
                }           
            }
            
            if (_httpSessionManager == null) {
                getLog().info("Using HTTP session management provided by servlet container");
            }
        }
        
    }
    
    private void initDefaultSerializer() {
        
        Dom4JDriver driver = new Dom4JDriver();
        OutputFormat format =OutputFormat.createCompactFormat();
        format.setSuppressDeclaration(true);
        driver.setOutputFormat(format);
        
        _defaultSerializer = new XStream(driver);
        _defaultSerializer.setClassLoader(getLibraryLoader());
        _defaultSerializer.alias("tmloption", TMLOption.class);
        _defaultSerializer.alias("version", Version.class);
        _defaultSerializer.alias("portletState", TMLPortletState.class);
        _defaultSerializer.registerConverter(ExpressionEngineFactory.getTMLScriptEngine());
        
        // DOM4J Objects
        _defaultSerializer.registerConverter(new SingleValueConverter() {

            public boolean canConvert(Class arg0) {
                return org.dom4j.Element.class.isAssignableFrom(arg0);
            }

            public Object fromString(String arg0) {
                try {
                    org.dom4j.Document doc = DocumentHelper.parseText(arg0);
                    return doc.getRootElement();
                }
                catch (DocumentException e) {
                    Logger.getLogger("wga.ajax").error("Exception deserializing DOM4J Element from TMLPortlet.SessionContext", e);
                    return "";
                }
            }

            public String toString(Object arg0) {

                Element elem = (Element) arg0;
                return elem.asXML();
                
            }
            
        });
        
        // Serialize TMLContext to their context path. All other info is dropped. Deserialize to a TMLContext.Serialized.
        _defaultSerializer.registerConverter(new SingleValueConverter() {
            
            @SuppressWarnings("rawtypes")
            @Override
            public boolean canConvert(Class arg0) {
                return TMLContext.class.isAssignableFrom(arg0);
            }
            
            @Override
            public String toString(Object arg0) {
                try {
                    TMLContext cx = (TMLContext) arg0;
                    return cx.getpath();
                }
                catch (WGAPIException e) {
                    throw new RuntimeException("Exception serializing TMLContext", e);
                }
            }
            
            @Override
            public Object fromString(String arg0) {
                return new TMLContext.Serialized(arg0);
            }
            
        });
        
        // Version object
        _defaultSerializer.registerConverter(new SingleValueConverter() {

            public boolean canConvert(Class arg0) {
                return Version.class.isAssignableFrom(arg0);
            }

            public Object fromString(String arg0) {
                return new Version(arg0);
            }

            public String toString(Object arg0) {

                return ((Version) arg0).toString();
                
            }
            
        });
        
        // GSON data structures
        _defaultSerializer.registerConverter(new SingleValueConverter() {
            
            private Gson _gson = new GsonBuilder().create();

            @Override
            public boolean canConvert(Class arg0) {
                return JsonElement.class.isAssignableFrom(arg0);
            }

            @Override
            public Object fromString(String arg0) {
                return new JsonParser().parse(arg0);
            }

            @Override
            public String toString(Object arg0) {
                return _gson.toJson((JsonElement) arg0);
            }
            
        });
        
        // Serialize types not meant to be serialized to empty string, deserialize to null
        _defaultSerializer.registerConverter(new SingleValueConverter() {
            
            public boolean canConvert(@SuppressWarnings("rawtypes") Class clazz) {
                return DEFAULT_SERIALIZER_NONSERIALIZABLE_TYPES.contains(clazz);
            }

            public Object fromString(String arg0) {
                return null;
            }

            public String toString(Object arg0) {
                return "";
            }
        });

    }

    /**
     * Do necessary updates to configuration when updating from an earlier version
     * @throws Exception 
     * @throws FileNotFoundException 
     */
    private void adaptWGAConfigurationToVersion() throws FileNotFoundException, Exception {

        Version configVersion = new Version(6,0,0);
        if (_wgaConfiguration.getWgaVersion() != null) {
            configVersion = new Version(_wgaConfiguration.getWgaVersion());
        }
        
        Version wgaVersion = WGAVersion.toCsConfigVersion();
        if (configVersion.equals(wgaVersion)) {
            return;
        }
        
        
        String hashingServiceOption = WGAConfiguration.SERVEROPTION_SERVICE_APIS_PREFIX + HashingSchemeType.class.getName();
        if (!configVersion.isAtLeast(6,1)) {
            // Set hashing scheme for existing installations to SHA-1
            if (_wgaConfiguration.getServerOptions().get(hashingServiceOption) == null) {
                _wgaConfiguration.getServerOptions().put(hashingServiceOption, SHA1HashingScheme.class.getName());
            }
        }
        
        _wgaConfiguration.setWgaVersion(wgaVersion.toString());
        
    }

    private void registerMandatoryJobs(List<String> currentJobs) {
        
        if (isRunSingleNodeFunctionalities()) {
            JobSchedule pendingReleaseSchedule = new JobSchedule();
            pendingReleaseSchedule.setEnabled(true);
            pendingReleaseSchedule.setType(JobSchedule.TYPE_CRON);
            pendingReleaseSchedule.setScheduleData("0 0/1 * * * ?");
            try {
                Task task = new PendingReleaseTask();
                Job job = _scheduler.addCustomTaskJob(JOBNAME_PUBLISH_PENDING_RELEASE, task, false, pendingReleaseSchedule);
                job.setQuiet(true);
                job.setDescription("Publishes documents that passed workflow and are now are pending release");
                job.setOrigin(Job.ORIGIN_WGACONFIG);
                currentJobs.add(job.getName());
            }
            catch (Exception e) {
                getLog().error("Unable to add pending release publishing task to wga scheduler." , e);
            }
        }
        
        if (getWgaConfiguration().getLuceneManagerConfiguration().isOptimizeIndexAutomatically()) {
            JobSchedule optimizeIndexSchedule = new JobSchedule();
            optimizeIndexSchedule.setEnabled(true);
            optimizeIndexSchedule.setType(JobSchedule.TYPE_CRON);
            optimizeIndexSchedule.setScheduleData("0 30 0 * * ?");
            try {
                Task task = new OptimizeLuceneIndexTask();
                
                Job job = _scheduler.addCustomTaskJob(JOBNAME_OPTIMIZE_LUCENE_INDEX, task, false, optimizeIndexSchedule);
                job.setQuiet(true);
                job.setDescription("Optimizes the lucene index for size and performance");
                job.setOrigin(Job.ORIGIN_WGACONFIG);
                currentJobs.add(job.getName());
            }
            catch (Exception e) {
                getLog().error("Unable to add pending release publishing task to wga scheduler." , e);
            }
        }
        else {
            Job job = _scheduler.getJob(JOBNAME_OPTIMIZE_LUCENE_INDEX);
            if (job != null) {
                _scheduler.removeJob(JOBNAME_OPTIMIZE_LUCENE_INDEX);
            }
        }
        
        
    }

    private void adaptConfigurationToRegistry(boolean configMigrated) throws Exception, FileNotFoundException {
        
        boolean changed = false;
        
        // When migrated: remove default options from config (must be here so complete registry is available)
        if (configMigrated) {
            logCategoryInfo("Removing default options from migrated WGA configuration", 1);
            getWgaConfiguration().removeDefaultOptions(getModuleRegistry());
            changed = true;
        }
        
        // Create mandatory server options with full registry
        if (createMandatoryServerOptions()) {
            getLog().info("Creating mandatory server options in WGA configuration");
            changed = true;
        }
        
        if (changed == true) {
            getLog().info("Saving automatic adaptions to the WGA configuration");
            WGAConfiguration.write(getWgaConfiguration(), new FileOutputStream(configFile));
        }
            
    }

    private boolean createMandatoryServerOptions() {
        
        boolean somethingChanged = false;
        Map<String,String> serverOptions = getWgaConfiguration().getServerOptions();
        
        for (ModuleDefinition modDef : getModuleRegistry().getModulesForType(WGAServerOptionsModuleType.class).values()) {
            for (OptionDefinition optDef : modDef.getOptionDefinitions().values()) {
                if (!optDef.isOptional() && optDef.getDefaultValue() != null) {
                    if (!serverOptions.containsKey(optDef.getName())) {
                        serverOptions.put(optDef.getName(), optDef.getDefaultValue());
                        somethingChanged = true;
                    }
                }
            }
        }
        
        return somethingChanged;
        
    }

    public static String retrieveConfigPath() throws IOException {
        String configFilePath = System.getProperty(WGACore.SYSPROPERTY_CONFIGPATH);
        if (WGUtils.isEmpty(configFilePath)) {
            String userHome = System.getProperty("user.home");
            File defaultConfigDir = new File(userHome, DEFAULT_CONFIG_DIR);
            if (!defaultConfigDir.exists()) {
                if (!defaultConfigDir.mkdirs()) {
                    throw new IOException("Unable to create default config directory " + defaultConfigDir.getPath());
                }
            }
            
            configFilePath = defaultConfigDir.getPath();
        }
        return configFilePath;
    }

    private void initAccessLogger() {
        
        // Close old access logger if present
        if (_accessLogger != null) {
            _accessLogger.close();
            _accessLogger = null;
        }
    	
        if (_wgaConfiguration.getAccessLog() != null) {
            logCategoryInfo("Access logger", 1);
            try {
                _accessLogger = new WGALoggerWrapper(_wgaConfiguration.getAccessLog(), this);
            }
            catch (Exception e) {
                this.log.error("Exception instantiating access logger", e);
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), AccessLoggingScope.INSTANCE, "updateConfigProblem.accessLoggerException", ProblemSeverity.LOW, e));
                _accessLogger = null;
            }   
        }
        
    }

    
    private synchronized void updateDatabaseServers() {
        
        logCategoryInfo("Database servers", 1);

        Map<String, WGDatabaseServer> newServers = new ConcurrentHashMap<String, WGDatabaseServer>();
        
        // Add singleton servers
        Iterator<ModuleDefinition> singletons = getModuleRegistry().getModulesForType(DatabaseServerModuleType.class).values().iterator();
        while (singletons.hasNext()) {
            ModuleDefinition serverDefinition = singletons.next();
            DatabaseServerProperties properties = (DatabaseServerProperties) serverDefinition.getProperties();
            if (properties == null) {
                   getLog().error("Database server type '" + serverDefinition.getTitle(Locale.getDefault()) + "' (" + serverDefinition.getImplementationClass().getName() + ") is invalid, misses neccessary properties definition");
            }
            else if (properties.isSingleton()) {
                try {
                    serverDefinition.testDependencies();
                    WGDatabaseServer server = (WGDatabaseServer) getModuleRegistry().instantiate(serverDefinition);
                    server.init(WGAConfiguration.SINGLETON_SERVER_PREFIX + server.getClass().getName(), null, new HashMap<String,String>());
                    newServers.put(server.getUid(), server);
                    getLog().info("Registering database server '" + server.getTitle(Locale.getDefault()) + "' (Automatically created)");
                }
                catch (ModuleDependencyException e) {
                    getLog().error("Database server " + serverDefinition.getTitle(Locale.getDefault()) + " deactivated in current WGA runtime: " + e.getMessage());
                    getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new DBServerScope(properties), "updateConfigProblem.serverMissingDependencies", ProblemSeverity.HIGH, Problem.var("msg", e.getMessage()), e));
                }
                catch (Exception e) {
                    getLog().error("Exception instantiating database server", e);
                    getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new DBServerScope(properties), "updateConfigProblem.serverException", ProblemSeverity.HIGH, e));
                }
            }
            
        }
        
        // Add servers from configuration
        Iterator<DatabaseServer> servers = getWgaConfiguration().getDatabaseServers().iterator();
        while (servers.hasNext()) {
            DatabaseServer databaseServer = servers.next();
            if (databaseServer.isEnabled()) {
                try {
                    Class serverClass = getLibraryLoader().loadClass(databaseServer.getImplClassName());
                    ModuleDefinition serverDefinition = getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, serverClass);
                    if (serverDefinition != null) {
                        serverDefinition.testDependencies();
                    }
                    WGDatabaseServer server = (WGDatabaseServer) getModuleRegistry().instantiate(serverClass);
                    
                    Map<String,String> serverOptions = new HashMap<>();
                    putDefaultDbServerOptions(serverOptions);
                    serverOptions.putAll(databaseServer.getOptions());
                    server.init(databaseServer.getUid(), databaseServer.getTitle(), serverOptions);
                    newServers.put(server.getUid(), server);
                    getLog().info("Registering database server '" + server.getTitle(Locale.getDefault()) + "'");
                }
                catch (ModuleDependencyException e) {
                    getLog().error("Database server " + databaseServer.getTitle() + " deactivated in current WGA runtime: " + e.getMessage());
                    getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new DBServerScope(databaseServer), "updateConfigProblem.serverMissingDependencies", ProblemSeverity.HIGH, Problem.var("msg", e.getMessage()), e));
                }
                catch (Exception e) {
                    getLog().error("Exception instantiating database server", e);
                    getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new DBServerScope(databaseServer), "updateConfigProblem.serverException", ProblemSeverity.HIGH, e));
                }
            }
        }
        
        // Import state from previous servers
        for (WGDatabaseServer newServer : newServers.values()) {
            WGDatabaseServer oldServer = _databaseServers.get(newServer.getUid());
            if (oldServer != null) {
                newServer.importState(oldServer);
            }
        }
        
        _databaseServers = newServers;
        
        
    }
    
    private void putDefaultDbServerOptions(Map<String, String> serverOptions) {
        serverOptions.put(DatabaseServer.OPTION_SHAREDPOOL_LEGACY_DBCP_MONITORING, String.valueOf("true".equals(_wgaConfiguration.getServerOptions().get(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_LEGACY_DBCP))));
    }

    private Map<String,WGDatabaseServer> _databaseServers = new ConcurrentHashMap<String, WGDatabaseServer>();

    private List<HTMLHeadInclusion> _htmlHeadInclusions = new ArrayList<HTMLHeadInclusion>();



    private WGAPublisherOptionReader _publisherOptionsReader;



    private WGAServerOptionReader _serverOptionsReader;

    private void initModuleRegistry() {

        // Initialize module registry and retrieve definitions
        _moduleRegistry = new WGAModuleRegistry();
        WGFactory.setModuleRegistry(_moduleRegistry);
        _moduleRegistry.getContextObjects().put(WGACore.class, this);
        _moduleRegistry.getContextObjects().put(WGA.class, WGA.get(this));
        
        // Register mandatory registry change listeners
        _publisherOptionsReader = new WGAPublisherOptionReader();
        _moduleRegistry.addChangeListener(_publisherOptionsReader, ContentStorePublisherOptionsCollector.class);
        _moduleRegistry.addChangeListener(_publisherOptionsReader, ContentDatabasePublisherOptionsCollector.class);
        _moduleRegistry.addChangeListener(_publisherOptionsReader, ContentStorePublisherOptionsModuleType.class);
        _moduleRegistry.addChangeListener(_publisherOptionsReader, ContentDatabasePublisherOptionsModuleType.class);
        
        _serverOptionsReader = new WGAServerOptionReader();
        _moduleRegistry.addChangeListener(_serverOptionsReader, WGAServerOptionsModuleType.class);

        HTMLHeadInclusionUpdater inclusionUpdater = new HTMLHeadInclusionUpdater();
        _moduleRegistry.addChangeListener(inclusionUpdater, HTMLHeadInclusionModuleType.class);
        
        ServletFilterUpdater filterUpdater = new ServletFilterUpdater();
        _moduleRegistry.addChangeListener(filterUpdater, FilterConfigModuleType.class);
        _moduleRegistry.addChangeListener(filterUpdater, WGAWebServiceModuleType.class);
        
        FileAnnotatorUpdater fileAnnotatorUpdater = new FileAnnotatorUpdater();
        _moduleRegistry.addChangeListener(fileAnnotatorUpdater, FileAnnotatorModuleType.class);
        
        
        // Do initial module definition search
        try {
            _moduleRegistry.searchModuleDefinitions(this);
        }
        catch (IOException e) {
            getLog().error("Exception searching module definitions", e);
        }

        initPasswordEncoding();
        
    }
    
    private void initPasswordEncoding(){
        PasswordOptionEncoder encoder;
        String passwordEncoderKey = _wgaConfiguration.getPasswordEncoding();
    	getLog().info("Setup Password Encoding: " + passwordEncoderKey);
        try {
            ModuleDefinition passwordEncoderModDef = _moduleRegistry.getModuleDefinitionByKey(PasswordEncodingType.class, passwordEncoderKey);
            if (passwordEncoderModDef != null) {
                Class passwordEncoderClass = passwordEncoderModDef.getImplementationClass();
                encoder = (PasswordOptionEncoder) passwordEncoderClass.newInstance();
            }
            else {
                getLog().error("Unknown password encoder key " + passwordEncoderKey);
                getLog().error("Falling back to Base64 encoder for encoding new passwords");
                encoder = new Base64();
            }
        }
        catch (Throwable e1) {
            getLog().error("Exception creating password encoder " + passwordEncoderKey, e1);
            getLog().error("Falling back to Base64 encoder for encoding new passwords");
            encoder = new Base64();
        }
        _moduleRegistry.getContextObjects().put(PasswordEncodingType.class, encoder);    	
    }
    
    private void logModuleRegistry() {
        
     // Log the most important registered modules to applog
        HashMap<ModuleDefinition, Throwable> dependencyFailures = new HashMap<ModuleDefinition, Throwable>();
        
        Iterator<ModuleDefinition> serverMods =  _moduleRegistry.getModulesForType(DatabaseServerModuleType.class).values().iterator();
        if (serverMods.hasNext()) {
            logCategoryInfo("Database server types available in this runtime", 2);
            while (serverMods.hasNext()) {
                ModuleDefinition moduleDefinition = serverMods.next();
                try {
                    moduleDefinition.testDependencies();
                    DatabaseServerProperties props = (DatabaseServerProperties) moduleDefinition.getProperties();
                    if (props != null && props.isSingleton()) {
                        getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH) + " (Automatic instance)");
                    }
                    else {
                        getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                    }
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }

        Iterator<ModuleDefinition> dbMods =  _moduleRegistry.getModulesForType(ContentStoreModuleType.class).values().iterator();
        if (dbMods.hasNext()) {
            logCategoryInfo("Content store types available in this runtime", 2);
            while (dbMods.hasNext()) {
                ModuleDefinition moduleDefinition = dbMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> customdbMods =  _moduleRegistry.getModulesForType(ContentDatabaseModuleType.class).values().iterator();
        if (customdbMods.hasNext()) {
            logCategoryInfo("Other content database types available in this runtime", 2);
            while (customdbMods.hasNext()) {
                ModuleDefinition moduleDefinition = customdbMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> persdbMods =  _moduleRegistry.getModulesForType(PersonalisationDatabaseModuleType.class).values().iterator();
        if (persdbMods.hasNext()) {
            logCategoryInfo("Personalisation database types available in this runtime", 2);
            while (persdbMods.hasNext()) {
                ModuleDefinition moduleDefinition = persdbMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        
        Iterator<ModuleDefinition> authMods =  _moduleRegistry.getModulesForType(AuthenticationSourceModuleType.class).values().iterator();
        if (authMods.hasNext()) {
            logCategoryInfo("Authentication types available in this runtime", 2);
            while (authMods.hasNext()) {
                ModuleDefinition moduleDefinition = authMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> wfMods =  _moduleRegistry.getModulesForType(WorkflowEngineModuleType.class).values().iterator();
        if (wfMods.hasNext()) {
            logCategoryInfo("Workflow engine types available in this runtime", 2);
            while (wfMods.hasNext()) {
                ModuleDefinition moduleDefinition = wfMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> designMods =  _moduleRegistry.getModulesForType(DesignSourceModuleType.class).values().iterator();
        if (designMods.hasNext()) {
            logCategoryInfo("Design source types available in this runtime", 2);
            while (designMods.hasNext()) {
                ModuleDefinition moduleDefinition = designMods.next();
                try {
                    moduleDefinition.testDependencies();
                    DesignSourceProperties props = (DesignSourceProperties) moduleDefinition.getProperties();
                    if (props != null && props.isSingleton()) {
                        getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH) + " (Automatic instance)");
                    }
                    else {
                        getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                    }
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> contentShareMods =  _moduleRegistry.getModulesForType(ShareModuleType.class).values().iterator();
        if (contentShareMods.hasNext()) {
            logCategoryInfo("Share types available in this runtime", 2);
            while (contentShareMods.hasNext()) {
                ModuleDefinition moduleDefinition = contentShareMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> taskMods =  _moduleRegistry.getModulesForType(SchedulerTaskModuleType.class).values().iterator();
        if (taskMods.hasNext()) {
            logCategoryInfo("Scheduler tasks available in this runtime", 2);
            while (taskMods.hasNext()) {
                ModuleDefinition moduleDefinition = taskMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> filterMods =  _moduleRegistry.getModulesForType(FilterConfigModuleType.class).values().iterator();
        if (filterMods.hasNext()) {
            logCategoryInfo("Request filters available in this runtime", 2);
            while (filterMods.hasNext()) {
                ModuleDefinition moduleDefinition = filterMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<ModuleDefinition> clusterMods =  _moduleRegistry.getModulesForType(ClusterServiceModuleType.class).values().iterator();
        if (clusterMods.hasNext()) {
            logCategoryInfo("Cluster service implementations available in this runtime", 2);
            while (clusterMods.hasNext()) {
                ModuleDefinition moduleDefinition = clusterMods.next();
                try {
                    moduleDefinition.testDependencies();
                    getLog().info("- " + moduleDefinition.getTitle(Locale.ENGLISH));
                }
                catch (Throwable e) {
                    dependencyFailures.put(moduleDefinition, e);
                }
            }
        }
        
        Iterator<Map.Entry<ModuleDefinition,Throwable>> failureMods =  dependencyFailures.entrySet().iterator();
        if (failureMods.hasNext()) {
            logCategoryInfo("Modules not available to this runtime because of missing dependencies", 2);
            while (failureMods.hasNext()) {
                Map.Entry<ModuleDefinition,Throwable> failure = failureMods.next();
                try {
                    getLog().info("- " + failure.getKey().getTitle(Locale.ENGLISH) + ": " + failure.getValue().getMessage());
                }
                catch (Throwable e) {
                    getLog().error("- " + failure.getKey().getImplementationClass() + ": Exception while logging dependency failure", e);
                }
                
            }
        }
        
        
    }

    private void createDefaultWGAConfiguration(File file) throws Exception {
    	log.info("Creating default wga configuration in file '" + file.getAbsolutePath() + "'.");
    	WGAConfiguration defaultConfig = WGAConfiguration.createDefaultConfig(WGAVersion.toCsConfigVersion());
    	WGAConfiguration.write(defaultConfig, new FileOutputStream(file));		
	}

	private void migrateWGAConfiguration(File oldConfigFile, File newConfigFile) throws Exception {
    	MigrationResult migrationResult = WGAConfigurationMigrator.createFromWGAXML(new FileInputStream(oldConfigFile), oldConfigFile.getParentFile().getAbsolutePath());
    	Iterator<MigrationMessage> logEntries = migrationResult.getLog().iterator();
    	File migrationLogFile = new File(newConfigFile.getParentFile(), "migration.log");
    	BufferedWriter writer = new BufferedWriter(new FileWriter(migrationLogFile));
    	while (logEntries.hasNext()) {
    		MigrationMessage message = logEntries.next();
    		if (message.getLevel() == MigrationMessage.INFO) {
    			log.info(message.getMessage());	
    			writer.write("INFO - " + message.getMessage() + "\n");
    			writer.write("\n");
    		} else if (message.getLevel() == MigrationMessage.WARNING) {
    			if (message.getThrowable() != null) {
    				log.warn(message.getMessage(), message.getThrowable());
    				writer.write("WARN - " + message.getMessage() + "\n");
    				message.getThrowable().printStackTrace(new PrintWriter(writer));
    			} else {
    				log.warn(message.getMessage());
    				writer.write("WARN - " + message.getMessage() + "\n");
    				writer.write("\n");
    			} 
    		} else if (message.getLevel() == MigrationMessage.ERROR) {
    			if (message.getThrowable() != null) {
    				log.error(message.getMessage(), message.getThrowable());
    				writer.write("ERROR - " + message.getMessage() + "\n");
    				message.getThrowable().printStackTrace(new PrintWriter(writer));
    				writer.write("\n");
    			} else {
    				log.error(message.getMessage());
    				writer.write("ERROR - " + message.getMessage() + "\n");
    			}
    		}    		
    	}
    	writer.close();
        try {
            WGAConfiguration.write(migrationResult.getConfig(), new FileOutputStream(newConfigFile));
        }
        catch (Exception e) {
            newConfigFile.delete();
            throw e;
        }		
	}

	private void initCustomCoreListeners() {

	    // From configuration
        customCoreListeners = new ArrayList<Object>();
        Iterator classesIt = customCoreListenerClassnames.iterator();
        while (classesIt.hasNext()) {
            String clazzName = (String) classesIt.next();
            try {
                Class clazz = getLibraryLoader().loadClass(clazzName);
                Object listenerObj = clazz.newInstance();
                if (!(listenerObj instanceof WGACoreEventListener)) {
                    getLog().error("Event listener '" + clazzName + "' is no implementation of " + WGACoreEventListener.class.getName());
                    continue;
                }

                addEventListener((WGACoreEventListener) listenerObj);
                customCoreListeners.add(listenerObj);
            }
            catch (ClassNotFoundException e) {
                getLog().error("Cannot load event listener '" + clazzName + "' because class could not be found");
            }
            catch (InstantiationException e) {
                getLog().error("Error instantiating event listener '" + clazzName + "'", e);
            }
            catch (IllegalAccessException e) {
                getLog().error("Access error instantiating event listener '" + clazzName + "'", e);
            }
        }

    }

    private void initTestCore() throws JAXBException {

        // if not set yet create TestCore and disable
        if (_testCore == null) {
            boolean testsEnabled = Boolean.parseBoolean(System.getProperty(SYSPROPERTY_UNITTEST));
            String logDir = System.getProperty(SYSPROPERTY_UNITTEST_LOGDIR, null);
            if (logDir != null) {
            	_testCore = new TestCore(this, testsEnabled, logDir);
            } else {
            	_testCore = new TestCore(this, testsEnabled);
            }
        }

    }

    private void initQuartz() {

        try {
            int threadCount = 5;
            String threadCountConfig = System.getProperty(SYSPROPERTY_QUARTZ_THREADCOUNT);
            if (threadCountConfig != null) {
                try {
                    threadCount = Integer.parseInt(threadCountConfig);
                }
                catch (NumberFormatException e) {
                    getLog().error("Unable to parse " + SYSPROPERTY_QUARTZ_THREADCOUNT + " as integer. Using default of " + threadCount);
                }
            }

            int threadPriority = Thread.MIN_PRIORITY;
            String threadPrioConfig = System.getProperty(SYSPROPERTY_QUARTZ_THREADPRIORITY);
            if (threadPrioConfig != null) {
                try {
                    threadPriority = Integer.parseInt(threadPrioConfig);
                }
                catch (NumberFormatException e) {
                    getLog().error("Unable to parse " + SYSPROPERTY_QUARTZ_THREADPRIORITY + " as integer. Using default of " + threadPriority);
                }
            }
            ThreadPool threadPool = new SimpleThreadPool(threadCount, threadPriority);
            threadPool.initialize();
            JobStore jobStore = new RAMJobStore();
            DirectSchedulerFactory.getInstance().createScheduler(threadPool, jobStore);
            _quartzScheduler = DirectSchedulerFactory.getInstance().getScheduler();
            _quartzScheduler.start();
        }
        catch (SchedulerException e) {
            getLog().fatal("Unable to start wga scheduler. Some background tasks may not run!", e);
        }

    }

    private Map<String, WGADomain> initReadDomains() {
        
        logCategoryInfo("Domains configuration", 1);
        
        // Map new configs
        Map<String, WGADomain> newDomainConfigs = new HashMap<String, WGADomain>();
        Iterator<Domain> domains = _wgaConfiguration.getDomains().iterator();
        while (domains.hasNext()) {
            Domain domain = domains.next();
            WGADomain domainConfig = new WGADomain(this, domain);
            
            // TODO this check should be done in configuration
            if (domainConfig.getName().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                this.log.error("Domain name '" + domainConfig.getName() + "' invalid. The prefix '" + PluginConfig.PLUGIN_DBKEY_PREFIX + "' is reserved for WGA plugin domains.");
                _problemRegistry.addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.invalidDomainName", ProblemSeverity.HIGH, Problem.var("name", domainConfig.getName())));
                continue;
            }
            
            newDomainConfigs.put(domainConfig.getName(), domainConfig);
            this.log.info("Reading configuration of domain '" + domainConfig.toString() + "'");
            
        }
        
        return newDomainConfigs;
    }
    
    private void initStartupDomains(Map<String, WGADomain> newDomainConfigs)  {

        logCategoryInfo("Domains startup", 1);
        
        // Initialize new domain configurations (their auth modules, that is)
        Iterator<WGADomain> doms = newDomainConfigs.values().iterator();
        while (doms.hasNext()) {
            WGADomain domain = doms.next();
            logCategoryInfo("Domain " + domain.getName(), 2);
            try {
                domain.init();
            }
            catch (Exception e) {
                getLog().error("Exception initializing domain " + domain.getName(), e);
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new DomainScope(domain.getName()), "updateConfigProblem.domainException", ProblemSeverity.HIGH, e));
            }
        }
        
        // Replace old domain configs with new domain configs
        Map<String, WGADomain> oldDomainConfigs = this.domains;
        this.domains = newDomainConfigs;
        closeDomainConfigs(oldDomainConfigs);
    }


    private void closeDomainConfigs(Map<String, WGADomain> oldDomainConfigs) {
        
        if (oldDomainConfigs == null) {
            return;
        }
        
        for (WGADomain cfg : oldDomainConfigs.values()) {
            try {
                cfg.destroy();
            }
            catch (Throwable e) {
                getLog().error("Exception closing domain " + cfg.getName(), e);
            }
        }
        
        
    }

    private void updateScheduler() {

        logCategoryInfo("Scheduler", 1);
        
        try {
            List<String> currentJobs = new ArrayList<String>();
            
            registerMandatoryJobs(currentJobs);            
            
            try {
                String loggingDirStr = _wgaConfiguration.getSchedulerConfiguration().getLoggingDir();
                if (loggingDirStr != null) {
                    _scheduler.setLoggingDir(new File(loggingDirStr));
                }
                else {
                    _scheduler.setLoggingDir(new File(getConfigFile().getParentFile(), Scheduler.LOGGINGDIR_DEFAULT));
                }
            }
            catch (IllegalArgumentException e) {
                getLog().error("Configuration error initializing directory for permanent job logs", e);
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), GlobalScope.INSTANCE, "updateConfigProblem.schedulerLoggingDirError", ProblemSeverity.HIGH, e));
            }

            // Register all currently configured jobs
            Iterator<de.innovationgate.wga.config.Job> jobs = _wgaConfiguration.getSchedulerConfiguration().getJobs().iterator();
            
            while (jobs.hasNext()) {
            	de.innovationgate.wga.config.Job job = jobs.next();
                String jobName = job.getName();
                try {
                    _scheduler.addJob(job);
                    currentJobs.add(jobName);
                }
                catch (ConfigurationException e) {
                    getLog().error("Configuration error initializing job '" + jobName + "': " + e.getMessage(), e);
                    getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new JobScope(job.getName()), "updateConfigProblem.jobConfigError", ProblemSeverity.LOW, Problem.var("msg", e.getMessage()), e));
                }
                catch (Throwable e) {
                    getLog().error("Exception initializing job '" + jobName + "': " + e.getMessage(), e);
                    getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), new JobScope(job.getName()), "updateConfigProblem.jobInitError", ProblemSeverity.LOW, e));
                }
            }

            // Remove all wgaconfig jobs, no more in configuration
            List jobsToRemove = new ArrayList(_scheduler.getJobNames());
            jobsToRemove.removeAll(currentJobs);
            Iterator removeIt = jobsToRemove.iterator();
            while (removeIt.hasNext()) {
                String jobName = (String) removeIt.next();
                Job job = _scheduler.getJob(jobName);
                if (job != null && job.getOrigin() == Job.ORIGIN_WGACONFIG) {
                    _scheduler.removeJob(jobName);
                }
            }
        }
        catch (RuntimeException e) {
            getLog().error("Failed to update WGA jobs", e);
            getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.jobException", ProblemSeverity.HIGH, e));
        }

    }


    private void parseConfigFile() throws Exception {
    	try {
    		logCategoryInfo("Loading WGA Configuration", 1);
    		_wgaConfiguration = WGAConfiguration.read(new FileInputStream(configFile));
    		List<ValidationError> errors = _wgaConfiguration.validate();
    		if (!errors.isEmpty()) {
    			log.error("WGA configuration contains validation errors:");
    			for (ValidationError error : errors) {
    				log.error("- " + error.getMessage());
    			}
    		}    		
    	} catch (ConfigValidationException e) {
        	log.fatal("WGA configuration integrity check fails:");
        	Iterator<ValidationError> errors = e.getValidationErrors().iterator();
        	while (errors.hasNext()) {
        		ValidationError error = errors.next();
        		log.fatal("- " + error.getMessage());
        	}
        	throw e;
        }    	
    }

    /**
     * retrieves the old wga config file "wga.xml"
     * @return
     */
    private File retrieveOldConfigFile() {
        File configFile = null;
        String configFilePath = System.getProperty(WGACore.SYSPROPERTY_CONFIGPATH);

        if (configFilePath == null) {
            configFilePath = System.getProperty("user.home");
        }

        return new File(configFilePath, WGACore.OLD_CONFIGFILE_NAME);
    }
    
    /**
     * retrieve the new wga config file "wgaconfig.xml"
     * @return
     * @throws ServletException 
     */
    private File retrieveConfigFile() throws IOException {
        return  new File(retrieveConfigPath(), WGACore.CONFIGFILE_NAME);
    }
    
    
    // Maps of objects
    private Map<String,WGDatabase> contentdbs = new ConcurrentHashMap<String, WGDatabase>();
    private Map<String,String> _contentdbUuidsToKeys = new ConcurrentHashMap<String, String>();

    Map<String,WGDatabase> personalisationdbs = new ConcurrentHashMap<String,WGDatabase>();

    private Map<String,MediaKey> systemMediaKeys = new ConcurrentHashMap<String, MediaKey>();
    private Map<String, MediaKey> customMediaKeys = new ConcurrentHashMap<String, MediaKey>();

    private Map<String, String> systemElements = new ConcurrentHashMap<String, String>();
    private Map<String, String> customElements = new ConcurrentHashMap<String, String>();

    

    private File configFile;

    private long configFileLastModified;



    private WGAPluginSet pluginSet;

    private TMLScriptGlobalRegistry _tmlscriptGlobalRegistry = new TMLScriptGlobalRegistry(this);
    
    private ScopeObjectRegistry _serverScopeObjectRegistry = new ScopeObjectRegistry(ObjectScope.SERVER, "Server", new NoopScopeObjectContextCreator());

    private int _status;
    
    protected WGAConfigurationOptionReader _variousServerOptionReader;
    
    public WGAConfigurationOptionReader getVariousServerOptionReader() {
        return _variousServerOptionReader;
    }

    private WGAFilter _filter;

	private String _errorPage;



    private AbstractWGAHttpSessionManager _httpSessionManager;
    
    private PageConnectionManager _pageConnectionManager = null;
    
    
    public PageConnectionManager getPageConnectionManager() {
        return _pageConnectionManager;
    }
    
    private IndependentWebSocketManager _independentWebSocketManager = null;

    public IndependentWebSocketManager getIndependentWebSocketManager() {
        return _independentWebSocketManager;
    }

    public static final String ATTRIB_IS_AJAX_SUBFORM_OF = "de.innovationgate.request.attrib.isAjaxSubFormOf";

    public static final String DBATTRIB_INCIDENTS_APP = "IncidentsApp";

    public static final String DEFAULT_FILE_ENCODING = "ISO-8859-1";
    
    public static final int DEFAULT_QUERY_MAXRESULTS = 500;

    public static final String ATTRIB_BI_VERSION4 = "BrowserInterfaceVersion4";

    public static final java.text.DateFormat DATEFORMAT_LOCAL = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z");

    public static final String SESSION_PORTLETMODES = "portletmodes";

    public static final String DBATTRIB_SESSIONCOOKIEDOMAIN = "SessionCookieDomain";

    public static final String ATTRIB_TMLDEBUG_TRACE_OPTIONS = "tmlDebugTraceOptions";

    public static final String DBATTRIB_CSCONFIG = "CSConfig";

    public static final String SESSION_FIREDPORTLETEVENTS = "firedPortletEvents";
    
    public static final String DBATTRIB_PLUGIN_FILETIME = "PluginFileTime";

    public static final String DBATTRIB_PLUGIN_VERSION = "PluginVersion";

    public static final Object WGASERVICES_WSDL_URL = "wgaservices.wsdl";

    public static final String ATTRIB_URLBUILDER = "URLBuilder";

    public static final String DBATTRIB_TITLEPATHURL_SHORTCUTAREA = "TitlePathURL.ShortcutArea";

    public static final String DBATTRIB_ALLOW_PUBLISHING = "AllowPublishing";



    public static final String DBATTRIB_SHARE_ALLOWALLCHARS = "Share.AllowAllCharacters";
    
    public static final String DBATTRIB_CREATE_NAME_URLS = "CreateNameURLs";

    public static final String ATTRIB_AUTHORINGMODE = "AuthoringMode.";

    public static final String DBATTRIB_PLUGIN_ID = "PluginID";

    /**
     * dummy dbkey for static tml requests
     */
	public static final String STATICTML_DESIGNDBKEY = "de.innovationgate.wga.statictml.DesignDB";

    public static final String DBATTRIB_HDBMODEL = "HDBModel";
    
    public static final String DBATTRIB_ENHANCED_ITEMEXPRESSIONS = "WebTML.EnhancedItemExpressions";

    private static final String ENCODER_NONE = "none";

	public static final String DBATTRIB_ENABLE_ACCESSLOGGING = "EnableAccessLogging";

	public static final String DBATTRIB_URLBUILDER = "URLBuilder";

    public static final String DBATTRIB_SECURE_APP = "SecureApp";

    public static final String DBATTRIB_LANGUAGEBEHAVIOUR_INSTANCE = "LanguageBehaviourInstance";
    
    public static final String DBATTRIB_EXTERNAL_FILE_SERVING_ENABLED = "ExternalFileServingEnabled";

    public static final String ATTRIB_VAR_PARAMETERS = "VarParameters";
    
    public static final String ATTRIB_TRANSIENTPORTLETREGISTRY = "TransientPortletRegistry";
    
    public static final String ATTRIB_PORTLETSTATESTORAGE = "PortletStateStorage";

    public static final String DBATTRIB_SERIALIZABLE_USERPROFILE = "SerializableUserProfile";

    public static final String DBATTRIB_AJAX_KEEP_URL_PARAMS = "AJAX.KeepURLParams";

    public static final String DBATTRIB_SHOW_SESSION_EXPIRED_MESSAGE = "AJAX.ShowSessionExpiredMessage";

    public static final String DBATTRIB_HOME_PAGE_NAME = "HomePageName";

    public static final String DBATTRIB_USE_LOGINPARAMETER_ON_CONTENTURLS = "UseLoginParameterOnContentURLs";
    
    public static final String DBATTRIB_FILE_DERIVATES_ENABLED = "FileDerivates.Enabled";
    
    public static final String DBATTRIB_FILE_DERIVATES_CREATORS = "FileDerivates.Creators";
    
    public static final String DBATTRIB_SCOPEOBJECTREGISTRY = "ScopeObjectRegistry";



    public static final String ATTRIB_URI_HASH = "URIHash";

    public static final String ATTRIB_FORMDATA = "FormData";
    
    public static final String ATTRIB_COOKIES = "HttpCookies";
    public static final String ATTRIB_AJAX_NOREFRESH = "AjaxNoRefresh";
    public static final String ATTRIB_AJAX_FAILURE = "AjaxFailure";
    public static final String ATTRIB_RESPONSE_SETUP ="ResponseSetup";
    public static final String ATTRIB_ACTIVE_PAGECONNECTIONS = "ActivePageConnections";
    public static final String ATTRIB_NEW_PAGE_CONNECTIONS = "NewPageConnections";

    public static final String ATTRIB_PAGECONNECTION = "PageConnection";

    public static final String ATTRIB_SESSION_LIFECYCLE_LISTENER = "SessionLifecycleListener";

    
    // Server options
    public WGPDispatcher getDispatcher() {
        return (WGPDispatcher) _context.getAttribute(ATTRIB_DISPATCHER);
    }

    public static Logger staticLog() {
        return log;
    }
    
    public Logger getLog() {
        return log;
    }

    /*
    public Document getConfigDocument() {
        return configDocument;
    }*/

    public Map<String, WGDatabase> getContentdbs() {
        return contentdbs;
    }

    public String getDefaultMediaKey() {
        return defaultMediaKey;
    }

    public WGPDeployer getDeployer() {
        return _deployer;
    }



    public Date getInstanceActiveSince() {
        return instanceActiveSince;
    }



    public Map<String, WGDatabase> getPersonalisationdbs() {
        return personalisationdbs;
    }



    public int getTmlBuffer() {
        return tmlBuffer;
    }

    public String getTmlHeader() {
        return tmlHeader;
    }





    public ObjectFormatter getEncodingFormatter(String encode, TMLContext context) throws FormattingException {

        encode = encode.toLowerCase();
        
        List<String> flagList = new ArrayList<String>();
        Set<String> flags = new HashSet<String>();
        // split up flags from encoder name
        int pos = encode.indexOf(":");
        if (pos != -1) {
        	String strFlags = encode.substring(pos + 1);
        	flagList = Arrays.asList(strFlags.split("\\|"));
        	flags.addAll(flagList);
        	encode = encode.substring(0, pos);        	
        }

        ObjectFormatter formatter = null;
        if (encode.equalsIgnoreCase(ENCODER_NONE)) {
            formatter = NoneEncodingFormatter.INSTANCE;
        }
        else if (encode.equalsIgnoreCase(ENCODER_HTML) || encode.equalsIgnoreCase(ENCODER_XML)) {
            formatter = new HTMLXMLEncodingFormatter(encode, (context != null ? context.getDesignContext().getVersionCompliance() : WGAVersion.toCsConfigVersion()));
        }
        else if (encode.equalsIgnoreCase(ENCODER_RTF)) {
            formatter = new RTFEncodingFormatter(flags);
        }
        else if (encode.equalsIgnoreCase(ENCODER_RTFSYSTEM)) {
        	flags.add(RTFEncodingFormatter.FLAG_ONLY_SYSTEM_MACROS);
            formatter = new RTFEncodingFormatter(flags);
        }
        else if (encode.equalsIgnoreCase(ENCODER_CRLF)) {
            formatter = new CRLFEncoder();
        }
        else if (encode.equalsIgnoreCase(ENCODER_PLAINTEXT)) {
            formatter = new PlainTextFormatter();
        }
        else if (encode.equalsIgnoreCase(ENCODER_JAVASCRIPT)) {
            formatter = new JavaScriptEncodingFormatter();
        }
        else if (encode.equalsIgnoreCase(ENCODER_JSON)) {
            formatter = new JSONEncodingFormatter();
        }
        else if (encode.equalsIgnoreCase(ENCODER_NAMEPART)) {
            formatter = UniqueNamePartFormatter.INSTANCE;
        }
        else if (encode.equalsIgnoreCase(ENCODER_URL)) {
            formatter = new URLEncodingFormatter();
        }
        else if (encode.equalsIgnoreCase(ENCODER_URLQUERY)) {
            formatter = new URLQueryEncodingFormatter();
        }
        else {
            ModuleDefinition modDef = null;
            Class formatterClass = _systemFormatters.get(encode);
            if (formatterClass == null) {
                formatterClass = _customFormatters.get(encode);
            }
            
            if (formatterClass == null) {
                modDef = getModuleRegistry().getModuleDefinitionByKey(WebTMLEncoderModuleType.class, encode);
                if (modDef != null) {
                    try {
                        modDef.testDependencies();
                    }
                    catch (ModuleDependencyException e) {
                        throw new FormattingException("WebTML encoder '" + encode + "' not available bc. of missing dependency: " + e.getMessage());
                    }
                    formatterClass = modDef.getImplementationClass();
                }
            }
            
            if (formatterClass != null) {
                try {
                    if (modDef != null) {
                        formatter = (ObjectFormatter) getModuleRegistry().instantiate(modDef);
                    }
                    else {
                        formatter = (ObjectFormatter) formatterClass.newInstance();
                    }
                }
                catch (Exception e) {
                    throw new FormattingException("The encoder class " + formatterClass.getName() + " is not instantiable", e);
                }
            }
            else {
                throw new FormattingException("Unknown encoder " + encode);
        }
        }

        if (formatter instanceof TMLContextAwareFormatter) {
            if (context != null) {
                ((TMLContextAwareFormatter) formatter).setContext(context);
            }
            else {
                throw new FormattingException("Encoder '" + encode + "' needs a WebTML context");
            }
        }
        
        if (formatter instanceof FlagAwareFormatter) {
            ((FlagAwareFormatter) formatter).setFlags(flagList);
        }

        return formatter;
    }

    public synchronized String tmlCacheDump() throws IOException {

        File outFile = new File(System.getProperty("user.home"), "tmlcachedump_" + new SimpleDateFormat("yyyyMMddHHmmSS").format(new Date()) + ".csv");
        FileWriter out = new FileWriter(outFile);
        getWebTMLCache().dump(out);
        out.flush();
        out.close();
        return outFile.getPath();

    }

    public File getLicenseFile() {
        return licenseFile;
    }

    public static WGACore retrieve(PageContext pageContext) {
        return (WGACore) pageContext.getAttribute(ATTRIB_CORE, PageContext.APPLICATION_SCOPE);
    }

    public static WGACore retrieve(ServletContext servletContext) {
        return (WGACore) servletContext.getAttribute(ATTRIB_CORE);
    }

    public boolean isTemporary() {
        return false;
    }

    public static ClassLoader getLibraryLoader() {
        return libraryClassLoadingChain;
    }
    
    public static List<Document> getDebugDocumentsList(HttpSession session) {

        synchronized (session) {
            @SuppressWarnings("unchecked")
            List<Document> debugDocuments = (List<Document>) session.getAttribute(WGACore.ATTRIB_TMLDEBUG_DOCUMENTS);
            if (debugDocuments == null) {
                debugDocuments = new ArrayList<Document>();
                session.setAttribute(WGACore.ATTRIB_TMLDEBUG_DOCUMENTS, debugDocuments);
            }
            return debugDocuments;
        }

    }

    public boolean defaultActionSequenceIdAlreadyUsed(String sequenceId) {
         try {
            return (_calledSequenceIds.readEntry(sequenceId) != null);
        }
        catch (CacheException e) {
            getLog().error("Exception determining action sequence id usage", e);
            return false;
        }
    }

    public void defaultActionCalledWithSequenceId(String sequenceId) {
        try {
            _calledSequenceIds.writeEntry(sequenceId, sequenceId);
    }
        catch (CacheException e) {

        }
    }



    public synchronized void updateConfig() throws Exception  {
        
        ProblemOccasion occ = new UpdateConfigOccasion();
        getProblemRegistry().clearProblemOccasion(occ);
        
        configFileLastModified = configFile.lastModified();
        WGAConfiguration oldConfig = getWgaConfiguration();
        
        parseConfigFile();

        // password endoding
        initPasswordEncoding();
        
        // Update general config
        initReadGeneralConfig(true);
        
        // Update filter mappings
        initReadFilterMappings();
        if (getFilter() != null) {
            getFilter().initFilterChain();
        }
        
        // Event Manager
        _eventManager.reloadConfig();
        
        // Read domain configurations
        Map<String, WGADomain> newDomainConfigs = initReadDomains();
        
        deployErrorPage();
        
        // Update WGA plugin connections
        updatePlugins(newDomainConfigs);
        
        updateDatabaseServers();
        
        // Startup domain configurations (must be after plugin connection so that domains can use plugin-provided functionalities)
        initStartupDomains(newDomainConfigs);
        updateContentDBs();
        updateScheduler();
        updateShares();
        initAccessLogger();
        initExternalFileServing();
        initClusterService(oldConfig);
        initHttpSessionManager(oldConfig);
        _jmx.setup();
        
        cleanupProblemRegistry();
        
        WGAConfigurationUpdateEvent event = new WGAConfigurationUpdateEvent(this, getWgaConfiguration(), oldConfig);
        fireConfigEvent(event);
        
        logCategoryInfo("Reloading WGA Configuration finished", 1);
        
        
    }
    
    private void cleanupProblemRegistry() {

        for (ProblemScope scope : getProblemRegistry().getProblemScopes(AdministrativeProblemType.class)) {
            
            if (scope instanceof DatabaseScope) {
                String dbkey = ((DatabaseScope) scope).getDbkey();
                
                boolean stillExists = false;
                if (dbkey.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                    // We have no good way to determine an existing plugin from its database key. So we will just keep everything.
                    stillExists = true;
                }
                else {
                ContentDatabase dbConfig = getWgaConfiguration().getContentDatabase(dbkey);
                    if (dbConfig != null) {
                        stillExists = true;
                    }
                }
                if (!stillExists) {
                    getLog().info("Clearing problem registry scope for database '" + dbkey + "'");
                    getProblemRegistry().clearProblemScope(scope);
                }
            }
            
            else if (scope instanceof DomainScope) {
                String name = ((DomainScope) scope).getName();
                if (!name.equals("default")) {
                    Domain domain = getWgaConfiguration().getDomainByName(name);
                    if (domain == null) {
                        getLog().info("Clearing problem registry scope for domain '" + name + "'");
                        getProblemRegistry().clearProblemScope(scope);
                    }
                }
            }
            
            else if (scope instanceof DBServerScope) {
                String serverId = ((DBServerScope) scope).getServerId();
                DatabaseServer dbServer = (DatabaseServer) getWgaConfiguration().getByUid(serverId); // Configured servers
                if (dbServer == null) {
                    
                    WGDatabaseServer server = getDatabaseServers().get(serverId); // Registered servers
                    if (server == null) {
                        getLog().info("Clearing problem registry scope for database server with id '" + serverId + "'");
                        getProblemRegistry().clearProblemScope(scope);
                    }
                }
            }
            
            else if (scope instanceof ContentShareScope) {
                String name = ((ContentShareScope) scope).getName();
                Share share = getWgaConfiguration().getShare(name);
                if (share == null) {
                    getLog().info("Clearing problem registry scope for content share '" + name + "'");
                    getProblemRegistry().clearProblemScope(scope);
                }
            }
            
            else if (scope instanceof JobScope) {
                String name = ((JobScope) scope).getName();
                de.innovationgate.wga.config.Job job = getWgaConfiguration().getSchedulerConfiguration().getJobByName(name);
                if (job == null) {
                    Job runtimeJob = getScheduler().getJob(name);
                    if (runtimeJob == null) {
                        getLog().info("Clearing problem registry scope for job '" + name + "'");
                        getProblemRegistry().clearProblemScope(scope);
                    }
                }
            }
            
            else if (scope instanceof PluginScope) {
                String name = ((PluginScope) scope).getPluginName();
                List<WGAPlugin> plugins = getPluginSet().getPluginsByUniqueName(name);
                if (plugins.size() == 0) {
                    getLog().info("Clearing problem registry scope for plugin '" + name + "'");
                    getProblemRegistry().clearProblemScope(scope);
                }
            }
            
            
        }
        
    }
    

    private void initExternalFileServing() {
        String enabled = getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ENABLED);
        
        if (enabled != null) {
            _externalFileServingConfig.setEnabled(WGUtils.stringToBoolean(enabled));
        }
        
        
        if (getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_DIRECTORY) != null) {
            File dir = new File((String)getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_DIRECTORY));
            _externalFileServingConfig.setDirectory(dir);
        } else if (_externalFileServingConfig.isEnabled()) {            
            getLog().warn("Mandantory option '" + WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_DIRECTORY + "' is missing. External file serving will be disabled.");
            _externalFileServingConfig.setEnabled(false);
        }
        
        if (getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ROOT_URL) != null) {
            String url = (String)getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ROOT_URL);
            url = url.trim();
            if (!url.endsWith("/")) {
                url += "/";
            }
            _externalFileServingConfig.setRootURL(url);
        }
        
        if (getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_THRESHOLD) != null) {
            try {
                _externalFileServingConfig.setThreshold(Long.parseLong((String)getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_THRESHOLD)) * 1024);
            }
            catch (Exception e) {
                _externalFileServingConfig.setThreshold(ExternalFileServingConfig.DEFAULT_THRESHOLD);
                getLog().warn("Unable to parse external file serving threshold as long. Using default value " + _externalFileServingConfig.getThreshold() / 1024 + "kb.", e);
            }
        } else {
            _externalFileServingConfig.setThreshold(ExternalFileServingConfig.DEFAULT_THRESHOLD);
        }
        
        if (_externalFileServingConfig.isEnabled()) {
            if (!_externalFileServingConfig.getDirectory().exists()) {
                _externalFileServingConfig.getDirectory().mkdirs();
            }
            
            if (!_externalFileServingConfig.getDirectory().exists() || !_externalFileServingConfig.getDirectory().canWrite()) {
                getLog().warn("External file serving directory '" + _externalFileServingConfig.getDirectory().getAbsolutePath() + "' does not exist, could not be created or is not writable. External file serving will be disabled.");
                getProblemRegistry().addProblem(Problem.create(new UpdateConfigOccasion(), "configUpdateProblem.extFileServingFailed", ProblemSeverity.HIGH, Problem.var("dir", _externalFileServingConfig.getDirectory().getAbsolutePath())));
                _externalFileServingConfig.setEnabled(false);
            }
        }
    }

	private void deployErrorPage() {
		// deploy error page
        if (_wgaConfiguration.isCustomErrorPageEnabled() && _wgaConfiguration.getCustomErrorPage() != null) {
        	try {
                this.log.info("Deploying custom error page.");
                _errorPage = getDeployer().deployErrorPage(_wgaConfiguration.getCustomErrorPage());
            }
            catch (IOException e) {
                this.log.error("Error deploying error page.", e);
                _problemRegistry.addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.errorPageDeploymentError", ProblemSeverity.HIGH));
            }
        }
	}
	
	private void unDeployErrorPage() {
		 if (_errorPage != null) {
             String erroPagePath = getServletContext().getRealPath(_errorPage);
             File errorPage = new File(erroPagePath);
             if (errorPage.exists()) {
                 errorPage.delete();
             }
         }
	}
    
    private synchronized void updatePlugins(Map<String, WGADomain> domainConfigs) {
        
        logCategoryInfo("Plugins", 1);
        
        Set<String> newConnectedDBKeys = new HashSet<String>();
        
        if (_wgaDataDir == null) {
            getLog().warn("Unable to handle WGA plugins bc. the WGA data dir could not be initialized!");
            return;
        }
        
        try {
            WGFactory.getInstance().closeSessions();
            
            // Create plugins base dir
            File pluginsDir = new File(_wgaDataDir, "plugins");
            if (!pluginsDir.exists() && !pluginsDir.mkdir()) {
                getLog().error("Could not create plugins directory '" + pluginsDir.getPath() + "'. No WGA Plugins will be connected.");
                _problemRegistry.addProblem(Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.invalidPluginsDir", ProblemSeverity.HIGH, Problem.var("dir", pluginsDir.getAbsolutePath())));
                return;
            }
            
            // Load plugins definition file
            File pluginsDefFile = new File(pluginsDir, "plugins.xml");
            WGAPluginSet currentPlugins;
            boolean reinstallPluginsDir = false;
            if (pluginsDefFile.exists()) {
                currentPlugins = WGAPluginSet.load(pluginsDefFile);
            }
            else {
                currentPlugins = new WGAPluginSet();
                reinstallPluginsDir = true;
            }
            currentPlugins.init(this, pluginsDir);
            if (this.pluginSet != null) {
                currentPlugins.importRuntimeContexts(this.pluginSet);
            }
            
            // Replace plugins field with new plugin set - This must be done before connecting plugins so plugin init/connection scripts can
            // refer to already installed plugins
            this.pluginSet = currentPlugins;
            
            // Cope with default plugins. Install new, uninstall removed (must be in that order so installation can determine status of previous version to remove)
            // At last ensure that for every default plugin of the distro at least one version is active
            installDefaultPlugins(currentPlugins, reinstallPluginsDir);
            uninstallRemovedDefaultPlugins(); 
            ensureActiveDefaultPlugins();
            
            // Validate and connect plugins
            currentPlugins.validatePlugins();
            newConnectedDBKeys = currentPlugins.connectPlugins(domainConfigs);
            
            // Save plugins def file
            this.pluginSet.save();
            
            // Update module registry if any plugins got connected
            if (newConnectedDBKeys.size() > 0) {
                List<URLClassLoader> loaders = new ArrayList<URLClassLoader>();
                for (String dbkey : newConnectedDBKeys) {
                    DynamicClassLoadingChain.SubLoader loader = getLibraryClassLoadingChain().getSubLoader(dbkey);
                    if (loader != null) {
                        loaders.add(loader);
                    }
                }
                
                try {
                    _moduleRegistry.searchModuleDefinitions(this, loaders);
                }
                catch (IOException e) {
                    getLog().error("Exception searching module definitions", e);
                }
            }
            
        }
        catch (Throwable e) {
            getLog().error("Error initializing plugins", e);
            Problem.create(new UpdateConfigOccasion(), "updateConfigProblem.pluginUpdateFailed", ProblemSeverity.HIGH);
        }
        
        return;
        
    }
    
    private void ensureActiveDefaultPlugins() throws MalformedURLException {

        
        Iterator defPlugins = getServletContext().getResourcePaths("/WEB-INF/default-plugins/").iterator();
        File managementPluginFile = null;
        while (defPlugins.hasNext()) {
            String pluginPath = (String) defPlugins.next();
            URL pluginURL = getServletContext().getResource(pluginPath);
            if (pluginURL == null) {
                continue;
            }
            
            String fileName = pluginURL.getFile();
            if (!fileName.endsWith(PluginConfig.WGAPLUGIN_SUFFIX)) {
                continue;
            }
            
            File pluginFile = new File(getServletContext().getRealPath(pluginPath));
            
            try {
                WGAPlugin.Configuration config = WGAPlugin.loadConfiguration(pluginFile, false);
                if (config != null) {
                    WGAPlugin activePlugin = this.pluginSet.getActivePluginsByUniqueName().get(config.getCsConfig().getPluginConfig().getId().getUniqueName());
                    if (activePlugin == null) {
                        WGAPlugin distroPlugin = this.pluginSet.getPluginByFile(pluginFile);
                        if (distroPlugin != null && distroPlugin.isActive() == false) {
                            this.pluginSet.activatePlugin(distroPlugin, WGAPluginSet.UPDATESTRATEGY_UPDATE_KEEP_DATA, false, null);
                        }
                        else {
                            this.pluginSet.installPlugin(pluginFile, WGAPluginSet.UPDATESTRATEGY_UPDATE_KEEP_DATA, true);
                        }
                        
                    }
                }
            }
            catch (Exception e) {
                getLog().error("Exception ensuring active default plugin  " + pluginFile.getName(), e);
            }
            
        }
        
    }

    protected boolean uninstallRemovedDefaultPlugins() {
        
        boolean anythingRemoved = false;
        for (WGAPlugin plugin : getPluginSet().getPlugins()) {
            if (plugin.isDefaultPlugin() || plugin.getRegisteredFilePath().contains("${wga.devpluginsdir}")) {
                
                Configuration pluginConfig = null;
                try {
                    pluginConfig = WGAPlugin.loadConfiguration(plugin.getPluginFile(), false);
                }
                catch (Exception e) {
                    getLog().error("Exception reading configuration of default plugin " + plugin.getPluginFile().getAbsolutePath(), e);
                }
                
                if (pluginConfig == null) {
                    if (plugin.isActive()) {
                        try {
                            getPluginSet().deactivatePlugin(plugin);
                            disconnectPlugin(plugin);
                        }
                        catch (Exception e) {
                            getLog().error("Exception disabling removed default plugin", e);
                        }
                    }
                    getPluginSet().uninstallPlugin(plugin, false);
                    anythingRemoved = true;
                }
            }
        }
        
        if (anythingRemoved) {
            try {
                getPluginSet().save();
            }
            catch (Exception e) {
                getLog().error("Exception uninstalling removed default plugins", e);
            }
        }
        
        return anythingRemoved;
        
    }

    public synchronized void updatePlugins() {
        updatePlugins(this.domains);
        updateContentDBs(); // Bc. of design consumers that may have been disconnected
        cleanupProblemRegistry();
    }

    private boolean installDefaultPlugins(WGAPluginSet currentPlugins, boolean reinstallPluginsFolder) throws FileSystemException, MalformedURLException {
        
        boolean anythingInstalled = false;
        
        // Reinstall the plugins folder if told to
        if (reinstallPluginsFolder) {
            File pluginsFolder = currentPlugins.getPluginFilesDir();
            File[] files = pluginsFolder.listFiles();
            for (int i = 0; i < files.length; i++) {
                File file = files[i];
                if (!file.getName().endsWith(PluginConfig.WGAPLUGIN_SUFFIX)) {
                    continue;
                }
                
                try {
                    WGAPlugin.Configuration config = WGAPlugin.loadConfiguration(file, false);
                    currentPlugins.installPlugin(file, WGAPluginSet.UPDATESTRATEGY_UPDATE_KEEP_DATA, false);
                    anythingInstalled = true;
                    
                }
                catch (Exception e) {
                    getLog().error("Exception installing plugin file " + file.getName(), e);
                }
            }
        }
        
        // STEP 1: Collect all default plugins to priorize identical plugins from different locations
        Map<PluginID,File> defaultPlugins = new LinkedHashMap<PluginID, File>();
        
        // Default plugins from within WGA distribution
        Iterator defPlugins = getServletContext().getResourcePaths("/WEB-INF/default-plugins/").iterator();
        File managementPluginFile = null;
        while (defPlugins.hasNext()) {
            String pluginPath = (String) defPlugins.next();
            URL pluginURL = getServletContext().getResource(pluginPath);
            if (pluginURL == null) {
                continue;
            }

            String fileName = pluginURL.getFile();
            if (!fileName.endsWith(PluginConfig.WGAPLUGIN_SUFFIX)) {
                continue;
            }
            
            File pluginFile = new File(getServletContext().getRealPath(pluginPath));
            try {
                WGAPlugin.Configuration config = WGAPlugin.loadConfiguration(pluginFile, false);
                if (config != null) {
                    defaultPlugins.put(config.getCsConfig().getPluginConfig().getId(), pluginFile);
                }
            }
            catch (Exception e) {
                getLog().error("Exception installing registering plugin file " + pluginFile.getName() + " for installation", e);
            }
            
        }
        
        // Additionally custom folder for default plugins
        String customDefaultPlugins = System.getProperty(SYSPROPERTY_DEFAULT_PLUGINS);
        if (customDefaultPlugins != null) {
            File pluginsFolder = getWGAFile(customDefaultPlugins);
            if (pluginsFolder != null && pluginsFolder.isDirectory()) {
                File[] files = pluginsFolder.listFiles();
                for (int i = 0; i < files.length; i++) {
                    File file = files[i];
                    
                    if (!file.getName().endsWith(PluginConfig.WGAPLUGIN_SUFFIX)) {
                        continue;
                    }
                    
                    try {
                        WGAPlugin.Configuration config = WGAPlugin.loadConfiguration(file, false);
                        if (config != null) {
                            defaultPlugins.put(config.getCsConfig().getPluginConfig().getId(), file);
                        }
                    }
                    catch (Exception e) {
                        getLog().error("Exception installing registering plugin file " + file.getName() + " for installation", e);
                    }
                }
            }
            else {
                getLog().error("The default plugins folder '" + customDefaultPlugins + "' does not exist or is no directory");
            }
        }
        
        // Folder with "developer plugins", i.e. design directories containing plugin code
        String devPlugins = getDeveloperPluginsPath();
        if (devPlugins != null) {
            File pluginsFolder = getWGAFile(devPlugins);
            if (pluginsFolder != null && pluginsFolder.isDirectory()) {
                File[] files = pluginsFolder.listFiles();
                for (int i = 0; i < files.length; i++) {
                    File file = files[i];
                    if (!file.isDirectory()) {
                        if (!file.getName().endsWith(PluginConfig.WGAPLUGIN_SUFFIX)) {
                            continue;
                        }
                    }
                    
                    try {
                        WGAPlugin.Configuration config = WGAPlugin.loadConfiguration(file, false);
                        if (config != null) { 
                            defaultPlugins.put(config.getCsConfig().getPluginConfig().getId(), file);
                        }
                        
                    }
                    catch (Exception e) {
                        getLog().error("Exception installing registering plugin file " + file.getName() + " for installation", e);
                    }
                    
                }
            }
            else {
                getLog().error("The default plugins folder '" + devPlugins + "' does not exist or is no directory");
            }
        }
        
        // STEP 2: Actually install collected default plugins - only one plugin per uname/version
        for (File file : defaultPlugins.values()) {
            if (installDefaultPlugin(currentPlugins, file)) {
                anythingInstalled = true;
            }
        }
        
        return anythingInstalled;
    }

    private boolean installDefaultPlugin(WGAPluginSet currentPlugins, File file) {
        try {
            WGAPlugin.Configuration config = WGAPlugin.loadConfiguration(file, false);
            if (config == null) {
                return false;
            }
            
            // Look if the plugin is already installed
            PluginID defaultPluginId = config.getCsConfig().getPluginConfig().getId();
            List<WGAPlugin> installedPlugins = currentPlugins.getPluginsByUniqueName(defaultPluginId.getUniqueName());
            for (WGAPlugin plugin : installedPlugins) {
                
                // If the plugin file does not exist (removed, not yet uninstalled, default plugin), there's no point in testing
                if (!plugin.getPluginFile().exists() || WGAPlugin.loadConfiguration(plugin.getPluginFile(), false) == null) {
                    continue;
                }

                // Same plugin file = plugin already installed
                if (plugin.getPluginFile().equals(file)) {
                    return false;
                }
                
                // Don't overwrite (active) dev plugin dirs
                if (plugin.isDirectory() && plugin.isActive()) {
                    return false;
                }
                
                // Skip reasons that do not apply to dev plugins
                if (!file.isDirectory()) {
                    
                    // Don't overwrite (active) plugins that are of higher version
                    int versionComparison = plugin.getPluginID().getVersion().compareTo(defaultPluginId.getVersion());
                    if (versionComparison > 0 && plugin.isActive()) {
                        return false;
                    }
                    
                    // Don't overwrite (active) plugins of same version and same or higher build
                    if (versionComparison == 0 && plugin.isActive() && plugin.getPluginID().getVersion().getBuildVersion() >= defaultPluginId.getVersion().getBuildVersion()) {
                        return false;
                    }
                }
            }
            
            currentPlugins.installPlugin(file, WGAPluginSet.UPDATESTRATEGY_UPDATE_KEEP_DATA, true);
            return true;
            
        }
        catch (Exception e) {
            getLog().error("Exception installing default plugin file " + file.getName(), e);
            return false;
        }
        
    }

    /**
     * This method creates a file path to the given file that, if possible, is relative to the current WGA runtime folders.
     * If the file is below any WGA configation paths the path returned will be relative to it and so independent of the
     * absolute location of the WGA configuration. The WGA configuration folders are then represented as path variables.
     * If the file is not below any WGA Configuration path the absolute file path is returned. 
     * You must use method {@link #getWGAFile(String)} to resolve the paths created by this method and retrieve the file again
     * @param file
     * @return
     */
    public String createWGAFilePath(File file) {
        

        try {
            String devPluginsPath = getDeveloperPluginsPath();
            if (devPluginsPath != null) {
                String path = WGUtils.relativeFilePath(file.getAbsolutePath(), devPluginsPath);
                return "${wga.devpluginsdir}/" + path;
            }
        }
        catch (IllegalArgumentException e) {
        }
        
        try {
            String path = WGUtils.relativeFilePath(file.getAbsolutePath(), getWgaDataDir().getAbsolutePath());
            return "${wga.datadir}/" + path;
        }
        catch (IllegalArgumentException e) {
        }
        
        try {
            String path = WGUtils.relativeFilePath(file.getAbsolutePath(), getConfigFilePath());
            return "${wga.cfgdir}/" + path;
        }
        catch (IllegalArgumentException e) {
        }
        
        try {
            String path = WGUtils.relativeFilePath(file.getAbsolutePath(), getServletContext().getRealPath("/WEB-INF/default-plugins"));
            return "${wga.defaultpluginsdir}/" + path;
        }
        catch (IllegalArgumentException e) {
        }
        
        
        
        return file.getAbsolutePath();

        
    }

    public WGDatabase connectPlugin(WGAPlugin plugin, Map domainConfigs, Set connectedPlugins) throws Problem, InvalidPluginException, WGIllegalArgumentException, FileSystemException, IOException {

            // Look if already connected to the correct file
            String dbKey = plugin.buildDatabaseKey();
            WGDatabase db = contentdbs.get(dbKey);
            if (db != null) {
                try {
                    Long pluginFileTime = (Long) db.getAttribute(DBATTRIB_PLUGIN_FILETIME);
                    Version pluginVersion = (Version) db.getAttribute(DBATTRIB_PLUGIN_VERSION);
                    if (pluginVersion.equals(plugin.getPluginID().getVersion()) && 
                        (pluginFileTime != null && pluginFileTime.equals(new Long(plugin.getFileLastModified())))) {
                        if (!db.isSessionOpen()) {
                            db.openSession();
                        }
                        return null;
                    }
                    else {
                        removeContentDB(dbKey);
                        db = null;
                    }
                }            
                catch (Exception e) {
                    throw new InvalidPluginException(plugin, "Error checking existent plugin database " + dbKey, e);
                }
            }

            if (!plugin.isActive()) {
                throw new InvalidPluginException(plugin, "Plugin is deactivated");
            }
            if (!plugin.isValid()) {
                throw new InvalidPluginException(plugin, "Plugin is invalid");
            }

            
            // First connect all mandatory plugins
            try {

                Iterator mandatoryPlugins = plugin.getMandatoryPlugins().values().iterator();
                while (mandatoryPlugins.hasNext()) {
                    WGAPlugin mandatoryPlugin  = (WGAPlugin) mandatoryPlugins.next();
                    connectPlugin(mandatoryPlugin, domainConfigs, connectedPlugins);
                }
            }
            // A mandatory plugin is invalid. Cancel connect.
            catch (InvalidPluginException e) {
                throw e;
            }
   
            logCategoryInfo("Plugin " + plugin.getInstallationKey(), 2);
            
            // Mandatory db options (for plugins)
            Map<String, String> dbOptions = new HashMap<String, String>();
            dbOptions.put(WGDatabase.COPTION_DBREFERENCE, dbKey.toLowerCase());
            dbOptions.put(WGDatabase.COPTION_READERPROFILECREATION, "true");
            putDefaultDbOptions(dbOptions);
            dbOptions.put(WGDatabase.COPTION_CLUSTERED, "false");
            
            // We try to automatically migrate plugin content stores to CS5 format
            dbOptions.put(WGDatabase.COPTION_CONTENT_STORE_VERSION, String.valueOf(WGDatabase.CSVERSION_WGA5));
            dbOptions.put(Database.OPTION_PATH, plugin.getInstallationKey());
            
            // Ugly hack to insert DB options for plugin debugging
            File optionsFile = getWGAFile(plugin.getPluginID().getUniqueName() + ".dboptions.properties");
            if (optionsFile != null && optionsFile.exists()) {
                Properties props = new Properties();
                FileInputStream in = new FileInputStream(optionsFile);
                props.load(in);
                in.close();
                for (Map.Entry<Object,Object> option : props.entrySet()) {
                    dbOptions.put(String.valueOf(option.getKey()), String.valueOf(option.getValue()));
                }
            }
            
            // Clear the plugin database before connecting if the plugin is updated and should clear the db on update
            if (plugin.getRuntimeContext().isUpdated()) {
                if (plugin.getCsConfig().getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
                    de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig v3Config = (de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) plugin.getCsConfig().getPluginConfig();
                    if (v3Config.isClearDatabaseOnUpdate()) {
                        getLog().info("Clearing plugin database for installation key " + plugin.getInstallationKey());
                        plugin.getParent().deletePluginDatabase(plugin);
                    }
                }
                plugin.getRuntimeContext().setUpdated(false);
            }
            
            // Optionally add hotpatches path
            String hotPatchesPath = System.getProperty(SYSPROPERTY_JDBC_HOTPATCHES);
            if (hotPatchesPath != null) {
                File hotPatchesFile = getWGAFile(hotPatchesPath);
                if (hotPatchesFile != null) {
                    dbOptions.put(WGDatabaseImpl.COPTION_HOTPATCH, hotPatchesFile.getAbsolutePath());
                }
            }
            
            // Determine WGAPI implementation
            boolean usesDatabase = true;
            if (plugin.getCsConfig().getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) {
                de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig v5PluginConfig = (de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig) plugin.getCsConfig().getPluginConfig();
                if ("true".equals(v5PluginConfig.getOptions().get(de.innovationgate.wga.common.beans.csconfig.v5.PluginConfig.OPTION_NO_DATABASE))) {
                    usesDatabase = false;
                }
            }
            
            Class<? extends WGDatabaseCore> implClass;
            if (usesDatabase) {
                if (!"false".equals(System.getProperty("de.innovationgate.wga.plugin.lazydbs"))) {
                    implClass = de.innovationgate.webgate.api.hsql.WGLazyDatabaseImpl.class;
                }
                else {
                    implClass = de.innovationgate.webgate.api.hsql.WGDatabaseImpl.class;
                }
            }
            else {
                implClass = WGFakeContentStore.class;
            }
            
            // Connect
            getLog().info("Connecting plugin " + plugin.getPluginID().getUniqueName() + " Version " + plugin.getPluginID().getVersion().toString());
               
            try {
                db = getPluginSet().getDbServer().openDatabase(implClass, dbOptions);
            }
            catch (Throwable e1) {
                throw new InvalidPluginException(plugin, "Could not connect plugin \"" + plugin.getPluginID().getUniqueName() + "\"", e1);
            }

            if (db == null || !db.isSessionOpen()) {
                throw new InvalidPluginException(plugin, "Could not connect plugin \"" + plugin.getPluginID().getUniqueName() + "\" - Check logged messages above for error details");
            }

            try {
                db.getSessionContext().setTask("Initializing database in WGA");
                
                // Plugin dbs are always CS5 since they are automatically migrated
                //getLog().info("Database of plugin " + plugin.getPluginID().getUniqueName() + " is content store version " + db.getContentStoreVersion());
                
                PluginConfig pc = plugin.getCsConfig().getPluginConfig();
                String auth = pc.getAuthentication();
                db.setTitle(pc.getTitle());
                
                
                // Set mandatory database attributes
                initializeDBAttributes(db, dbKey, dbKey, new HashSet());
                
                // Create authentication
                if (auth != null) {
                    String authImplClass = null;
                    Map<String,String> authOptions = new HashMap<String, String>();
                    
                    // Delegate authentication to the default domain
                    if (auth.equals(PluginConfig.AUTHSOURCE_DEFAULT_DOMAIN)) {
                        authImplClass = WGAAuthModuleFactory.AUTHMODULE_DELEGATE;
                        authOptions.put(DelegatingAuthModule.COPTION_DOMAIN, "default");
                    }
                    // Use some plugin for authentication
                    else {
                        WGAPlugin authPlugin = plugin.getParent().getPluginByUniqueName(auth);
                    if (authPlugin != null) {
                        authImplClass = CSAuthModule.class.getName();
                        authOptions.put(CSAuthModule.COPTION_DBKEY, authPlugin.buildDatabaseKey());
                    }
                    else {
                            getLog().error("Unable to find authentication plugin " + auth);
                        }
                    }
                    
                    if (authImplClass != null) {
                        AuthenticationModule authModule = WGFactory.getAuthModuleFactory().getAuthModule(authImplClass, authOptions, db);
                        db.setAuthenticationModule(authModule);
                    }
                    
                }
                
                // Enforce some plugin settings via db attributes
                if (usesDatabase) {
                    db.setAttribute(DBATTRIB_PERSMODE, String.valueOf(pc.getPersonalisationMode()));
                }
                else {
                    db.setAttribute(DBATTRIB_PERSMODE, String.valueOf(Constants.PERSMODE_SESSION));
                }
                db.setAttribute(DBATTRIB_PERSSTATMODE, String.valueOf(Constants.PERSSTATMODE_SESSION));
                db.setAttribute(DBATTRIB_PLUGIN_FILETIME, new Long(plugin.getFileLastModified()));
                db.setAttribute(DBATTRIB_PLUGIN_ID, plugin.getPluginID());
                db.setAttribute(DBATTRIB_PLUGIN_VERSION, plugin.getPluginID().getVersion());
                
                if (!pc.isUsageAsContentStore()) {
                    db.setAttribute(DBATTRIB_ALLOW_PUBLISHING, "false");
                }
                
                if (!pc.isShowOnStartPage()) {
                    db.setAttribute(DBATTRIB_STARTPAGE, "false");
                }
    
                // Configure design provider
                DesignReference ref = new DesignReference(Constants.DESIGNCOL_PLUGIN, plugin.getInstallationKey(), null);
                Map<String,String> options = new HashMap<String, String>();
                db.setDesignProvider(new FileSystemDesignProvider(ref, this, db, plugin.getDesignURL().toString(), options));
                db.setAllowDesignModification(false);
                getDesignFileCache().flushGroup(ref.toString());
               
                // Determine if ACL is empty
                boolean aclEmpty = false;
                try {
                    if (db.isConnected() && db.hasFeature(WGDatabase.FEATURE_ACL_MANAGEABLE) && db.getACL().getAllEntries().size() == 0) {
                        aclEmpty = true;
                    }
                }
                catch (WGBackendException e1) {
                    getLog().error("Error retrieving ACL state of db '" + db.getDbReference() + "'", e1);
                }
                
                // Process system container
                SystemContainerManager.SystemContainerContext scContext = null;
                try {
                    scContext = _systemContainerManager.addDatabase(db, plugin, aclEmpty);
                }
                catch (Problem p) {
                    throw p;
                }
                catch (Exception e) {
                    throw new InvalidPluginException(plugin, "Exception processing system file container for plugin '" + plugin.getPluginID().getUniqueName() + "'", e);
                }
                        
                // Build map of publisher options from wga.xml. We only use gobal options here since plugins have no own options in wga.xml
                // and csconfig.xml options are processed via system container
                Map<String, String> publisherOptions = new HashMap<String, String>();
                // publisherOptions.putAll(_globalPublisherOptions); Plugins should not be influenced by global options of the current configuration
                if (scContext != null) {
                    scContext.putPublisherOptions(publisherOptions);
                }
    
                // Publisher options initialisation which is equal for content dbs and plugins
                processPublisherOptions(db, publisherOptions);
                
                // Set plugin homepage. The method chooses either the plugin-specific homepage or the publisher option
                // Must be after publisher option initialisation to be able to react on them
                db.setAttribute(DBATTRIB_HOME_PAGE, plugin.getPluginHomepage());
                
                // check if db is empty before hdb script runs
                boolean isEmptyDB = db.isContentEmpty();
                                
                // Validate default language definition
                if (!isEmptyDB) {
                    db.determineDefaultLanguage();
                }
                
                // Add listeners for events and register embedded event scripts
                db.addDatabaseEventListener(_eventManager);
                db.addContentEventListener(_eventManager);
                db.addWorkflowEventListener(_eventManager);
                if (db.isConnected()) {
                    _eventManager.updateDatabaseEvents(db);
                }
                
                // Add file annotators
                updateFileAnnotators(db);

                // add File Converter
                db.setFileConverter(_fileConverter);

                // System container initialisations
                if (scContext != null) {
                    scContext.performInitialisation(new Boolean(isEmptyDB));
                    if (isEmptyDB) {
                        db.onConnect(new ValidateDefaultLanguageAction());
                    }
                }
                
                // Registering connection
                this.contentdbs.put(db.getDbReference(), db);
                performNewDBOperations(db);
                
                // Mark this database as fully connected
                db.setAttribute(DBATTRIB_FULLY_CONNECTED, "true");
                
                // Initially create field mappings. These can only come from csconfig.xml for plugins
                updateFieldMappings(db, null);
                
                // Add to connected plugins
                connectedPlugins.add(db.getDbReference());
                
                return db;
            }
            catch (Throwable e) {
                try {
                    db.close();
                }
                catch (WGAPIException e2) {
                    // Silent failure of closing an uninitialized plugin bc. the connection failure is more important
                }
                plugin.setValid(false);
                if (e instanceof Problem) {
                    throw (Problem) e;
                }
                else {
                    throw new InvalidPluginException(plugin, "Error connecting plugin" ,e);
                }
            }
            
            
            
            
        
    }


    private void processPublisherOptions(WGDatabase db, Map<String, String> publisherOptions) throws WGException {
        
        // Put some default options for internal use
        db.setAttribute(DBATTRIB_STORED_QUERIES, new HashMap());
        db.setAttribute(DBATTRIB_META_MAPPINGS, new HashMap());
        db.setAttribute(DBATTRIB_ITEM_MAPPINGS, new HashMap());
        db.setAttribute(DBATTRIB_PLUGIN_SHORTCUTS, new HashMap());
                
        // Put publisher options
        Iterator<String> optionKeys = publisherOptions.keySet().iterator();
        while (optionKeys.hasNext()) {
            String optionName = optionKeys.next();
            String optionValue = publisherOptions.get(optionName);
            db.setAttribute(optionName, optionValue);
        }
        
        // Create a file cache object.
        try {
            db.setAttribute(WGACore.DBATTRIB_FILECACHE, new FileCache(db, this));
        }
        catch (CacheException e) {
            getLog().error("Error initializing file cache for database " + db.getDbReference(), e);
        };
        
        // Create a PPR cache object.
        try {
            db.setAttribute(WGACore.DBATTRIB_PPRCACHE, new PostprocessedResourcesCache(db, this));
        }
        catch (CacheException e) {
            getLog().error("Error initializing PPR cache for database " + db.getDbReference(), e);
        };
        
        // CS Only initialisations
        if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {

            // Fetch design encoding
            String currentDesignEncoding = (String) WGUtils.getValueOrDefault(db.getAttribute(DBATTRIB_DESIGN_ENCODING), Charset.defaultCharset().name());
            
            // design encoding changed
            // this is used for SC_NOT_MODIFIED in WGPDispatcher
            if (getLastDesignEncoding(db.getDbReference()) == null || !getLastDesignEncoding(db.getDbReference()).equals(currentDesignEncoding)) {
                setLastDesignEncoding(db.getDbReference(), currentDesignEncoding);
            }

            // Initialize title path url manager
            boolean allowPublishing = db.getBooleanAttribute(WGACore.DBATTRIB_ALLOW_PUBLISHING, true);
            if(allowPublishing){
	            try {
	                TitlePathManager titlePathManager = new TitlePathManager(db, this, db.getBooleanAttribute(DBATTRIB_TITLEPATHURL, false));
	                db.setAttribute(DBATTRIB_TITLEPATHMANAGER, titlePathManager);
	            }
	            catch (Exception e) {
	                getLog().error("Error initializing title path manager for database " + db.getDbReference(), e);
	            }
            }
            
            // Determine language behaviour
            LanguageBehaviour langBehaviour = createLanguageBehaviour(db);
            db.setAttribute(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR_INSTANCE, langBehaviour);
            
        }
        
        // Non-CS only initialisations
        else {
            db.setAttribute(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR_INSTANCE, new OnlyDefaultLanguageBehaviour());
        }
        

        
    }

    private LanguageBehaviour createLanguageBehaviour(WGDatabase db) throws WGException {
        String langBehaviourName = (String) db.getAttribute(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR);
        if (langBehaviourName == null) {
            boolean isMultiLanguage = db.getBooleanAttribute(WGACore.DBATTRIB_MULTILANGUAGE_CONTENT, true);
            if (isMultiLanguage) {
                langBehaviourName = StaticLanguageBehaviour.class.getName();
            }
            else {
                langBehaviourName = OnlyDefaultLanguageBehaviour.class.getName();
            }
        }
        
        // Convert pre OpenWGA 5.1 values to their 5.1 pendants
        else {
            if (langBehaviourName.equals("default")) {
                langBehaviourName = DynamicLanguageBehaviour.class.getName();
            }
            else if (langBehaviourName.equals("maincontent")) {
                langBehaviourName = StaticLanguageBehaviour.class.getName();
            }
            else if (langBehaviourName.equals("browser")) {
                langBehaviourName = OnlyDefaultLanguageBehaviour.class.getName();
            }
       }
        
        LanguageBehaviour langBehaviour;
        
        // For applications
        if (getModuleRegistry() != null) {
            ModuleDefinition languageBehaviourMD = getModuleRegistry().getModuleDefinition(LanguageBehaviourModuleType.class, langBehaviourName);
            if (languageBehaviourMD == null) {
                getLog().error("Unable to load language behaviour " + langBehaviourName + ". Falling back to mode 'static'.");
                languageBehaviourMD = getModuleRegistry().getModuleDefinition(LanguageBehaviourModuleType.class, StaticLanguageBehaviour.class);
            }
            
            
            try {
                langBehaviour = (LanguageBehaviour) getModuleRegistry().instantiate(languageBehaviourMD);
            }
            catch (ModuleInstantiationException e) {
                getLog().error("Exception instantiating language behaviour '" + languageBehaviourMD.getTitle(Locale.getDefault()) + "'. Falling back to mode 'static'.", e);
                langBehaviour = new StaticLanguageBehaviour();
            }
        }
        
        // For Plugins (no registry yet)
        else {
            try {
                langBehaviour = (LanguageBehaviour) getLibraryLoader().loadClass(langBehaviourName).newInstance();
            }
            catch (Exception e) {
                getLog().error("Exception instantiating language behaviour '" + langBehaviourName + "'. Falling back to mode 'static'.", e);
                langBehaviour = new StaticLanguageBehaviour();
            }
        }
        
        // Initialize
        if (langBehaviour instanceof InitializableLanguageBehaviour) {
            ((InitializableLanguageBehaviour) langBehaviour).init(WGA.get(this), db);
        }
        
        return langBehaviour;
    }

    public Scheduler getScheduler() {
        return _scheduler;
    }

    public static String getBIClientString() {

        return "BI/" + WGAVersion.WGAPUBLISHER_MAJOR_VERSION + "." + WGAVersion.WGAPUBLISHER_MINOR_VERSION + "." + WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION + "_"
                + WGAVersion.WGAPUBLISHER_BUILD_VERSION;
    }

    public static String getWebformClientString() {

        return "Webform/" + WGAVersion.WGAPUBLISHER_MAJOR_VERSION + "." + WGAVersion.WGAPUBLISHER_MINOR_VERSION + "." + WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION + "_"
                + WGAVersion.WGAPUBLISHER_BUILD_VERSION;

    }



    public static String getBuildSignature() {
        return _buildSignature;
    }

    /**
     * @param wgaTempDir
     */
    private void killTempDir(File wgaTempDir) {

        Logger.getLogger("wga").info("Deleting temporary files");
        WGUtils.delTree(wgaTempDir);

    }

    /**
     * @return Returns the luceneManager.
     */
    public LuceneManager getLuceneManager() {
        return luceneManager;
    }

    public boolean isLuceneEnabled() {
        return (luceneManager != null);
    }

    /**
     * @deprecated use isAdminLoggedIn(HttpServletRequest) instead
     * @param session
     * @return
     */
    public static boolean isAdminLoggedIn(HttpSession session) {
        String adminName = (String) session.getAttribute(WGACore.SESSION_ADMINNAME);
        return (adminName != null);
    }
    
    public static boolean isDevelopmentModeEnabled() {
        return Boolean.parseBoolean(System.getProperty(SYSPROPERTY_DEVELOPMENT_MODE, "false"));
    }

    public boolean isAdminLoggedIn(HttpServletRequest request) {
    	// check if we should allow passwordless admin login from localhost
    	if (request != null && isLocalRequest(request) && Boolean.parseBoolean(System.getProperty(SYSPROP_SKIP_LOCAL_ADMIN_LOGINS, "false"))) {
    		return true;
    	} else if (request != null) {
    		String adminName = (String) request.getSession().getAttribute(WGACore.SESSION_ADMINNAME);
    		String adminPassword = (String) request.getSession().getAttribute(WGACore.SESSION_ADMINPASSWORD);
    		return isAdminLogin(adminName, adminPassword, request);
    	} else {
    		return false;
    	}
    }
    
    public Analyzer getAnalyzerForLanguageCode(String lang) {
        return analyzerMappings.get(lang);
    }

    public Analyzer getDefaultAnalyzer() {
        return defaultAnalyzer;
    }

    public FileHandler getFileHandlerForExtension(String extension) {
        return fileHandlerMappings.get(extension.toLowerCase());
    }
    
    public boolean hasFileHandler(String extension) {
    	FileHandler handler = getFileHandlerForExtension(extension);
    	return handler != null;
    }

    public WGADomain getDomain(Map<String, WGADomain> configs, String name) {

        if (name.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
            WGADomain dc = new WGADomain(this, name);
            return dc;
        }
        
        return configs.get(name.toLowerCase());
    }
    
    public WGADomain getDomains(String name) {
        // Prevent nullpointer when this is called before domain init in startup
        if (this.domains != null) {
            return getDomain(this.domains, name);
        }
        else {
            return null;
        }
    }

    public WGADomain getDomainForDatabase(WGDatabase db) {
        
        String domain = (String) db.getAttribute(DBATTRIB_DOMAIN);
        return getDomains(domain);

    }
    
    /**
     * @deprecated Use {@link #getDomainForDatabase(WGDatabase)}
     */
    public WGADomain getDomainConfigForDatabase(WGDatabase db) {
        return getDomainForDatabase(db);
    }

    /**
     * @return Returns the quartzScheduler.
     */
    public org.quartz.Scheduler getQuartzScheduler() {
        return _quartzScheduler;
    }

    /**
     * @return Returns the userAgentVerifier.
     */
    public UserAgentVerifier getUserAgentVerifier() {
        return userAgentVerifier;
    }

    private String readOptionValue(Element dbOptionElem) throws IOException {
        String optionValue = dbOptionElem.attributeValue("value");
        String encoding = dbOptionElem.attributeValue("encode");
        if (encoding != null) {
            if (encoding.equals("base64")) {

                optionValue = new String(Base64.decode(optionValue));

            }
        }
        return optionValue;
    }

    /**
     * @deprecated Use {@link #getSymmetricEncryptionEngine()}
     */
    public DESEncrypter getDesEncrypter() {
        return desEncrypter;
    }

    /**
     * @deprecated
     */
    public void setDesEncrypter(DESEncrypter desEncrypter) {
        this.desEncrypter = desEncrypter;
    }
    
    public SymmetricEncryptionEngine getSymmetricEncryptionEngine() {
        return _symmetricEncryptionEngine;
    }

    public void addEventListener(WGACoreListener listener) {

        if (listener instanceof WGACoreEventListener) {
            WGACoreEventListener coreListener = (WGACoreEventListener) listener;
            if (!coreEventListeners.contains(coreListener)) {
                coreEventListeners.add(coreListener);
            }
        }
        
        if (listener instanceof WGAConfigurationUpdateListener) {
            WGAConfigurationUpdateListener configListener = (WGAConfigurationUpdateListener) listener;
            if (!configUpdateListeners.contains(configListener)) {
                configUpdateListeners.add(configListener);
            }
        }

    }

    public void removeEventListener(Object listener) {
        
        if (listener instanceof WGACoreEventListener) {
            coreEventListeners.remove(listener);

        }
        
        if (listener instanceof WGAConfigurationUpdateListener) {
            configUpdateListeners.remove(listener);
        }
        
    }

    private void fireCoreEvent(WGACoreEvent event) {
        
        // Set core status
        if (event.getType() <= WGACoreEvent.WGACORE_STATE_MAXTYPE)  {
            _status = event.getType();
        }

        
        List listenersList = new ArrayList(coreEventListeners);
        Iterator<WGACoreEventListener> listeners = listenersList.iterator();
        while (listeners.hasNext()) {
            WGACoreEventListener listener = listeners.next();
            try {
                switch (event.getType()) {
    
                    case WGACoreEvent.TYPE_CS_CONNECTED:
                        listener.contentStoreConnected(event);
                        break;
    
                    case WGACoreEvent.TYPE_CS_DISCONNECTED:
                        listener.contentStoreDisconnected(event);
                        break;
    
                    case WGACoreEvent.TYPE_STARTUP_PRE_CONNECT:
                        listener.startupPreConnect(event);
                        break;
    
                    case WGACoreEvent.TYPE_STARTUP_POST_CONNECT:
                        listener.startupPostConnect(event);
                        break;
    
                    case WGACoreEvent.TYPE_SHUTDOWN_PRE_DISCONNECT:
                        listener.shutdownPreDisconnect(event);
                        break;
    
                    case WGACoreEvent.TYPE_SHUTDOWN_POST_DISCONNECT:
                        listener.shutdownPostDisconnect(event);
                        break;
    
                }
            } 
            catch (Exception e) {
            	log.error("WGACoreEventListener '" + listener.getClass().getName() + "' failed with exception: " + e.getMessage(), e);
            }
        }

    }
    
private void fireConfigEvent(WGAConfigurationUpdateEvent event) {
        
        List listenersList = new ArrayList(configUpdateListeners);
        Iterator<WGAConfigurationUpdateListener> listeners = listenersList.iterator();
        while (listeners.hasNext()) {
            WGAConfigurationUpdateListener listener = listeners.next();
            try {
                listener.configurationUpdated(event);
            } 
            catch (Exception e) {
                log.error("WGACoreEventListener '" + listener.getClass().getName() + "' failed with exception: " + e.getMessage(), e);
            }
        }

    }

    public WGAResourceBundleManager getResourceBundleManager(WGDatabase database) {

        WGAResourceBundleManager manager = (WGAResourceBundleManager) database.getAttribute(DBATTRIB_RESOURCEBUNDLE_MANAGER);
        if (manager == null) {
        synchronized (database) {
                manager = (WGAResourceBundleManager) database.getAttribute(DBATTRIB_RESOURCEBUNDLE_MANAGER);
            if (manager == null) {
                manager = new WGAResourceBundleManager(database);
                database.setAttribute(DBATTRIB_RESOURCEBUNDLE_MANAGER, manager);
            }
                
            }
        }
            return manager;

    }

    public TestCore getTestCore() {
        return _testCore;
    }

    public Locale languageCodeToLocale(String prefLang) {

        if (prefLang == null) {
            return null;
        }

        return WGLanguage.languageNameToLocale(prefLang);
    }

    /**
     * @return Returns the designSyncFileVerifier.
     */
    public DesignFileValidator getDesignFileValidator() {
        return designFileValidator;
    }

    public boolean isClientPermitted(WGDatabase db, HttpServletRequest request) {
        if (db.getAttribute(DBATTRIB_CLIENTRESTRICTIONS) != null) {
            IPAddress ip;
            try {
                ip = IPs.parseIPAddress(request.getRemoteAddr());
                if (!ip.validate()) {
                    log.info("Client ip '" + request.getRemoteAddr() + "' from last request is invalid. Client is not permitted to access db '" + db.getDbReference() + "'.");
                    return false;
                }
            }
            catch (Exception e) {
                // ip could not be parsed
                log.info("Client ip '" + request.getRemoteAddr() + "' from last request is invalid. Client is not permitted to access db '" + db.getDbReference() + "'.");
                return false;
            }
            boolean permitted = isClientPermitted(db, ip);
            if (!permitted) {
                log.info("Client ip '" + request.getRemoteAddr() + "' was not permitted to access db '" + db.getDbReference() + "'.");
            }
            return permitted;
        }
        else {
            // if no restrictions client is permitted
            return true;
        }

    }

    public boolean isClientPermitted(WGDatabase db, IPAddress ip) {
        List clientRestrictions = (List) db.getAttribute(DBATTRIB_CLIENTRESTRICTIONS);
        if (clientRestrictions != null) {
            Iterator it = clientRestrictions.iterator();
            while (it.hasNext()) {
                de.innovationgate.utils.net.IPRestriction restriction = (de.innovationgate.utils.net.IPRestriction) it.next();
                if (restriction.exists(ip)) {
                    return true;
                }
            }
            return false;
        }
        else {
            // if no restrictions client is permitted
            return true;
        }
    }

    /**
     * @return Returns the bruteForceLoginBlocker.
     */
    public BruteForceLoginBlocker getBruteForceLoginBlocker() {
        return bruteForceLoginBlocker;
    }

    /**
     * @return Returns the webserviceEnabled.
     */
    public boolean isWebserviceEnabled() {
        return _webserviceEnabled;
    }

    public boolean isAdministrativePort(int port) {
        
        // If no admin ports configured administration is open
        if (getWgaConfiguration().getAdminToolsPortRestrictions().size() == 0) {
            return true;
        }
        
        for (Integer adminPort : getWgaConfiguration().getAdminToolsPortRestrictions()) {
            if (adminPort.equals(port)) {
                return true;
            }
        }
        
        return false;
        
        
    }

    public boolean isAuthoringPort(int port) {

        // If no authoring ports configured authoring is open
        if (getWgaConfiguration().getAuthoringDesignAccessPortRestrictions().size() == 0) {
            return true;
        }
        
        for (Integer authoringPort : getWgaConfiguration().getAuthoringDesignAccessPortRestrictions()) {
            if (authoringPort.equals(port)) {
                return true;
            }
        }
        
        return false;
        
    }

    public String updateConfigDocument(Document newConfig) throws UnsupportedEncodingException, FileNotFoundException, IOException, ParserConfigurationException, DocumentException {
        String newTimestamp = WGACore.DATEFORMAT_GMT.format(new Date());
        newConfig.getRootElement().addAttribute("timestamp", newTimestamp);

        OutputFormat outputFormat = OutputFormat.createPrettyPrint();
        outputFormat.setTrimText(true);
        outputFormat.setNewlines(true);

        XMLWriter writer = new XMLWriter(new FileOutputStream(getConfigFile()), outputFormat);
        writer.write(newConfig);
        writer.close();
        return newTimestamp;
    }

    public String getDesignDatabaseKey(WGDatabase db) {

        WGDesignProvider prov = db.getDesignProvider();
        if (prov instanceof DBDesignProvider) {
            return ((DBDesignProvider) prov).getDesignDBKey();
        }
        else {
            return db.getDbReference();
        }

    }

    public String getDesignDatabaseKey(String dbkey) {

        WGDatabase db = getContentdbs().get(dbkey);
        if (db == null) {
            return dbkey;
        }

        return getDesignDatabaseKey(db);

    }

    /**
     * @return Returns the usageStatistics.
     */
    public WGAUsageStatistics getUsageStatistics() {
        return _usageStatistics;
    }

    public void databaseConnected(WGDatabaseEvent event) {

        getLog().info("Prepared database " + event.getDatabase().getDbReference() + " has been connected");
        _eventManager.updateDatabaseEvents(event.getDatabase());

    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseConnectListener#databaseConnectionError(de.innovationgate.webgate.api.WGDatabaseEvent)
     */
    public void databaseConnectionError(WGDatabaseEvent event) {
        getLog().warn("Removing database " + event.getDatabase().getDbReference() + " because of failure of initial connection");

        if (getContentdbs().containsValue(event.getDatabase())) {
            removeContentDB(event.getDatabase().getDbReference());
        }
        else if (getPersonalisationdbs().containsValue(event.getDatabase())) {
            removePersonalisationDB(getDomainForDatabase(event.getDatabase()).getName());
        }

    }

    public static String getConfigfileName() {

        String fileName = System.getProperty(SYSPROPERTY_CONFIGFILE);
        if (fileName != null) {
            return fileName;
        }

        return CONFIGFILE_NAME;
    }

    public String getCharacterEncoding() {
    	if (_characterEncoding == null) {
    		// default to UTF-8
    		return "UTF-8";
    	} else {
    		return _characterEncoding;
    	}
    }


    public byte[] getLicenseData() throws WGSystemException {
        if (licenseFile != null && licenseFile.exists()) {
            try {
                FileInputStream fin = new FileInputStream(licenseFile);
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                WGUtils.inToOut(fin, out, 1024);
                out.close();
                return out.toByteArray();
            }
            catch (Exception e) {
                throw new WGSystemException("Error reading license data.", e);
            }
        }
        else {
            return null;
        }
    }


    /**
     * checks if the given db is available, returns when database is available
     * or timeout is reached
     * 
     * @param dbKey -
     *            db to check for
     * @param interval -
     *            interval in ms for checks
     * @param timeout -
     *            timeout in ms for the method
     * @return true if db is available, false if timeout has been reached
     */
    public boolean waitForDatabase(String dbKey, long interval, long timeout) {
        long startTime = System.currentTimeMillis();
        boolean dbAvailable = getContentdbs().containsKey(dbKey.toLowerCase());
        while (!dbAvailable && ((System.currentTimeMillis() - startTime) < timeout)) {
            try {
                Thread.sleep(interval);
            }
            catch (InterruptedException e) {
            }
            dbAvailable = getContentdbs().containsKey(dbKey.toLowerCase());
        }
        return dbAvailable;
    }

    public File getConfigFile() {
        return configFile;
    }

    public void updateLibraryLoader() {
        synchronized (libraryClassLoadingChain) {
            _systemContainerManager.updateLibraryLoader(libraryClassLoadingChain);
            
            // Ensure all TMLScript caches were cleared (#00003829)
            RhinoExpressionEngine tmlScriptEngine = ExpressionEngineFactory.getTMLScriptEngine();
            if (tmlScriptEngine != null) {
                tmlScriptEngine.clearCache();
            }
        }
    }

    public static IsolatedJARLoader getBaseLibraryLoader() {
        return baseLibraryLoader;
    }

    /**
     * Enforces all data in system file container that effects the whole WGA configuration.
     * If the database got the system file container via DBDesignProvider, there is no need to process most of 
     * WGA wide configurations again, since they already were enforced by the design provider. So these operations
     * are only executed when info.isFromProviderDB() is false.
     * @param csConfig
     */
    public void enforceCSConfig(WGDatabase db, ContainerInfo info) {
        
        // Update library loader. Can be bypassed if the system file container is from a provider DB.
        if (!info.isFromProviderDB()) {
            updateLibraryLoader();
            info.setEnforcedLibraryUpdate(true);
        }
        
        // If no csconfig.xml we are finished here
        CSConfig csConfig = info.getCsConfig();
        if (csConfig == null) {
            return;
        }
        CSConfig overlayConfig = info.getOverlayCsConfig();
        
        // WGA wide configurations, that can be bypassed if the system file container is from a provider DB
        // (because they already were enforced by the provider db)
        if (!info.isFromProviderDB()) {
        
            // Add encoder mappings
            Iterator encoderMappings = csConfig.getEncoderMappings().iterator();
            while (encoderMappings.hasNext()) {
                EncoderMapping mapping = (EncoderMapping) encoderMappings.next();
                getLog().info("Adding WebTML encoder '" + mapping.getName() + "'");
                if (addEncoderMapping(mapping.getName(), mapping.getImplementationClass(), false)) {
                    info.getEnforcedEncoderMappings().add(mapping.getName());
                }
            }
            
            if (overlayConfig != null) {
                encoderMappings = overlayConfig.getEncoderMappings().iterator();
                while (encoderMappings.hasNext()) {
                    EncoderMapping mapping = (EncoderMapping) encoderMappings.next();
                    getLog().info("Adding WebTML encoder '" + mapping.getName() + "'");
                    if (addEncoderMapping(mapping.getName(), mapping.getImplementationClass(), false)) {
                        info.getEnforcedEncoderMappings().add(mapping.getName());
                    }
                }
            }
            
            // Add element mappings
            Iterator elementMappings = csConfig.getElementMappings().iterator();
            while (elementMappings.hasNext()) {
                ElementMapping mapping = (ElementMapping) elementMappings.next();
                getLog().info("Adding WebTML element '" + mapping.getName() + "'");
                if (addElementMapping(mapping.getName(), mapping.getImplementationClass(), false)) {
                    info.getEnforcedElementMappings().add(mapping.getName());
                }
            }
            
            if (overlayConfig != null) {
                elementMappings = overlayConfig.getElementMappings().iterator();
                while (elementMappings.hasNext()) {
                    ElementMapping mapping = (ElementMapping) elementMappings.next();
                    getLog().info("Adding WebTML element '" + mapping.getName() + "'");
                    if (addElementMapping(mapping.getName(), mapping.getImplementationClass(), false)) {
                        info.getEnforcedElementMappings().add(mapping.getName());
                    }
                }    
            }
            
            // Add media keys
            Iterator mediaKeys = csConfig.getMediaKeys().iterator();
            while (mediaKeys.hasNext()) {
                de.innovationgate.wga.common.beans.csconfig.v1.MediaKey mediaKey = (de.innovationgate.wga.common.beans.csconfig.v1.MediaKey) mediaKeys.next();
                getLog().info("Adding WebTML media key '" + mediaKey.getKey() + "' for MIME type '" + mediaKey.getMimeType() + "'");
                addMediaMapping(mediaKey, false);
                info.getEnforcedMediaMappings().add(mediaKey.getKey());
            }
            
            if (overlayConfig != null) {
                mediaKeys = csConfig.getMediaKeys().iterator();
                while (mediaKeys.hasNext()) {
                    de.innovationgate.wga.common.beans.csconfig.v1.MediaKey mediaKey = (de.innovationgate.wga.common.beans.csconfig.v1.MediaKey) mediaKeys.next();
                    getLog().info("Adding WebTML media key '" + mediaKey.getKey() + "' for MIME type '" + mediaKey.getMimeType() + "'");
                    addMediaMapping(mediaKey, false);
                    info.getEnforcedMediaMappings().add(mediaKey.getKey());
                }    
            }
            

            if (csConfig instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) {
                
                // Add TMLScript global shortcuts
                de.innovationgate.wga.common.beans.csconfig.v2.CSConfig v2 = (de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) csConfig;
                Iterator shortcuts = v2.getShortcuts().iterator();
                while (shortcuts.hasNext()) {
                    Shortcut shortcut = (Shortcut) shortcuts.next();
                    if (shortcut.getType() == Shortcut.TYPE_TMLSCRIPT_GLOBAL) {
                        try {
                            getTmlscriptGlobalRegistry().registerGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(shortcut.getShortcut(), TMLScriptGlobal.TYPE_PACKAGE_OR_CLASS, shortcut.getReference()));
                        }
                        catch (WGException e) {
                            getLog().error("Exception instantiating TMLScript global: " + shortcut.getShortcut());
                        }
                    }
                }
                
            }
            
            if (overlayConfig instanceof de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) {
                
                // Add TMLScript global shortcuts
                de.innovationgate.wga.common.beans.csconfig.v2.CSConfig v2 = (de.innovationgate.wga.common.beans.csconfig.v2.CSConfig) overlayConfig;
                Iterator shortcuts = v2.getShortcuts().iterator();
                while (shortcuts.hasNext()) {
                    Shortcut shortcut = (Shortcut) shortcuts.next();
                    if (shortcut.getType() == Shortcut.TYPE_TMLSCRIPT_GLOBAL) {
                        try {
                            getTmlscriptGlobalRegistry().registerGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(shortcut.getShortcut(), TMLScriptGlobal.TYPE_PACKAGE_OR_CLASS, shortcut.getReference()));
                        }
                        catch (WGException e) {
                            getLog().error("Exception instantiating TMLScript global: " + shortcut.getShortcut());
                        }
                    }
                }
                
            }
            
        }
        
        // Set WGA version compliance
        Set firstLevelOptions = (Set) db.getAttribute(DBATTRIB_FIRSTLEVELDBOPTIONS);
        db.enforceVersionCompliance(csConfig.getVersionCompliance(), !firstLevelOptions.contains(WGDatabase.COPTION_NOITEMBEHAVIOUR));
        if (overlayConfig != null)
        	db.setOverlayComplianceVersion(overlayConfig.getVersionCompliance());
        
        // Add jobs
        if (!isPluginInitDisabled(db, csConfig)) {
            addCsConfigJobs(db, info, csConfig.getJobDefinitions().iterator());
        }
        
        if (overlayConfig != null && !isPluginInitDisabled(db, overlayConfig)) {
            addCsConfigJobs(db, info, overlayConfig.getJobDefinitions().iterator());
        }
        
    }

    public boolean isPluginInitDisabled(WGDatabase db, CSConfig csConfig) {
        if (csConfig.getPluginConfig() instanceof de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) {
            de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig v3Config = (de.innovationgate.wga.common.beans.csconfig.v3.PluginConfig) csConfig.getPluginConfig();
            if (v3Config.isDisablePluginInit() && db.getDbReference().startsWith("plugin-")) {
                return true;
            }
        }
        return false;
    }

    protected void addCsConfigJobs(WGDatabase db, ContainerInfo info, Iterator jobs) {
        while (jobs.hasNext()) {
            JobDefinition job = (JobDefinition) jobs.next();
            String jobName = db.getDbReference() + "." + job.getName();
            try {
                Task task = null;
                
                if (job.getType() == JobDefinition.TYPE_TMLSCRIPTMODULE) {
                    ScriptTask scriptTask = new ScriptTask();
                    scriptTask.setCancelJobOnFail(true);
                    scriptTask.setDatabase(db.getDbReference());
                    scriptTask.setModule(job.getResource());
                    task = scriptTask;
                }
                else if (job.getType() == JobDefinition.TYPE_JAVA) {
                    JavaTask javaTask = new JavaTask();
                    javaTask.setClassName(job.getResource());
                    task = javaTask;
                }
                else {
                    getLog().error("Error adding job '" + jobName + "'. Unknown job type: " + job.getType());
                    continue;
                }
                
                task.setDescription(job.getDescription());
                
                JobSchedule schedule = null;
                if (job.getSchedule() != null && !job.getSchedule().trim().equals("")) {
                    schedule = new JobSchedule();
                    schedule.setEnabled(true);
                    schedule.setScheduleData(job.getSchedule());
                    schedule.setType(JobSchedule.TYPE_CRON);
                }
                
                Job schedulerJob = getScheduler().addCustomTaskJob(jobName, task, false, schedule);
                schedulerJob.setDescription(job.getDescription());
                schedulerJob.getOptions().put("database", db.getDbReference());
                info.getEnforcedJobDefinitions().add(schedulerJob.getName());
            }
            catch (Exception e) {
                getLog().error("Error adding job '" + jobName + "' from content store configuration", e);
            }
        }
    }

    protected boolean addMediaMapping(MediaKey mediaKey, boolean system) {
        if (system) {
            this.systemMediaKeys.put(mediaKey.getKey(), mediaKey);
            return true;
        }
        else if (systemMediaKeys.containsKey(mediaKey.getKey())) {
            MediaKey key = systemMediaKeys.get(mediaKey.getKey());
            if (!key.equals(mediaKey)) {
                getLog().warn("Cannot add media key '" + mediaKey.getKey() + "' because the key is already used in WGA configuration with different data");
            }
            return false;
        }
        else {
            this.customMediaKeys.put(mediaKey.getKey(), mediaKey);
            return true;
        }
    }
    
    public InputStream dumpContentStore(WGDatabase dbSource, String filterExpression, boolean autoCorrect, Logger log) throws WGAPIException, IOException {
        return dumpContentStore(dbSource, filterExpression, autoCorrect, log, false);
    }

    public InputStream dumpContentStore(WGDatabase dbSource, String filterExpression, boolean autoCorrect, Logger log, boolean includeACL) throws WGAPIException, IOException {
        return dumpContentStore(dbSource, filterExpression, autoCorrect, log, includeACL, false);
    }

    public InputStream dumpContentStore(WGDatabase dbSource, String filterExpression, boolean autoCorrect, Logger log, boolean includeACL, boolean includeSystemAreas) throws WGAPIException, IOException {
    	return dumpContentStore(dbSource, filterExpression, autoCorrect, log, includeACL, includeSystemAreas, true);
    }

    public InputStream dumpContentStore(WGDatabase dbSource, String filterExpression, boolean autoCorrect, Logger log, boolean includeACL, boolean includeSystemAreas, boolean includeArchived) throws WGAPIException, IOException {

        log.info("Creating dump database");
        
        // Create working folder for dump database
        File dir = File.createTempFile("csd", ".tmp", WGFactory.getTempDir());
        dir.delete();
        dir.mkdir();
        
        // Create dump database
        Map<String, String> options = new HashMap<String, String>();
        options.put(WGDatabase.COPTION_MONITORLASTCHANGE, "false");
        options.put(WGDatabaseImpl.COPTION_DISTINCTFILECONTENTS, "false");
        WGDatabase dbTarget = WGFactory.getInstance().openDatabase(null, de.innovationgate.webgate.api.hsql.WGDatabaseImpl.class.getName(), dir.getAbsolutePath() + "/wgacs", "sa", null, options);
        dbTarget.setDbReference("Temporary WGACS dump target");
        
        // Replicate
        log.info("Synchronizing data to dump database");
        dbSource.lock();
        try {
            ContentStoreDumpManager importer = new ContentStoreDumpManager(dbSource, dbTarget, log);
            importer.exportDump(includeACL, includeSystemAreas, includeArchived);
        }
        finally {
            dbSource.unlock();
        }
        
        // Close database and zip up its contents
        log.info("Creating dump file");
        dbTarget.close();
        File zipFile = File.createTempFile("csz", ".tmp", WGFactory.getTempDir());
        ZipOutputStream out = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(zipFile)));
        out.setLevel(9);
        File[] files = dir.listFiles();
        for (int i = 0; i < files.length; i++) {
            File file = files[i];
            out.putNextEntry(new ZipEntry(file.getName()));
            InputStream in = new BufferedInputStream(new FileInputStream(file));
            WGUtils.inToOut(in, out, 2048);
            in.close();
            out.closeEntry();
        }
        out.close();
        
        // Delete temp dir
        WGUtils.delTree(dir);
        
        // Return input stream for zip file
        return new TempFileInputStream(zipFile);
        
    }
    
    public String runTransientTask(String taskImplementation, String taskName, Map options) throws ClassNotFoundException, InstantiationException, IllegalAccessException, JobFailedException, ConfigurationException {
        
        Class clazz = getLibraryLoader().loadClass(taskImplementation);
        if (!Task.class.isAssignableFrom(clazz)) {
            throw new IllegalArgumentException("Class " + taskName + " does not implement " + Task.class.getName());
        }
        
        Task task = (Task) clazz.newInstance();
        
        
        Job job = getScheduler().addCustomTaskJob(taskName, task, true, null);
        job.getOptions().putAll(options);
        task.configure(this);
        
        getScheduler().run(job.getName(), "WGA Scheduler Custom Task Runner", options, null);
        return job.getName();

        
    }
    
    public boolean importContentStoreDump(ZipInputStream zipIn, WGDatabase targetDB, Logger log) throws IOException, WGAPIException {
        return importContentStoreDump(zipIn, targetDB, log, false);
    }
    
    public void importContentStoreDump(ZipInputStream zipIn, WGDatabase targetDB) throws IOException, WGAPIException {
        importContentStoreDump(zipIn, targetDB, null);
    }
    
    public boolean importContentStoreDump(ZipInputStream zipIn, WGDatabase targetDB, Logger log, boolean includeACL) throws IOException, WGAPIException {
        return importContentStoreDump(zipIn, targetDB, log, includeACL, false);
    }
    
    public boolean importContentStoreDump(ZipInputStream zipIn, WGDatabase targetDB, Logger log, boolean includeACL, boolean includeSystemAreas) throws IOException, WGAPIException {
        
        if (log != null) {
            log.info("Extracting dump data");
        }
        
        // Create working folder for dump database
        File dir = File.createTempFile("csd", ".tmp", WGFactory.getTempDir());
        dir.delete();
        dir.mkdir();
        
        // Extract dump to folder
        ZipEntry entry = zipIn.getNextEntry();
        while (entry != null) {
            FileOutputStream out = new FileOutputStream(new File(dir, entry.getName()));
            WGUtils.inToOut(zipIn, out, 2048);
            out.close();
            
            entry = zipIn.getNextEntry();
        }
        
        // Open dump database
        Map<String, String> options = new HashMap<String, String>();
        options.put(WGDatabase.COPTION_MONITORLASTCHANGE, "false");
        WGDatabase sourceDB = WGFactory.getInstance().openDatabase(null, de.innovationgate.webgate.api.hsql.WGDatabaseImpl.class.getName(), dir.getAbsolutePath() + "/wgacs", null, null, options);
        sourceDB.setDbReference("WGA Content Store Initial data dump");
        
        if (log != null) {
            log.info("Importing dump data");
        }
        
        // Do the dump
        ContentStoreDumpManager importer = new ContentStoreDumpManager(sourceDB, targetDB, log);
        boolean result = importer.importDump(includeACL, includeSystemAreas);
        
        // Close dump database
        sourceDB.close();
        
        return result;
    }

    protected static Map<String, String> getDbDefaultAttributes() {
        return _dbDefaultAttributes;
    }

    
    /**
     * stores the given licenses data as wga.skey in config path and do necessary updates on wga.xml to reflect new license file
     * @param licenseData
     * @throws WGSystemException 
     */
    /*
    public void storeLicenseData(byte[] licenseData) throws WGSystemException {
        try {            
            File licenseFile = new File(configFile.getParentFile(), LICENSE_FILENAME);
                        
            if (licenseFile.exists()) {
                licenseFile.createNewFile();
            }
            ByteArrayInputStream bin = new ByteArrayInputStream(licenseData);
            FileOutputStream fout = new FileOutputStream(licenseFile);
            WGUtils.inToOut(bin, fout, 1024);
            fout.close();
            log.info("New license data successfully saved in license file '" + licenseFile.getAbsolutePath()  + "'.");
                        
            // update configuration
            updateConfig();            
        }
        catch (Exception e) {
            log.error("Unable to store license data.", e);
            throw new WGSystemException("Unable to store license data.", e);
        }
        
    }*/

    private void loadOptions(Map<String, String> thePublisherOptions, Element publisherOptionElements) {
        Iterator publisherOptionsIt = publisherOptionElements.elementIterator("option");
        while (publisherOptionsIt.hasNext()) {
            Element publisherOptionElem = (Element) publisherOptionsIt.next();
            try {
                thePublisherOptions.put(publisherOptionElem.attributeValue("name"), readOptionValue(publisherOptionElem));
            }
            catch (IOException e) {
                getLog().error("Exception decoding option '" + publisherOptionElem.attributeValue("name") + "'", e);
            }
        }
    }

    public void removeCSConfig(WGDatabase database, ContainerInfo info, boolean finalRemove) {
        
        /* This is not safe. The media key may be defined by multiple designs.
        Iterator<String> mediaMappings = info.getEnforcedMediaMappings().iterator();
        while (mediaMappings.hasNext()) {
            String mediaKey = mediaMappings.next();
            this.customMediaKeys.remove(mediaKey);
        }*/
        
        Iterator<String> elementMappings = info.getEnforcedElementMappings().iterator();
        while (elementMappings.hasNext()) {
            String elementName = elementMappings.next();
            this.customElements.remove(elementName);
        }
        
        Iterator<String> encoderMappings = info.getEnforcedEncoderMappings().iterator();
        while (encoderMappings.hasNext()) {
            String elementName = encoderMappings.next();
            _customFormatters.remove(elementName.toLowerCase());
        }
        
        Iterator<String> jobDefinitions = info.getEnforcedJobDefinitions().iterator();
        while (jobDefinitions.hasNext()) {
            String jobName = jobDefinitions.next();
            getScheduler().removeJob(jobName);
        }
        
        // We only do this if the db is really removed.
        // If not, and this is only an info update, the library loader would get updated
        // right after this method and we can spare the overhead here
        if (finalRemove && info.isEnforcedLibraryUpdate()) {
            if (!info.isStaticClasspath() && getLibraryClassLoadingChain().hasSubLoader(info.getDbkey())) {
                getLog().info("Removing libraries of database " + info.getDbkey() + " from WGA java library loader");
                try {
                    getLibraryClassLoadingChain().removeSubLoader(info.getDbkey());
                }
                catch (IllegalStateException e) {
                    getLog().warn("The libraries of database " + info.getDbkey() + " cannt be removed from WGA java library loader as they are marked static");
                }
            }
            updateLibraryLoader();
        }
        
        // Remove module definitions (May be independent of any classpath)
        getModuleRegistry().removeDefinitionsFromLoader(info.getDbkey());
    }

    public SystemContainerManager getSystemContainerManager() {
        return _systemContainerManager;
    }

    public boolean logout(String domain, javax.servlet.http.HttpSession session, HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse, boolean fireEvent) throws WGException {
        
        // Remove the sessionLogin for this domain
        Map<Object, DBLoginInfo> sessionLogins = getSessionLogins(session);
        DBLoginInfo oldLoginInfo = null;
        if (domain != null) {
            oldLoginInfo = sessionLogins.remove(domain);
        }
        else {
            sessionLogins.clear();
            session.removeAttribute("$defaultlogin");
        }
        
        // Remove profiles of self-personalized dbs, fire events
        WGA wga = WGA.get(httpServletRequest, httpServletResponse, this);
        for (WGDatabase db : getDatabasesForDomain(domain)) {
            int persMode = Integer.parseInt((String) readPublisherOptionOrDefault(db, WGACore.DBATTRIB_PERSMODE));
            if (persMode == Constants.PERSMODE_LOGIN) {
                session.removeAttribute(PersonalisationManager.SESSION_PROFILENAME_INDIVIDUALDB + db.getDbReference());
            }
            if (fireEvent && oldLoginInfo != null && !WGDatabase.ANONYMOUS_USER.equals(oldLoginInfo.getUserName()) && db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                wga.app(db).createEvent("auth=logout")
                .param("userName", oldLoginInfo.getUserName())
                .param("sessionId", session.getId())
                .param("authType", oldLoginInfo.getAuthenticationType())
                .fireOnSession();
            }
        }
        
        return true;
    }
    
    public boolean login(String user, Object password, String domain, HttpServletRequest request, HttpServletResponse response) throws WGException {
        // Get the domain configuration
        WGADomain domainConfig = getDomains(domain);
        if (domainConfig == null) {
            throw new LoginException("The domain '" + domain + "' does not exist");
        }
        
        // Do login. Either on domain's auth module or on a DB of the domain
        boolean isLoginSuccessful = false;
        if (domainConfig.getAuthModule() != null) {
            // Do a login on the domain's auth module
            try {
                AuthenticationSession authSession = getBruteForceLoginBlocker().login(domainConfig, user, password, request);
                if (authSession != null) {
                    isLoginSuccessful = true;
                    user = authSession.getDistinguishedName();
                }
            }
            catch (AuthenticationException e) {
               throw new LoginException("Unable to login on domain '" + domain + "'.", e);
            }            
            
        }
        else {
            // Find a database of the requested domain
            Collection<WGDatabase> dbObjs = getContentdbs().values();
            if (dbObjs.isEmpty()) {
                return false;
            }

            Iterator<WGDatabase> dbs = getContentdbs().values().iterator();
            WGDatabase db = null;
            int accessLevel = WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
            
            // Do a login on the first database found that belongs to this domain
            while (dbs.hasNext()) {
                db = dbs.next();
                String compareDomain = (String) db.getAttribute(WGACore.DBATTRIB_DOMAIN);
                if (compareDomain != null && compareDomain.equalsIgnoreCase(domain)) {
                    accessLevel = getBruteForceLoginBlocker().login(db, user, password, request);
                    if (accessLevel > WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
                        isLoginSuccessful = true;
                        if (db.isSessionOpen()) {
                            setSessionCookie(request, response, db);
                        }
                        break;
                    } else {
                        // unable to login to db of domain
                        // do not try to login to further dbs in this domain bc.
                        // - each db of a domain must support the same auth information
                        // - we will block the login to early bc. each further login request are counted by bruteForceLoginBlocker
                        isLoginSuccessful = false;
                        break;
                    }
                }
            }
        }
        

        // React on login success
        if (isLoginSuccessful) {
            
            // First do a logout for sure
            logout(domain, request.getSession(), request, response, true);
            
            getSessionLogins(request.getSession()).put(domain, new DBLoginInfo(user, password, DBLoginInfo.AuthType.PASSWORD));
            
            WGA wga = WGA.get(request, response, this);
            
            // F00004852
            // perform a reopenSession on all opened databases of this domain, fire application event
            Iterator<WGDatabase> dbsInDomain = getDatabasesForDomain(domain).iterator();
            while (dbsInDomain.hasNext()) {
            	WGDatabase db = dbsInDomain.next();
            	if (db.isSessionOpen()) {
            		db.reopenSession(user, password);
            	}
            	if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            	    wga.app(db).createEvent("auth=login").param("userName", user).param("authType", DBLoginInfo.AuthType.PASSWORD).fireOnSession();
            	}
            }
            
            return true;
        } else {
            return false;
        }
    }    
    

    protected void setSessionCookie(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, WGDatabase database) {
        try {
            if (!database.getSessionContext().isAnonymous() && database.hasFeature(WGDatabase.FEATURE_SESSIONTOKEN)) {
                String cookieName = (String) database.getAttribute(WGACore.DBATTRIB_SESSIONCOOKIE);
                String sessionToken = database.getSessionContext().getSessionToken();
                if (cookieName != null && sessionToken != null && !sessionToken.equals(request.getSession().getAttribute(WGACore.SESSION_COOKIESET + cookieName))) {
                    
                    WGCookie sessionCookie = new WGCookie(cookieName, sessionToken);
                    sessionCookie.setPath("/");
                    String sessionCookieDomain = (String) database.getAttribute(WGACore.DBATTRIB_SESSIONCOOKIEDOMAIN);
                    if (sessionCookieDomain != null) {
                        sessionCookie.setDomain(sessionCookieDomain);
                    }
                    else {
                        sessionCookie.setDomain(getSecondLevelDomain(request.getServerName()));
                    }
                    sessionCookie.addCookieHeader(response);
                    request.getSession().setAttribute(WGACore.SESSION_COOKIESET + cookieName, sessionToken);
                }
            }
        }
        catch (WGBackendException e) {
           getLog().error("Error setting session cookie", e);           
        }
    }

    private String getSecondLevelDomain(String serverName) {
        
        int noOfPoints = WGUtils.countOccurences(serverName, ".");
        
        // If we have less than three parts we cannot cutoff subdomains
        if (noOfPoints < 2) {
            return serverName;
        }
        
        int tldPos = serverName.lastIndexOf(".");
        int sldPos = serverName.lastIndexOf(".", tldPos - 1);
        
        return serverName.substring(sldPos);
        
    }

    public void disconnectPlugin(WGAPlugin plugin) {
        String dbKey = plugin.buildDatabaseKey();
        WGDatabase db = this.contentdbs.get(dbKey);
        if (db != null) {
            getLog().info("Disconnecting plugin " + plugin.getPluginID().getUniqueName() + " Version " + plugin.getPluginID().getVersion().toString());
            removeContentDB(dbKey);
        }
        getProblemRegistry().clearProblemScope(new PluginScope(plugin.getPluginID().getUniqueName()));
        
    }

    public WGAPluginSet getPluginSet() {
        return pluginSet;
    }

    public File getWgaDataDir() {
        return _wgaDataDir;
    }   

	public List<WGAFilterConfig> getFilterMappings() {
		return filterMappings;
	}



    public TMLScriptGlobalRegistry getTmlscriptGlobalRegistry() {
        return _tmlscriptGlobalRegistry;
    }
    
    public int getDefaultPort(WGDatabase db, String protocol) {
        
        // Retrieve from publisher option
        String strProtocol = (String) db.getAttribute(DBATTRIB_DEFAULTPORT_BASE + protocol.toUpperCase());
        if (strProtocol != null) {
            try {
                int port = Integer.parseInt(strProtocol);
                return port;
            }
            catch (NumberFormatException e) {
                getLog().error("Default port of '" + protocol + "' for database '" + db.getDbReference() + "' not numeric: " + strProtocol);
            }
        }
        
        // Default ports
        Integer defaultPort = URLBuilder.getDefaultPortForProtocol(protocol);
        if (defaultPort != null) {
            return defaultPort.intValue();
        }
        
        // Fallback
        return 80;
         
    }

    /*
    public ResourceConfiguration getResourceConfiguration() {
        return _resourceConfiguration;
    }*/
    
    /**
     * retrieves the global wga mail configuration
     * might be null if not configured in detail mailhost is not set in wgaxml
     * @return
     */
	public WGAMailConfiguration getMailConfig() {
		return _mailConfig;
	}
	
	public void send(WGAMailNotification notification) {
		WGAMailConfiguration config = getMailConfig();
		
		if (config != null && config.isEnableAdminNotifications()) {
			try {
				Message msg = new MimeMessage(config.createMailSession());
				
				// set recipient and from address
				String toAddress = config.getToAddress();
				if (toAddress == null) {
				    getLog().error("Unable to send wga admin notification because no recipient address is configured");
				    return;
				}
				
                msg.setRecipient(Message.RecipientType.TO, new InternetAddress(toAddress));
				InternetAddress[] fromAddr = new InternetAddress[1];
				fromAddr[0] = new InternetAddress(config.getFromAddress());
				msg.addFrom(fromAddr);
				
				msg.setSentDate(new Date());
				
                InetAddress localMachine = InetAddress.getLocalHost();  
                String hostname = localMachine.getHostName();
                String serverName = getWgaConfiguration().getServerName();
                if (serverName == null) {
                    serverName = hostname;
                }

				msg.setSubject(notification.getSubject());
				
				msg.setHeader(WGAMailNotification.HEADERFIELD_TYPE, notification.getType());
				
				
				MimeMultipart content = new MimeMultipart();
		        MimeBodyPart body = new MimeBodyPart();
		       		        
		          
		        StringBuffer strBody = new StringBuffer();
		        strBody.append("<html><head></head><body style=\"color:#808080\">");
		        strBody.append(notification.getMessage());
		        String rootURL = getWgaConfiguration().getRootURL();
		        if (rootURL != null) {
		        	strBody.append("<p><a href=\"" + rootURL + "/plugin-admin\">Open " + WGABrand.getName() + " Admin Client ...</a></p>");
		        }
		        // append footer
		        strBody.append("<hr>");
				strBody.append("<b>Server:</b> " + serverName + " / " + WGACore.getReleaseString() + "<br>");
				strBody.append("<b>Host:</b> " + hostname + "<br>");
				//strBody.append("<b>Operation System:</b> " + System.getProperty("os.name") + " Version " + System.getProperty("os.version") + " (" + System.getProperty("os.arch") + ")<br>");						
		        //strBody.append("<b>Java virtual machine:</b> " + System.getProperty("java.vm.name") + " Version " + System.getProperty("java.vm.version") + " (" + System.getProperty("java.vm.vendor") + ")");
		        
		        strBody.append("</body></html>");		        	  
		        body.setText(strBody.toString());
		        body.setHeader( "MIME-Version" , "1.0" );
		        body.setHeader( "Content-Type" , "text/html");  
		        
		        content.addBodyPart( body );
		        AppLog appLog = WGA.get(this).service(AppLog.class);
		        
		        if (notification.isAttachLogfile()) {
	                 MimeBodyPart attachmentBody = new MimeBodyPart();
	                 
	                 StringWriter applog = new StringWriter();
	                 int applogSize = appLog.getLinesCount();
	                 int offset = applogSize - notification.getLogfileLines();
	                 if (offset < 0) {
	                	 offset = 1;
	                 }
	                 appLog.writePage(offset, notification.getLogfileLines(), applog, LogLevel.LEVEL_INFO, false);
	                 
	                 attachmentBody.setDataHandler(new DataHandler(applog.toString(), "text/plain"));
	                 attachmentBody.setFileName("wga.log");
	                 content.addBodyPart(attachmentBody);
		         }
	             msg.setContent(content);
				
				// Send mail
				Thread mailThread = new Thread(new AsyncMailSender(msg), "WGAMailSender");
				mailThread.start();
			} catch (Exception e) {
				getLog().error("Unable to send wga admin notification.", e);
			}
		}
	}
	
		
	private class AsyncMailSender implements Runnable {

		private Message _message;

		public AsyncMailSender(Message message) {
			_message = message;
		}
		
		public void run() {
			try {
				Transport.send(_message);
			} catch (MessagingException e) {
				getLog().error("Unable to send wga notification email.", e);
			}			
		}
		
	}

    public static class DetermineDatabaseDefaultLanguageProblemOccasion implements ProblemOccasion {
        
        private String _dbkey;
        private DatabaseScope _scope;
        
        public DetermineDatabaseDefaultLanguageProblemOccasion(String dbkey) {
            _dbkey = dbkey;
            _scope = new DatabaseScope(dbkey);
        }
        
        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
    
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_dbkey == null) ? 0 : _dbkey.hashCode());
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
            ConnectDatabaseProblemOccasion other = (ConnectDatabaseProblemOccasion) obj;
            if (_dbkey == null) {
                if (other._dbkey != null)
                    return false;
            }
            else if (!_dbkey.equals(other._dbkey))
                return false;
            return true;
        }
    
        @Override
        public ProblemScope getDefaultScope() {
            return _scope;
        }
    
        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }
        
        @Override
        public Class<?> getDefaultRefClass() {
            return WGACore.class;
        }
    
    }

    public class WGAServerOptionReader implements ModuleRegistryChangeListener {
        
        private Map<String,OptionDefinition> _optionDefCache = new ConcurrentHashMap<String, OptionDefinition>();
        
        public Object readServerOptionOrDefault(String name) {
    
            // Fetch option definition
            OptionDefinition optionDef = _optionDefCache.get(name);
            if (optionDef == null) {
                for (ModuleDefinition modDef : WGACore.this.getModuleRegistry().getModulesForType(WGAServerOptionsModuleType.class).values()) {
                    optionDef = modDef.getOptionDefinitions().get(name);
                    if (optionDef != null) {
                        break;
                    }
                }

                if (optionDef == null) {
                    optionDef = new DefaultOptionDefinition(name);
                }

                
                _optionDefCache.put(name, optionDef);
            }
            
    
            // Read the option value, default it if neccessary
            String value = getWgaConfiguration().getServerOptions().get(name);
            if (value == null && optionDef.getDefaultValue() != null) {
                value = optionDef.getDefaultValue();
            }
            
            try {
                return OptionReader.unconvertOptionValue(optionDef, value);
            }
            catch (Exception e) {
                throw new RuntimeException("Exception unconverting server option '" + name + "' with value '" + value + "'", e);
            }
                  
        }
    
        @Override
        public void moduleRegistryChanged(ModuleRegistry registry, Class<? extends ModuleType> moduleType) {
            _optionDefCache.clear();
        }
               
    }

    public File getLoggingDir() {
        return loggingDir;
    }

    public static List<WGLanguage> getLanguageDefinitions(WGDatabase database, Collection langKeys) throws WGAPIException {
    
        List<WGLanguage> langs = new ArrayList<WGLanguage>();
        Iterator keys = langKeys.iterator();
        while (keys.hasNext()) {
            String key = (String) keys.next();
            WGLanguage lang  = database.getLanguage(key);
            if (lang != null) {
                langs.add(lang);
            }
        }
        
        return langs;
        
    }

    public int getStatus() {
        return _status;
    }

	public WGAConfiguration getWgaConfiguration() {
		return _wgaConfiguration;
	}
	

	public String getErrorPage() {
		return _errorPage;
	}

    public WGADesignManager getDesignManager() {
        return _designManager;
    }
    
    public void logCategoryInfo(String msg, int level) {        
    	WGUtils.logCategoryInfo(getLog(), msg, level);                
    }

    public Cache getDesignFileCache() {
        return designFileCache;
    }
    
    public synchronized void saveWgaConfiguration(final WGAConfiguration config) throws Exception {
        
        try {
            
            // Prevent this in dev studio to reduce VCS conflicts
            if (!isDevelopmentModeEnabled()) {
                config.setLastModified(new Date());
            }
        
            // First write to buffer, so serialisation/validation errors are catched before it is actually written to disk
            ByteArrayOutputStream buffOut = new ByteArrayOutputStream();
            WGAConfiguration.write(config, buffOut);
            
            // Write buffered output to file
            FileOutputStream configOut = new FileOutputStream(configFile);
            configOut.write(buffOut.toByteArray());
            configOut.flush();
            configOut.close();
            
            // enforce configuration
            updateConfig();
        
        }
        catch (ConfigValidationException e) {
            getLog().error("Configuration update failed because of validation errors:");
            Iterator<ValidationError> errors = e.getValidationErrors().iterator();
            while (errors.hasNext()) {
                ValidationError validationError = (ValidationError) errors.next();
                getLog().error(validationError.getMessage());    
            }
            throw e;
        }
      
    }
    
    public WGAConfiguration cloneWgaConfiguration() {
        return (WGAConfiguration) XStreamUtils.clone(_wgaConfiguration);
    }
    
    public void setFilter(WGAFilter filter) {
        _filter = filter;
    }

    public WGAFilter getFilter() {
        return _filter;
    }



    public Map<String, WGDatabaseServer> getDatabaseServers() {
        return _databaseServers;
    }
    
    public List<String> getEncodingFormatterNames() {
        
        List<String> formatters = new ArrayList<String>();
        formatters.addAll(_customFormatters.keySet());
        formatters.addAll(_systemFormatters.keySet());
        
        // Embedded system formatters - that are served from outside the formatters registry
        
        formatters.add(ENCODER_CRLF);
        formatters.add(ENCODER_HTML);
        formatters.add(ENCODER_RTF);
        formatters.add(ENCODER_RTFSYSTEM);
        formatters.add(ENCODER_XML);
        formatters.add(ENCODER_JAVASCRIPT);
        formatters.add(ENCODER_JSON);
        formatters.add(ENCODER_NAMEPART);
        formatters.add(ENCODER_PLAINTEXT);
        formatters.add(ENCODER_URL);
        formatters.add(ENCODER_URLQUERY);
        formatters.add(ENCODER_NONE);
        return formatters;
        
    }

    public WebTMLCache getWebTMLCache() {
        return _webTMLCache;
    }
	
    public Map<String,WGADomain> getDomains() {
        return domains;
    }

    public static DynamicClassLoadingChain getLibraryClassLoadingChain() {
        return libraryClassLoadingChain;
    }

	public WGALoggerWrapper getAccessLogger() {
		return _accessLogger;
	}



    public Map<String, ShareDefinition> getShares() {
        return _shares;
    }	
    
    public void addCustomShare(ShareDefinition def) throws ShareInitException {
        def.setOrigin(ShareDefinition.ORIGIN_CUSTOM);
        getShares().put(def.getName(), def);
        getLog().info("Added custom share '" + def.getName() + "'");
    }
    
    public Object readPublisherOptionOrDefault(WGDatabase db, String name) {

       if (_publisherOptionsReader != null) {
           return _publisherOptionsReader.readPublisherOptionOrDefault(db, name);
       }
       else {
           return db.getAttribute(name);
       }
        
    }
    
    public Object readServerOptionOrDefault(String name) {
       if (_serverOptionsReader != null) {
           return _serverOptionsReader.readServerOptionOrDefault(name);
       }
       else {
           return getWgaConfiguration().getServerOptions().get(name);
       }
    }
    
    public WGAURLBuilder retrieveURLBuilder(HttpServletRequest request, WGDatabase db) {
        return retrieveURLBuilder(request, db, new Class[] {});
    }
    
    public WGAURLBuilder retrieveURLBuilder(HttpServletRequest request, WGDatabase db, Class[] extensionInterfaces) {

        WGAURLBuilder builder = null;
        if (request != null) {
            builder = (WGAURLBuilder) request.getAttribute(WGACore.ATTRIB_URLBUILDER);
        }
        
        // Create a new builder
        if (builder == null) {
            
            // Look if the builder to use is predefined by DB attribute
        	Object urlBuilderClassName = db.getAttribute(WGACore.DBATTRIB_URLBUILDER);
        	if (urlBuilderClassName != null && urlBuilderClassName instanceof String) {
        		try {
					Class builderClazz = WGACore.getLibraryLoader().loadClass((String)urlBuilderClassName);
					builder = (WGAURLBuilder) builderClazz.newInstance();
				} catch (Exception e) {
					getLog().error("Unable to instatiate url builder '" + urlBuilderClassName + "'. Using default url builder.", e);
						
				}        		
        	} 
        	
        	// Check the extension interfaces. If the builder does not satisfy them, remove it
        	if (builder != null) {
            	for (Class ifClass : extensionInterfaces) {
            	    if (!ifClass.isAssignableFrom(builder.getClass())) {
            	        builder = null;
            	        break;
            	    }
            	}
        	}
        	
        	// Default/Fallback URL builder
        	if (builder == null || (request == null && !(builder instanceof RequestIndependentURLBuilder))) {
        		DefaultURLBuilder defBuilder = new RequestIndependentDefaultURLBuilder();
        		if (request != null && "true".equals(request.getParameter("forceClassicURL"))) {
                    defBuilder.setTitlePathAllowed(false);
                }
        		builder = defBuilder;
        	}   
        	
        	// Register new builder with current request
        	if (request != null) {
        	    request.setAttribute(WGACore.ATTRIB_URLBUILDER, builder);
        	}
            
        }
        
        // Initialize the new builder
        if (request == null) {
           ((RequestIndependentURLBuilder) builder).newIndependentInstance(this);
        }
        else {
            builder.newRequest(this, request);
        }
        
        return builder;
    }

    public List<HTMLHeadInclusion> getHtmlHeadInclusions() {
        return _htmlHeadInclusions;
    }

    public void setDefaultAnalyzer(Analyzer defaultAnalyzer) {
        this.defaultAnalyzer = defaultAnalyzer;
    }
    
    public String getClassLocation(String className) {
        return WGUtils.which(className, getLibraryLoader()); 
    }
    
    public boolean isRunSingleNodeFunctionalities() {
        ClusterConfiguration clusterConfig = getWgaConfiguration().getClusterConfiguration();
        if (clusterConfig != null && clusterConfig.isEnabled()) {
            return clusterConfig.isDefaultMasterNode();
        }
        return true;
    }

    public EventManager getEventManager() {
        return _eventManager;
    }
    

    
    public boolean testPasswordAgainstHash(String pwd, HashedPassword hashedPwd) throws HashingException {
        return hashedPwd.check(pwd, getModuleRegistry());
    }
    
    public boolean testPasswordAgainstHash(String pwd, String storedHash) throws HashingException {
        return testPasswordAgainstHash(pwd, new HashedPassword(storedHash));
    }

    public WGAConfigurationOptionReader getServicesServerOptionReader() {
        return _servicesServerOptionReader;
    }

    public JMX getJmx() {
        return _jmx;
    }

    protected long getConfigFileLastModified() {
        return configFileLastModified;
    }

    public XStream getDefaultSerializer() {
        return _defaultSerializer;
    }

    public Map<String, String> getContentdbUuidsToKeys() {
        return _contentdbUuidsToKeys;
    }

    public AbstractWGAHttpSessionManager getHttpSessionManager() {
        return _httpSessionManager ;
    }

    private void updateFileAnnotators(WGDatabase db) throws WGException {
        
        if (!db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            return;
        }
        
        // Fetch definitions of configured or all annotators
        List<String> configuredAnnotators = (List<String>) WGA.get(WGACore.this).app(db).getPublisherOption(DBATTRIB_FILE_ANNOTATORS);
        List<ModuleDefinition> annotatorDefs = new ArrayList<ModuleDefinition>();
        if (configuredAnnotators != null) {
            for (String annotatorName : configuredAnnotators) {
                ModuleDefinition annotatorDef = getModuleRegistry().getModuleDefinition(FileAnnotatorModuleType.class, annotatorName);
                if (annotatorDef != null) {
                    annotatorDefs.add(annotatorDef);
                }
                else {
                    getLog().warn("Unknown file annotator '" + configuredAnnotators + "' configured on app '" + db.getDbReference() + "'");
                }
            }
        }
        else {
            annotatorDefs.addAll(getModuleRegistry().getModulesForType(FileAnnotatorModuleType.class).values());
        }
        
        // Instantiate annotators
        List<WGFileAnnotator> annotators = new ArrayList<WGFileAnnotator>();
        for (ModuleDefinition annotatorDef : annotatorDefs) {
            try {
                WGFileAnnotator annotator = (WGFileAnnotator) getModuleRegistry().instantiate(annotatorDef);
                annotators.add(annotator);
            }
            catch (Throwable e) {
                getLog().error("Exception instantiating file annotator " + annotatorDef.getImplementationClass().getName(), e);
            }
        }
        
        // Set new annotators
         db.setFileAnnotators(annotators);
    }

    public AppLogAppender getTransientLogAppender() {
        return transientLogAppender;
    }
    
    public String getSystemLabel(String systemBundleName, String labelKey, HttpServletRequest servletRequest) {
        
        String label = null;
        PropertyResourceBundle bundle = null;
        
        List<Locale> locales = new ArrayList<Locale>();
        if (servletRequest != null) {
            locales.addAll(WGUtils.extractEntryList(servletRequest.getLocales()));
        }
        locales.add(Locale.getDefault());
        locales.add(Locale.ENGLISH);
        
        for (Locale locale : locales) {
            try{
                bundle = (PropertyResourceBundle)ResourceBundle.getBundle(WGACore.SYSTEMLABEL_BASE_PATH + systemBundleName, locale, this.getClass().getClassLoader());
                label = bundle.getString(labelKey);
                if (label != null) {
                    return label;
                }
            }
            catch(java.util.MissingResourceException e) {
            }
        }
        
        return null;
    }
    
    public VirtualLinkTarget resolveVirtualLink(WGA wga, final WGContent content) throws WGAPIException {
        VirtualLinkTarget target = null;
        try {
            VirtualLinkResolver resolver = null;
            ModuleDefinition modDef = getModuleRegistry().getModuleDefinitionByKey(VirtualLinkResolverModuleType.class, content.getVirtualLinkType());
            if (modDef != null) {
                 resolver = (VirtualLinkResolver) getModuleRegistry().instantiate(modDef);
            }
            
            // Fallback: External URL
            else {
                resolver = new ExternalVirtualLinkResolver();
            }
            
            target = resolver.resolve(wga, content);
        
        }
        catch (Exception e) {
            wga.server().getLog().error("Exception resolving virtual link on content " + content.getContentKey(), e);
        }
        
        if (target == null) {
            ProblemOccasion occ = new ProblemOccasion() {
                
                @Override
                public boolean isClearedAutomatically() {
                    return false;
                }
                
                @Override
                public Class<? extends ProblemType> getDefaultType() {
                    return AdministrativeProblemType.class;
                }
                
                @Override
                public ProblemScope getDefaultScope() {
                    return new DatabaseScope(content.getDatabase().getDbReference());
                }
                
                @Override
                public Class<?> getDefaultRefClass() {
                    return WGACore.class;
                }
            };
            getProblemRegistry().addProblem(Problem.create(occ, "contentProblem.invalidVirtualLink#" + content.getContentKey(), ProblemSeverity.LOW, Problem.var("title", content.getTitle()).var("key", content.getContentKey().toString())));
        }
        
       
        return target;
    }

    public String getStartPageURL() {
        String url = WGABrand.get("startpage.url");
        if (url == null) {
            url = "/plugin-management/html/homepage:main.int.html";
        }
        return url;
            
    }

    public ScopeObjectRegistry getScopeObjectRegistry() {
        return _serverScopeObjectRegistry;
    }

    @Override
    public ClassLoader getClassLoader() {
       return getLibraryLoader();
    }

    public boolean isClusteredDatabase(WGDatabase db) {

        if (getWgaConfiguration().getClusterConfiguration() == null || getWgaConfiguration().getClusterConfiguration().isEnabled() == false) {
            return false;
        }
        
        if ("false".equals(db.getCreationOptions().get(WGDatabase.COPTION_CLUSTERED))) {
            return false;
        }
        
        
        return true;
        
    }

}


