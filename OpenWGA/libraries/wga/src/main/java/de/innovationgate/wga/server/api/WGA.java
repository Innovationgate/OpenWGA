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

package de.innovationgate.wga.server.api;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.mail.internet.AddressException;
import javax.servlet.ServletContext;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.io.IOUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.DynamicClassLoadingChain;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ISO8601DateFormat;
import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.IteratorWrapper;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.TextualDateFormat;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGStructEntryList;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.types.ServiceApiProperties;
import de.innovationgate.wga.modules.types.ServiceApiType;
import de.innovationgate.wga.modules.types.WGAEncoderModuleType;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wga.server.api.tml.FormInfo;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.ClientHints;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACore.SessionLoginMap;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.encoders.EncoderOutput;
import de.innovationgate.wgpublisher.encoders.EncodingFormatterEncoder;
import de.innovationgate.wgpublisher.encoders.TextChunk;
import de.innovationgate.wgpublisher.encoders.WGAEncoder;
import de.innovationgate.wgpublisher.encoders.WGAInputOnlyEncoder;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.jsputils.JspHelper;
import de.innovationgate.wgpublisher.lang.SingleLanguageChooser;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.log.WGARequestInformation;
import de.innovationgate.wgpublisher.mail.SmtpMail;
import de.innovationgate.wgpublisher.mail.WGAMailConfiguration;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.sessions.api.WGAHttpSession;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.ImageScalerFactory;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextEnvironment;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

/**
 * Base class for the WGA Server API, providing access to all of the APIs functionalities.
 * Use one of the get() methods to aquire an instance using different context objects for parameters:
 * <ul>
 * <li>{@link #get()} when running on behalf of a WebTML/TMLScript environment
 * <li>{@link #get(Context)} when a WebTML context object is available
 * <li>{@link #get(JobContext)} in OpenWGA Java Jobs
 * <li>{@link #get(HttpServletRequest, HttpServletResponse, ServletContext)} or {@link #get(ServletContext)} in Servlet Filters
 * <li>{@link #get(WGACore)} in other core functionalities
 * </ul>
 * The number of available services may differ depending on the context object for which this instance was created.
 * If a functionality needs a WGA resource that is not available on the context it throws an {@link UnavailableResourceException}.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class WGA {
    
    /**
     * Dividing character for WebTML scoping, dividing the scope qualifier from the scoped data
     */
    private static final String SCOPE_DIVIDER = "_";
    
    interface OpenDatabaseService {
        
        public void openDatabase(WGDatabase db) throws WGException;
        
    }
    
    /**
     * Iterator over {@link Context}s of the query result set
     */
    protected class TMLContextWrapperIterator extends PrefetchingIterator<Context> implements SkippingIterator<Context>, IteratorWrapper<WGContent> {
        
        protected SkippingIterator<WGContent> _it;
        private boolean _fetched = false;

        private TMLContextWrapperIterator(SkippingIterator<WGContent> it) {
            _it = it;
        }

        @Override
        public void remove() {
            _fetched  = true;
            _it.remove();
        }

        @Override
        public int skip(int nrOfElements) {
            if (_fetched) {
                throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
            }
            return _it.skip(nrOfElements);
        }
        
        @Override
        public Iterator<WGContent> getWrappedIterator() {
            return _it;
        }

        @Override
        protected Context fetchNextValue() {
            _fetched = true;
            try {
                if (_it.hasNext()) {
                	WGContent doc = _it.next();
                	if(doc!=null)
                		return WGA.this.createTMLContext(doc);
                	else return null; 
	            }
                else {
                    return null;
                }
            }
            catch (Exception e) {
                throw new RuntimeException("Exception stepping through iterator", e);
            }
        }

        @Override
        public String toString() {

            List<String> its = new ArrayList<String>();
            Iterator<?> it = this;
            while (true) {
                its.add(it.getClass().getName());
                if (it instanceof IteratorWrapper) {
                    it = ((IteratorWrapper<?>) it).getWrappedIterator();
        }
                else {
                    break;
            }
        }
        
            return WGUtils.serializeCollection(its, " -> ");
            
        }
        
    }
    
    protected class CountReportingTMLContextWrapperIterator extends TMLContextWrapperIterator implements CountReportingIterator<Context> {

        public CountReportingTMLContextWrapperIterator(SkippingIterator<WGContent> it) {
            super(it);
        }
        
        @Override
        public int getCount() {
            return ((CountReportingIterator<?>) _it).getCount();
        }

        @Override
        public int getCurrentOffset() {
            return ((CountReportingIterator<?>) _it).getCurrentOffset();
        }
        
    }
    

    private static class ListComparatorByMeta implements Comparator<Object> {
    
        private String metaName = new String();
    
        private int sortSign = 1;

        private TMLContext tmlContext;
    
        public ListComparatorByMeta(String metaName, int sortDir, TMLContext context) {
            if (metaName != null) {
                this.metaName = metaName;
            }
            else {
                this.metaName = "name";
            }
            this.sortSign = sortDir;
            this.tmlContext = context;
        }
    
        @SuppressWarnings("unchecked")
        public int compare(Object o1, Object o2) {
    
            int ret = 0;
    
            if (o1 instanceof WGContent && o2 instanceof WGContent) {
                Object value1 = null;
                Object value2 = null;
                
                try {
                    value1 = (Object) tmlContext.context((WGContent) o1).meta(this.metaName);
                    value2 = (Object) tmlContext.context((WGContent) o2).meta(this.metaName);

                    if (value1 instanceof Date && value2 instanceof Date) {
                        Long millis1 = new Long(((Date) value1).getTime());
                        Long millis2 = new Long(((Date) value2).getTime());
   
                        ret = millis1.compareTo(millis2);
                    }
                    else if (value1 instanceof Double && value2 instanceof Double) {
                        ret = ((Double) value1).compareTo(((Double) value2));
                    }
                    else if (value1 instanceof Integer && value2 instanceof Integer) {
                        ret = ((Integer) value1).compareTo(((Integer) value2));
                    }
                    else {
                        ret = ((String) value1).toLowerCase().compareTo(((String) value2).toLowerCase());
                    }
                }
                catch (WGAPIException e) {
                    return 0;
                }
            }
            else if (o1 instanceof Comparable && o2 instanceof Comparable) {
            	if(o1 instanceof Integer)
            		o1 = ((Integer) o1).doubleValue();
            	if(o2 instanceof Integer)
            		o2 = ((Integer) o2).doubleValue();
                return ((Comparable<Object>) o1).compareTo(o2);
            }
            else {
                String value1 = o1.toString();
                String value2 = o2.toString();
                if (value1 != null && value2 != null) {
                    ret = value1.toLowerCase().compareTo(value2.toLowerCase());
                }
                else {
                    ret = 1;
                }
            }
            return ret * this.sortSign;
        }
    
        public boolean equals(Object o1) {
            return this.equals(o1);
        }
    }

    private static class ListComparatorByMetaList implements Comparator<Object> {
    
        private List<String> metaNames = new ArrayList<String>();
    
        private int sortSign = 1;

        private TMLContext tmlContext;
    
        public ListComparatorByMetaList(List<String> metaNames, int sortDir, TMLContext context) {
            if (metaNames != null) {
                this.metaNames = metaNames;
            }
            else {
                throw new IllegalArgumentException();
            }
            this.sortSign = sortDir;
            this.tmlContext = context;
        }
    
        public int compare(Object o1, Object o2) {
    
            int ret = 0;
    
            if (this.metaNames == null || this.metaNames.isEmpty())
                return 0;
    
            if (o1 instanceof WGContent && o2 instanceof WGContent) {
                Iterator<String> it = this.metaNames.iterator();
                while (it.hasNext()) {
    
                    String metaName = (String) it.next();
    
                    Object value1 = null;
                    Object value2 = null;
                    
                    try {
                        value1 = (Object) tmlContext.context((WGContent) o1).meta(metaName);
                        value2 = (Object) tmlContext.context((WGContent) o2).meta(metaName);

   
                        if (value1 instanceof Date && value2 instanceof Date) {
                            Long millis1 = new Long(((Date) value1).getTime());
                            Long millis2 = new Long(((Date) value2).getTime());
   
                            ret = millis1.compareTo(millis2);
                        }
                        else if (value1 instanceof Double && value2 instanceof Double) {
                            ret = ((Double) value1).compareTo(((Double) value2));
                        }
                        else if (value1 instanceof Integer && value2 instanceof Integer) {
                            ret = ((Integer) value1).compareTo(((Integer) value2));
                        }
                        else {
                            ret = ((String) value1).toLowerCase().compareTo(((String) value2).toLowerCase());
                        }
                    }
                    catch (WGAPIException e) {
                        return 0;
                    }
                    
                    if (ret != 0) {
                        break;
                    }
                }
            }
    
            return ret * this.sortSign;
        }
    
        public boolean equals(Object o1) {
            return this.equals(o1);
        }
    }
    
    /**
     * Retrieves an WGA instance trying to use the current threads main WebTML context for an environment
     * Note however that the context used may not be the most specific context of your threads stack.  
     * Without a main context it returns a WGA instance with core functionalities only.

     * @throws IllegalStateException
     */
    public static WGA get() throws WGException {
        
        TMLContext context = TMLContext.getThreadMainContext();
        if (context != null) {
            return WGA.get(context);
        }
        else {
            return WGA.get(WGACore.INSTANCE);
        }
        
    }
    
    /**
     * Returns a WGA instance using the given {@link WGAContext} object for a context.
     * The service provided depends on the context known by the WGAContext object.
     * @param context A WGA context object
     */
    public static WGA get(WGAContext context) {
    	if(context!=null)
    		return new WGA(context);
    	try {
			return get();	// best chance
		} catch (WGException e) {
			e.printStackTrace();
			return null;
		} 
    }
    
    public static WGA get(final TMLContextEnvironment env) {
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return null;
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return env.getResponse();
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return env.getRequest();
            }
            
            @Override
            public HttpSession getHttpSession() {
                return env.getRequest().getSession();
            }
            
            @Override
            public Logger getLog() {
                return env.getLog();
            }
            
            @Override
            public WGACore getCore() {
                return env.getCore();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }
        });
    }
    
    /**
     * Returns a WGA instance using the given TMLContext object for a context.
     * The returned instance provides full service.
     * @param context A TMLContext object
     */
    public static WGA get(final Context context) {
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return (TMLContext) context;
            }
            
            @Override
            public HttpSession getHttpSession() {
                return ((TMLContext) context).getEnvironment().getSession();
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return context.getresponse();
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return context.getrequest();
            }
            
            @Override
            public WGACore getCore() {
                return getTMLContext().getwgacore();
            }
            
            @Override
            public Logger getLog() {
                return context.getlog();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }
        });
    }
    
    /**
     * Returns a WGA instance using the given servlet context for a context.
     * The instance provided may have no WebTML/TMLScript/Request specific functionalities available.
     * @param servletContext The servlet context
     */
    public static WGA get(ServletContext servletContext) {
        
        TMLContext context = TMLContext.getThreadMainContext();
        if (context != null) {
            return WGA.get(context);
        }

        final WGACore core = WGACore.retrieve(servletContext);
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return null;
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return null;
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return null;
            }
            
            @Override
            public HttpSession getHttpSession() {
                return null;
            }
            
            @Override
            public WGACore getCore() {
                return core;
            }
            
            @Override
            public Logger getLog() {
                return core.getLog();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }
        });
        
    }
    
    /**
     * Returns a WGA instance using the given JavaEE objects for a context, for use in HTTP servlet requests.
     * The instance provided may have no WebTML/TMLScript specific functionalities available.
     * @param request A HTTP request object
     * @param response A HTTP response object
     * @param servletContext A servlet context object
     */
    public static WGA get(final HttpServletRequest request, final HttpServletResponse response, ServletContext servletContext) {
        
        TMLContext context = TMLContext.getThreadMainContext();
        if (context != null) {
            return WGA.get(context);
        }

        final WGACore core = WGACore.retrieve(servletContext);
        return get(request, response, core);
        
    }

    /**
     * Returns a WGA instance using the given JavaEE objects for a context, for use in HTTP servlet requests.
     * The instance provided may have no WebTML/TMLScript specific functionalities available.
     * @param request A HTTP request object
     * @param response A HTTP response object
     * @param core The internal OpenWGA core object
     */
    public static WGA get(final HttpServletRequest request, final HttpServletResponse response, final WGACore core) {
        return get(request, response, request.getSession(), core);
    }
    
    /**
     * Returns a WGA instance using the given JavaEE objects for a context, for use in HTTP servlet requests.
     * The instance provided may have no WebTML/TMLScript specific functionalities available.
     * @param request A HTTP request object
     * @param response A HTTP response object
     * @param session A HTTP session object
     * @param core The internal OpenWGA core object
     */
    public static WGA get(final HttpServletRequest request, final HttpServletResponse response, final HttpSession session, final WGACore core) {
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return null;
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return response;
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return request;
            }
            
            @Override
            public HttpSession getHttpSession() {
                return session;
            }
            
            @Override
            public WGACore getCore() {
                return core;
            }
            
            @Override
            public Logger getLog() {
                return core.getLog();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }

        });
    }
    
    /**
     * Returns a WGA instance using the given {@link JobContext} for a context, for use within WGA job tasks
     * The instance provided may have no WebTML/TMLScript/Request specific functionalities available.
     * @param jobContext A job context
     */
    public static WGA get(final JobContext jobContext) {
        
        TMLContext context = TMLContext.getThreadMainContext();
        if (context != null) {
            return WGA.get(context);
        }
        
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return null;
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return null;
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return null;
            }
            
            @SuppressWarnings("deprecation")
            @Override
            public WGACore getCore() {
                return jobContext.getWgaCore();
            }
            
            @Override
            public Logger getLog() {
                return jobContext.getLog();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public HttpSession getHttpSession() {
                return null;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }

        });
        
    }
    
    /**
     * Returns a WGA instance using the given {@link WGACore} object for a context.
     * The instance provided may have no WebTML/TMLScript/Request specific functionalities available.
     * @param core WGA core object
     */
    public static WGA get(final WGACore core) {
        
        TMLContext context = TMLContext.getThreadMainContext();
        if (context != null) {
            return WGA.get(context);
        }
        
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return null;
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return null;
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return null;
            }
            
            @Override
            public HttpSession getHttpSession() {
                return null;
            }
            
            @Override
            public WGACore getCore() {
                return core;
            }
            
            @Override
            public Logger getLog() {
                return core.getLog();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }

            
        });
        
    }
    
    /**
     * Returns a WGA instanceof using the given {@link JspHelper} as context
     * @param jspHelper
     * @return
     */
    public static WGA get(final JspHelper jspHelper) {
        
        return new WGA(new WGAContext() {
            
            @Override
            public TMLContext getTMLContext() {
                return null;
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return (HttpServletResponse) jspHelper.getPageContext().getResponse();
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return (HttpServletRequest) jspHelper.getPageContext().getRequest();
            }
            
            @Override
            public HttpSession getHttpSession() {
                return getServletRequest().getSession();
            }
            
            @Override
            public WGACore getCore() {
                return jspHelper.getCore();
            }

            @Override
            public Logger getLog() {
                return jspHelper.getCore().getLog();
            }
            
            @Override
            public WGAContext narrow() {
                return this;
            }
            
            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return null;
            }

        });
        
    }
    
    /**
     * Returns a WGA instance for an independent WebSocket session
     * @param wsSession The session
     */
    public static WGA get(final javax.websocket.Session wsSession) {
        
        return get(wsSession, null);
            
    }
    
    /**
     * Returns a WGA instance for an HTTP session
     * @param wsSession The session
     */
    public static WGA get(HttpSession session) {
        
        return get(null , session);
            
    }
    
    
    /**
     * Returns a WGA instance for an WebSocket session attached to a HTTP session
     * @param wsSession The session
     */
    public static WGA get(final javax.websocket.Session wsSession, final HttpSession httpSession) {
        
        return new WGA(new WGAContext() {

            @Override
            public WGACore getCore() {
                return WGACore.INSTANCE;
            }

            @Override
            public TMLContext getTMLContext() {
                return null;
            }

            @Override
            public HttpServletRequest getServletRequest() {
                return null;
            }

            @Override
            public HttpServletResponse getServletResponse() {
                return null;
            }

            @Override
            public HttpSession getHttpSession() {
                return httpSession;
            }

            @Override
            public javax.websocket.Session getWebsocketSession() {
                return wsSession;
            }

            @Override
            public Logger getLog() {
                return WGACore.INSTANCE.getLog();
            }

            @Override
            public WGAContext narrow() {
                return this;
            }

            @Override
            public boolean isIsolated() {
                return false;
            }
        });
            
    }

    
    private WGAContext _context;
    
    protected WGA(WGAContext context) {
        _context = context;
    }
    
    /**
     * Returns the WGACore object in context
     */
    public WGACore getCore() {
        return _context.getCore();
    }

    /**
     * reates a formatted date string from a {@link Date} object or a formatted number string from a {@link Number} object
     * @param toBeFormatted Either a {@link Date} or a {@link Number}
     * @param formatString The format pattern, just like known from {@link SimpleDateFormat} or {@link DecimalFormat}
     * @param language A language string determining the language for language dependent formattings
     * @return String representing the Date/Number in the specified format 
     * @throws UnavailableResourceException
     */
    public String format(Object toBeFormatted, String formatString, String language) throws WGException  {

        String formatTarget = null;

        if (toBeFormatted instanceof Number) {
            Number number = (Number) toBeFormatted;
            NumberFormat numFormat = getNumberFormat(formatString, getCore().languageCodeToLocale(language));
            return numFormat.format(number.doubleValue());
        }
        else if (toBeFormatted instanceof Date) {
            Date date = (Date) toBeFormatted;
            DateFormat dateFormat = getDateFormat(formatString, getCore().languageCodeToLocale(language));
            return dateFormat.format(date);
        }
        else {
            return null;
        }
    }
    


    /**
     * creates a formatted date string from a {@link Date} object or a formatted number string from a {@link Number} object
     * @param toBeFormatted Either a {@link Date} or a {@link Number}
     * @param formatString The format pattern, just like known from {@link SimpleDateFormat} or {@link DecimalFormat}
     * @return String representing the Date/Number in the specified format 
     * @throws UnavailableResourceException
     */
    public String format(Object toBeFormatted) throws WGException  {
        return format(toBeFormatted, "", null);
    }
    public String format(Object toBeFormatted, String formatString) throws WGException  {
        return format(toBeFormatted, formatString, null);
    }
    
    /**
     * Sorts a List, either by a {@link Comparator} or, if the list contains {@link WGDocument} instances, by one or more metadata fields.
     * The sorting is done in place, i.e. the given list object is sorted.
     * @param listObj The list to be sorted
     * @param comparatorObj Comparator object, either a {@link List} of metadata names, or a {@link String} containing a metadata name, or a {@link Comparator}
     * @param sortDir Sort direction, either "up" or "down".
     * @return The list
     * @throws UnavailableResourceException
     */
    @SuppressWarnings("unchecked")
    public List<Object> sortList(Object listObj, Object comparatorObj, int sortDir) throws WGException {


        if (listObj instanceof WGStructEntryList) {
            listObj = new ArrayList<Object>(((WGStructEntryList) listObj));
        }

        if (listObj instanceof List) {

            List<Object> lObj = (List<Object>) listObj;
            
            // Natural order or sort by toString() value for uncomparable objects
            if (comparatorObj == null) {
                Collections.<Object>sort(lObj, new ListComparatorByMeta(null, sortDir, (TMLContext) tmlcontext()));
            }
            
            // List of metadata names
            else if (comparatorObj instanceof List) {
                Collections.sort(lObj, new ListComparatorByMetaList((List<String>) comparatorObj, sortDir, (TMLContext) tmlcontext()));
            }

            //Java comparator
            else if (comparatorObj instanceof Comparator) {
                Collections.sort(lObj, (Comparator<Object>) comparatorObj);
            }

            // Single metadata name 
            else if (comparatorObj instanceof String) {
                Collections.sort(lObj, new ListComparatorByMeta((String) comparatorObj, sortDir, (TMLContext) tmlcontext()));
            }
            else {
                throw new IllegalArgumentException("Invalid comparator object: " + comparatorObj.getClass().getName());
            }
            return lObj;
        }
        else {
            return new ArrayList<Object>();
        }
    }

    /**
     * Removes multiple equal entries from a list.
     * This method is provided to the TMLScript API. In Java it has little sense as {@link Set}s can be used for the same thing.
     * @param lObj The list
     * @return List with removed duplicates
     */
    public List<Object> deleteDoublets(List<Object> lObj) {

        if (lObj == null) {
            return null;
        }

        java.util.HashSet<Object> hObj = new java.util.HashSet<Object>(lObj);
        List<Object> list = new ArrayList<Object>();
        list.addAll(hObj);
        return list;
        
    }


    /**
     * Calls a WebTML action
     * This can be used to call WebTML actions that are defined in any allowed way, including master and async actions. This however needs an WebTML environment in context.
     * In case of a master action the call is automatically executed in a separate session. Data changes done in this master action will not be automatically visible to the calling code as its current database session will not pick them up. To have the updated state in the calling code you can reopen the current session of changed databases after the action call, but note that this will drop all unsaved changes to these databases from the calling session.
     * In case of an async action the call of the action is triggered but the calling TMLScript code continues while the action is executed.
     * Parameters param1 to param5 are given as argument list "actionArgs", element 0 to 4. They will be available to the action as WebTML variables "tmlparam1" to "tmlparam5" and as List object "actionParams" containing the params as list elements 0 to 4.
     * 
     * @param context TMLContext ot execute the action on. Specify null to use one from WGA object context.
     * @param actionID ID of the action to call. This may be the ID of a <tml:action> tag or the name of a TMLScript module. If you want to call an action that is defined in a different applications design you may prefix the ID with the database key of this application plus a slash. So to call an action "actions:doit" from application "myapp", use "myapp/actions:doit" as ID parameter.
     * @param actionArgs Parameters for the action, available as "tmlparam1" (index 0) to "tmlparam5" (index 4)
     * @param baseReference The base reference from which to resolve the action id. 
     * @param globalScope The global expression scope under which to execute the action. Specify null to have none.
     * @return Return value of the action, null if there is none
     * @throws WGAServerException
     */
    public Object callAction(Context context, String actionID, List<Object> actionArgs, DesignResourceReference baseReference, GlobalExpressionScope globalScope) throws WGException {

        if (context == null) {
            context = tmlcontext();
        }
        
        TMLContext tmlContext = (TMLContext) context;
        TMLAction action = tmlContext.getActionByID(actionID, null, baseReference);
        if (action == null) {
            throw new WGAServerException("Could not retrieve action for ID '" + actionID + "'");
        }

        // Call action
        Object actionResult;
        try {
            TMLActionLink actionLink = action.createActionLink(null, actionArgs, tmlContext);
            Map<String, Object> globalScopeObjects = globalScope != null ? globalScope.getObjects() : Collections.<String,Object>emptyMap();
            actionResult = tmlContext.callCustomAction(action, actionLink, globalScopeObjects).getResult();
            return actionResult;
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception running action " + actionID, e);
        }

    }
    
    /**
     * Calls a WebTML action
     * This can be used to call WebTML actions that are defined in any allowed way, including master and async actions. This however needs an WebTML environment in context.
     * In case of a master action the call is automatically executed in a separate session. Data changes done in this master action will not be automatically visible to the calling code as its current database session will not pick them up. To have the updated state in the calling code you can reopen the current session of changed databases after the action call, but note that this will drop all unsaved changes to these databases from the calling session.
     * In case of an async action the call of the action is triggered but the calling TMLScript code continues while the action is executed.
     * Parameters param1 to param5 are given as argument list "actionArgs", element 0 to 4. They will be available to the action as WebTML variables "tmlparam1" to "tmlparam5" and as List object "actionParams" containing the params as list elements 0 to 4.
     * 
     * @param actionID ID of the action to call. This may be the ID of a <tml:action> tag or the name of a TMLScript module. If you want to call an action that is defined in a different applications design you may prefix the ID with the database key of this application plus a slash. So to call an action "actions:doit" from application "myapp", use "myapp/actions:doit" as ID parameter.
     * @param actionArgs Parameters for the action, available as "tmlparam1" (index 0) to "tmlparam5" (index 4)
     * @param baseReference The base reference from which to resolve the action id. 
     * @param globalScope The global expression scope under which to execute the action. Specify null to have none.
     * @return Return value of the action, null if there is none
     * @throws WGAServerException
     */
    public Object callAction(String actionID, List<Object> actionArgs, DesignResourceReference baseReference, GlobalExpressionScope globalScope) throws WGException {
        return callAction(null, actionID, actionArgs, baseReference, globalScope);
    }

    /**
     * encrypts a String
     * @param text
     * @return encrytped String
     * @throws GeneralSecurityException
     * @throws UnsupportedEncodingException
     */
    public String encryptString(String text) throws GeneralSecurityException, UnsupportedEncodingException{
    	return getCore().getSymmetricEncryptionEngine().encryptBase64Web(text.getBytes("UTF-8"));
    }
    /**
     * decrypts a string
     * @param secret
     * @return the decrypted string
     * @throws GeneralSecurityException
     * @throws IOException
     */
    public String decryptString(String secret) throws GeneralSecurityException, IOException{
    	return new String(getCore().getSymmetricEncryptionEngine().decryptBase64Web(secret), "UTF-8");
    }
    
    /**
     * Serializes an object to an encrypted string
     * The resulting string is a representation of the given object as string which is compressed as well as encrypted. It works with trivial data objects like {@link String}, {@link Number}, {@link Boolean} or {@link Date}, also with {@link List}, {@link Map} or JavaBeans containing those type of objects.
     * Uses this function to either use complex objects on places that only allow strings (for example on WebTML action parameters) or to prevent any data from being manipulated by the browser user when it needs to be transported to and returned from the browser.
     * Use {@link #deserializeObject(String)} to convert the string back to the originating object.
     * @param obj Object to serialize
     * @return The serialized object
     * @throws WGException  
     */
    public String serializeObject(Object obj) throws WGException {        
        try {
            String xml = getCore().getLibraryXStream().toXML(obj);
            byte[] zipped = WGUtils.zipString(xml);                
            String encrypted = getCore().getSymmetricEncryptionEngine().encryptBase64Web(zipped);
            return encrypted;
        }
        catch (GeneralSecurityException e) {
            throw new WGException("Failed to serialize object.", e);
        }                
    }
    
    /**
     * Converts a string created by {@link #serializeObject(Object)} back to the original object.
     * The object returned by this method is in fact an exact copy of the object that was used on serializeObject() with the exact same data. So in terms of object equality and identity it is "equal" but not "the same".
     * @param encrypted Serialized object
     * @return Deserialized object
     * @throws WGException  
     */
    public Object deserializeObject(String encrypted) throws WGException {        
        try {
            byte[] zipped = getCore().getSymmetricEncryptionEngine().decryptBase64Web(encrypted);
	        if (zipped == null) {
	            throw new IllegalArgumentException("Undecryptable serialized object");
	        }
        
            String xml = WGUtils.unzipString(zipped);
            Object obj = getCore().getLibraryXStream().fromXML(xml);
            return obj;
        } catch (Exception e) {
            throw new WGException("Failed to deserialize object.", e);
        }    
        
    }
    
    /**
     * Qualifies the given text with either a given or the current WebTML scope
     * This is the WGA Server API pendant to the dynamic WebTML attribute function "{scoped:text}". It can be used for any text that should be qualified with the current WebTML scope, so it won't collide with other usages of this text in its domain.
     * @param str The string to scope.
     * @param scope The scope. Specify null to use scope from current WebTML context
     * @return The scoped string
     * @throws UnavailableResourceException
     */
    public String scoped(String str, String scope) throws WGException {
                    
        if (scope == null) {
            scope = design().getWebtmlScope();
        }
        
        if (scope != null) {
            return str + SCOPE_DIVIDER + scope;
        }
        else {
            return str;
        }
        
    }
    
    /**
     * Qualifies the given text with the current WebTML scope
     * This is the WGA Server API pendant to the dynamic WebTML attribute function "{scoped:text}". It can be used for any text that should be qualified with the current WebTML scope, so it won't collide with other usages of this text in its domain.
     * @param str The string to scope.
     * @return The scoped string
     * @throws UnavailableResourceException
     */
    public String scoped(String str) throws WGException {
        return scoped(str, null);
    }

    public Plugin plugin() throws WGException {
        TMLContext cx = fetchTMLContext();
        if (cx != null) {
        	return plugin(cx.db());
        }
        else {
            throw new UnavailableResourceException("Cannot retrieve current plugin as we are outside WebTML environment");
        }
    }
    
    /**
     * Returns a Plugin object for any OpenWGA plugin of the given unique nam.
     * This method can be used to retrieve information about any active OpenWGA plugin. It will not return invalid or inactive plugins. 
     * @param uniqueName Plugin unique name
     * @return Plugin object
     * @throws WGAPIException
     * @throws WGException
     */
    public Plugin plugin(String uniqueName) throws WGException {
        
        WGAPlugin plugin = getCore().getPluginSet().getPluginByUniqueName(uniqueName);
        if (plugin == null || !plugin.isActive() || !plugin.isValid()) {
            return null;
        }
        WGDatabase db = db(plugin.buildDatabaseKey());
        if (db != null) {
            return new Plugin(this, db);
        }
        else {
            return null;
        }
            
    }

    /**
     * Returns a Plugin object for the WGAPI database object.
     * The WGAPI database object must belong to the database of a plugin. 
     * @param db The WGAPI database object
     * @return Plugin object
     * @throws WGAPIException
     * @throws WGException
     */
    public Plugin plugin(WGDatabase db) throws WGException {
        return new Plugin(this, db);
    }
    
    /**
     * Returns a Lucene object providing functionalities related to fulltext search
     * @param context WebTML context for the lucene object.
     * @throws WGAServerException 
     */
    public Lucene lucene(Context context) throws WGException {
        return new Lucene(this, (TMLContext) context);        
    }
    
    /**
     * Converts a string containing date/time information to a {@link Date} object
     * This method parses the given string based on a custom date format and return it as date object. Use date format strings known from {@link java.text.SimpleDateFormat}
     * @param date The date string
     * @param format The format string
     * @param language The language string of the language to use for parsing language dependent date parts
     * @return The parsed date
     * @throws WGException
     * @throws ParseException If the string is unparseable for the given date format
     */
    public Date parseDate(String date, String format, String language) throws WGException, ParseException {
        return getDateFormat(format, getCore().languageCodeToLocale(language)).parse(date);
    }
    
    /**
     * Converts a string containing date/time information to a {@link Date} object
     * This method parses the given string based on a custom date format and return it as date object. Use date format strings known from {@link java.text.SimpleDateFormat}
     * @param date The date string
     * @param format The format string
     * @return The parsed date
     * @throws WGException
     * @throws ParseException If the string is unparseable for the given date format
     */
    public Date parseDate(String date) throws WGException, ParseException {
        return parseDate(date, "", null);
    }
    public Date parseDate(String date, String format) throws WGException, ParseException {
        return parseDate(date, format, null);
    }
    
    /**
     * Creates a new {@link Date} object for the current time.
     * This method is provided for TMLScript. In Java one can simply construct a {@link Date} object.
     * @param includeMillis true ot include milliseconds in time
     */
    public Date createDate(boolean includeMillis) throws WGException {
        if (includeMillis) {
            return new Date();
        }
        else {
            return new Date(WGUtils.cutoffTimeMillis(System.currentTimeMillis()));
        }
    }
    
    /**
     * Modifies a date by adding/substracting a certain timespan
     * This is a utility method for simple modifications to dates without the use of {@link Calendar} objects. it returns a new {@link Date} object which represents the date of the parameter "date" plus/minus a given timespan.
     * In order to add a timespan one is first to choose which time unit to use via argument "unit" and then the amount of units to add via argument "amount". In order to substract a timespan from the date a negative amount value is to be used.
     * The unit is determined by the date string character that would be used by {@link SimpleDateFormat} for the time unit:
     * <ul>
     * <li> d" or "D" - Day
     * <li> "M" - Month
     * <li> "w" or "W" - Week
     * <li> "y" - Year
     * <li> "H" or "h" - Hour
     * <li> "m" - Minute
     * <li> "s" - Second
     * <li> "S" - Millisecond
     * <ul>
     * @param date Date to modify
     * @param unit Modify unit. See table above.
     * @param amount Amount of date units to add to the date. Specify negative amounts to substract. 
     * @return The modified date
     * @throws WGAServerException
     */
    public Date modifyDate(Date date, String unit, int amount) throws WGException {
        
        int field;
        switch (unit.charAt(0)) {
            
            case 'd':
            case 'D':
                field = GregorianCalendar.DAY_OF_MONTH;
                break;
                
            case 'M':
                field = GregorianCalendar.MONTH;
                break;
                
            case 'w':
            case 'W':
                field = GregorianCalendar.WEEK_OF_YEAR;
                break;
                
            case 'y':
                field = GregorianCalendar.YEAR;
                break;
                
            case 'H':
            case 'h':
                field = GregorianCalendar.HOUR;
                break;
                
            case 'm':
                field = GregorianCalendar.MINUTE;
                break;
                
            case 's':
                field = GregorianCalendar.SECOND;
                break;
                
            case 'S':
                field = GregorianCalendar.MILLISECOND;
                break;
                
            
            default:
                throw new WGAServerException("Unknown date field literal: " + unit);
            
            
        }
        
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(date);
        cal.add(field, amount);
        return cal.getTime();
        
    }
    
    /**
     * Creates a new TMLForm object as configured by a FormInfo object.
     * While TMLForm objects are mostly created implicitly by the WebTML tag <tml:form> this method provides the possibility to create a form in Java. This is only useful in WebTML environments where the form can actually be used.
     * The created form object can then be used by a subsequent <tml:form> tag to offer it for editing. For that to work the id of the form tag must match the ID of the created form object.
     * @param formInfo Form info object
     * @return A new TMLForm object
     * @throws WGException
     */
    public Form createForm(FormInfo formInfo) throws WGException {
        return ((TMLContext) tmlcontext()).createform((TMLFormInfo) formInfo);
    }
    
    /**
     * Creates a new FormInfo object
     * Objects of this type define settings for creating TMLForm objects from Java and TMLScript. Use this object on method {@link #createForm(FormInfo)} to finally create a TMLForm object.
     * This method creates form info objects that belong to the design of the current WebTML context and behave according to its settings.
     * @param id Form id 
     * @param htmlInput Determines if the form will accept plain HTML inputs
     * @param persistent Determines if the form will be persistent
     * @return new form info object
     * @throws WGException
     */
    public FormInfo createFormInfo(String id, boolean htmlInput, boolean persistent) throws WGException {
        FormInfo formInfo = ((TMLContext) tmlcontext()).createforminfo(id, htmlInput, persistent);
        
        if (isTMLContextAvailable()) {
            formInfo.setDefinition(design());            
        }
        return formInfo;
    }
    
    /**
     * Creates an ImageScaler object to perform scaling operations on an image file.
     * ImageScaler objects are capable of scaling images to custom sizes. This method needs the image to be available as a file in the servers file system.
     * @param file The file to scale
     * @return An image scaler object with the file loaded
     * @throws WGException
     * @throws ModuleInstantiationException
     * @throws IOException
     */
    public ImageScaler createImageScaler(File file) throws WGException, IOException {
        
        FileInputStream is = new FileInputStream(file);
        ImageScaler scaler = null;
        try {
             scaler = createImageScaler(is);
        }
        finally {
            is.close();
        }
        
        scaler.setSourceFileName(file.getName());
        return scaler;
        
    }
    
    /**
     * Creates an ImageScaler object to perform scaling operations on an image provided by an input stream
     * ImageScaler objects are capable of scaling images to custom sizes.
     * @param in Stream providing image data
     * @return An image scaler object with the file loaded
     * @throws WGException
     * @throws ModuleInstantiationException
     * @throws IOException
     */
    public ImageScaler createImageScaler(InputStream in) throws WGException, IOException {
            
        ImageScaler scaler = ImageScalerFactory.createImageScaler(_context.getCore());
        if (scaler == null) {
            throw new WGNotSupportedException("There is no (module-based) Image Scaler registered. Please install an Image Scaler Plugin or update an existing one.");
        }
        scaler.load(in);
        
        return scaler;
        
    }
    
    /**
     * Creates a list object.
     * This method is provided for TMLScript. In Java one can simply instantiate an {@link ArrayList} or other list variants.
     */
    public List<Object> createList() {
        return new ArrayList<Object>();
    }
    
    /**
     * Creates a list object from an array..
     * This method is provided for TMLScript. In Java one can simply use {@link Arrays#asList(Object...)}.
     */
    
    public List<Object> createList(Object[] array) {
        return new ArrayList<Object>(Arrays.asList(array));
    }

    /**
     * Creates a list object from a collection..
     */
    public List<Object> createList(Collection<Object> objects) {
        return new ArrayList<Object>(objects);
    }

    /**
     * Creates an list object from a collection string with delimiter.
     * This method is provided for TMLScript. In Java one can simply use {@link WGUtils#deserializeCollection(String, String)} and variants.
     * @param collection
     * @param delimiter
     */
    public List<String> createList(String collection, String delimiter) {
        return WGUtils.deserializeCollection(collection, delimiter);
    }
    
    /**
     * Creates a map, known to TMLScript as "lookup table", initializing it with the given maps entries
     * This method is provided for TMLScript. In Java one can simply instantiate a {@link HashMap} or other map variants.
     * @param map Map providing entries
     * @return New map
     */
    public Map<Object,Object> createLookupTable(Map<Object,Object> map) {
        return new HashMap<Object,Object>(map);
    }
    
    /**
     * Creates a map, known to TMLScript as "lookup table"
     * This method is provided for TMLScript. In Java one can simply instantiate a {@link HashMap} or other map variants.
     * @return New map
     */
    public Map<Object,Object> createLookupTable() {
        return new HashMap<Object,Object>();
    }
    
    /**
     * creates a map. Same as createLookupTable() but naming is more intuitive.
     * @param map
     * @return New map
     */
    public Map<Object,Object> createMap(Map<Object,Object> map) {
        return new HashMap<Object,Object>(map);
    }
    /**
     * creates an empty map. Same as createLookupTable() but naming is more intuitive.
     * @return New map
     */
    public Map<Object,Object> createMap() {
        return new HashMap<Object,Object>();
    }
    
    
    /**
     * Creates a Mail object for sending e-mails.
     * This variant uses the mail configuration of the OpenWGA server.
     * @return A new Mail object
     * @throws UnsupportedEncodingException
     * @throws UnavailableResourceException
     */
    public SmtpMail createMail() throws UnsupportedEncodingException, WGException {

        WGAMailConfiguration mailConfig = getCore().getMailConfig();
        if (mailConfig != null){
            return new SmtpMail(mailConfig);
        }
        else {
            throw new UnavailableResourceException("No mail configuration available on this server");
        }
        
    }

    /**
     * Creates a Mail object for sending e-mails.
     * This variant uses the mail configuration of the OpenWGA server.
     * @param config - a map containing config params for the mail object
     * @return A new Mail object
     * @throws UnsupportedEncodingException
     * @throws UnavailableResourceException
     */
    public SmtpMail createMail(Map<String,Object> config) throws UnsupportedEncodingException, WGException {

        WGAMailConfiguration mailConfig = getCore().getMailConfig();
        if (mailConfig != null){
        	SmtpMail mail = new SmtpMail(mailConfig);
        	try{
                
                Boolean encodeText = (Boolean)config.get("encodeText");
                if(encodeText!=null)
                	mail.setEncodeText(encodeText);
                else mail.setEncodeText(true);

                String from = (String)config.get("from");
                if(from!=null)
                	mail.setFrom(from);
                
                Object to = config.get("to");
                if(to!=null){
		            if(to instanceof List)
		            	mail.setTo((List<String>)to);
		            else mail.setTo((String)to);
                }
                
                Object cc = config.get("cc");
                if(cc!=null){
		            if(cc instanceof List)
		            	mail.setCc((List<String>)cc);
		            else mail.setCc((String)cc);
                }
                
                Object bcc = config.get("bcc");
                if(bcc!=null){
		            if(bcc instanceof List)
		            	mail.setBcc((List<String>)bcc);
		            else mail.setBcc((String)bcc);
                }
                
                if(config.get("mimeType")!=null)
                	mail.setMimeType((String)config.get("mimeType"));
                if(config.get("subject")!=null)
                	mail.setSubject((String)config.get("subject"));
                if(config.get("body")!=null)
                	mail.setBody((String)config.get("body"));
        	}
        	catch(Exception e){
        		getLog().error("Mail config error", e);
        		return null;
        	}
            return mail;
        }
        else {
            throw new UnavailableResourceException("No mail configuration available on this server");
        }
        
    }

    /**
     * Creates a Mail object for sending e-mails.
     * This variant receives host, username and password of the SMTP account.
     * @param smtpHost The host name
     * @param username The user name of the account
     * @param password The password of the account
     * @return A new Mail object
     * @throws UnsupportedEncodingException
     */
    public SmtpMail createMail(String smtpHost, String username, String password) throws WGException, UnsupportedEncodingException {
        return new SmtpMail(smtpHost, username, password);
    }
    
    /**
     * Converts a string containing numeric information into a Number object
     * This method parses the given string based on a custom number format and return it as {@link Number} object. You must specify the number format that the string contains in parameter "format" using the syntax known from {@link DecimalFormat}.
     * @param format The number format string
     * @param number The number string
     * @param language Language string of language to use for language-dependent parts of the number string.
     * @return The parsed number
     * @throws ParseException If the string is unparseable for the given number format
     * @throws UnavailableResourceException
     */
    public Number parseNumber(String number, String format, String language) throws ParseException, WGException {
        return getNumberFormat(format, getCore().languageCodeToLocale(language)).parse(number);
    }
    
    /**
     * Converts a string containing numeric information into a Number object
     * This method parses the given string based on a custom number format and return it as {@link Number} object. You must specify the number format that the string contains in parameter "format" using the syntax known from {@link DecimalFormat}.
     * @param format The date format string
     * @param number The number string
     * @return The parsed number
     * @throws ParseException If the string is unparseable for the given number format
     * @throws UnavailableResourceException
     */
    public Number parseNumber(String number) throws ParseException, WGException {
        return parseNumber(number, "", null);
    }
    public Number parseNumber(String number, String format) throws ParseException, WGException {
        return parseNumber(number, format, null);
    }

    /**
     * Returns the WGAPI database object for the given database key
     * When accessing a database that has not yet been accessed in this request OpenWGA will try to login the user to this database, using whatever login information is available on the session. If that fails - bc. the user has no access - the database is still returned but has no open session. You can test the availability of an open session by {@link WGDatabase#isSessionOpen()}
     * The method may return OpenWGA content stores as well as data sources.
     * The method returns null if no database of the given dbkey is connected.
     * @param dbKey Database key
     * @return The WGAPI database object
     * @throws UnavailableResourceException
     * @throws WGAPIException
     * @throws WGException
     */
    public WGDatabase db(String dbKey) throws WGException  {
        return db(dbKey, true);
    }
    
    /**
     * Returns the WGAPI database object for the given database key
     * When accessing a database that has not yet been accessed in this request  and parameter "open" is true then OpenWGA will try to login the user to this database, using whatever login information is available on the session. If that fails - bc. the user has no access - the database is still returned but has no open session. You can test the availability of an open session by {@link WGDatabase#isSessionOpen()}
     * The method may return OpenWGA content stores as well as data sources.
     * The method returns null if no database of the given dbkey is connected.
     * @param dbKey Database key
     * @param open Whether a session should be opened on the database using the environments authentication information
     * @return The WGAPI database object
     * @throws UnavailableResourceException
     * @throws WGAPIException
     * @throws WGException
     */
    public WGDatabase db(String dbKey, boolean open) throws WGException  {
        
        WGDatabase db = null;
        
        if (isTMLContextAvailable()) {
            dbKey = design().resolveDbKey(dbKey);
            
            // Fetch the db from current design. Necessary in situations where the db is
            // Just connecting and not yet registered in cores contentdbs
            WGDatabase designDb = fetchTMLContext().getDesignContext().getDesignDB();
            if (designDb.getDbReference().equals(dbKey)) {
                db = designDb;
            }
        }
        
        if (db == null) {
            db = getCore().getContentdbs().get(dbKey);
        }
        
        if (db != null) {
            if (open) {
                openDatabase(db);
            }
            return db;
        }
        
        return null;
        
    }

    /**
     * Opens an WGAPI database object with the credentials available in the current environment.
     * In WebTML/TMLScript environments this method tries to use a user login that is available.
     * In other environments it opens a master session on the database.
     * This method ist mostly used internally from WebTML/Server API to open sessions on databases.
     * Occasionally this may be used to open WGAPI databases that were retrieved from other sources. 
     * @param db The database to open.
     * @return true if the database is now open, false if not
     * @throws WGException
     * @throws UnavailableResourceException
     * @throws WGAPIException
     */
    @CodeCompletion
    public boolean openDatabase(WGDatabase db) throws WGException {
        getOpenDatabaseService().openDatabase(db);        
        return db.isSessionOpen(); 
    }
    
    /**
     * Returns a WGAPI HDB object for an application
     * The HDB database object is a tool to use a OpenWGA content store as a simple hierarchical database for data storage.
     * Since OpenWGA 5.3 we recommend usage of {@link HDBModel}, which builds upon this interface instead of directly using the HDB object.
     * @param dbKey Database key of the application
     * @return HDB database object
     * @throws WGException
     */
    public WGHierarchicalDatabase hdb(String dbKey) throws WGException {
        
        WGDatabase db = db(dbKey);
        if (db != null) {
            return WGHierarchicalDatabase.getOrCreateInstance(db);
        }
        
        return null;
        
    }
    
    /**
     * Encodes some text with a WebTML encoding
     * This is the server API pendant of WebTML attribute encode. You can use all encodings that are available to the current OpenWGA runtime and are also usable on the attribute.
     * @param encoding Name of the encoding, f.e. "html", "rtf"
     * @param obj The text to encode. Non strings will internally be converted to strings.
     * @return Encoded text
     * @throws FormattingException
     */
    public String encode(String encoding, Object obj) throws WGException, FormattingException {
    	return encode(encoding, obj, null);
    }
    public String encode(String encoding, Object obj, TMLContext ctx) throws WGException, FormattingException {
        
            List<TextChunk> chunks = new ArrayList<TextChunk>();
            chunks.add(new TextChunk(TextChunk.Origin.INPUT, "text/plain", String.valueOf(obj)));
        
            Iterator<String> encoders = WGUtils.deserializeCollection(encoding, ",", true).iterator();
            while (encoders.hasNext()) {
                String encoderName = encoders.next();
                
                // Modern WGA encoder, processing chunks
                WGAEncoder encoder = null;
                Map<String,String> flags = new HashMap<String, String>();
                String encoderModuleName = parseEncoderFlags(encoderName, flags);
                ModuleDefinition def = getCore().getModuleRegistry().getModuleDefinitionByKey(WGAEncoderModuleType.class, encoderModuleName);
                if (def != null) {
                    try {
                        def.testDependencies();
                        encoder = (WGAEncoder)  getCore().getModuleRegistry().instantiate(def);
                    }
                    catch (ModuleDependencyException e) {
                        if (!"true".equals(flags.get("optional"))) {
                            throw new WGAServerException("The WebTML encoder has missing dependencies: " + encoderName, e);
                        }
                        else {
                            continue;
                        }
                    }
                    catch (ModuleInstantiationException e) {
                        if (!"true".equals(flags.get("optional"))) {
                            throw new WGAServerException("An exception occured instantiating WebTML encoder '" + encoderName + "'", e);
                        }
                        else {
                            continue;
                        }
                    }
                }
                
                // Legacy WebTML encoder, just converting a string into another string, without chunk handling. They are only fed chunks of origin INPUT.
                if (encoder == null) {
                	
                    ObjectFormatter formatter = getCore().getEncodingFormatter(encoderName, 
                    		(ctx!=null ? ctx : isTMLContextAvailable() ? (TMLContext) tmlcontext() : null));
                    if (formatter != null) {
                        encoder = new EncodingFormatterEncoder(formatter);
                    }
                }
                
                // Encode
                if (encoder != null) {
                    chunks = processEncoder(chunks, encoder, flags);
                }
                else if (!"true".equals(flags.get("optional"))) {
                    throw new FormattingException("WebTML Encoder '" + encoderName + "' is unknown");
                }
               
            }
            
            return WGUtils.serializeCollection(chunks, "");

        
    }

    private List<TextChunk> processEncoder(List<TextChunk> chunks, WGAEncoder encoder, Map<String, String> flags) throws WGException {
        List<TextChunk> allOutputChunks = new ArrayList<TextChunk>();
        
        if ("true".equals(flags.get("nochunks"))) {
            List<TextChunk> oneChunk = new ArrayList<TextChunk>();
            oneChunk.add(new TextChunk(TextChunk.Origin.INPUT, "text/plain", WGUtils.serializeCollection(chunks, "")));
        }
        
        for (TextChunk inputChunk : chunks) {
            if (inputChunk.getOrigin() != TextChunk.Origin.INPUT && encoder instanceof WGAInputOnlyEncoder) {
                allOutputChunks.add(inputChunk);
                continue;
            }
            
            EncoderOutput output = new EncoderOutput(inputChunk);
            encoder.encode(this, inputChunk, output, flags);
            List<TextChunk> outputChunks = output.getOutputChunks();
            for (WGAEncoder chainedEncoder : output.getChainedEncoders()) {
                outputChunks = processEncoder(outputChunks, chainedEncoder, flags);
            }
            allOutputChunks.addAll(outputChunks);
        }
        
        return allOutputChunks;        
    }

    private String parseEncoderFlags(String encode, Map<String, String> flags) {
        int pos = encode.indexOf(":");
        if (pos != -1) {
            String strFlags = encode.substring(pos + 1);
            for (String flagText : WGUtils.deserializeCollection(strFlags, "|")) {
                int equalPos = flagText.indexOf("=");
                if (equalPos != -1) {
                    flags.put(flagText.substring(0, equalPos), flagText.substring(equalPos + 1));
                }
                else {
                    flags.put(flagText, "true");
                }
            }
            
            return encode.substring(0, pos);          
        }
        else {
            return encode;
        }
    }
        
    /**
     * Creates a WGAPI URLBuilder object to contruct and modify URLs
     * @param context A WebTML context which is used as application context for the URL to generate (for example to retrieve configurations about protocol ports to use). Specify null to use implicit environment context. 
     * @param urlStr Some URL that you want the URLBuilder to modify. It may be an absolute URL or a relative one that will be complemented with the URL of the current request.
     * @return URL Builder object
     * @throws WGException
     */
    public URLBuilder urlBuilder(Context context, String urlStr) throws WGException {
        
        // Defaulting the URL string
        Database enforceDatabaseSettings = null;
        if (urlStr == null) {
            if (context != null) {
                urlStr = context.contenturl();
                enforceDatabaseSettings = database(context.db());
            }
            else {
                WGA unlocked = Unlocker.unlock(this);
                if (unlocked.isRequestAvailable()) {
                    urlStr = unlocked.getRequestURL();
                    if (unlocked.isTMLContextAvailable()) {
                        enforceDatabaseSettings = unlocked.database(unlocked.tmlcontext().context("main").db());
                    }
                }
                else if (unlocked.isTMLContextAvailable()) {
                    urlStr = unlocked.tmlcontext().contenturl();
                    enforceDatabaseSettings = unlocked.database(unlocked.tmlcontext().db());
                }
            }
        }
        
        // The base URL to resolve relative links
        String wgaUrlStr = server().getBaseURL();
        if (WGUtils.isEmpty(wgaUrlStr)) {
            throw new WGException("No OpenWGA Base URL could be determined");
        }

        // Constructing the URL builder
        try {
            URL url = new URL(new URL(wgaUrlStr), urlStr);
            
            // Use a given TMLContext to build the WGA object to pass on to the builder
            WGA wga = this;
            if (context != null) {
                wga = WGA.get(context);
            }
            
            URLBuilder urlBuilder = new URLBuilder(wga, url);
            if (enforceDatabaseSettings != null) {
                urlBuilder.enforceDatabaseSettings(enforceDatabaseSettings);
            }
            urlBuilder.setAbsoluteUrlGiven(urlStr.contains("://"));
            return urlBuilder;
        }
        catch (Exception e) {
            throw new WGException("Failed to create url builder for url '" + urlStr + "' on base url '" + wgaUrlStr + "'", e); 
        }
    }
    
    /**
     * Returns the URL by which this request was called
     * This is the original URL which was used to call the current WebTML request. It may differ from the URL that was interpreted to determine the right WebTML/Content to render as this might have been modified by OpenWGA filters.
     * @throws UnavailableResourceException
     * @{@link Deprecated} Use Call object on {@link #call()} to retrieve request information
     */
    public String getRequestURL() throws WGException {
        return String.valueOf(getRequest().getAttribute(WGACore.ATTRIB_REQUESTURL));
    }

    /**
     * Creates a WGAPI URLBuilder object to contruct and modify URLs, using the current request URL
     * @param context A WebTML context which is used as application context for the URL to generate (for example to retrieve configurations about protocol ports to use). Specify null to use implicit environment context. 
     * @return URL Builder object
     * @throws WGException
     */
    public URLBuilder urlBuilder(Context context) throws WGException {
        return urlBuilder(context, null);
    }
    
    /**
     * Creates a WGAPI URLBuilder object to contruct and modify URLs.
     * @param urlStr Some URL that you want the URLBuilder to modify. It may be an absolute URL or a relative one that will be complemented with the URL of the current request.
     * This variant uses an implicit TMLContext for application context.
     * @return URL Builder object
     * @throws WGException 
     */
    public URLBuilder urlBuilder(String urlStr) throws WGException {
        return urlBuilder(null, urlStr);
    }
    
    /**
     * Creates a WGAPI URLBuilder object to contruct and modify URLs, using the current request URL
     * @return URL Builder object
     * @throws WGException
     */
    public URLBuilder urlBuilder() throws WGException {
        return urlBuilder(null, null);
    }
    
    /**
     * Redirects the users browser to a different URL
     * This method is only valid in WebTML environments and is only effective on a request if no content has yet been sent back to the browser. It then cancels the current request (the called WebTML page is neither rendered nor sent to the browser) and directly instructs the users browser to head to the given URL.
     * When used in an AJAX action this method will cancel the rendering of the portlet and trigger a reload of the complete webpage. All other HTTP response or cookie settings that may have been set before in the request are ineffective if this operation is called.
     * @param url The URL to redirect to
     * @throws UnavailableResourceException
     * @throws IOException
     */
    public void redirectTo(String url) throws WGException, IOException {
    	if(isIsolated()){
    		getLog().error("WGA.redirectTo() called in isolated environment. No request/response availabe.");
    	}
    	else ((TMLContext) tmlcontext()).redirectto(url);
    }
    
    /**
     * Redirects the users browser to the URL of the current TMLContext (without url parameters)
     * @throws WGException 
     * @throws IOException 
     */
    public void reloadPage() throws WGException, IOException  {
    	TMLContext ctx = ((TMLContext) tmlcontext());
    	redirectTo(ctx.contenturl());
    }
    
    /**
     * Builds options for WebTML form inputs from a list of contents
     * Use this method to build the options of WebTML form inputs whose choices are based on content documents. This will be the case if you want to store them as content relations, but may also be the case if you want to store document keys for a choice of documents.
     * The method takes a list of content documents and builds a list of option strings from them. The titles of these options are either the titles of the content documents or the result of a title expression given as parameter. The option values will always be the struct keys of the documents in the list (which is mostly used to store relations to documents of equal languages).
     * Normally the returned options list will only host entries for the content documents in the list. If you specify the "emptyTitle" parameter it will also host an entry for an empty value at the top of the list.
     * @param contents List of WGAPI content objects that should be used to calculate the options
     * @param titleExpr A TMLScript expression that is evaluated against the individual contents to form the title of the options. Omit it to simply use the content titles as option titles.
     * @param emptyTitle A title that is used for an entry that represents an empty value. Omit it to have no empty value entry.
     * @return A list of strings, representing input options
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public List<String> buildOptions(Iterable<WGContent> contents, String titleExpr, String emptyTitle) throws WGException {
        
        RhinoExpressionEngine runtime = ExpressionEngineFactory.getTMLScriptEngine();
        
        List<String> options = new ArrayList<String>();
        if (emptyTitle != null) {
            options.add(String.valueOf(emptyTitle) + "|" + TMLForm.RELATION_NULLPLACE_HOLDER);
        }
        
        for (WGContent content : contents) {
            String title = content.getTitle();
            if (titleExpr != null) {
                TMLContext conContext = (TMLContext) tmlcontext().context(content);
                ExpressionResult result = runtime.evaluateExpression(titleExpr, conContext, ExpressionEngine.TYPE_EXPRESSION, null);
                if (!result.isError()) {
                    title = String.valueOf(result.getResult());
                }
            }
            options.add(title + "|" + content.getStructKey());
        }
        
        return options;
        
    }
    
    /**
     * Removes all daytime information from a date
     * This method returns a new date which represents the date give but has hour, minute, second and millisecond information set to 0. So this date represents the day at second 0.
     * This method can be useful if you want to compare dates only on behalf of the day that they represent. Dates that are on the same day but with different daytimes will be equal when run through this method.
     * @param date The date
     * @return A date object with resetted daytime information
     */
    public Date dateOnly(Date date) throws WGException {
        return WGUtils.dateOnly(date);
    }

    /**
     * Removes all day information from a date
     * This method returns a new date which represents the date give but has day, month and day information set to 0. So this date represents the time at day 1 on year 1.
     * This method can be useful if you want to compare dates only on behalf of the time that they represent. Dates that are on the same time but for different days will be equal when run through this method.
     * @param date The date
     * @return A date object with resetted day information
     */
    public Date timeOnly(Date date) throws WGException {
        return WGUtils.timeOnly(date);
    }
    
    /**
     * Creates a Java Calendar object offering diverse functionality to modify date values
     * This method is provided for TMLScript. In Java one can simly instantiate a {@link GregorianCalendar}.
     * @param date The date to represent
     * @return The calendar object
     */
    public Calendar createCalendar(Date date) throws WGException {
        Calendar cal = new GregorianCalendar();
        if (date != null) {
            cal.setTime(date);
        }
        return cal;
    }

    /**
     * Creates a Java Calendar object offering diverse functionality to modify date values
     * This method is provided for TMLScript. In Java one can simly instantiate a {@link GregorianCalendar}.
     * @return The calendar object
     */
    public Calendar createCalendar() throws WGException {
        Calendar cal = new GregorianCalendar();
        return cal;
    }
    
    /**
     * Returns a design context object for the given database
     * This will only work if the database represents an OpenWGA application.
     * @param db The database
     * @throws UnavailableResourceException
     */
    public Design design(WGDatabase db) throws WGException {
        return new Design(this, db);
    }
    
    /**
     * Returns a design context resembling the given design for the current WGA context
     * @param otherDesign Other design
     * @throws WGException
     */
    public Design design(Design otherDesign) throws WGException {
        TMLDesignContext designContext = design().getDesignContext();
        return new Design(this, designContext.createContextDelegate(designContext.getDesignDB(), otherDesign.getBaseReference().toString()));
    }
    
    /**
     * Returns a design context object for the current environment
     * This will only work in WebTML/TMLScript environments and returns the design of the main context app.
     * @throws UnavailableResourceException
     */
    public Design design() throws WGException {
        return new Design(this, ((TMLContext) context()).getDesignContext());
    }
    
    /**
     * Returns a design context object for the database of the given key
     * This will only work if the database represents an OpenWGA application.
     * @param dbKey The database key
     * @throws UnavailableResourceException
     * @throws WGException
     */
    public Design design(String dbKey) throws WGException {
    	try{
    		return new Design(this, dbKey);
    	}
    	catch(Exception e){
    		getLog().error("Unable to determine design for dbkey '" + dbKey + "'");
    		throw e;
    	}
    }
    
    /**
     * Returns a design context object for the given design resource reference
     * @param ref The reference
     * @throws WGException
     */
    public Design design(DesignResourceReference ref) throws WGException {
        ref = ref.normalize();
        
        if (ref.getDesignApp() != null) {
            return design(ref.getDesignApp()).resolve(ref.getResourceName());
        }
        else {
            return design().resolve(ref.getResourceName());
        }
        
    }
    
    /**
     * Returns a design context object for the given WebTML design context
     * The parameter object is an object of the WebTML implementation.
     * @param designContext The WebTML design context
     * @throws UnavailableResourceException
     * @throws WGException
     */
    @CodeCompletion
    public Design design(TMLDesignContext designContext) throws WGException {
        return new Design(this, designContext);
    }
    
    /**
     * Returns an application object for the current environment
     * This will only work in WebTML/TMLScript environments and returns the object of the main context app.
     * This will only work if the main context database represents an OpenWGA application.
     * @throws WGAServerException
     */
    public App app() throws WGException {
        TMLContext cx = fetchTMLContext();
        if (cx != null) {
            return app(cx.db());
        }
        else {
            throw new UnavailableResourceException("Cannot retrieve current app as we are outside WebTML environment");
        }
    }
    
    /**
     * Returns an application object for the given database
     * This will only work if the database represents an OpenWGA application.
     * @param db The database
     * @throws WGAServerException
     */
    public App app(WGDatabase db) throws WGException {
        return new App(this, db);
    }
    
    /**
     * Returns an application object for the WebTML context
     * This will only work if the context database represents an OpenWGA application.
     * @param cx The WebTML context
     * @throws WGAServerException
     */
    public App app(Context cx) throws WGException {
        return app(cx.db());
    }

    /**
     * Returns an application object for the database of the given key
     * This will only work if the database represents an OpenWGA application.
     * @param dbKey Key of the database
     * @throws WGAServerException
     */
    public App app(String dbKey) throws WGException {
        
        WGDatabase db = db(dbKey);
        if (db == null) {
            return null;
        }
        
        return app(db);
        
    }
    
    /**
     * Returns a data source object for the current environment
     * This will only work in WebTML/TMLScript environments and returns the object of the main context data source.
     * This will only work if the main context database represents an OpenWGA data source.
     * @throws WGAServerException
     */
    public DataSource dataSource() throws WGException {
        return dataSource(tmlcontext().db());
    }
    
    /**
     * Returns a data source object for the given database
     * This will only work if the database represents an OpenWGA data source.
     * @param db The database
     * @throws WGAServerException
     */
    public DataSource dataSource(WGDatabase db) throws WGException {
        return new DataSource(this, db);
    }
    
    /**
     * Returns a data source object for the WebTML context
     * This will only work if the context database represents an OpenWGA data source.
     * @param cx The WebTML context
     * @throws WGAServerException
     */
    public DataSource dataSource(Context cx) throws WGException {
        return new DataSource(this, cx.db());
    }
    
    /**
     * Returns an application object for the database of the given key
     * This will only work if the database represents an OpenWGA application.
     * @param dbKey Key of the database
     * @throws WGAServerException
     */
    public DataSource dataSource(String dbKey) throws WGException {
        
        WGDatabase db = db(dbKey);
        if (db == null) {
            return null;
        }
        
        return dataSource(db);
        
    }
    
    /**
     * Returns a database object for the current environment.
     * This will only work in WebTML/TMLScript environments and returns the object of the main context database.
     * This either returns an {@link App} or {@link DataSource} object, depending on the database type
     * @throws WGAServerException
     */
    public Database database() throws WGException {
        return database(tmlcontext().db());
    }
    
    /**
     * Returns a database object for the given database
     * This either returns an {@link App} or {@link DataSource} object, depending on the database type
     * @param db The database
     * @throws WGAServerException
     */
    public Database database(WGDatabase db) throws WGException {
        if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            return app(db);
        }
        else {
            return dataSource(db);
        }
    }
    
    
    /**
     * Returns a database object for the WebTML context
     * This either returns an {@link App} or {@link DataSource} object, depending on the database type
     * @param cx The WebTML context
     * @throws WGAServerException
     */
    public Database database(Context cx) throws WGException {
        return database(cx.db());
    }
    
    /**
     * Returns a database object for the database of the given key
     * This either returns an {@link App} or {@link DataSource} object, depending on the database type
     * @param dbKey Key of the database
     * @throws WGAServerException
     */
    public Database database(String dbKey) throws WGException {
        
        WGDatabase db = db(dbKey);
        if (db == null) {
            return null;
        }
        
        return database(db);
        
    }

    /**
     * Creates a browser cookie object that can be added to the users browser session
     * This method is provided for TMLScript. In Java one can simply instantiate a {@link Cookie} object.
     * @param name Cookie name
     * @param value Cookie value
     */
    public Cookie createCookie(String name, String value) throws WGException {
        return new Cookie(name, value);
    }
    
    /**
     * Creates an object to do custom HTTP requests
     * The returned object is an instance of the Jakarta Commons HTTP Client, a Java class providing extensive functionality to perform HTTP requests. If a proxy is configured for the system the client object will already be configured to use it.
     */
    public HttpClient createHttpClient() throws WGException {
        return WGFactory.getHttpClientFactory().createHttpClient();
    }

    /**
     * Returns a subobject "Xml" which collects functionalities to load and parse Xml documents.
     */
    public Xml xml() {
        return new Xml(this);
    }

    /**
     * Returns a subobject "Auth" which collects functionalities regarding user authentication.
     * @deprecated Instead use {@link Domain#auth()} to get a domain specific authentication object.
     */
    @CodeCompletion
    public Auth auth() {
        return new Auth(this);
    }

    /**
     * Returns a subobject "Html" which collects functionalities to load and parse Html documents.
     */
    public Html html() {
        return new Html(this);
    }
    
    /**
     * Returns a subobject "Server" providing information and services regarding the OpenWGA server runtime and installation
     */
    @CodeCompletion
    public Server server() {
        return new Server(this);
    }
    
    /**
     * Returns a subobject "TMLPage" providing operations regarding the current output of WebTML page
     */
    @CodeCompletion
    public TMLPage tmlPage() {
        return new TMLPageImpl(this);        
    }
    
    /**
     * Returns if a WebTML/TMLScript environment is available
     */
    public boolean isTMLContextAvailable() {
        return (_context!=null && !isIsolated() && fetchTMLContext() != null);
    }

    /**
     * Returns if this WGA object is isolated, i.e. does not provide direct access to environment-dependent
     * resources like calls or sessions.
     */
    public boolean isIsolated() {
        return _context.isIsolated();
    }

    /**
     * Returns the WebTML context of the environment
     * Will only work in those environments that run on behalf of WebTML functionalties, like WebTML elements etc.
     * @throws UnavailableResourceException
     */
    @CodeCompletion
    public Context tmlcontext() throws WGException {
    	TMLContext tmlContext = (TMLContext)context();
        if (tmlContext != null) {
            return tmlContext;
        }
        else {
            throw new UnavailableResourceException("TMLContext is not available but needed for this operation");
        }
    }

    /*
     * Better and more useful version then tmlcontext()
     */
    public Context context() throws WGException{
    	TMLContext ctx = fetchTMLContext();
    	if(ctx!=null && isIsolated())
    		return ctx.toIsolatedVersion();
    	return ctx;
    }
    public Context context(String expression) throws WGException{
    	return context().context(expression);
    }
    public Context context(String expression, boolean returnContextOnError) throws WGException{
    	return context().context(expression, returnContextOnError);
    }
    public Context context(WGContent content) throws WGException{
    	return context().context(content);
    }
    
    protected TMLContext fetchTMLContext() {
        TMLContext context = _context.getTMLContext();
        
        // Global fallback: Take the threads main context
        if (context == null) {
            context = TMLContext.getThreadMainContext();
        }
        
        return context;
    }
    
    protected HttpSession fetchSession() {
        HttpSession session = _context.getHttpSession();
        
        if (session == null) {
            
            // Global fallback: Ask the threads main context
            TMLContext context = TMLContext.getThreadMainContext();
            if (context != null) {
                session = context.gethttpsession();
            }

        }
        return session;
    }

    /**
     * Returns the Java object for the HTTP servlet request of the environment
     * Will only work if the environment runs on behalf of a request
     * @throws UnavailableResourceException
     */
    @CodeCompletion
    public HttpServletRequest getRequest() throws WGException {
        
        if (!isIsolated()) {
            HttpServletRequest request = fetchRequest();
            if (request != null) {
                return request;
            }
        }
        
        throw new UnavailableResourceException("Request is not available but needed for this operation");
        
    }
    
    public HttpSession getHttpSession() throws WGException {
        
        if (!isIsolated()) {
            HttpSession session = fetchSession();
            if (session != null) {
                return session;
            }
        }
        
        throw new UnavailableResourceException("Http Session is not available but needed for this operation");
        
    }
    
    protected javax.websocket.Session getWebSocketSession() throws WGException {
        if (!isIsolated()) {
            javax.websocket.Session wsSession = fetchWebSocketSession();
            if (wsSession != null) {
                return wsSession;
            }
        }
        
        throw new UnavailableResourceException("Websocket Session is not available but needed for this operation");
    }
    
    protected javax.websocket.Session fetchWebSocketSession() {
        return _context.getWebsocketSession();
    }

    /**
     * Returns an object providing information about the call that inited the current runtime environment, if available.
     * If no call is available the returned object will provide no data.
     */
    public Call call() {
        return new Call(this);
    }
    
    /**
     * Returns an object providing information about the current HTTP user session, if available.
     * If no session is available the returned object will provide no data.
     */
    public Session session() {
        return new Session(this);
    }

    protected HttpServletRequest fetchRequest() {
        HttpServletRequest request = _context.getServletRequest();
        if (request == null) {
        
            // Global fallback: Ask the threads main context
            TMLContext context = TMLContext.getThreadMainContext();
            if (context != null) {
                request = context.getrequest();
            }

        }
        return request;
    }
    
    protected HttpServletResponse fetchResponse() {
        HttpServletResponse response = _context.getServletResponse();
        if (response == null) {
        
            // Global fallback: Ask the threads main context
            TMLContext context = TMLContext.getThreadMainContext();
            if (context != null) {
                response = context.getresponse();
            }

        }
        return response;
    }
    
    
    /**
     * Returns if the environment runs on behalf of a request and a request object is available
     */
    public boolean isRequestAvailable() {
        return (!isIsolated() && fetchRequest() != null);
    }
    
    /**
     * Returns if the environment has a HTTP session in access
     */
    public boolean isHttpSessionAvailable() {
        return (!isIsolated() && (fetchSession() != null));
    }
    
    /**
     * Returns if the environment has a HTTP session in access
     */
    protected boolean isWebSocketSessionAvailable() {
        return (!isIsolated() && (fetchWebSocketSession() != null));
    }
    
    
    /**
     * Returns if the environment runs on behalf of a request and a response object is available
     */
    public boolean isResponseAvailable() {
        return (!isIsolated() && fetchResponse() != null);
    }

    /**
     * Returns the Java object for the HTTP servlet response of the environment
     * Will only work if the environment runs on behalf of a request
     * @throws UnavailableResourceException
     */
    public HttpServletResponse getResponse() throws WGException {
        
        if (!isIsolated()) {
            HttpServletResponse response = fetchResponse();
            if (response != null) {
                return response;
            }
        }
        
        throw new UnavailableResourceException("Response is not available but needed for this operation");
        
    }
    
    /**
     * Creates an WebTML context object for the given database
     * The context will be on a dummy document of the given database.
     * If a WebTML environment is available this context will belong to this environment. It will use its language choosing configuration to choose the dummy docs language.
     * Otherwise this will be a new independent context creating its own environment. It will use the databases default language.
     * @param db The database for which to create the context
     * @param design The design which the context should use for reference on design operations. Specify null to use the design of the database where the content comes from.
     * @return A new WebTML context object
     * @throws WGAServerException
     */
    public Context createTMLContext(WGDatabase db, Design design) throws WGException {
        
        try {
            WGLanguageChooser chooser = null;
            WGLanguage fallback = null;
            if (isTMLContextAvailable()) {
                try {
                    chooser = new WebTMLLanguageChooser(db, (TMLContext) tmlcontext());
                }
                catch (UnavailableResourceException e) {
                    // Cannot happen
                }
            }
            else {
                chooser = new SingleLanguageChooser(db.getDefaultLanguage());
            }
            
            return createTMLContext(db, chooser, design);
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception creating TMLContext", e);
        }
        
    }
    
    /**
     * Creates an WebTML context object for the given database
     * The context will be on a dummy document of the given database.
     * If a WebTML environment is available this context will belong to this environment. It will use its language choosing configuration to choose the dummy docs language.
     * Otherwise this will be a new independent context creating its own environment. It will use the databases default language.
     * @param db The database for which to create the context
     * @return A new WebTML context object
     * @throws WGAServerException
     */
    public Context createTMLContext(WGDatabase db) throws WGException {
        return createTMLContext(db, (Design) null);
    }

    /**
     * Creates an WebTML context object for the given database, using the given language chooser
     * The context will be on a dummy document of the given database.
     * If a WebTML environment is available this context will belong to this environment. 
     * Otherwise this will be a new independent context creating its own environment.
     * @param db The database for which to create the context
     * @param chooser A language choosing object, responsible for choosing the language of the context doc
     * @param design The design which the context should use for reference on design operations. Specify null to use the design of the database where the content comes from.
     * @return A new WebTML context object
     * @throws WGAServerException
     */
    public Context createTMLContext(WGDatabase db, WGLanguageChooser chooser, Design design) throws WGException {
        WGLanguage lang = chooser.selectDatabaseLanguage(db);
        
        WGContent dummyContent;
        if (lang != null) {
            dummyContent = db.getDummyContent(lang.getName());
        }
        else {
            dummyContent = db.getDummyContent(db.getDefaultLanguage());
        }
        
        if (dummyContent != null) {
            return createTMLContext(dummyContent, design);
        }
        else {
            return null;
        }
    }
    
    /**
     * Creates an WebTML context object for the given database, using the given language chooser
     * The context will be on a dummy document of the given database.
     * If a WebTML environment is available this context will belong to this environment. 
     * Otherwise this will be a new independent context creating its own environment.
     * @param db The database for which to create the context
     * @param chooser A language choosing object, responsible for choosing the language of the context doc
     * @return A new WebTML context object
     * @throws WGAServerException
     */
    public Context createTMLContext(WGDatabase db, WGLanguageChooser chooser) throws WGException {
        return createTMLContext(db, chooser, null);
    }
    
    /**
     * Creates an WebTML context object for the given content
     * If a WebTML environment is available this context will belong to this environment.
     * Otherwise this will be a new independent context creating its own environment.
     * @param content The content document which should be in context
     * @param design The design which the context should use for reference on design operations. Specify null to use the design of the database where the content comes from.
     * @return A new WebTML context object
     * @throws WGAServerException
     */
    public Context createTMLContext(WGContent content, Design design) throws WGException {
        
        TMLContext cx;
        if (isTMLContextAvailable()) {
            cx = ((TMLContext) tmlcontext().context(content));
        }
        else {
            HttpServletRequest req = (isRequestAvailable() ? fetchRequest() : null);
            HttpServletResponse res = (isResponseAvailable() ? fetchResponse() : null);
            HttpSession ses = (isHttpSessionAvailable() ? fetchSession() : null);
            
            cx = new TMLContext(content, getCore(), null, null, req, res, ses);
        }
        
        if (design != null) {
            cx = cx.designContext(design.getBaseReference().toString());
        }
        return cx;
        
    }
    
    /**
     * Creates an WebTML context object for the given content
     * If a WebTML environment is available this context will belong to this environment.
     * Otherwise this will be a new independent context creating its own environment.
     * @param content The content document which should be in context
     * @return A new WebTML context object
     * @throws WGAServerException
     */
    public Context createTMLContext(WGContent content) throws WGException {
        return createTMLContext(content, null);
    }
    
    /**
     * Returns a domain object for the domain of the given name
     * @throws WGAServerException
     */
    public Domain domain(String name) throws WGException {
        return new Domain(this, name);
    }
    
    /**
     * Returns a subobject "TMLScript" for executing TMLScript expressions
     */
    @CodeCompletion
    public TMLScript tmlscript() throws WGException {
        return new TMLScript(this);
    }
    
    /**
     * Returns a subobject "Jobs" providing services regarding OpenWGA Jobs 
     */
    @CodeCompletion
    public Jobs jobs() {
        return new Jobs(this);
    }
    
    /**
     * Returns a subobject "Validate" offering comfortable functions to validate data in common ways, prominently in WebTML form validations.
     */
    @CodeCompletion
    public Validate validate() {
        return new Validate(this);
    }
    
    /**
     * Reopens all currently open database sessions
     * @throws WGAServerException
     */
    public void reopenSessions() throws WGException {
        
            for (WGDatabase db : getCore().getContentdbs().values()) {
                if (db.isSessionOpen()) {
                    try {
                        db.reopenSession();
                    }
                    catch (WGAPIException e) {
                        throw new WGAServerException("Exception reopening database '" + db.getDbReference() + "'", e);
                    }
                }
            }

    }
    
    /**
     * Returns the request information object for the current WebTML request to modify access logging data
     * @throws UnavailableResourceException
     */
    public WGARequestInformation accessLogging() throws WGException {
        return (WGARequestInformation) getRequest().getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
    }
    
    /**
     * Converts any value(s) into list form
     * This method does the same list conversion known from itemList() or metaList() in TMLScript on the given values. It is ensured that the return value is a non-null List with differing contents.
     * The behaviour regarding each single input input value is:
     * <ul>
     * <li>Given a list it returns that list unmodified
     * <li>Given other collections and arrays it returns a list containing the collection values
     * <li>Given null it returns an empty list
     * <li>All other values return a list with the value as single element
     * </ul>
     * If multiple objects are given as values it will concatenate the elements of their resulting lists into one big list
     * @param values Values that are either objects representing a list or elements to go into the list
     * @return The value as list
     */
    public List<Object> toList(Object... values) throws WGException {
        List<Object> theList = new ArrayList<Object>();
        for (Object value : values) {
            theList.addAll(TMLContext.toList(value));
        }
        return theList;
    }
    
    /**
     * Returns classes and interfaces of service APIs currently registered for this WGA runtime
     * @throws WGException
     */
    public List<Class<?>> serviceClasses() throws WGException {
        
        DynamicClassLoadingChain loader = server().getLibraryLoader();
        ModuleRegistry reg = getCore().getModuleRegistry();
        List<Class<?>> serviceClasses = new ArrayList<Class<?>>();
        for (ModuleDefinition def : reg.getModulesForType(ServiceApiType.class).values()) {
            try {
                serviceClasses.add(loader.loadClass(((KeyBasedModuleDefinition) def).getRegistrationKey()));
            }
            catch (ClassNotFoundException e) {
            }
        }
        return serviceClasses;
        
    }
    
    /**
     * Returns a service implementation for the given service API class/interface
     * @param serviceClass The service API class or interface
     * @return The implementation fo the service API which is currently active for the WGA runtime
     * @throws WGException
     */
    @SuppressWarnings("unchecked")
    public <T> T service(Class<T> serviceClass) throws WGException {
        
        ModuleRegistry reg = getCore().getModuleRegistry();
        ModuleDefinition serviceDefinition = reg.getModuleDefinitionByKey(ServiceApiType.class, serviceClass.getName());
        if (serviceDefinition == null) {
            throw new WGAServerException("Unknown service interface: " + serviceClass.getName());
        }
        
        ServiceApiProperties serviceProps = (ServiceApiProperties) serviceDefinition.getProperties();
        
        // Service implements a custom fetcher?
        ServiceApiProperties.Fetcher fetcher = serviceProps.getFetcher();
        if (fetcher != null) {
            try {
                return (T) fetcher.fetchService(this);
            }
            catch (Exception e) {
            }
        }

        // Try to find server configuration about what implementation to use.
        ModuleDefinition implDef = null;
        if (serviceProps.isImplementable()) {
            String configuredApi = getCore().getWgaConfiguration().getServerOptions().get(WGAConfiguration.SERVEROPTION_SERVICE_APIS_PREFIX + serviceDefinition.getImplementationClass().getName());
            
            if (!WGUtils.isEmpty(configuredApi)) {
                implDef = reg.getModuleDefinition((Class<? extends ModuleType>) serviceDefinition.getImplementationClass(), configuredApi);
                if (implDef != null) {
                    try {
                        return instantiateService(implDef);
                    }
                    catch (Exception e) {
                    }
                }
            }
        }
        
        // Try to instantiate default implementation given by service definition
        if (serviceProps.isImplementable()) {
            Class<?> defaultImpl = serviceProps.getDefaultImplementation();
            if (defaultImpl != null) {
                implDef = reg.getModuleDefinition((Class<? extends ModuleType>) serviceDefinition.getImplementationClass(), defaultImpl);
            }
        }
        else {
            implDef = serviceDefinition;
        }
        
        if (implDef != null) {
            try {
                return instantiateService(implDef);
            }
            catch (Exception e) {
            }
        }
        
        // Try to use "first best" one from registry
        if (serviceProps.isImplementable()) {
            Map<String,ModuleDefinition> impls = reg.getModulesForType((Class<? extends ModuleType>) serviceDefinition.getImplementationClass());
            if (impls.size() > 0) {
                for (Map.Entry<String,ModuleDefinition> impl : impls.entrySet()) {
                    try {
                        return instantiateService(impl.getValue());
                    }
                    catch (Exception e) {
                    }
                }
            }
        }
        
        return null;
        
        
        
    }

    
    /**
     * Instantiates a specific service API implementation
     * @param implementationClass The service API implementation class
     * @throws WGException
     */
    public <T> T instantiateService(Class<?> implementationClass) throws WGException {
        
        ModuleRegistry reg = server().getModuleRegistry();
        ModuleDefinition implDef = reg.getModuleDefinition(ServiceApiType.class, implementationClass);
        if (implDef == null) {
            throw new WGAServerException("Unknown Service API implementation: " + implementationClass.getName());
        }
        return instantiateService(implDef);
    }

    /**
     * Instantiates a specific service API implementation
     * @param implDef The module definition of the implementation
     * @throws WGException
     */
    public <T> T instantiateService(ModuleDefinition implDef) throws WGException {
        try {
            try {
                implDef.testDependencies();
            }
            catch (ModuleDependencyException e) {
                throw new WGAServerException("Dependencies of Service API implementation not met: " + implDef.getImplementationClass().getName(), e);
            }
            
            @SuppressWarnings("unchecked")
            T service =  (T) server().getModuleRegistry().instantiate(implDef);
            if (service instanceof WGAAwareService) {
                ((WGAAwareService) service).injectWGA(this);
            }
            return service;
        }
        catch (ModuleInstantiationException e) {
            throw new WGAServerException("Exception instantiating service module", e);
        }
    }
    
    /**
     * Selects a file derivate of a file attachment which is the best match for a derivate query
     * @param cx Context of the content holding the attachment
     * @param fileName Name of the attachment
     * @param derivateQueryStr The derivate query
     * @return Metadata of the best matching derivate. Null if no match was found.
     * @throws WGException
     */
    public WGFileDerivateMetaData selectDerivate(Context cx, String fileName, String derivateQueryStr) throws WGException {
        
        Float devicePixelRatio = null;
        if (isRequestAvailable()) {
            String dprStr = _context.getServletRequest().getHeader("CH-DPR");
            if (dprStr != null) {
                try {
                    devicePixelRatio = Float.parseFloat(dprStr);
                }
                catch (NumberFormatException e) {
                    getCore().getLog().warn("Client uses unparseable device pixel ratio: " + dprStr);
                }
            }
        }
        
        FileDerivateManager manager = getCore().getFileDerivateManager();
        DerivateQuery derivateQuery = FileDerivateManager.parseDerivateQuery(derivateQueryStr);        
        
        // With a set device pixel ratio > 1 we do additional queries for the given ratio and its bisections
        WGFileAnnotations md = null;
        if (devicePixelRatio != null) {
            while (true) {
                md = manager.queryDerivate(cx.content(), fileName, derivateQuery, new ClientHints(devicePixelRatio, null), false); 
                if (md != null)  {
                    break;
                }
                if (devicePixelRatio == 1) {
                    break;
                }
                else if (devicePixelRatio % 2 == 0) {
                    devicePixelRatio = devicePixelRatio / 2;
                }
                else {
                    devicePixelRatio = 1f;
                }
            }
        }
        else {
            md = manager.queryDerivate(cx.content(), fileName, derivateQuery, new ClientHints(), false);
        }
        
        
        if (md instanceof WGFileDerivateMetaData) {
            return (WGFileDerivateMetaData) md;
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Selects a file derivate of a file attachment which is the best match for a derivate query. This variant uses the current WebTML context document to find file attachment and derivates.
     * @param fileName Name of the attachment
     * @param derivateQuery The derivate query
     * @return Metadata of the best matching derivate. Null if no match was found.
     * @throws WGException
     */
    public WGFileDerivateMetaData selectDerivate(String fileName, String derivateQuery) throws WGException {
        return selectDerivate(fetchTMLContext(), fileName, derivateQuery);
    }
    
    /**
     * Returns an alias for a string value from a WebTML aliases string
     * The aliases string given as parameter is expected to consist of a comma-separated list of elements of value plus corresponding alias, divided by a pipe symbol: alias|value,alias|value,alias|value ...
     * @param str The string value
     * @param aliasesStr The aliases string
     * @return The found alias or the original string value if no alias was found
     */
    public String alias(String str, String aliasesStr) {
        return alias(str, WGUtils.deserializeCollection(aliasesStr, ",", true));
    }
    
    /**
     * Returns an alias for a string value from a list of WebTML aliases.
     * The list of aliases given as parameter is expected to contain elements of value plus corresponding alias, divided by a pipe symbol: alias|value
     * @param str The string value
     * @param aliases The list of aliases
     * @return The found alias or the original string value if no alias was found
     */
    public String alias(String str, List<String> aliases) {
    	ArrayList<String> values = new ArrayList<String>();
    	values.add(str);
    	return this.aliases(values, aliases).get(0);
    }
    
    /**
     * Returns a list of alias for a string list
     * The list of aliases given as parameter is expected to contain elements of value plus corresponding alias, divided by a pipe symbol: alias|value
     * If values is null or empty the alias for an empty string is returned (if found)
     * @param values The list of value
     * @param aliasesStr The aliases string
     * @return The found aliases or the original string value if no alias was found
     */
    public ArrayList<String> aliases(List<String> values, String aliasesStr) {
    	return aliases(values, WGUtils.deserializeCollection(aliasesStr, ",", true));
    }

    /**
     * Returns a list of alias for a string list
     * The list of aliases given as parameter is expected to contain elements of value plus corresponding alias, divided by a pipe symbol: alias|value
     * If values is null or empty the alias for an empty string is returned (if found)
     * @param values The list of value
     * @param aliases The list of aliases
     * @return The found aliases or the original string value if no alias was found
     */
    public ArrayList<String> aliases(List<String> values, List<String> aliases) {
    	return aliases(values, aliases, false);
    }

    /**
     * Returns a list of alias for a string list
     * The list of aliases given as parameter is expected to contain elements of value plus corresponding alias, divided by a pipe symbol: alias|value
     * If values is null or empty the alias for an empty string is returned (if found)
     * @param values The list of value
     * @param aliases The list of aliases
     * @return The found aliases or the original string value
     */
    public ArrayList<String> aliases(List<String> values, List<String> aliases, boolean alias_values_only) {
    	ArrayList<String> result = new ArrayList<String>();
    	HashMap<String,String> options = new HashMap<String,String>();
        for (String alias : aliases) {
            String optionText = alias;
            String optionValue = alias;
            int divider = alias.lastIndexOf("|");
            if (divider != -1) {
                optionText = alias.substring(0, divider);
                optionValue = alias.substring(divider + 1);
            }
            options.put(optionValue.trim(), optionText.trim());
        }
        if(values==null)
        	values = new ArrayList<String>();
        if(values.isEmpty()){
        	values.add("");
        }
        for(String value: values){
        	String alias = options.get(value);
        	if(alias!=null)
        		result.add(alias);
        	else if(!alias_values_only)
        		result.add(value); 
        }
        return result;
    }
    
    /**
     * Returns a logging facility appropriate for the current WGA environment
     */
    public Logger getLog() {
        return _context.getLog();
    }

    /**
     * Returns an OpenWGA date format
     * @param pattern The date format pattern
     * @param locale A locale to use for locale-dependent date parts. Specify null to let the current WebTML context choose the locale.
     * @throws WGException
     */
    public DateFormat getDateFormat(String pattern, Locale locale) throws WGException {
        
        // Select language for language dependent date formats
        if (locale == null) {
            locale = chooseLocale(locale);
        }
        
        // Language Fallback(s)
        
        if (WGUtils.isEmpty(pattern)) {
            return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM, locale);
        }
        
        // For default patterns
        String lcPattern = pattern.toLowerCase();
        if (lcPattern.equals("htmldate")) {
        	return new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH);
        }
        else if (lcPattern.equals("htmldatetime")) {
        	return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm", Locale.ENGLISH);
        }
        else if (lcPattern.equals("htmltime")) {
        	return new SimpleDateFormat("HH:mm", Locale.ENGLISH);
        }
        else if (lcPattern.equals("iso8601")) {
            return new ISO8601DateFormat();
        }
        else if (lcPattern.endsWith("date") || lcPattern.endsWith("time")) {
            int patternLength;
            if (lcPattern.startsWith("short")) {
                patternLength = DateFormat.SHORT;
            }
            else if (lcPattern.startsWith("medium")) {
                patternLength = DateFormat.MEDIUM;
            }
            else if (lcPattern.startsWith("long")) {
                patternLength = DateFormat.LONG;
            }
            else {
                patternLength = DateFormat.FULL;
            }
            
            if (lcPattern.endsWith("datetime")) {
                return new TextualDateFormat(locale, DateFormat.getDateTimeInstance(patternLength, patternLength, locale));
            }
            else if (lcPattern.endsWith("time")) {
                return new TextualDateFormat(locale, DateFormat.getTimeInstance(patternLength, locale));
            }
            else {
                return new TextualDateFormat(locale, DateFormat.getDateInstance(patternLength, locale));
            }
            
        }
        
        // For custom patterns
        SimpleDateFormat dateFormat = new SimpleDateFormat(pattern, locale);
        dateFormat.setLenient(false);
        return new TextualDateFormat(locale, dateFormat);
           
             
    }

    private Locale chooseLocale(Locale locale) {
        
        if (isTMLContextAvailable()) {
            WGDatabase db = fetchTMLContext().db();
            WGLanguageChooser chooser = new WebTMLLanguageChooser(db, (TMLContext) fetchTMLContext());
            WGLanguage lang = null; 
            try {
                lang = chooser.selectDatabaseLanguage(db);
                if (lang != null) {
                    locale = lang.getLocale();
                }
            }
            catch (WGAPIException e) {
                getLog().error("Exception selecting language for date format", e);
            }
        }
        
        if (locale == null) {
            locale = Locale.getDefault();
        }
        return locale;
    }

    /**
     * Returns an OpenWGA number format
     * @param pattern The number format pattern
     * @param locale A locale to use for locale-dependent number parts. Specify null to let the current WebTML context choose the locale.
     * @throws WGException
     */
    public NumberFormat getNumberFormat(String pattern, Locale locale) throws WGException {
    
        // Select language for language dependent number formats
        if (locale == null) {
            locale = chooseLocale(locale);
        }
    
        if (WGUtils.isEmpty(pattern)) {
            NumberFormat numberFormat = NumberFormat.getNumberInstance(locale);
            numberFormat.setMaximumFractionDigits(Integer.MAX_VALUE);
            numberFormat.setMaximumIntegerDigits(Integer.MAX_VALUE);
            return numberFormat;
        }
        
        // For default pattern
        if (pattern.toLowerCase().equals("decimal")) {
            return NumberFormat.getNumberInstance(locale);
        }
        
        // Custom pattern
        return new DecimalFormat(pattern, new DecimalFormatSymbols(locale));
    
    }
    
    /**
     * Returns a Nav object for the current WebTML context
     * @throws WGException
     */
    public Nav nav() throws WGException {
        return new Nav(this, tmlcontext());
    }
    
    /**
     * Returns a Nav Object for the given WebTML context
     * @param context
     * @throws WGException
     */
    public Nav nav(Context context) throws WGException {
        return new Nav(this, context);
    }

    public Nav nav(String expression) throws WGException {    	
        return new Nav(this, context(expression));
    }

    protected TMLContextWrapperIterator wrapIntoTMLContextIterator(SkippingIterator<WGContent> it) {
        if (it instanceof CountReportingIterator<?>) {
            return new CountReportingTMLContextWrapperIterator(it);
        }
        else {
            return new TMLContextWrapperIterator(it);
        }
    }
    
    protected OpenDatabaseService getOpenDatabaseService() {
        
        // Use WebTML environment to open databases
        final TMLContext cx = fetchTMLContext();
        if (cx != null) {
            return new OpenDatabaseService() {
                @Override
                public void openDatabase(WGDatabase db) throws WGException {
                    cx.getEnvironment().openDB(db);
                }
            };
        };
        
        // Use request information to open databases
        final HttpServletRequest req = fetchRequest();
        if (req != null) {
            return new OpenDatabaseService() {
                @Override
                public void openDatabase(WGDatabase db) throws WGException {
                    getCore().openContentDB(db, req, false);
                }
            };
        };
        
        // Use HTTP session information to open databases
        final HttpSession session = fetchSession();
        if (session != null) {
            return new OpenDatabaseService() {
                @Override
                public void openDatabase(WGDatabase db) throws WGException {
                    getCore().openContentDB(db, req, session, false, null);
                }
            };
        }
        
        // Use WebSocket information to open databases
        final javax.websocket.Session wsSession = fetchWebSocketSession();
        if (wsSession != null) {
            return new OpenDatabaseService() {
                @Override
                public void openDatabase(WGDatabase db) throws WGException {
                    if (!db.isSessionOpen()) {
                        SessionLoginMap logins = session().getLogins();
                        DBLoginInfo loginInfo = logins.get(_context.getCore().getDomainForDatabase(db).getName());
                        if (loginInfo != null) {
                            db.openSession(loginInfo.getUserName(), loginInfo.getCredentials(), loginInfo.getAccessFilter());
                        }
                        else {
                            db.openAnonymousSession();
                        }
                    }
                    
                }
            };
        }
        
        // No environment. Open as master.
        return new OpenDatabaseService() {
            
            @Override
            public void openDatabase(WGDatabase db) throws WGException {
                if (!db.isSessionOpen()) {
                    db.openSession();
                }
            }
        };
        
    }
    
    /**
     * Returns an isolated version of this WGA instance, which is prevented from accessing any optional environment data.
     * It only is able to authenticate using given WebTML/request environment data, so the users rights are enforced.
     */
    public WGA isolate() {
        return WGA.get(new WGAContext() {
            
            @Override
            public WGAContext narrow() {
                return _context.narrow();
            }
            
            @Override
            public boolean isIsolated() {
                return true;
            }
            
            @Override
            public TMLContext getTMLContext() {
                return _context.getTMLContext();
            }
            
            @Override
            public HttpServletResponse getServletResponse() {
                return _context.getServletResponse();
            }
            
            @Override
            public HttpServletRequest getServletRequest() {
                return _context.getServletRequest();
            }
            
            @Override
            public HttpSession getHttpSession() {
                return _context.getHttpSession();
            }
            
            @Override
            public Logger getLog() {
                return _context.getLog();
            }
            
            @Override
            public WGACore getCore() {
                return _context.getCore();
            }
            
            @Override
            public javax.websocket.Session getWebsocketSession() {
                return _context.getWebsocketSession();
            }
        });
    }
    
    /**
     * In TMLScript creates a narrowed down version of the WGA object with fixed environment
     */
    public WGA narrow() {
        return WGA.get(_context.narrow());
    }

    protected WGAContext getContext() {
        return _context;
    }
    
    // WGA.Date
    public WGADate Date(){
    	return new WGADate(this);
    }
    public WGADate Date(Date d){
    	return new WGADate(this, d);
    }
    public WGADate Date(String date, String format, String language) throws ParseException, WGException{
    	return new WGADate(this, date, format, language);
    }
    public WGADate Date(String date, String format) throws ParseException, WGException{
    	return new WGADate(this, date, format);
    }

    // WGA.List
    public WGAList<Object> List(Collection<Object> list){
    	return new WGAList<Object>(list);
    }
    public WGAList<Object> List(){
    	return new WGAList<Object>();
    }
    
    // WGA.File
    public WGAFile File(String filename){
    	return new WGAFile(this, filename);
    }
    public WGAFile File(File file){
    	return new WGAFile(this, file);
    }
    public WGAFile File(WGAFile file, String filename){
    	return new WGAFile(this, file, filename);
    }
 
    // WGA.HttpClient
    public WGAHttpClient HttpClient(String url) throws WGException{
    	return new WGAHttpClient(this, url);
    }
 
    // WGA.TempFile
    public WGATempFile TempFile(String filename) throws IOException, WGException{
    	return new WGATempFile(this, filename);
    }
     
    // WGA.Logger
    public WGALogger Logger(String name){
    	return new WGALogger("wga."+name);
    }
   
    public class WGALogger{
    	
    	Logger logger;

		public WGALogger(String name){
    		logger = Logger.getLogger(name);
    	}
    	
    	public WGALogger setLevel(String level){
    		if(level.equalsIgnoreCase("debug"))
    			logger.setLevel(Level.DEBUG);
    		if(level.equalsIgnoreCase("info"))
    			logger.setLevel(Level.INFO);
    		if(level.equalsIgnoreCase("warn"))
    			logger.setLevel(Level.WARN);
    		if(level.equalsIgnoreCase("error"))
    			logger.setLevel(Level.ERROR);
    		return this;
    	}
    	public Level getLevel(){
    		return logger.getLevel();
    	}

        public Logger getLogger(){
        	return logger;
        }
    	
    }
    
    public static class Base64{
    	public static String encode(String text) throws UnsupportedEncodingException{
    		return org.apache.commons.codec.binary.Base64.encodeBase64URLSafeString(text.getBytes("UTF-8"));
    	}
    	public static String encode(InputStream in) throws IOException{
    		byte[] bytes = IOUtils.toByteArray(in);
    		return org.apache.commons.codec.binary.Base64.encodeBase64URLSafeString(bytes);
    	}
    	public static String decode(String text) throws UnsupportedEncodingException{
    		return new String(org.apache.commons.codec.binary.Base64.decodeBase64(text), "UTF-8");
    	}
    }
 
	/*
	 * Test if this document currently is edited in content manager
	 * See de.innovationgate.wgpublisher.webtml.Root.processAbsoluteRoot() where this information is set as request attribute 
	 */
	public boolean isEditMode(WGContent content) throws WGException {
		Object attribEdit = getRequest().getAttribute(WGACore.ATTRIB_EDITDOCUMENT);
		if(attribEdit != null && attribEdit.equals(content.getContentKey().toString()) ){
			return true;
		}
		return false;
	}
	public boolean isEditMode() throws WGException {
		return isEditMode(((TMLContext)context()).content());
	}

}
 