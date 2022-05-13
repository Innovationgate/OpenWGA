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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.ConversionUtils;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAbstractResultSet;
import de.innovationgate.webgate.api.WGCachedResultSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.webgate.api.WGStandardResultSet;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGStructEntryList;
import de.innovationgate.webgate.api.utils.NativeQueryOptions;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.hdb.HDBModelException;
import de.innovationgate.wgpublisher.lang.SingleLanguageChooser;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.webtml.Query;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Object representing any OpenWGA database. Normally only subclasses {@link App} or {@link DataSource} are created.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Database {
    
    /**
     * WebTML output attribute "simplifiedquery", returning a simplified variant of a given Lucene fulltext query
     */
    public static final String QUERYOUT_SIMPLIFIED_QUERY = "simplifiedquery";
    /**
     * Name of WebTML query attribute "highlight"
     */
    public static final String QUERYATT_HIGHLIGHT = "highlight";
    /**
     * Name of WebTML query attribute "onlypublished"
     */
    public static final String QUERYATT_ONLYPUBLISHED = "onlypublished";
    
    
    public static final String QUERYATT_ONLYVISIBLE = "onlyvisible";
    
    /**
     * Name of WebTML query attribute "options"
     */
    public static final String QUERYATT_OPTIONS = "options";
    /**
     * Name of WebTML query attribute "max"
     */
    public static final String QUERYATT_MAX = "max";
    /**
     * Name of WebTML query attribute "alllanguages"
     */
    public static final String QUERYATT_ALLLANGUAGES = "alllanguages";
    /**
     * Name of WebTML query attribute "role"
     */
    public static final String QUERYATT_ROLE = "role";
    /**
     * Name of WebTML query attribute "includecurrent"
     */
    public static final String QUERYATT_INCLUDECURRENT = "includecurrent";
    /**
     * Name of WebTML query attribute "language"
     */
    public static final String QUERYATT_LANGUAGE = "language";
    /**
     * Name of WebTML query attribute "cache"
     */
    public static final String QUERYATT_CACHE = "cache";
    /**
     * Name of WebTML query attribute "db"
     */
    public static final String QUERYATT_DB = "db";
    protected WGDatabase _db;
    protected WGA _wga;

    protected Database(WGA wga, WGDatabase db) {
        _db = db;
        _wga = wga;
    }
    
    /**
     * Returns the corresponding WGAPI {@link WGDatabase} object for this app 
     */
    public WGDatabase db() {
        return _db;
        
    }
    
    /**
     * Performs a "native" query on the database without any options and parameter.
     * This method is the WGA server API pendant to WebTML tag <tml:query> and works quite equal. The mandatory parameter "query" is equal to the contents of the query tag.
     * The queryType for this query is taken from db attribute DBATTRIB_QUERY_DEFAULT (as in <tml:query>) - normaly "native".
     * @param queryString The actual query to execute
     * @throws WGException
     */
    public QueryResult query(String queryString) throws WGException {
        return query(queryString, Collections.<String, Object> emptyMap());
    }

    /**
     * Performs a "native" query on the database
     * This method is the WGA server API pendant to WebTML tag <tml:query> and works quite equal. The mandatory parameters "queryType" and "query" are equal to attribute "type" and the contents of the query tag.
     * The queryType for this query defaults to db attribute DBATTRIB_QUERY_DEFAULT (as in <tml:query>) - normaly "native".
     * The map argument "options" uses the names of other <tml:query>-Attributes as keys and interprets them the same way. So filling it with an entry of key "max" and a value 100 will have the same effect as specifying attribute max="100" on a query tag. Specifying or omitting an attribute here has the same effect as it would have on <tml:query>.
     * @param queryString The actual query to execute
     * @param options Query options. Use <tml:query> attribute names as keys, their values as values
     * @throws WGException
     */
    public QueryResult query(String queryString, Map<String,Object> options) throws WGException {
    	Map<String,Object> params = new HashMap<String,Object>();
    	String queryType = _db.getAttribute(WGACore.DBATTRIB_QUERY_DEFAULT).toString();
    	Context ctx = null;
    	if(options!=null){
	    	for(String key: options.keySet()){
	    		if(key.startsWith("p_")){
	    			params.put(key.substring(2), options.get(key));
	    			options.remove(key);
	    		}
	    		else if(key.equals("type")){
	    			queryType = (String)options.get(key);
	    			options.remove(key);
	    		}
	    		else if(key.equals("context")){
	    			Object value = options.get(key);
	    			if(value instanceof Context)
	    				ctx = (Context)value;
	    			else if(value instanceof String)
	    				ctx = _wga.tmlcontext().context((String)value);
	    			options.remove(key);
	    		}
	    	}
    	}
        return query(queryType, queryString, options, params, ctx);
    }

    /**
     * Performs a query on the database
     * This method is the WGA server API pendant to WebTML tag <tml:query> and works quite equal. The mandatory parameters "queryType" and "query" are equal to attribute "type" and the contents of the query tag.
     * The map argument "attributes" uses the names of other <tml:query>-Attributes as keys and interprets them the same way. So filling it with an entry of key "max" and a value 100 will have the same effect as specifying attribute max="100" on a query tag. Specifying or omitting an attribute here has the same effect as it would have on <tml:query>.
     * The map argument "queryParams" takes query parameters that in WebTML you would add to the query by using <tml:param>. Specify parameter names as keys and parameter values as values.
     * The argument "context" determines the WebTML context for which the query runs, which is important for some query types like "lucene" or "hdbmodel:*".
     * @param queryType The type of query to execute
     * @param queryString The actual query to execute
     * @throws WGException
     */
    public QueryResult query(String queryType, String queryString) throws WGException {
        return query(queryType, queryString, new HashMap<String,Object>(), Collections.<String, Object> emptyMap());
    }
    
    /**
     * Performs a query on the database
     * This method is the WGA server API pendant to WebTML tag <tml:query> and works quite equal. The mandatory parameters "queryType" and "query" are equal to attribute "type" and the contents of the query tag.
     * The map argument "atts" uses the names of other <tml:query>-Attributes as keys and interprets them the same way. So filling it with an entry of key "max" and a value 100 will have the same effect as specifying attribute max="100" on a query tag. Specifying or omitting an attribute here has the same effect as it would have on <tml:query>.
     * This uses the WebTML context of the environment as query context.
     * @param queryType The type of query to execute
     * @param queryString The actual query to execute
     * @param atts Query attributes. Use <tml:query> attribute names as keys, their values as values
     * @throws WGException
     */
    public QueryResult query(String queryType, String queryString, Map<String,Object> atts) throws WGException {
        return query(queryType, queryString, atts, Collections.<String, Object> emptyMap());
    }

    /**
     * Performs a query on the database
     * This method is the WGA server API pendant to WebTML tag <tml:query> and works quite equal. The mandatory parameters "queryType" and "query" are equal to attribute "type" and the contents of the query tag.
     * The map argument "atts" uses the names of other <tml:query>-Attributes as keys and interprets them the same way. So filling it with an entry of key "max" and a value 100 will have the same effect as specifying attribute max="100" on a query tag. Specifying or omitting an attribute here has the same effect as it would have on <tml:query>.
     * The map argument "queryParams" takes query parameters that in WebTML you would add to the query by using <tml:param>. Specify parameter names as keys and parameter values as values.
     * This uses the WebTML context of the environment as query context.
     * @param queryType The type of query to execute
     * @param queryString The actual query to execute
     * @param atts Query attributes. Use <tml:query> attribute names as keys, their values as values
     * @param queryParams Query parameters. Use parameter names as keys, values as values
     * @throws WGException
     */
    public QueryResult query(String queryType, String queryString, Map<String,Object> atts, Map<String,Object> queryParams) throws WGException {
        return query(queryType, queryString, atts, queryParams, null);
    }
    
    /**
     * Performs a query on the database
     * This method is the WGA server API pendant to WebTML tag <tml:query> and works quite equal. The mandatory parameters "queryType" and "query" are equal to attribute "type" and the contents of the query tag.
     * The map argument "atts" uses the names of other <tml:query>-Attributes as keys and interprets them the same way. So filling it with an entry of key "max" and a value 100 will have the same effect as specifying attribute max="100" on a query tag. Specifying or omitting an attribute here has the same effect as it would have on <tml:query>.
     * The map argument "queryParams" takes query parameters that in WebTML you would add to the query by using <tml:param>. Specify parameter names as keys and parameter values as values.
     * The argument "context" determines the WebTML context for which the query runs, which is important for some query types like "lucene" or "hdbmodel:*".
     * @param queryType The type of query to execute
     * @param queryString The actual query to execute
     * @param atts Query attributes. Use <tml:query> attribute names as keys, their values as values
     * @param queryParams Query parameters. Use parameter names as keys, values as values
     * @param context The WebTML context for the query.
     * @throws WGException
     */
    public QueryResult query(String queryType, String queryString, Map<String,Object> atts, Map<String,Object> queryParams, Context context) throws WGException {

        if(context==null)
        	context = _wga.context();
        
        if (atts == null) {
            atts = Collections.emptyMap();
        }
        
        if (queryParams == null) {
            queryParams = Collections.emptyMap();
        }

        Map<String,Object> allQueryParams = new HashMap<String,Object>();
        if (context != null) {
       		allQueryParams.putAll(buildDefaultQueryParams(context.content()));
        }
        allQueryParams.putAll(queryParams);
        queryParams = allQueryParams;
        
        Map<String, Object> wgapiParams = buildWgapiQueryParameters((TMLContext) context, atts, queryParams);
        
        WGResultSet set;
        Map<String,Object> outputParams = new HashMap<String, Object>();
        if (queryType.startsWith("xp:")) {
            set = executeExpressionQuery(context, queryType, queryString, wgapiParams, outputParams);
        }
        else if (queryType.equals("lucene") || queryType.startsWith("lucene:")) {
            set = executeLuceneQuery(queryType, queryString, wgapiParams, outputParams);
        }
        else if (queryType.startsWith("hdbmodel:")) {
            set = executeHdbModelQuery(queryType, queryString, wgapiParams, context, outputParams);
        }
        else {
            set = executeDbQuery(queryType, queryString, wgapiParams);
        }
        
        // Enforce resultset limiting if the resultset does not enforce it itself
        Number maxResults = (Number) wgapiParams.get(WGDatabase.QUERYOPTION_MAXRESULTS);
        if (maxResults != null && set!=null && !set.isLimitingResultsInBackend()) {
            set.limitResults(maxResults.intValue());
        }
        
        return new QueryResult(_wga, set, 
                (String) wgapiParams.get(WGDatabase.QUERYOPTION_RETURNQUERY), 
                wgapiParams.containsKey(WGDatabase.QUERYOPTION_USEDCACHE),
                outputParams, queryType, queryString);
        
    }

    private WGAbstractResultSet executeDbQuery(String queryType, String queryString, Map<String,Object> apiParams) throws WGException {
        return _db.query(queryType, queryString, apiParams);
    }
    
    private Map<String, Object> buildWgapiQueryParameters(TMLContext context, Map<String, Object> options, Map<String,Object> queryParameters) throws WGException {
        
        Map<String,Object> parameters = new java.util.HashMap<String, Object>();
        
        WGLanguageChooser chooser;
        if (context != null) {
            chooser = new WebTMLLanguageChooser(_db, context);
        }
        else {
            String language = (String) options.get(QUERYATT_LANGUAGE);
            if (language == null) {
                language = _db.getDefaultLanguage();
            }
            chooser = new SingleLanguageChooser(language);
        }

        // Include current behaviour
        if (ConversionUtils.getBoolean(options.get(QUERYATT_INCLUDECURRENT), true) == false && context != null && !context.content().isTemporary()) {
            parameters.put(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT, context.content());
        }

        // Role
        parameters.put(WGDatabase.QUERYOPTION_ROLE, WGUtils.getValueOrDefault(options.get(QUERYATT_ROLE), WGContent.DISPLAYTYPE_SEARCH));

        // Behaviour regarding unpublished documents
        if (ConversionUtils.getBoolean(options.get(QUERYATT_ONLYPUBLISHED), true) == true) {
            parameters.put(WGDatabase.QUERYOPTION_ONLYRELEASED, "");
            parameters.put(WGDatabase.QUERYOPTION_ENHANCE, ConversionUtils.getBoolean(options.get(QUERYATT_ONLYVISIBLE), true));
        }
        else {
            parameters.put(WGDatabase.QUERYOPTION_ENHANCE, false);
        }

        // Language behaviour
        if (ConversionUtils.getBoolean(options.get(QUERYATT_ALLLANGUAGES), false) == false) {
            List<WGLanguage> langs = chooser.getQueryLanguages(_db);
            if (langs.size() == 0) {
                throw new WGAServerException("No allowed content languages for query on database " + _db.getDbReference());
            }
            parameters.put(WGDatabase.QUERYOPTION_ONLYLANGUAGE, langs.get(0).getName()); // Compatibility with query types not supporting multiple languages
            parameters.put(WGDatabase.QUERYOPTION_LANGUAGES, langs);
        }
        
        //Set MaxResults
        int maxResults = ConversionUtils.getInteger(options.get(QUERYATT_MAX), (Integer) _wga.getCore().readPublisherOptionOrDefault(_db, WGACore.DBATTRIB_MAXQUERYRESULTS)); 
        if (maxResults != 0) {
            parameters.put(WGDatabase.QUERYOPTION_MAXRESULTS, new Integer(maxResults));
        }

        if (ConversionUtils.getBoolean(options.get(QUERYATT_CACHE), false) == true) {
            parameters.put(WGDatabase.QUERYOPTION_CACHERESULT, new Boolean(true));
        }

        // Eventually add native options
        Object nativeOptions = options.get(QUERYATT_OPTIONS);
        if (nativeOptions != null) {
            parameters.put(WGDatabase.QUERYOPTION_NATIVEOPTIONS, ConversionUtils.getString(nativeOptions));
        }
        
        // Add LuceneOption SearchScope 
        // Default SearchScope == DB;
        String searchScope = LuceneManager.SEARCHSCOPE_DB;
        String strDb = (String) options.get(QUERYATT_DB);
        if (strDb != null) {
            if (strDb.trim().equals("*")) {
                searchScope = LuceneManager.SEARCHSCOPE_DOMAIN;
            }
            if (strDb.trim().equals("**")) {
                searchScope = LuceneManager.SEARCHSCOPE_WGA;
            }
            if (strDb.trim().indexOf(",") != -1) {
                searchScope = LuceneManager.SEARCHSCOPE_DB_LIST;
                parameters.put(LuceneManager.QUERYOPTION_SEARCHDBKEYS, strDb);
            }
        }
        parameters.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, searchScope);
        parameters.put(LuceneManager.QUERYOPTION_HIGHLIGHT, ConversionUtils.getBoolean(options.get(QUERYATT_HIGHLIGHT), false));
        
        parameters.put(WGDatabase.QUERYOPTION_QUERY_PARAMETERS, queryParameters);
        
        return parameters;
    }

    private WGResultSet executeHdbModelQuery(String queryType, String queryString, Map<String,Object> apiParams, Context context, Map<String,Object> outputParams) throws WGException {
    
        if (context == null) {
            throw new UnavailableResourceException("Cannot execute HDBModel query without TMLContext");
        }
        
        try {
            HDBModel model = HDBModel.getModel(_db);
            if (model == null) {
                throw new WGQueryException(queryString, "App " + _db.getDbReference() + " does not use HDBModel");
            }
            
            queryType = queryType.substring(9);
            
            
            NativeQueryOptions nativeOptions = new NativeQueryOptions((String) apiParams.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS));
            @SuppressWarnings("unchecked")
            Map<String,Object> queryParameters = (Map<String,Object>) apiParams.get(WGDatabase.QUERYOPTION_QUERY_PARAMETERS);
            
            WGResultSet resultSet = null;
            
            if (queryType.startsWith("relationtargets:")) {
                
                String relation = queryType.substring(16);
                String contentClass = context.content().getContentClass();
                Boolean includeCurrent = true;
                if (nativeOptions.containsKey("contentclass")) {
                    contentClass = nativeOptions.get("contentclass");
                }
                if (nativeOptions.containsKey("currenttarget")) {
                    includeCurrent = Boolean.valueOf(nativeOptions.get("currenttarget"));
                }
                
                Map<String,Object> options = new HashMap<String, Object>(apiParams);
                if (!WGUtils.isEmpty(queryString.trim())) {
                    options.put(HDBModel.OPTION_EXTRACLAUSE, queryString);
                }
                options.put(HDBModel.OPTION_EXTRAPARAMS, queryParameters);
                options.put(HDBModel.OPTION_INCLUDECURRENT, includeCurrent);
                
                resultSet = model.getRelationTargets(context.content(), contentClass, relation, options);
                apiParams.put(WGDatabase.QUERYOPTION_RETURNQUERY, options.get(WGDatabase.QUERYOPTION_RETURNQUERY));
            }
            
            else if (queryType.startsWith("relationsources:")) {
                
                String relation = queryType.substring(16);
                String contentClass = nativeOptions.get("contentclass");
                if (contentClass == null) {
                    throw new WGQueryException(queryString, "Option 'contentclass' is mandatory for HDBModel query type 'relationsources'");
                }
                
                Map<String,Object> options = new HashMap<String, Object>(apiParams);
                if (!WGUtils.isEmpty(queryString.trim())) {
                    options.put(HDBModel.OPTION_EXTRACLAUSE, queryString);
                }
                options.put(HDBModel.OPTION_EXTRAPARAMS, queryParameters);
    
                
                resultSet =  model.getRelationSources(context.content(), contentClass, relation, options);
                apiParams.put(WGDatabase.QUERYOPTION_RETURNQUERY, options.get(WGDatabase.QUERYOPTION_RETURNQUERY));
                
            }
                    
            else {
                throw new WGQueryException(queryString, "Unknown HDBModel query type: " + queryType);
            }
            
            
            return resultSet;
    
            
        }
        catch (HDBModelException e) {
            throw new WGQueryException(queryString, "Exception executing HDBModel query", e);
        }
        
    }

    private WGResultSet executeLuceneQuery(String queryType, String queryString, Map<String,Object> wgapiParams, Map<String,Object> outputParams) throws WGException {
        
        // Tweak WGAPI parameters: Lucenes search method defaults to max results 500, so if we do not want to set a limit we must enforce the parameter being 0 here (#00002483)
        if (!wgapiParams.containsKey(WGDatabase.QUERYOPTION_MAXRESULTS)) {
            wgapiParams.put(WGDatabase.QUERYOPTION_MAXRESULTS, new Integer(0));
        }
       
        LuceneManager manager = _wga.getCore().getLuceneManager();
        if (manager == null) {
            throw new WGQueryException(queryString, "Lucene index is disabled");
        }
        
        List<String> fields=new ArrayList<String>();
        if(queryType.startsWith("lucene:")){
        	String field_list = queryType.substring("lucene:".length());
        	fields = WGUtils.deserializeCollection(field_list, ",");
        }
        
        WGResultSet resultSet= manager.search(_db, fields, queryString, wgapiParams, _wga);
        
        outputParams.put(QUERYOUT_SIMPLIFIED_QUERY, wgapiParams.get(LuceneManager.TAGINFO_SIMPLIFIEDQUERY));
        
        // if highlighting enabled store simplified lucene query in session
        if (ConversionUtils.getBoolean(wgapiParams.get(LuceneManager.QUERYOPTION_HIGHLIGHT), false) == true && _wga.isRequestAvailable()) {
            _wga.getHttpSession().setAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY, wgapiParams.get(LuceneManager.TAGINFO_SIMPLIFIEDQUERY));
        }
        
        return resultSet;
    }

    private WGResultSet executeExpressionQuery(Context context, String queryType, String queryString, Map<String,Object> wgapiParams, Map<String,Object> outputParams) throws WGException {
    
        if (context == null) {
            throw new UnavailableResourceException("Cannot execute expression query without TMLContext");
        }
        
        WGContent content = context.content();
        if (content.isDummy()) {
            throw new WGQueryException(queryString, "Cannot execute expression query from dummy context");
        }
    
        String scope = "children";
        boolean deep = true;
    
        // Parse native options
        String nativeOptionsString = (String) wgapiParams.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS);
        if (nativeOptionsString != null) {
            List<String> nativeOptions = WGUtils.deserializeCollection(nativeOptionsString, ",", true);
            if (nativeOptions.contains("flat")) {
                deep = false;
            }
            if (nativeOptions.contains("siblings")) {
                scope = "siblings";
            }
        }
    
        String language = queryType.substring(3);
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(language);
        if (engine == null) {
            throw new WGQueryException(queryString, "Unknown expression language: " + language);
        }
    
        WGStructEntryList structsToSearch;
        if (scope.equals("siblings")) {
            structsToSearch = content.getStructEntry().getSiblingEntries();
        }
        else {
            structsToSearch = content.getStructEntry().getChildEntries();
        }
    
        int maxDocs = ConversionUtils.getInteger(wgapiParams.get(WGDatabase.QUERYOPTION_MAXRESULTS), 0); 
    
        String prefLanguage = content.getLanguage().getName();
        if (prefLanguage == null) {
            prefLanguage = content.getLanguage().getName();
        }
    
        String role = null;
        if (!wgapiParams.containsKey(WGDatabase.QUERYOPTION_ENHANCE) || wgapiParams.get(WGDatabase.QUERYOPTION_ENHANCE).equals(new Boolean(true))) {
            role = (String) (wgapiParams.containsKey(WGDatabase.QUERYOPTION_ROLE) ? wgapiParams.get(WGDatabase.QUERYOPTION_ROLE) : "search");
        }
        
    
        List<WGContentKey> results = recurseExpressionQuery(engine, structsToSearch, prefLanguage, queryString, deep, maxDocs, role);
        WGCachedResultSet cachedSet = new WGCachedResultSet(results);
        return new WGStandardResultSet(content.getDatabase(), cachedSet, wgapiParams, queryString);
    
    }

    private List<WGContentKey> recurseExpressionQuery(ExpressionEngine engine, WGStructEntryList entryList, String language, String expr, boolean deep, int maxDocs, String role) throws WGException {
    
        Iterator<WGStructEntry> childEntries = entryList.iterator();
        WGStructEntry childEntry;
        WGContent childContent;
        List<WGContentKey> results = new ArrayList<WGContentKey>();
        while (childEntries.hasNext()) {
            childEntry = (WGStructEntry) childEntries.next();
            childContent = childEntry.getReleasedContent(language);
            if (childContent != null) {
                
                if (role == null || childContent.isVisibleFor(role)) {
                    ExpressionResult result = engine.evaluateExpression(expr, (TMLContext) _wga.createTMLContext(childContent), ExpressionEngine.TYPE_EXPRESSION, null);
                    if (result.isTrue()) {
                        results.add(childContent.getContentKey());
                        if (maxDocs != 0 && results.size() >= maxDocs) {
                            break;
                        }
                    }
                    
                    if (result.isError()) {
                        throw new WGQueryException(expr, "Exception executing expression query", result.getException());
                    }
                }
            }
    
            if (deep) {
                results.addAll(recurseExpressionQuery(engine, childEntry.getChildEntries(), language, expr, deep, maxDocs, role));
                if (maxDocs != 0 && results.size() >= maxDocs) {
                    break;
                }
            }
        }
    
        return results;
    
    }
    
    /**
     * Build default query parameters for the given content in context
     * @param content The content
     * @return Map of default query parameters
     * @throws WGAPIException
     */
    public static Map<String, Object> buildDefaultQueryParams(WGContent content) throws WGException {
        Map<String,Object> defaultParameters = new HashMap<String,Object>();
        if (content!=null && !content.isTemporary()) {
            defaultParameters.put("content", content);
            defaultParameters.put("key", content.getContentKey(true).toString());
            defaultParameters.put("language", content.getLanguage().getName());
            defaultParameters.put("structkey", String.valueOf(content.getContentKey(true).getStructKey()));
        }
        
        return defaultParameters;
    }
    
    /**
     * Creates a valid query parameter name for a given string.
     * Valid names contain only digits, letters and the underscore. Dashes and spaces are converted to underscores. All other characters omitted.
     * @param str A string to be converted to being a valid query parameter name
     * @return Valid query parameter name
     */
    public static String makeQueryParameterName(String str) throws WGException {
        
        StringBuilder paramName = new StringBuilder();
        
        char c;
        for (int i=0; i < str.length() ; i++) {
            c = str.charAt(i);
            if (Character.isDigit(c) || Character.isLetter(c) || c == '_') {
                paramName.append(c);
            }
            else if (c == '-' || c == ' ') {
                paramName.append("_");
            }
        }
        
        return paramName.toString();
        
    }
    
    /**
     * Returns the database key
     */
    public String getDbKey() {
        return _db.getDbReference();
    }
    
    /**
     * Returns a domain object for the domain that this database belongs to
     * @throws WGAServerException
     */
    public Domain domain() throws WGException {
        return _wga.domain((String) _db.getAttribute(WGACore.DBATTRIB_DOMAIN));
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_db == null) ? 0 : _db.hashCode());
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
        Database other = (Database) obj;
        if (_db == null) {
            if (other._db != null)
                return false;
        }
        else if (!_db.equals(other._db))
            return false;
        return true;
    }

    /**
     * Creates a WebTML Context object using this application for context.
     * This is a shortcut for {@link WGA#createTMLContext(WGDatabase)}.
     * @throws WGAServerException
     */
    @CodeCompletion
    public Context createTMLContext() throws WGException {
        return _wga.createTMLContext(_db);
    }
    
    /**
     * Reads a publisher option configured for the current database
     * This method returns the publisher option value in its native data type, like determined in module registry.
     * If the publisher option is not determined for this database this method will return the options default value from the module registry. 
     * @param name Name of the publisher option
     * @return Publisher option value
     */
    public Object getPublisherOption(String name) throws WGException {
        return _wga.getCore().readPublisherOptionOrDefault(_db, name);        
    }
    
    /**
     * Returns if the user has access to this database. If not many operations on this object will fail.
     */
    public boolean isOpen()  {
        return _db.isSessionOpen();
    }
    
    /**
     * Returns the access level that the user has on this database
     */
    public int getAccessLevel() {
        if (!isOpen()) {
            return WGDatabase.ACCESSLEVEL_NOACCESS;
        }
        else {
            return _db.getSessionContext().getAccessLevel();
        }
    }
    
    /**
     * Returns the distinguished name of the user currently logged in on this database
     */
    public String getUserName() {
        return _db.getSessionContext().getUserAccess().getPrimaryName();
    }
    
    /**
     * Returns if the user is logged in anonymously to this database
     */
    public boolean isAnonymous() {
        return isOpen() && _db.getSessionContext().isAnonymous();
    }
    
    /**
     * Returns if the user has at least READER access level on this database
     */
    public boolean isReader() {
        return getAccessLevel() >= WGDatabase.ACCESSLEVEL_READER;
    }
    
    /**
     * Returns if the user has at least AUTHOR access level on this database
     */
    public boolean isAuthor() {
        return getAccessLevel() >= WGDatabase.ACCESSLEVEL_AUTHOR;
    }
    
    /**
     * Returns if the user has at least EDITOR access level on this database
     */
    public boolean isEditor() {
        return getAccessLevel() >= WGDatabase.ACCESSLEVEL_EDITOR;
    }

    /**
     * Returns if the user has at least CHIEFEDITOR access level on this database
     */
    public boolean isChiefEditor() {
        return getAccessLevel() >= WGDatabase.ACCESSLEVEL_CHIEF_EDITOR;
    }
    
    /**
     * Returns if the user has MANAGER access level on this database
     */
    public boolean isManager() {
        return getAccessLevel() >= WGDatabase.ACCESSLEVEL_MANAGER;
    }
    
    /**
     * Reopens a currently open session on this database with the same authentication
     * If the session is not yet open it will stay closed
     */
    public void reopen() throws WGException {
        if (isOpen()) {
            _db.closeSession();
            _wga.openDatabase(_db);
        }
    }
    
    /**
     * Determines if the user currently logged in to the database is contained in the given names list
     * @param names A list of user names, group names and role names
     * @return True if the user is member, false if she is not
     */
    public boolean isUserMemberOf(List<String> names) throws WGException {
        return db().isMemberOfUserList(names);
    }


}
