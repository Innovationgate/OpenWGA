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
package de.innovationgate.wgpublisher.webtml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import net.sf.cglib.transform.impl.AddPropertyTransformer;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGCachedResultSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGReportingResultSetCore;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.webgate.api.WGStandardResultSet;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGStructEntryList;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.utils.NativeQueryOptions;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.QueryResult;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.hdb.HDBModelException;
import de.innovationgate.wgpublisher.lang.LanguageBehaviour;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;
import de.innovationgate.wgpublisher.lang.SingleLanguageChooser;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.webtml.utils.ResultSetTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Query extends Base implements DynamicAttributes  {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;


    public class QueryData {

        private String _query;

        private String _type;

        public String getQuery() {
            return _query;
        }

        public void setQuery(String query) {
            _query = query;
        }

        public String getType() {
            return _type;
        }

        public void setType(String type) {
            _type = type;
        }

    }

    public static final String TAGINFO_FULLQUERY = "fullquery";
    
    public static final String TAGINFO_UNSPECIFICQUERY = "unspecificquery";
    
    public static final String SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY = "de.innovationgate.wgpublisher.webtml.Query.SIMPLIFIED_LUCENE_QUERY";
    
    public static final String TAGINFO_ERROR = "error";
    public static final String TAGINFO_CACHEUSED = "cacheused";

    private static final Object TAGINFO_EXECUTIONTIME = "executiontime";
    public static final Object TAGINFO_TOTALPROCESSINGTIME = "totalprocessingtime";

    private String name;

    private String type;

    private String db;

    private String alllanguages;

    private String includecurrent;

    private String highlight;

    private String enhance;
    
    private String onlyvisible;

    private String max;

    private String cache;

    private String role;
    
    private String returnField;
    
    private String options;

    
    public static class Status extends BaseTagStatus implements TMLParameterReceiver  {
        
        private String fullQuery;
        
        private boolean unspecificQuery;
        
        private boolean usedCache;
    
        private WGContent firstContent = null;
        private boolean firstContentRetrieved = false; 
    
        private HashMap queryParameters;
        
        private Throwable error = null;
        
        private WGResultSet _resultSet;
        
        private String _role;
        
        private void retrieveFirstContent() {
            
            try {
                if (firstContentRetrieved) {
                    return;
                }
                
                WGContent tryContent = null;
                
                if (_resultSet != null) {
                    SkippingIterator<WGContent> resultIt = _resultSet.getResultIterator();
                    while  (resultIt.hasNext()) {
                        try {
                            tryContent = resultIt.next();
                            if (tryContent.mayBePublished(tmlContext.isbrowserinterface(), _role)) {
                                firstContent = tryContent;
                                break;
                            }
                        
                        }
                        catch (WGAPIException e) {
                            tmlContext.addwarning("Exception extracting first result from query. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                            log.error("Exception extracting first result from query", e);
                        }                    
                    }
                }
                
                firstContentRetrieved = true;
            }
            catch (Exception e) {
                tmlContext.addwarning("Exception extracting first result from query. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                log.error("Exception extracting first result from query", e);
            }
        }
        
        public WGContent getFirstContent() {
            retrieveFirstContent();
            return firstContent;
        }
        
        @Override
        public Object getTagInfo(String name) throws WGAPIException {
            if (name.equals(TAGINFO_FULLQUERY)) {
                return this.fullQuery;
            }
            else if (name.equals(TAGINFO_UNSPECIFICQUERY)) {
                return Boolean.valueOf(this.unspecificQuery);
            }
            else if (name.equals(TAGINFO_ERROR)) {
                return this.error;
            }
            else if (name.equals(TAGINFO_CACHEUSED)) {
                return new Boolean(this.usedCache);
            }
            else if (name.equals(TAGINFO_EXECUTIONTIME)) {
                if (_resultSet != null) {
                    return _resultSet.getExecutionTime();
                }
                else {
                    return 0;
                }
            }
            if (name.equals("count")) {
                if (_resultSet != null) {
                    try {
                        return _resultSet.results();
                    }
                    catch (WGBackendException e) {
                        tmlContext.getlog().error("Exception retrieving result size for query", e);
                    }
                }
                return 0;
            }
            
            
            return super.getTagInfo(name);
        }
        
        public void addParam(String name, Object value, String type) {
            queryParameters.put(name.toLowerCase(), value);
        }
        
        
    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
    
    





    

    public String getReturn() {
        return this.getTagAttributeValue("return", returnField, null);
    }

    public void setReturn(String returnField) {
        returnField = returnField;
    }

    public void tmlEndTag() throws WGException, TMLException {

        Status status = (Status) getStatus();
        
        this.setResultOutput(false);

        // Get the db
        WGDatabase db = null;
        if ( (this.getDb() == null) || this.getDb().trim().startsWith("*") || this.getDb().indexOf(",") != -1){
            db = this.getTMLContext().getdocument().getDatabase();
        }
        else {
            try {
                db = (WGDatabase) this.openContentDB(getTMLContext().resolveDBKey(this.getDb().toLowerCase()));
            }
            catch (WGUnavailableException e) {
                this.addWarning("Database '" + getDb() + "' is currently unavailable");
                status.error = e;
                return;
            }
            catch (WGException e) {
                this.addWarning(e.getMessage());
                status.error = e;
                return;                
            }
            if (db == null) {
                this.addWarning("Could not find database to query: " + this.getDb(), true);
                status.error = new WGInvalidDatabaseException("Could not find database to query: " + this.getDb());
                return;
            }
            if (db.isSessionOpen() == false) {
                this.addWarning("User cannot open database " + this.getDb(), true);
                status.error = new WGAuthorisationException("User cannot open database " + this.getDb());
                return;
            }
        }
        
        // Process dynamic attributes
        for (DynamicAttribute att : status.dynamicOptions.values()) {
            if (att.getPrefix().equals("p")) {
                status.addParam(att.getBaseName(), att.getDynamicValue(getTMLContext()), null);
            }
        }

        Map<String,Object> options = buildAttributes(db);
        String queryString = this.getResultString().trim();
        String queryType = this.getType();
        
        status._role = getRole();
        QueryResult queryResult;
        try {
            queryResult = WGA.get(getTMLContext()).database(db).query(queryType, queryString, options, status.queryParameters);
            status._resultSet = queryResult.getSet();
        }
        catch (Throwable e) {
            status.error = e;
            throw new TMLException("Exception executing query", e, true);
        }

        if (status._resultSet != null) {
            
            // Transfer output parameters back to status object
            status.fullQuery = queryResult.getFullQuery();
            status.usedCache = queryResult.isServedFromCache();
            status.unspecificQuery = WGUtils.toBoolean(queryResult.getOutputParams().get(Database.QUERYOUT_SIMPLIFIED_QUERY), false);

            ResultSetTagStatus parent = (ResultSetTagStatus) getStatus().getAncestorTag(ResultSetTagStatus.class);
            if (parent != null) {
                String language = (String) (options.containsKey(WGDatabase.QUERYOPTION_ONLYLANGUAGE) ? options.get(WGDatabase.QUERYOPTION_ONLYLANGUAGE) : ResultSetTagStatus.MULTILANGUAGE_RESULT);
                parent.addResultSet(status._resultSet, language);
            }

            // Extract the first result for direct access (only neccessary if the query tag is addressable via ID or if return attribute is used)
            if (getReturn() != null && status._resultSet.hasResults()) {
                status.retrieveFirstContent();

                String returnItem = this.getReturn();
                if (returnItem != null) {
                    if (status.firstContent != null && status.firstContent.hasItem(returnItem)) {
                        this.setResult(status.firstContent.getItemValue(returnItem));
                        this.setResultOutput(true);
                    }
                }
            }

        }
        else {
            this.addWarning("Query had no result. type: " + this.getType() + " - query: " + this.getResult(), false);
            return;
        }
    }


    public Map<String, Object> buildAttributes(WGDatabase db) throws WGAPIException, TMLException {
        
        Map<String,Object> params = new HashMap<String, Object>();
        params.put(Database.QUERYATT_ONLYPUBLISHED, this.getOnlypublished());
        params.put(Database.QUERYATT_ONLYVISIBLE, this.getOnlyvisible());
        params.put(Database.QUERYATT_ALLLANGUAGES,this.getAlllanguages());
        params.put(Database.QUERYATT_INCLUDECURRENT, this.getIncludecurrent());
        params.put(Database.QUERYATT_ROLE, this.getRole());
        params.put(Database.QUERYATT_CACHE, this.getCache());
        params.put(Database.QUERYATT_DB, getDb());
        params.put(Database.QUERYATT_OPTIONS, getOptions());
        params.put(Database.QUERYATT_MAX, getMaxResults(db));
        params.put(Database.QUERYATT_HIGHLIGHT, getHighlight());

        return params;
        
    }

    
    /**
     * determine max query results
     * @return int maxQueryResults
     */
    private int getMaxResults(WGDatabase db) {
        // Determine max query results default
        int defaultMaxResults = WGACore.DEFAULT_QUERY_MAXRESULTS;
        if (db != null) {
            defaultMaxResults = ((Integer) getCore().readPublisherOptionOrDefault(db, WGACore.DBATTRIB_MAXQUERYRESULTS)).intValue();
        }

        // Try to find max results for this query
        String maxStr = this.getMax();
        return stringToInteger(maxStr, defaultMaxResults);

    }



    /**
     * Gets the db
     * 
     * @return Returns a String
     */
    public String getDb() {
        return this.getTagAttributeValue("db", db, null);
    }

    /**
     * Sets the db
     * 
     * @param db
     *            The db to set
     */
    public void setDb(String db) {
        this.db = db;
    }

    /**
     * Gets the type
     * 
     * @return Returns a String
     */
    public String getType() {
        return this.getTagAttributeValue("type", type, this.getTMLContext().content().getDatabase().getAttribute(WGACore.DBATTRIB_QUERY_DEFAULT).toString());
    }

    /**
     * Sets the type
     * 
     * @param type
     *            The type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Gets the alllanguages
     * 
     * @return Returns a String
     */
    public String getAlllanguages() {
        return this.getTagAttributeValue("alllanguages", alllanguages, "false");
    }

    /**
     * Sets the alllanguages
     * 
     * @param alllanguages
     *            The alllanguages to set
     */
    public void setAlllanguages(String alllanguages) {
        this.alllanguages = alllanguages;
    }

    /**
     * Gets the max
     * 
     * @return Returns a String
     */
    public String getMax() {
        return this.getTagAttributeValue("max", max, null);
    }

    /**
     * Sets the max
     * 
     * @param max
     *            The max to set
     */
    public void setMax(String max) {
        this.max = max;
    }

    public String getOnlyvisible(){
    	return this.getTagAttributeValue("onlyvisible", onlyvisible, "true");
    }
    public void setOnlyvisible(String flag){
    	this.onlyvisible=flag;
    }
    
    /**
     * Gets the enhance
     * 
     * @return Returns a String
     */
    public String getOnlypublished() {
        return this.getTagAttributeValue("onlypublished", enhance, "true");
    }

    /**
     * Sets the enhance
     * 
     * @param enhance
     *            The enhance to set
     */
    public void setOnlypublished(String enhance) {
        this.enhance = enhance;
    }

    /**
     * Gets the name
     * 
     * @return Returns a String
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name
     * 
     * @param name
     *            The name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.QueryParameterReceiver#addParam(java.lang.String, java.lang.Object)
     */


    /**
     * @see Base#tmlStartTag()
     */
    public void tmlStartTag() throws TMLException {
        
        Status status = (Status) getStatus();
        
        status.queryParameters = new HashMap();
        status.firstContent = null;
        status.firstContentRetrieved = false;
        status.error = null;
        status.usedCache = false;
        
        Collection.Status collectionTag = (Collection.Status) getStatus().getAncestorTag(Collection.class);
        if (collectionTag != null) {
            status.queryParameters.putAll(collectionTag.getQueryParameters());
        }
        
    }



















    /**
     * Returns the cache.
     * 
     * @return String
     */
    public String getCache() {
        return this.getTagAttributeValue("cache", cache, "false");
    }

    /**
     * Sets the cache.
     * 
     * @param cache
     *            The cache to set
     */
    public void setCache(String cache) {
        this.cache = cache;
    }

    /**
     * Returns the role.
     * 
     * @return String
     */
    public String getRole() {
        return this.getTagAttributeValue("role", role, WGContent.DISPLAYTYPE_SEARCH);
    }

    /**
     * Sets the role.
     * 
     * @param role
     *            The role to set
     */
    public void setRole(String role) {
        this.role = role;
    }

    /**
     * Returns the includecurrent.
     * 
     * @return String
     */
    public String getIncludecurrent() {
        return this.getTagAttributeValue("includecurrent", includecurrent, "false");
    }

    /**
     * Sets the includecurrent.
     * 
     * @param includecurrent
     *            The includecurrent to set
     */
    public void setIncludecurrent(String includecurrent) {
        this.includecurrent = includecurrent;
    }

    /**
     * @return
     */
    public String getOptions() {
        return this.getTagAttributeValue("options", options, null);
    }

    /**
     * @param string
     */
    public void setOptions(String string) {
        options = string;
    }

    public void setUnspecificQuery(boolean unspecificQuery) {
        Status status = (Status) getStatus();
        status.unspecificQuery = unspecificQuery;
    }

    public String getHighlight() {
        return this.getTagAttributeValue("hightlight", highlight, "false");
    }

    public void setHighlight(String highlight) {
        this.highlight = highlight;
    }

    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "p"); 
    }

}
