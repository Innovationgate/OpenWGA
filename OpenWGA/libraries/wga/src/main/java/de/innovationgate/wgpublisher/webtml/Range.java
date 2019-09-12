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

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cache.WebTMLCache;
import de.innovationgate.wgpublisher.cache.WebTMLCache.CacheEntry;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.webtml.utils.BooleanItemExpression;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

public class Range extends Base {
    
    enum WrapMode {
        
        NONE,HTMLTAG,WRAPBODY
        
    }
    
    private static final long serialVersionUID = 1L;
    public static final String CACHEUPDATE_NEVER = "never";
    public static final String CACHEUPDATE_DB = "dbupdate";
    public static final String CACHEUPDATE_PRELOAD = "preload";
    public static final String CACHEUPDATE_BRANCH = "branchupdate";
    public static final Object CACHEUPDATE_CONTENT = "content";

    private String cachekey;
    private String cachelatency;
    private String cacheupdate;

    public String getCacheupdate() {
        return getTagAttributeValue("cacheupdate", cacheupdate, CACHEUPDATE_DB);
    }

    public void setCacheupdate(String cacheupdate) {
        this.cacheupdate = cacheupdate;
    }


    private String exprlanguage;
    
    private String waittimeout;

    private String defaultxpl;
    
    private String labelcontainer;
    private String labelfile;
    private String labellanguage;
    
    private String tmlscope;
    
    private String imagederivates;
    
    private String wraphtmltag;
    
    private String wrapendtag;
    
    public String getWrapendtag() {
        return getTagAttributeValue("wrapendtag", wrapendtag, "true");
    }

    public void setWrapendtag(String wrapendtag) {
        this.wrapendtag = wrapendtag;
    }


    private String wrapif;
    
    private String wrapbodytag;


    public String getWrapbodytag() {
        return getTagAttributeValue("wrapbody", wrapbodytag, null);
    }

    public void setWrapbodytag(String wrapbody) {
        this.wrapbodytag = wrapbody;
    }

    public String getWraphtmltag() {
        return getTagAttributeValue("wraptag", wraphtmltag, null);
    }

    public void setWraphtmltag(String wraptag) {
        this.wraphtmltag = wraptag;
    }

    public String getImagederivates() {
        return getTagAttributeValue("imagederivates", imagederivates, null);
    }

    public void setImagederivates(String filederivates) {
        this.imagederivates = filederivates;
    }

    public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
        private String _currentCacheKey;
        private String _currentCacheId;
        private int cacheLatency = 0;
    
        private boolean cacheActive = false;
        private boolean putCache;
        private boolean servedStale = false;
        private WrapMode wrapMode = WrapMode.NONE;
        private boolean wrapActive = false;
        private Object wrapBodyResult = null;
        private Date cacheDate;
        
        @Override
        public Object getTagInfo(String name) throws WGAPIException {
            if (name.equals("cachekey")) {
                return this._currentCacheKey;
            }
            else if (name.equals("cacheused")) {
                return new Boolean(this.cacheActive && !this.putCache);
            }
            else if (name.equals("cachestale")) {
                return new Boolean(this.servedStale);
            }
            
            return super.getTagInfo(name);
        }
        
        @Override
        public boolean isDirectOutputCapable() {
            return !this.cacheActive && this.wrapMode != WrapMode.WRAPBODY;
        }
    }
    
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }


    private static Map<String,Long> currentlyEvaluatedCaches = Collections.synchronizedMap(new HashMap<String,Long>());

    
    
    public void tmlEndTag() throws TMLException, WGAPIException {

        Status status = (Status) getStatus();
        
        // Wrapped tag
        if (status.wrapActive) {
            String tagName = getWraphtmltag();
            if (tagName != null) {
                if (stringToBoolean(getWrapendtag())) {
                    StringBuilder endTag = new StringBuilder();
                    endTag.append("</").append(tagName);
                    endTag.append(">");
                    appendResult(endTag.toString());
                }
            }
        }
        else if (status.wrapMode == WrapMode.WRAPBODY) {
            clearResult();
            if (status.wrapBodyResult != null) {
                setResult(status.wrapBodyResult);
            }
        }
        
        // WebTML cache storage
        if (status.putCache == true) {
            try {
                getCore().getWebTMLCache().putCacheEntry(getTMLContext().db().getDbReference(), status._currentCacheId, status._currentCacheKey, this.getResultString(), status.cacheDate, status.cacheLatency);
            }
            catch (CacheException e) {
                getTMLContext().getlog().error("Exception writing WebTML cache data for key " + getTMLContext().db().getDbReference() + "/" + status._currentCacheId + "/" + status._currentCacheKey, e);
            }
        }
              
   }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.wgpublisher.webtml.Base#tmlStartTag()
     */
    public void tmlStartTag() throws TMLException, WGException {

        final Status status = (Status) getStatus();

        // Initialize
        String latencyStr = getCachelatency();
        if (!WGUtils.isEmpty(latencyStr)) {
            status.cacheLatency = stringToInteger(latencyStr, 0) * 60;
        }

        status._currentCacheKey = null;
        status._currentCacheId = getId();
        status.putCache = false;
        status.cacheDate = null;
        status.servedStale = false;

        // Set default xpl
        String xpl = this.getDefaultxpl();
        if (xpl != null) {
            status.setOption(Base.OPTION_DEFAULT_XPLANGUAGE, xpl, null);
        }
        
        // Set label information
        String labelContainer = getLabelcontainer();
        if (labelContainer != null) {
            status.setOption(Base.OPTION_DEFAULT_LABELCONTAINER + getTMLContext().getDesignDBKey(), labelContainer, null);
        }
        
        String labelFile = getLabelfile();
        if (labelFile != null) {
            status.setOption(Base.OPTION_DEFAULT_LABELFILE + getTMLContext().getDesignDBKey(), labelFile, null);
        }

        String labelLanguage = getLabellanguage();
        if (labelLanguage != null) {
            status.setOption(Base.OPTION_DEFAULT_LABELLANGUAGE, labelLanguage, null);
        }

        // Set scope information
        String scope = getTmlscope();
        if (scope != null) {
            status.setOption(Base.OPTION_WEBTML_SCOPE, scope, TMLOption.SCOPE_GLOBAL);
        }
        
        // Set file derivate information
        String fileDerivates = getImagederivates();
        if (fileDerivates != null) {
            DerivateQuery derivateQuery = getTMLContext().enhanceFileDerivateQuery(fileDerivates);
            if (!derivateQuery.isNoDerivate()) {
                status.setOption(Base.OPTION_IMAGE_DERIVATES, derivateQuery.toString(), TMLOption.SCOPE_GLOBAL);
            }
        }

        // WebTML Cache functionality
        String currentCacheby = this.getCachekey();
        if (currentCacheby != null && getCore().isWebTMLCachingEnabled()) {
            if (!processWebTMLCache(status, currentCacheby)) {
                return;
            }
        }
        
        // Wrapping
        String wrapHtmltag = getWraphtmltag();
        final String wrapBody = getWrapbodytag();
        if (wrapHtmltag != null || wrapBody != null) {
            if (wrapBody != null) {
                status.wrapMode = WrapMode.WRAPBODY;
            }
            else {
                status.wrapMode = WrapMode.HTMLTAG;
            }
            
            String wrapIf = getWrapif();
            status.wrapActive = true;
            if (wrapIf != null) {
                DynamicAttribute wrapEquals = getStatus().dynamicOptions.get("wrapif_equals");
                DynamicAttribute wrapIn = getStatus().dynamicOptions.get("wrapif_in");
                status.wrapActive = evaluateItemConditionAttribute(wrapIf, wrapEquals, wrapIn);
            }
            
            if (status.wrapActive) {
                
                if (wrapHtmltag != null) {
                    StringBuilder startTag = new StringBuilder();
                    startTag.append("<").append(wrapHtmltag)
                        .append(buildDynamicHtmlAttributes())
                        .append(">");
                    appendResult(startTag.toString());
                }
            }
            else if (wrapBody != null) {
                addResultInterceptor(new ResultInterceptor() {
                    @Override
                    public boolean interceptResult(BaseTagStatus interceptStatus) {
                        if (wrapBody.equals(interceptStatus.id)) {
                            status.wrapBodyResult = interceptStatus.result;
                            return false;
                        }
                        return true;
                    }
                });
            }
        }
    }

    protected boolean processWebTMLCache(Status status, String currentCacheby) throws TMLException {
        
        if (status._currentCacheId == null) {
            this.addWarning("To use cache, the range tag must have a unique id");
            return true;
        }
        
        // Eval cache key
        if (!currentCacheby.equals("")) {
            ExpressionEngine engine = ExpressionEngineFactory.getEngine(this.getExprlanguage());
            ExpressionResult result = engine.evaluateExpression(currentCacheby, this.getTMLContext(), ExpressionEngine.TYPE_EXPRESSION, null);
            if (result.isError()) {
                addExpressionWarning(currentCacheby, result);
                return true;
            }
            Object key = result.getResult();
            if(key==null)
            	return true;
            status._currentCacheKey = String.valueOf(key);
        }
        else {
            status._currentCacheKey = "";
        }
        
        status.cacheActive = true;
        String cacheUpdate = getCacheupdate();
        
        // In browser interface we add a custom value to the cache key, so BI caches and Non-Bi caches differ (B000059DA)
        if (getTMLContext().isbrowserinterface() && !CACHEUPDATE_PRELOAD.equals(cacheUpdate)) {
            status._currentCacheKey = status._currentCacheKey + "###BROWSER-INTERFACE";
        }

        WGDatabase db = this.getTMLContext().content().getDatabase();
        

        // Look if the current cache is in evaluation right now, wait for 10 seconds
        int sleepTimes = 0;       
        String cacheLockKey = buildCacheLockKey();
        int waitTimeout = 60;
        try {
            waitTimeout = Integer.parseInt(getWaittimeout());
        }
        catch (NumberFormatException e) {
            addWarning("Cannot parse waittimeout as number: " + getWaittimeout());
        }
        
        if (!CACHEUPDATE_PRELOAD.equals(cacheUpdate) && currentlyEvaluatedCaches.containsKey(cacheLockKey)) { 
            
            // If we are allowed to serve stale data while the cache processes, we serve the current content of the cache
            boolean serveStaleData = db.getBooleanAttribute(WGACore.DBATTRIB_WEBTMLCACHE_SERVESTALEDATA, true);
            if (serveStaleData) {
                String content = null;
                try {
                    WebTMLCache.CacheEntry cacheEntry = getCore().getWebTMLCache().getCacheEntry(db.getDbReference(), status._currentCacheId, status._currentCacheKey);
                    if (cacheEntry != null) {
                        content = cacheEntry.getCode();
                    }
                }
                catch (CacheException e) {
                    getTMLContext().getlog().error("Exception retrieving WebTML cache data for key " + getTMLContext().db().getDbReference() + "/" + status._currentCacheId + "/" + status._currentCacheKey, e);
                }
                if (content != null) {
                    
                    // Tell all ranges above us to have a cache latency of 1 minute, so cached stale data is updated after this time
                    Range.Status ancestor = status;
                    while ((ancestor = (Range.Status) ancestor.getAncestorTag(Range.class)) != null) {
                        ancestor.cacheLatency = 1 * 60;
                    }
                    
                    status._currentCacheKey = null;
                    status._currentCacheId = null;
                    status.servedStale = true;
                    this.setEvalBody(false);
                    this.setResult(content);
                    return false;
                }
            }
            
            // Else we wait until cache calculation is completed or we run into the timeout
            while (currentlyEvaluatedCaches.containsKey(cacheLockKey)) {
                
                Long cacheTime = (Long) currentlyEvaluatedCaches.get(cacheLockKey);
                if( cacheTime != null ){
                long cacheTimeout = cacheTime.longValue() + (1000 * 60 * 15);
                    if (System.currentTimeMillis() > cacheTimeout) {
                        log.warn("Former cache creation of key '" + status._currentCacheKey + "' took more than 15 minutes. Removing synchronisation lock now");
                        currentlyEvaluatedCaches.remove(cacheLockKey);
                        break;
                    }
                }
                try {
                    Thread.sleep(1000);
                }
                catch (InterruptedException e) {
                }
                sleepTimes++;
                if (sleepTimes > waitTimeout) {
                    log.warn("Waiting for cache creation of key '" + cacheLockKey + "' timed out after " + waitTimeout + " second(s). Tag is canceled.");
                    this.addWarning("Waiting for cache creation of key '" + cacheLockKey + "' timed out after " + waitTimeout + " second(s). Tag is canceled.", true);
                    this.setEvalBody(false);
                    this.setCancelTag(true);
                    this.setResult("This section is currently recalculated. Please try again later.");
                    return false;
                }
            }
        }
        
        if (!CACHEUPDATE_PRELOAD.equals(cacheUpdate)) { // No need to sync on preloaded cache
            currentlyEvaluatedCaches.put(cacheLockKey, new Long(System.currentTimeMillis()));
        }
        
        // Determine tested date by cacheupdate mode
        Date testedDate;
        try {
            if (CACHEUPDATE_DB.equals(cacheUpdate) || CACHEUPDATE_NEVER.equals(cacheUpdate) || CACHEUPDATE_BRANCH.equals(cacheUpdate) || CACHEUPDATE_PRELOAD.equals(cacheUpdate)) {
                testedDate = getCore().getDeployer().getLastChangedOrDeployed(db);
            }
            else if (CACHEUPDATE_CONTENT.equals(cacheUpdate)) {
                Date contentDate = getTMLContext().getcontent().getLastModified();
                Date designDate =  getCore().getDeployer().getLastDeployed(db.getDbReference());
                testedDate = (contentDate.after(designDate) ? contentDate : designDate);
            }
            else {
                throw new TMLException("Unknown value for attribute 'cacheupdate':" + cacheUpdate, true);
            }
        }
        catch (WGAPIException e1) {
            throw new TMLException("Exception determining cache test date", e1, true);
        }
        
        // Read cache and determine if to use it
        String content = null;
        try {
            
            CacheEntry cacheEntry;
            if (CACHEUPDATE_PRELOAD.equals(cacheUpdate)) {
                cacheEntry = getCore().getWebTMLCache().getPreloadCacheEntry(db.getDbReference(), status._currentCacheId, status._currentCacheKey);                    
            }
            else {
                cacheEntry = getCore().getWebTMLCache().getCacheEntry(db.getDbReference(), status._currentCacheId, status._currentCacheKey);
            }
            
            if (cacheEntry != null && cacheEntry.isValid()) {
                
                boolean useCache = false;
                if (CACHEUPDATE_DB.equals(cacheUpdate) || CACHEUPDATE_CONTENT.equals(cacheUpdate)) {
            		useCache = cacheEntry.getDate().getTime() >= testedDate.getTime();
                }
                else {
                    useCache = true;
                }

                if (useCache) {
                    content = cacheEntry.getCode();
                }
            }
        }
        catch (CacheException e) {
            throw new TMLException("Exception retrieving WebTML cache data for key "  + getTMLContext().db().getDbReference() + "/" + status._currentCacheId + "/" + status._currentCacheKey, e, true);
        }
        
        if (content != null) {
            Range.currentlyEvaluatedCaches.remove(cacheLockKey);
            status._currentCacheId = null;
            this.setEvalBody(false);
            this.setResult(content);
            return false;
        }
        else {
            if (CACHEUPDATE_PRELOAD.equals(cacheUpdate)) {
                throw new TMLException("Trying to load preloaded cache which is not existent: " + status._currentCacheId + "/" + status._currentCacheKey, true);
            }
            status.putCache = true;
            status.cacheDate = testedDate;
            if (log.isDebugEnabled()) {
                log.debug("Trying to evaluate cache for key '" + cacheLockKey + "'");
            }
        }
        
        return true;
    }

    private String buildCacheLockKey() {
        Status status = (Status) getStatus();
        return getTMLContext().db().getDbReference() + "//" + status._currentCacheId + "//" + status._currentCacheKey;
    }
    
    public static void clearCacheLocks() {
        Range.currentlyEvaluatedCaches.clear();
    }

    /**
     * Returns the cacheby.
     * 
     * @return String
     */
    public String getCachekey() {
        return this.getTagAttributeValue("cachekey", cachekey, null);
    }

    /**
     * Sets the cacheby.
     * 
     * @param cacheby
     *            The cacheby to set
     */
    public void setCachekey(String cacheby) {
        this.cachekey = cacheby;
    }

    /**
     * Returns the exprlanguage.
     * 
     * @return String
     */
    public String getExprlanguage() {
        return this.getTagAttributeValue("exprlanguage", exprlanguage, this.getDefaultExpressionLanguage());
    }

    public String getXplanguage() {
        return this.getExprlanguage();
    }

    public void setXplanguage(String xpl) {
        this.setExprlanguage(xpl);
    }

    /**
     * Sets the exprlanguage.
     * 
     * @param exprlanguage
     *            The exprlanguage to set
     */
    public void setExprlanguage(String exprlanguage) {
        this.exprlanguage = exprlanguage;
    }

    /**
     * Returns the defaultxpl.
     * 
     * @return String
     */
    public String getDefaultxpl() {
        return this.getTagAttributeValue("defaultxpl", defaultxpl, null);
    }

    /**
     * Sets the defaultxpl.
     * 
     * @param defaultxpl
     *            The defaultxpl to set
     */
    public void setDefaultxpl(String defaultxpl) {
        this.defaultxpl = defaultxpl;
    }

    /**
     * @return Returns the labelcontainer.
     */
    public String getLabelcontainer() {
        return getTagAttributeValue("labelcontainer", labelcontainer, null);
    }

    /**
     * @param labelcontainer The labelcontainer to set.
     */
    public void setLabelcontainer(String labelcontainer) {
        this.labelcontainer = labelcontainer;
    }

    /**
     * @return Returns the labelfile.
     */
    public String getLabelfile() {
        return getTagAttributeValue("labelfile", labelfile, null);
    }

    /**
     * @param labelfile The labelfile to set.
     */
    public void setLabelfile(String labelfile) {
        this.labelfile = labelfile;
    }

    /**
     * @return Returns the labellanguage.
     */
    public String getLabellanguage() {
        return getTagAttributeValue("labellanguage", labellanguage, null);
    }

    /**
     * @param labellanguage The labellanguage to set.
     */
    public void setLabellanguage(String labellanguage) {
        this.labellanguage = labellanguage;
    }

    public static Map<String,Long> getCurrentlyEvaluatedCaches() {
        return currentlyEvaluatedCaches;
    }

    protected void tmlCleanup() {
        
        Status status = (Status) getStatus();
        if (status.putCache == true && status._currentCacheKey != null) {
            Long time = (Long) Range.currentlyEvaluatedCaches.remove(buildCacheLockKey());
            if (time == null) {
                addWarning("Could not remove cache lock key '" + buildCacheLockKey() + "' because it was not registered anymore");
            }
        }
        
    }

    public String getWaittimeout() {
        return getTagAttributeValue("waittimeout", waittimeout, "60");
    }

    public void setWaittimeout(String waittimeout) {
        this.waittimeout = waittimeout;
    }

    public String getCachelatency() {
        return getTagAttributeValue("cachelatency",cachelatency, null); 
    }

    public void setCachelatency(String cachelatency) {
        this.cachelatency = cachelatency;
    }

    public String getTmlscope() {
        return getTagAttributeValue("tmlscope", tmlscope, null);
    }

    public void setTmlscope(String tmlscope) {
        this.tmlscope = tmlscope;
    }
    
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "html", "wrapif"); 
    }

    public String getWrapif() {
        return getTagAttributeValue("wrapif", wrapif, null);
    }

    public void setWrapif(String wrapif) {
        this.wrapif = wrapif;
    }


}

