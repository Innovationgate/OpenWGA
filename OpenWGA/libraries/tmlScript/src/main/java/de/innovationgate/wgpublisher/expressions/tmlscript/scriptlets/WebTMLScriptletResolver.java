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

package de.innovationgate.wgpublisher.expressions.tmlscript.scriptlets;

import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.authoring.remotedoc.RemoteDocReference;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.GlobalExpressionScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.utils.SrcSetCreator;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

public class WebTMLScriptletResolver {
    
    private RhinoExpressionEngineImpl _engine;

    public WebTMLScriptletResolver(RhinoExpressionEngineImpl engine) {
        _engine = engine;
        
    }

    /**
     * Method resolveScriptlets.
     * 
     * @param result
     * @return Object
     * @throws ParseException 
     * @throws WGAPIException 
     */
    public String resolveScriptlets(Object input, TMLContext context, Map<String,Object> engineParams) throws WGException {
    
        if (input == null) {
            return null;
        }
        
        if (input instanceof List) {
            Iterator<?> results = ((List<?>) input).iterator();
            List<String> resolvedResults = new ArrayList<String>();
            while (results.hasNext()) {
                resolvedResults.add(resolveScriptlets(results.next(), context, engineParams));
            }
            return WGUtils.serializeCollection(resolvedResults, "");
        }
    
        String unresolved = input.toString();
        
        Integer level = (Integer) engineParams.get(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL);
        if (level == null) {
        	level = RhinoExpressionEngine.LEVEL_SYSTEM_MACROS;
        }
        
        // System macros
        String returnValue = parseForScriptlets(context, unresolved, "{%", "%}", (level.intValue() >= RhinoExpressionEngine.LEVEL_SCRIPTLETS.intValue()), engineParams);
    
        // User macros and real scriptlets
        if (level.intValue() >= RhinoExpressionEngine.LEVEL_MACROS.intValue()) {
            returnValue = parseForScriptlets(context, returnValue, "<br>{@", "@}", (level.intValue() >= RhinoExpressionEngine.LEVEL_SCRIPTLETS.intValue()), engineParams);
            returnValue = parseForScriptlets(context, returnValue, "{@", "@}", (level.intValue() >= RhinoExpressionEngine.LEVEL_SCRIPTLETS.intValue()), engineParams);
        }
    
        return returnValue;
    
    
    }

    private String parseForScriptlets(TMLContext context, String unresolved, String startSeq, String endSeq, boolean allowScriptlets, Map<String,Object> engineParams) throws WGException {
        
        StringBuffer resolved = new StringBuffer();
        int lastParamEndPos = 0;
        int startPos = -1;
        int endPos = -1;
        try {
            String scriptlet;
            while ((startPos = unresolved.indexOf(startSeq, startPos + 1)) != -1) {
                endPos = unresolved.indexOf(endSeq, startPos + 1);
                if (endPos != -1) {
                    scriptlet = unresolved.substring(startPos + startSeq.length(), endPos);
                    
                    if (lastParamEndPos < startPos) {
                        resolved.append(unresolved.substring(lastParamEndPos, startPos));
                    }
                    resolved.append(executeScriptlet(scriptlet, context, allowScriptlets, engineParams));
                    lastParamEndPos = endPos + endSeq.length();
                    
                    // Look if we have a following <br> which we want to swallow
                    if (unresolved.length() >= lastParamEndPos + 4 && unresolved.substring(lastParamEndPos, lastParamEndPos + 4).equals("<br>")) {
                        lastParamEndPos += 4;
                    }
                    
                }
                else {
                    // If we cannot find a terminator we exit "gracefully" by returning the rest of the string unmodified
                    if (lastParamEndPos < startPos) {
                        resolved.append(unresolved.substring(lastParamEndPos, startPos));
                    }
                    resolved.append(unresolved.substring(startPos));
                    return resolved.toString();
                }
            }
            if (lastParamEndPos < unresolved.length()) {
                resolved.append(unresolved.substring(lastParamEndPos));
            }
            String returnValue = resolved.toString();
            return returnValue;
        }
        catch (UnsupportedEncodingException e) {
            throw new WGException("Unsupported encoding: " + e.getMessage());
        }
    
    }

    /**
     * Method executeScriptlet.
     * 
     * @param scriptletToken
     * @return Object
     * @throws UnsupportedEncodingException 
     * @throws WGAPIException 
     */
    protected String executeScriptlet(String scriptletToken, TMLContext context, boolean allowSecondLevelScriptlets, Map<String,Object> params) throws UnsupportedEncodingException, WGException {
    
        try {
        // Resolve context
        if (scriptletToken.startsWith("(")) {
            int endContext = scriptletToken.indexOf(")");
            if (endContext == -1) {
                context.addwarning("Error executing scriptlet. Context expression not terminated: " + scriptletToken, false);
                return "";
            }
            String contextExpression = scriptletToken.substring(1, endContext);
            TMLContext targetContext = context.context(contextExpression, false);
            if (targetContext == null) {
                context.addwarning("Error executing scriptlet. Context could not be resolved: " + contextExpression, false);
                return "";
            }
            context = targetContext;
            scriptletToken = scriptletToken.substring(endContext + 1);
        }
        
        // First level scriptlets
        if (scriptletToken.startsWith("$")) {
            String metaName = scriptletToken.substring(1);
            String metaType = "content";
            int colonPos = metaName.indexOf(":");
            if (colonPos != -1) {
                metaType = metaName.substring(0, colonPos);
                metaName = metaName.substring(colonPos + 1);
            }
            Object result = context.meta(metaType, metaName);
            return result != null ? String.valueOf(result) : "";
        }
        else if (scriptletToken.startsWith("#")) {
        	Object result = context.item(scriptletToken.substring(1));
            return result != null ? String.valueOf(result) : "";
        }
        else if (scriptletToken.startsWith("!")) {
            return executeCommandMacro(context, scriptletToken.substring(1), params);
        }
        else if (scriptletToken.startsWith("@")) {
            return "{" + scriptletToken + "@}";
        }
    
        if (!allowSecondLevelScriptlets) {
            return "";
        }
    
        // Second level scriptlets
        ExpressionResult result;
        if (scriptletToken.startsWith("=")) {
            result = _engine.evaluateExpression(scriptletToken.substring(1), context, ExpressionEngine.TYPE_EXPRESSION, null);
        }
        else {
            result = _engine.evaluateExpression(scriptletToken, context, ExpressionEngine.TYPE_SCRIPT, null);
        }
    
        if (result.isError()) {
            context.addwarning("Error executing scriptlet: " + result.getException().getMessage(), false);
            return "";
        }
        else {
            return String.valueOf(result.getResult());
        }
        }
        catch (Exception e) {
            context.addwarning("Error executing scriptlet: " + scriptletToken + " - " + e.getClass().getName() + " - " + e.getMessage(), false);
            context.getlog().error("Exception executing scriptlet: " + scriptletToken, e);
            return "";
        }
    
    }

    private String executeCommandMacro(TMLContext context, String command, Map<String,Object> engineParams) throws UnsupportedEncodingException, WGException {
        
        int colonPos = command.indexOf(":");
        String param = null;
        if (colonPos != -1) {
            param = command.substring(colonPos + 1);
            command = command.substring(0, colonPos);
        }
        
        
        if (command.equalsIgnoreCase("url")) {
            return context.contenturl(null ,null);
        }
        else if (command.equalsIgnoreCase("contenturl")) {
            return macroContentURL(context, param);
        }
        else if (command.equalsIgnoreCase("namedcontenturl")) {
            return macroNamedContentURL(context, param);
        }
        else if (command.equalsIgnoreCase("filelink") || command.equalsIgnoreCase("fileurl") || command.equalsIgnoreCase("imgurl")) {
            return macroFileURL(context, command, param, engineParams);            
        }
        else if (command.equalsIgnoreCase("srcset")) {
            return macroSrcset(context, param);
        }
        else if (command.equalsIgnoreCase("link")) {
            return macroLink(context, param);
        }
        else if (command.equalsIgnoreCase("div")) {
            return macroDiv(param);
        }
        else if (command.equalsIgnoreCase("/div") || command.equalsIgnoreCase("_div")) {
            return macroEndDiv();
        }
        else if (command.equalsIgnoreCase("span")) {
            return macroSpan(param);
        }
        else if (command.equalsIgnoreCase("/span") || command.equalsIgnoreCase("_span")) {
            return macroEndSpan();
        }
        else if (command.equalsIgnoreCase("label")) {
            return macroLabel(context, param);
        }
        else if (command.equalsIgnoreCase("rtfsystem")) {
            return macroRTFSystem(context, param, engineParams);
        }
        else {
            List<String> params = new ArrayList<String>();
            if (param != null) {
                params.addAll(WGUtils.deserializeCollection(param, ","));
            }
            return executeCustomScriptlet(context, command, params, engineParams);
        }
        
    }

    private String macroContentURL(TMLContext context, String param) throws UnsupportedEncodingException, WGException {
        
        List<String> parms = WGUtils.deserializeCollection(param, ",", true);
        String dbKey = null;
        String contentKey = null;
    	String defaultMediaKey;

        if (parms.size() == 2) {
            dbKey = (String) parms.get(0);
            contentKey = (String) parms.get(1);
        }
        else {
            contentKey = (String) parms.get(0);
        }
        
        // First try to fetch target context. When found we return the content url of it
        // If current doc is a remote doc we try a "roundtrip" to see which doc in the current
        // db represents the link target in the source database (B00004D92)
        if (context.db().getBooleanAttribute(WGACore.DBATTRIB_USEREMOTECS, false) && context.content().hasItem("remote_info")) {
            TMLContext remoteTargetContext = traceRemoteDocument(context, contentKey);
            if (remoteTargetContext != null) {
            	defaultMediaKey = (String) context.getwgacore().readPublisherOptionOrDefault(remoteTargetContext.db(), WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
            	return remoteTargetContext.contenturl(defaultMediaKey, null);
            }
        }
        else {
            String contextExpr = (dbKey != null ? "db:" + dbKey + "/" : "") + "docid:" + contentKey;
            TMLContext targetContext = context.context(contextExpr, false);
            if (targetContext != null) {
            	defaultMediaKey = (String) context.getwgacore().readPublisherOptionOrDefault(targetContext.db(), WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
                return targetContext.contenturl(defaultMediaKey, null);
            }
        }
        
        return createFallBackContentURL(context, dbKey, contentKey);
        
    
    }

    private String macroDiv(String param) {
        StringBuffer out = new StringBuffer();
        out.append("<div");
        if (param != null) {
            out.append(" class=\"" + param + "\"");
        }
        out.append(">");
        return out.toString();
    }

    private String macroEndDiv() {
        return "</div>";
    }

    private String macroEndSpan() {
        return "</span>";
    }

    private String macroFileURL(TMLContext context, String command, String param, Map<String,Object> engineParams) throws WGAPIException, WGException {
        Boolean generateDataURL = (Boolean) engineParams.get(RhinoExpressionEngine.SCRIPTLETOPTION_IMAGEURL_AS_DATAURL);
        if (generateDataURL == null) {
            generateDataURL = Boolean.FALSE;
        }
        
        List<String> parms = WGUtils.deserializeCollection(param, ",", true);
        String fileName = null;
        String containerName = null;
        String title = fileName;
        String derivates = null;
        
        if (parms.size() == 1) {
            fileName = (String) parms.get(0);
            title = fileName;
        }
        else if (parms.size() == 2){
            containerName = (String) parms.get(0);
            fileName = (String) parms.get(1);
            title = fileName;
        }
        else if (parms.size() >= 3) {
            containerName = (String) parms.get(0);
            fileName = (String) parms.get(1);
            title = (String) parms.get(2);
        }
        else {
            return "(invalid parameter count for !" + command + ": " + parms.size() + ")";
        }

        String[] parts = fileName.split("\\?");
        if(parts.length>1){
        	fileName = parts[0];        	
        	derivates = parts[1];
        }
        
        if (containerName != null) {                
            // if container name is present we should check if this is a content key
            // if the content key contains a version other than 0 we should rewrite the version to 0
            WGContentKey contentKey = null;
            try {
                contentKey = WGContentKey.parse(containerName, context.db());
            } catch (Exception e) {
                // container is no valid content key
            }   
            if (contentKey != null && contentKey.isValid() && contentKey.getVersion() != 0) {
                contentKey = new WGContentKey(contentKey.getStructKey(), contentKey.getLanguage(), 0);
                containerName = contentKey.toString();
            }                            
        }
        
        String url = null;
        if (generateDataURL.booleanValue() && command.equalsIgnoreCase("imgurl")) {
        	if (derivates == null)
        		derivates = (String) context.option(Base.OPTION_IMAGE_DERIVATES);
            return context.filedataurl(null, containerName, fileName, null, derivates);
        } else {
        	url = context.fileurl(containerName, fileName);
        }
        
        if (command.equalsIgnoreCase("imgurl")) {
        	if (derivates == null)
        		derivates = (String) context.option(Base.OPTION_IMAGE_DERIVATES);
            if (derivates != null) {
                DerivateQuery derivateQuery = context.getwgacore().getFileDerivateManager().parseDerivateQuery(derivates);
                url = addDerivateQueryToURL(context, derivateQuery, url);
            }
        }
        
        if (command.equalsIgnoreCase("fileurl") || command.equalsIgnoreCase("imgurl")) {
            return url;
        }
        else {
            StringBuffer out = new StringBuffer();
            out.append("<a href=\"").append(url).append("\"").append(">");
            out.append(title);
            out.append("</a>");
            return out.toString();
        }
    }

    private String macroLabel(TMLContext context, String param) {
        List<String> params = WGUtils.deserializeCollection(param, ",");
        String container = (params.size() >= 3 ? (String) params.get(params.size() - 3) : null);
        String file = (params.size() >= 2 ? (String) params.get(params.size() - 2) : null);
        String key = (String) params.get(params.size() - 1);
        return context.label(container, file, key);
    }

    private String macroLink(TMLContext context, String param) throws WGException, WGAPIException {
        StringBuffer out = new StringBuffer();
        out.append("<a href=\"").append(context.contenturl(null, null)).append("\"");
        if (param != null) {
            out.append(" class=\"").append(param).append("\"");
        }
        out.append(">");
        
        out.append(context.meta("title"));
        
        
        out.append("</a>");
        return out.toString();
    }

    private String macroNamedContentURL(TMLContext context, String param) throws UnsupportedEncodingException, WGException {
        
        List<String> parms = WGUtils.deserializeCollection(param, ",", true);
        String dbKey = null;
        String uniqueName = null;
        if (parms.size() == 2) {
            dbKey = (String) parms.get(0);
            uniqueName = (String) parms.get(1);
        }
        else {
        	uniqueName = (String) parms.get(0);
        }
        
    
        String contextExpr = (dbKey != null ? "db:" + dbKey + "/" : "") + "name:" + uniqueName;
        TMLContext targetContext = context.context(contextExpr, false);
        if (targetContext != null) {
            return targetContext.contenturl(null, null);
        }
        
        return createFallBackContentURL(context, dbKey, uniqueName);
                            
    }

    private String macroRTFSystem(TMLContext context, String param, Map<String,Object> engineParams) {
        
        Integer level = (Integer) engineParams.get(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL);
        if (level.equals(RhinoExpressionEngine.LEVEL_SYSTEM_MACROS)) {
            return param;
        }
        else {
            return "";
        }
        
    }

    private String macroSpan(String param) {
        StringBuffer out = new StringBuffer();
        out.append("<span");
        if (param != null) {
            out.append(" class=\"" + param + "\"");
        }
        out.append(">");
        return out.toString();
    }

    private String macroSrcset(TMLContext context, String param) throws WGException {
    
        WGA wga = WGA.get(context);
        boolean useNonfinalFeatures = (Boolean) wga.database(context.db()).getPublisherOption(WGACore.DBATTRIB_USE_NONFINAL_HT_FEATURES);
        if (!useNonfinalFeatures) {
            return "";
        }
        
        List<String> params = WGUtils.deserializeCollection(param, ",", true);
        String doc = params.size() >= 2 ? params.get(0) : null;
        String fileName = params.get(params.size() -1);
        String derivates = null;
        
        String[] parts = fileName.split("\\?");
        if(parts.length>1){
        	fileName = parts[0];        	
        	derivates = parts[1];
        }
        
        TMLContext targetContext = context;
        if (doc != null) {
            targetContext = targetContext.context("docid:" + doc, false);
            if (targetContext == null) {
                return "";
            }
        }
        
        String imgURL = targetContext.fileurl(fileName);
        if(derivates==null)
        	derivates = (String) context.option(Base.OPTION_IMAGE_DERIVATES);
        DerivateQuery derivateQuery = null;
        if (derivates != null) {
            derivateQuery = context.getwgacore().getFileDerivateManager().parseDerivateQuery(derivates);
            imgURL = addDerivateQueryToURL(context, derivateQuery, imgURL);
        }
        WGFileMetaData md = targetContext.content().getFileMetaData(fileName);
        if (md == null || md.getDisplayHeight() == -1 || md.getDisplayWidth() == -1) {
            return "";
        }
        
        SrcSetCreator scrSetCreator = wga.service(SrcSetCreator.class);
        String usage = WGFileAnnotations.USAGE_POSTER;
        if (derivateQuery != null && derivateQuery.containsKey(DerivateQuery.QUERYTERM_USAGE)) {
            usage = derivateQuery.get(DerivateQuery.QUERYTERM_USAGE).getValue();
        }
        
        String srcSet = scrSetCreator.createSrcSet(imgURL, scrSetCreator.getMaxAvailableSize(targetContext.content(), md, usage));
        if (!WGUtils.isEmpty(srcSet)) {
            return "srcset=\"" + srcSet + "\"";
        }
        else {
            return "";
        }
        
        
    }

    private String executeCustomScriptlet(TMLContext context, String command, List<String> params, final Map<String,Object> engineParams) throws WGException {
        
        // Isolate db part
        String dbkey = null;
        int slashPos = command.indexOf("/");
        if (slashPos != -1) {
            dbkey = command.substring(0, slashPos);
            command = command.substring(slashPos + 1);
        }
        
        // Build scriptlet module name
        String actionID = "scriptlets:" + command;
    
        // Resolve system script module
        WGA wga = WGA.get(context);
        Design baseDesign;
        if (dbkey != null) {
            baseDesign = wga.design(dbkey);
        }
        else {
            baseDesign = wga.design();
        }
        
        Design scriptletDesign = baseDesign.resolveSystemScriptModule(actionID, WGScriptModule.CODETYPE_TMLSCRIPT, true);
        if(scriptletDesign == null) {
        	baseDesign = wga.design(context.getmaincontext().getDesignDBKey());
        	scriptletDesign = baseDesign.resolveSystemScriptModule(actionID, WGScriptModule.CODETYPE_TMLSCRIPT, true);
        }
        if(scriptletDesign == null)
        	return "";
            
        GlobalExpressionScope globalScope = new GlobalExpressionScope() {
            @SuppressWarnings("unchecked")
            public Map<String, Object> getObjects() {
                return (Map<String,Object>) engineParams.get(RhinoExpressionEngine.SCRIPTLETOPTION_OBJECTS);
            }
        };
        return String.valueOf(wga.callAction(context, scriptletDesign.toString(), new ArrayList<Object>(params), null, globalScope));    
    }

    private TMLContext traceRemoteDocument(TMLContext context, String linkTargetContentKey) throws WGAPIException {
    
        try {
            String remoteInfo = context.content().getItemText("remote_info");
            RemoteDocReference ref = new RemoteDocReference(remoteInfo);
            WGDatabase targetDB = context.content().getDatabase();
            WGDatabase sourceDB = (WGDatabase) context.getwgacore().getContentdbs().get(ref.getDbKey());
            
            // Determine the language behaviour to use while resolving the link in source DB. If both dbs are multi-language we can
            // use the target language behaviour to allow behaviour integrity. Otherwise we must choose source behaviour.
            WGDatabase langBehaviourDB;
            if (LanguageBehaviourTools.isMultiLanguageDB(sourceDB) && LanguageBehaviourTools.isMultiLanguageDB(targetDB)) {
                langBehaviourDB = targetDB;
            }
            else {
                langBehaviourDB = sourceDB;
            }
            
            RemoteDocumentTracer tracer = new RemoteDocumentTracer(sourceDB, linkTargetContentKey, context.db().getDbReference(), new WebTMLLanguageChooser(langBehaviourDB, context));
            tracer.runWithExceptions();
            if (tracer.isDocumentFound()) {
                return context.context(tracer.getTargetContextPath(), false);
            }
            else {
                return null;
            }
        }
        catch (Throwable e) {
            context.getwgacore().getLog().error("Exception tracing remote document reference", e);
            return null;
        }
        
    }

    /**
     * Generates a content URL based on the parameter input without context change 
     * usefull when target context not retrievable and the document is invisible for the current user 
     * @param context
     * @param dbKey
     * @param anyURLContentKey
     * @return
     * @throws WGAPIException
     */
    private String createFallBackContentURL(TMLContext context, String dbKey, String anyURLContentKey) throws WGAPIException {
    
        if (dbKey == null) {
            dbKey = context.db().getDbReference();
        }
        
        String defaultMediaKey = "html";
        WGDatabase db = (WGDatabase) context.getwgacore().getContentdbs().get(dbKey);
        if (db != null) {
            defaultMediaKey = (String) db.getAttribute(WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
        }
        
        return context.meta("request", "wgaurl") + "/" + dbKey + "/" + defaultMediaKey + "/default/" + anyURLContentKey;
    }

    private String addDerivateQueryToURL(TMLContext context, DerivateQuery derivateQuery, String url) throws WGException {
        if (!derivateQuery.isNoDerivate()) {
            URLBuilder builder = WGA.get(context).urlBuilder(url);
            builder.setParameter(WGPDispatcher.URLPARAM_DERIVATE, derivateQuery.toString());
            url = builder.buildLikeGiven();
        }
        return url;
    }

}
