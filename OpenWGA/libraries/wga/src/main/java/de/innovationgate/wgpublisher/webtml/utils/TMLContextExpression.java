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

package de.innovationgate.wgpublisher.webtml.utils;

import java.util.StringTokenizer;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentNavigator;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.lang.SingleLanguageChooser;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.vlink.VirtualLinkTarget;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.Query;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;

public class TMLContextExpression {
    
    public static final int EXPRESSIONTYPE_SIMPLE = 1;
    public static final int EXPRESSIONTYPE_FUNCTION = 2;
    public static final int EXPRESSIONTYPE_PATH = 3;
    
    private int _type;
    private String _function;
    private String _expression;
    private String _role;
    private boolean _explicitLanguageChoice = false;
    private String _languageSuffix = null;
    private TMLContext _originContext;
    
    public TMLContextExpression(String expression, TMLContext originContext, String defaultLanguage) throws WGAPIException {
        
        _originContext = originContext;
        
        // Determine expression type and elements
        if (expression.indexOf("/") != -1 && !expression.startsWith("$")) {
            _type = EXPRESSIONTYPE_PATH;
            _function = null;
            _expression = expression;
        }
        else if (expression.indexOf(":") != -1) {
            _type = EXPRESSIONTYPE_FUNCTION;
            _function = expression.substring(0, expression.indexOf(":")).trim().toLowerCase();
            _expression = expression.substring(expression.indexOf(":") + 1).trim();
        }
        else {
            _type = EXPRESSIONTYPE_SIMPLE;
            _function = null;
            _expression = expression.toLowerCase().trim();
        }
        
        // Determine relevant languages, based on optional language suffix
        if (_expression.endsWith(">") && _type != EXPRESSIONTYPE_PATH) {
            _languageSuffix = _expression.substring(_expression.indexOf("<") + 1, _expression.indexOf(">")).toLowerCase();
            if (_languageSuffix.equals("")) {
                _languageSuffix = defaultLanguage; 
            }
            _explicitLanguageChoice = true;
            _expression = _expression.substring(0, _expression.lastIndexOf("<")).trim();
        }
        
        
        
        
    }
    
    public WGLanguageChooser getLanguageChooser(WGDatabase db) {
        if (_languageSuffix != null) {
            return new SingleLanguageChooser(_languageSuffix);
        }
        else {
            return new WebTMLLanguageChooser(db, _originContext);
        }
    }
    
    public WGLanguageChooser getOriginContextLanguageChooser() {
        return getLanguageChooser(_originContext.db());
    }

    /**
     * @return Returns the expression.
     */
    public String getExpression() {
        return _expression;
    }

    /**
     * @return Returns the function.
     */
    public String getFunction() {
        return _function;
    }

    public int getType() {
        return _type;
    }

    public String getRole() {
        return _role;
    }

    public void setRole(String role) {
        _role = role;
    }

    public String toString() {
        if (getType() == EXPRESSIONTYPE_FUNCTION) {
            return getFunction() + ":" + getExpression();
        }
        else {
            return getExpression();
        }
    }

    public TMLContext processExpression(TMLContext context, boolean returnContextOnError) {
    
        TMLContext errorReturnContext;
        if (returnContextOnError) {
            errorReturnContext = context;
        }
        else {
            errorReturnContext = null;
        }
        
        context.setLastError(null);
        
        try {
        
            // Retrieve and build neccessary objects and flags
            WGDocument mainDocument = context.getmaincontext().getdocument();
            WGContent mainContent = null;
            if (context.getmaincontext().getdocument() instanceof WGContent) {
                mainContent = context.getmaincontext().content();
            }
            else {
                mainContent = mainDocument.getDatabase().getDummyContent(null);
            }
            
            // Navigation role
            String navRole = context.getrole();
            if (getRole() != null) {
                navRole = getRole(); 
            }
            
            // Build content navigator
            WGContentNavigator navigator = new WGContentNavigator(navRole, getOriginContextLanguageChooser());
            navigator.setOnlyPublished(!context.isbrowserinterface());
    
            // Process by expression type
            if (getType() == EXPRESSIONTYPE_PATH) {
                return processExpressionPath(context, errorReturnContext);
            }
            else if (getType() == EXPRESSIONTYPE_FUNCTION) {
                return processExpressionFunction(context, errorReturnContext, mainContent, navigator);
            }
            else {
                return processSimpleExpression(context, errorReturnContext, navigator);
            }
    
        }
        catch (WGCreationException e) {
            context.setLastError("Creation exception processing context expression (" + toString() + ") : " + e.getMessage());
            return errorReturnContext;
        }
        catch (WGException e) {
            context.setLastError("Unable to change context to (" + toString() + ") bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            return errorReturnContext;
        }
    
    }

    TMLContext processExpressionPath(TMLContext context, TMLContext errorReturnContext) throws WGAPIException {
        
        // Cutoff obsolete path qualifier
        String path = getExpression();
        if (path.startsWith("path:")) {
            path = path.substring(5);
        }
    
        // Parse tokens
        StringTokenizer tokens = new StringTokenizer(path, "/", false);
        
        while (tokens.hasMoreTokens()) {
            
            String token = tokens.nextToken();
            
            // Special function: When the token starts with a "$"
            // it is the last token of the expression and we add eventually
            // remaining tokens to it (F00004CAE)
            if (token.startsWith("$") && tokens.hasMoreTokens()) {
                token += "/" + WGUtils.joinRemainingTokens(tokens, "/");
            }
            token.trim();
            TMLContextExpression expr = new TMLContextExpression(token, context, context.db().getDefaultLanguage());
            expr.setRole(getRole());
            TMLContext startContext = context;
            context = expr.processExpression(startContext, false);
            
            if (context == null) {
                if (errorReturnContext != null) {
                    errorReturnContext.setLastError(startContext.getlasterror());
                }
                return errorReturnContext;
            }
            
            // Store the role from this context expression part (function "role:" sets this) at the parent 
            // so it can be used for further steps
            setRole(expr.getRole());
        }
        return context;
    }

    TMLContext processSimpleExpression(TMLContext context, TMLContext errorReturnContext, WGContentNavigator navigator) throws WGAPIException {
        
        String contextExpression = getExpression();
        WGContent content = context.content();
        if (content == null) {
            context.setLastError(
                    "Cannot process expression '" + getExpression() + "' on a non-content context: " + context.getdocument().getDocumentKey());
            return errorReturnContext;
        }
        
        WGStructEntry struct = content.getStructEntry();
        WGDatabase db = context.getdocument().getDatabase();
    
        if (contextExpression.equals("parent")) {
    
            if (content.isDummy()) {
                context.setLastError("Cannot retrieve parent of dummy content");
                return errorReturnContext;
            }
    
            if (struct.isRoot()) {
                context.setLastError("Cannot get parent of root document");
                return errorReturnContext;
            }
            else {
                WGContent parentContent = navigator.getParentContent(content);
                if (parentContent != null) {
                    return context.getTMLContextForDocument(parentContent);
                }
                else {
                    context.setLastError(
                        "Could not retrieve parent content: "
                            + content.getTitle()
                            + " ("
                            + content.getContentKey().toString()
                            + ")");
                    return errorReturnContext;
                }
            }
        }
    
        else if (contextExpression.equals("root")) {
            WGContent rootContent = null;
            if (!content.isDummy()) {
                rootContent = navigator.getRootContent(content);
            }
            else {
                rootContent = content.getDatabase().getFirstReleasedContent(getOriginContextLanguageChooser(), true);
            }
    
            if (rootContent != null) {
                return context.getTMLContextForDocument(rootContent);
            }
            else {
                context.setLastError("Could not retrieve root content");
                return errorReturnContext;
            }
        }
    
        else if (contextExpression.equals("main") || contextExpression.equals("currentdocument")) {
            TMLContext mainContext = context.getmaincontext();
            return new TMLContext(mainContext.getdocument(), context);
        }
    
        else if (contextExpression.startsWith("children")) {
    
            if (content.isDummy()) {
                context.setLastError("Cannot retrieve children of dummy content");
                return errorReturnContext;
            }
    
            String idxString = contextExpression.substring(contextExpression.indexOf("[") + 1, contextExpression.indexOf("]"));
            int idx;
            try {
                idx = Integer.parseInt(idxString);
            }
            catch (NumberFormatException exc) {
                context.setLastError("Can't parse children index: " + contextExpression);
                return errorReturnContext;
            }
            WGContent childContent = navigator.getChildContent(content, idx);
            if (childContent != null) {
                return context.getTMLContextForDocument(childContent);
            }
            else {
                context.setLastError("Cant retrieve child content idx " + idx);
                return errorReturnContext;
            }
        }
    
        else if (contextExpression.equals("selectedchild")) {
    
            if (content.isDummy()) {
                context.setLastError("Cannot retrieve children of dummy content");
                return errorReturnContext;
            }
    
            WGContent mainContentParent = context.getmaincontext().content();
            while (mainContentParent != null && !mainContentParent.isDummy() && !mainContentParent.getStructEntry().isRoot()) {
                if (mainContentParent.getStructEntry().getParentEntry().equals(struct)) {
                    return context.getTMLContextForDocument(mainContentParent);
                }
                else {
                    mainContentParent = navigator.getParentContent(mainContentParent);
                }
            }
            context.setLastError("Could not retrieve selected child");
            return errorReturnContext;
    
        }
    
        else if (contextExpression.startsWith("siblings")) {
    
            if (content.isDummy()) {
                context.setLastError("Cannot retrieve siblings of dummy content");
                return errorReturnContext;
            }
    
            String idxString =
                contextExpression.substring(contextExpression.indexOf("[") + 1, contextExpression.indexOf("]")).trim();
            boolean relative = (idxString.charAt(0) == '+' || idxString.charAt(0) == '-' ? true : false);
            if (idxString.charAt(0) == '+') {
                idxString = idxString.substring(1);
            }
            int idx = 0;
            try {
                idx = Integer.parseInt(idxString);
            }
            catch (NumberFormatException exc) {
                context.setLastError("Can't parse siblings index: " + contextExpression);
                return errorReturnContext;
            }
    
            WGContent sibling = navigator.getSiblingContent(content, idx, relative);
            if (sibling != null) {
                return context.getTMLContextForDocument(sibling);
            }
            else {
                context.setLastError("Could not retrieve sibling " + idxString);
                return errorReturnContext;
            }
        }
    
        else if (contextExpression.equals("this")) {
            
            if (!_explicitLanguageChoice) {
                return context;
            }
    
            if (content.isDummy()) {
                WGLanguage lang = getOriginContextLanguageChooser().selectDatabaseLanguage(db);
                if (lang != null) {
                    return context.getTMLContextForDocument(db.getDummyContent(lang.getName()));
                }
                else {
                    return errorReturnContext;
                }
            }
    
            TMLContext thisContext = context.getTMLContextForDocument(navigator.getRelevantContent(content.getStructEntry()));
            if (thisContext != null) {
                return thisContext;
            }
            else {
                context.setLastError(
                    "Unable to find content for struct entry " + content.getStructEntry().getStructKey());
                ;
                return errorReturnContext;
            }
        }
        else if (contextExpression.equals("portlet")) {
        	TMLPortlet portlet = context.getportlet();
        	if (portlet != null) {
        		return portlet.getcontext();
        	}
        	else {
        		return errorReturnContext;
        	}
        }
        else if (contextExpression.equals("module")) {
            
            if (context.getDesignContext().getTag() == null) {
                context.setLastError("Cannot retrieve module context because this script does not run on a WebTML page");
                return errorReturnContext;
            }

            BaseTagStatus refTag = context.getDesignContext().getTag().getRootTag();
            if (refTag != null) {
                TMLContext tagContext = refTag.tmlContext;
                if (tagContext != null) {
                    return tagContext;
                }
                else {
                    context.setLastError("Module context could not be retrieved");
                    return errorReturnContext;
                }
            }
            else {
                context.setLastError("Module context could not be retrieved");
                return errorReturnContext;
            }
            
        }
        else if (contextExpression.equals("createddoc")) {
            TMLForm form = context.gettmlform();
            if (form != null && form.getcreateddoc() != null) {
                return context.context((WGContent) form.getcreateddoc());
            }
            else {
                context.setLastError("Cannot change context to created document because there is none");
                return errorReturnContext;
            }
        }
        else if (contextExpression.equals("vlinktarget")) {
            if (!content.isVirtual()) {
                context.setLastError("The current document is no virtual link, cannot change context to 'vlinktarget'");
                return errorReturnContext;
            }
            VirtualLinkTarget vlinkTarget = context.getwgacore().resolveVirtualLink(WGA.get(context), content);
            if (vlinkTarget.getType() == VirtualLinkTarget.Type.CONTENT) {
                TMLContext vlinkContext = context.context("docid:" + vlinkTarget.getContainerKey(), false);
                if (vlinkContext != null) {
                    return vlinkContext;
                }
                else {
                    context.setLastError("Unresolveable virtual link key: " + vlinkTarget.getContainerKey());
                    return errorReturnContext;
                }
            }
            else {
                context.setLastError("Virtual link targets no content, cannot change context to 'vlinktarget'");
                return errorReturnContext;                
            }
            
        }
        else {
            context.setLastError("Could not interpret context expression:" + contextExpression);
            return errorReturnContext;
        }
    }
    
    TMLContext processExpressionFunction(TMLContext context, TMLContext errorReturnContext, WGContent mainContent, WGContentNavigator navigator) throws WGException {
        
        String contextFunction = getFunction();
        String contextExpression = getExpression();
        WGDatabase db = context.getdocument().getDatabase();
        
        boolean isBI = context.isbrowserinterface();
    
        // Retrieve context by content key or unid
        if (contextFunction.equals("docid") || contextFunction.equals("content")) {
            WGContent content = null;
            WGContentKey tmpKey;
            try {
                tmpKey = WGContentKey.parse(contextExpression, context.getdocument().getDatabase());
            }
            catch (WGAPIException e) {
                context.setLastError("Error parsing contextExpression. Exception: " + e.getClass().getName() + " message: " + e.getMessage());
                return errorReturnContext;
            }
            if (tmpKey != null) {
                content = context.content().getDatabase().getContentByKey(tmpKey);
                if (content != null) {
                    return context.getTMLContextForDocument(content);
                }
            }
    
            content =
                    WGPDispatcher.getContentByAnyKey(
                        contextExpression,
                        context.content().getDatabase(),
                        getOriginContextLanguageChooser(),
                        context.isbrowserinterface());
    
    
            if (content != null) {
                return context.getTMLContextForDocument(navigator.chooseRelevantContent(content, mainContent));
            }
            else {
                context.setLastError("docid could not be resolved: " + contextExpression);
                return errorReturnContext;
            }
    
        }
    
        else if (contextFunction.equals("name")) {
            
    
            WGContent content = getOriginContextLanguageChooser().selectContentForName(context.content().getDatabase(), contextExpression, context.isbrowserinterface());
            if (content != null) {
                return context.getTMLContextForDocument(navigator.chooseRelevantContent(content, mainContent));
            }
        
            context.setLastError("Could not retrieve content for name: " + contextExpression);
            return errorReturnContext;
            
        }
    
        // Retrieve the context of another tag
        else if (contextFunction.equals("tag")) {
    
            if (context.getDesignContext().getTag() == null) {
                context.setLastError("Cannot retrieve tag because this script does not run on a WebTML page");
                return errorReturnContext;
            }
    
            BaseTagStatus refTag = context.getDesignContext().getTag().getTagStatusById(contextExpression);
            if (refTag != null) {
                TMLContext tagContext = refTag.tmlContext;
                if (tagContext != null) {
                    return tagContext;
                }
                else {
                    context.setLastError("Context of this Tag could not be retrieved: " + contextExpression);
                    return errorReturnContext;
                }
            }
            else {
                context.setLastError("Tag could not be retrieved: " + contextExpression);
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("db") || contextFunction.equals("plugin")) {
            
            if (contextFunction.equals("plugin")) {
                WGAPlugin plugin = context.getwgacore().getPluginSet().getPluginByUniqueName(contextExpression);
                if (plugin != null) {
                    contextExpression = plugin.buildDatabaseKey();
                }
                else {
                    context.setLastError("Unknown plugin unique name: " + contextExpression);
                    return errorReturnContext;
                }
            }
            
            WGDatabase dbTarget = null;
            try {
                dbTarget = context.db(context.resolveDBKey(contextExpression));
            }
            catch (WGUnavailableException e1) {
                context.setLastError("Database '" + contextExpression + "' is currently unavailable");
                return errorReturnContext;
            }
            catch (WGException e) {
                context.setLastError("Unable to open database '" + contextExpression + "'. Exception: " + e.getClass().getName() + " message: " + e.getMessage());
                return errorReturnContext;
            }
    
            if (dbTarget == null) {
                context.setLastError("No database with key " + contextExpression);
                return errorReturnContext;
            }
    
            if (dbTarget.isSessionOpen() == false) {
                context.setLastError("User cannot open database '" + contextExpression + "'");
                return errorReturnContext;
            }
    
            if (dbTarget.getSessionContext().getAccessLevel() <= WGDatabase.ACCESSLEVEL_NOACCESS) {
                context.setLastError("User has no access to database '" + contextExpression + "'");
                return errorReturnContext;
            }
    
            TMLContext dbContext = (TMLContext) WGA.get(context).createTMLContext(dbTarget, getLanguageChooser(dbTarget));
            if (dbContext == null) {
                context.setLastError("Target database " + contextExpression + " does not support db context changes");
                return errorReturnContext;
            }
            else {
                return dbContext;
            }
        }
    
        else if (contextFunction.equals("area")) {
    
            WGArea area = db.getArea(contextExpression);
            if (area == null) {
                context.setLastError("No area of name '" + contextExpression + "'");
                return errorReturnContext;
            }
    
            WGContent content = navigator.getRootContent(area);
            if (content != null) {
                return context.getTMLContextForDocument(content);
            }
            else {
                context.setLastError("No root content in area '" + contextExpression + "'");
                return errorReturnContext;
            }
    
        }
    
        else if (contextFunction.equals("query")) {
    
            if (context.getDesignContext().getTag() == null) {
                context.setLastError("Cannot retrieve tag because this script does not run on a WebTML page");
                return errorReturnContext;
            }
    
            BaseTagStatus tag = context.getDesignContext().getTag().getTagStatusById(contextExpression);
            if (tag != null && tag instanceof Query.Status) {
                Query.Status queryTag = (Query.Status) tag;
                WGContent content = queryTag.getFirstContent();
                if (content != null) {
                    return context.getTMLContextForDocument(queryTag.getFirstContent());
                }
                else {
                    context.setLastError(
                        "Could not retrieve context by query tag \"" + contextExpression + "\". Query had no result.");
                    return errorReturnContext;
                }
            }
            else {
                context.setLastError("No query tag with id: " + contextExpression);
                return errorReturnContext;
            }
        }
        
        else if (contextFunction.equals("role")) {
            setRole(contextExpression);
            return context;
        }
        
        else if (contextFunction.equals("relation")) {
            WGContent relationContent = context.content().getRelation(contextExpression);
            if (relationContent != null) {
                return context.getTMLContextForDocument(relationContent);
            }
            else {
                context.setLastError("Content " + context.meta("KEY") + " has no relation named '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
        else if (contextFunction.equals("level")) {
            
            int level;
            try {
                level = Integer.parseInt(contextExpression);
            }
            catch (NumberFormatException e) {
                context.setLastError("Cannot be parsed as level number: " + contextExpression);
                return errorReturnContext;
            }
            
            WGContent con = context.content();
            if (!con.hasCompleteRelationships()) {
                context.setLastError("Current content does not belong to a page hierarchy");
                return errorReturnContext;
            }
            
            WGStructEntry struct = con.getStructEntry();
            if (struct.getLevel() < level) {
                context.setLastError("Cannot go up to level " + level + " because the current page is already at level " + struct.getLevel());
                return errorReturnContext;
            }

            while (struct != null && struct.getLevel() > level) {
                struct = struct.getParentEntry();
            }
            
            if (struct != null) {
                WGContent targetCon = getOriginContextLanguageChooser().selectContentForPage(struct, isBI);
                if (targetCon != null) {
                    return context.getTMLContextForDocument(targetCon);
                }
                else {
                    context.setLastError("Page on level " + level + " has no appropriate content document");
                    return errorReturnContext;
                }
            }
            else {
                context.setLastError("Failed to go to page level " + level);
                return errorReturnContext;
            }
            
        }
        else if (contextFunction.equals("np")) {
            String namePart = UniqueNamePartFormatter.INSTANCE.format(contextExpression);
            String uname = context.content().getUniqueName();
            if (WGUtils.isEmpty(uname)) {
                uname = context.content().getStructEntry().getUniqueName();
            }
            
            String fullname;
            if (!WGUtils.isEmpty(uname)) {
                fullname = uname + "." + namePart;
            }
            else {
                fullname = namePart;
            }
            
            WGContent content = getOriginContextLanguageChooser().selectContentForName(context.content().getDatabase(), fullname, context.isbrowserinterface());
            if (content != null) {
                return context.getTMLContextForDocument(navigator.chooseRelevantContent(content, mainContent));
            }
        
            context.setLastError("Could not retrieve content for hdb unique name " + fullname);
            return errorReturnContext;
            
        }
        else if (contextFunction.equals("contentclass")) {
            
            WGContent con = context.content();
            if (!con.hasCompleteRelationships()) {
                context.setLastError("Current content does not belong to a page hierarchy");
                return errorReturnContext;
            }
            
            WGContent posContent = con;
            WGContent selectedContent = null;
            do {
                
                if (WGUtils.nullSafeEquals(posContent.getContentClass(), contextExpression)) {
                    selectedContent = posContent;
                    break;
                }
                
                posContent = posContent.getParentContent();
                
            } while (posContent != null);
            
    
            if (selectedContent != null) {
                 return context.context(selectedContent);
            }
            else {
                context.setLastError("No content of content class '" + contextExpression + "' in the document path");
                return errorReturnContext;
            }
            
        }
        else if (contextFunction.equals("contenttype")) {
            
            WGContent con = context.content();
            if (!con.hasCompleteRelationships()) {
                context.setLastError("Current content does not belong to a page hierarchy");
                return errorReturnContext;
            }
            
            WGContent posContent = con;
            WGContent selectedContent = null;
            do {
                
                if (WGUtils.nullSafeEquals(posContent.getStructEntry().getContentType().getName(), contextExpression)) {
                    selectedContent = posContent;
                    break;
                }
                
                posContent = posContent.getParentContent();
                
            } while (posContent != null);
            
    
            if (selectedContent != null) {
                 return context.context(selectedContent);
            }
            else {
                context.setLastError("No content of content type '" + contextExpression + "' in the document path");
                return errorReturnContext;
            }
            
        }
        else if (contextFunction.equals("storageid")) {
            
            HDBModel model = HDBModel.getModel(db);
            if (model == null) {
                context.setLastError("Context expression 'storageid' only valid for HDBModel applications");
                return errorReturnContext;
            }
            
            String storageId = UniqueNamePartFormatter.INSTANCE.format(contextExpression);
            if (model.isContent(context.content())) {
                TMLContext targetContext = context.context("np:" + storageId, false);
                if (targetContext != null) {
                    return targetContext;
                }
            }
            
            for (TMLContext parentContext = context; parentContext != null; parentContext = parentContext.context("parent", false)) {
                if (model.isStorage(parentContext.content()) && storageId.equals(parentContext.item(HDBModel.ITEM_STORAGE_ID))) {
                    return parentContext;
                }
            }
            
            context.setLastError("Could not find storage of id '" + contextExpression + "' from content " + context.content().getContentKey());
            return errorReturnContext;
    
            
        }
        
        else if (contextFunction.equals("contentid")) {
            
            HDBModel model = HDBModel.getModel(db);
            if (model == null) {
                context.setLastError("Context expression 'contentid' only valid for HDBModel applications");
                return errorReturnContext;
            }
            
            if (!model.isStorage(context.content())) {
                context.setLastError("Context expression 'contentid' does not work from non-storage document + '" + context.content().getContentKey());
                return errorReturnContext;
            }
            
            return context.context("np:" + contextExpression);
            
        }
    
        // From here special functions for BI
    
        else if (contextFunction.equals("$struct")) {
            WGStructEntry entry = db.getStructEntryByKey(contextExpression);
            if (entry != null) {
                return context.getTMLContextForDocument(entry);
            }
            else {
                context.setLastError("Could not retrieve struct entry with key '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("$area")) {
            WGArea area = db.getArea(contextExpression);
            if (area != null) {
                return context.getTMLContextForDocument(area);
            }
            else {
                context.setLastError("Could not retrieve area with name '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("$contenttype")) {
            WGContentType contentType = db.getContentType(contextExpression);
            if (contentType != null) {
                return context.getTMLContextForDocument(contentType);
            }
            else {
                context.setLastError("Could not retrieve contenttype with name '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("$language")) {
            WGLanguage language = db.getLanguage(contextExpression);
            if (language != null) {
                return context.getTMLContextForDocument(language);
            }
            else {
                context.setLastError("Could not retrieve language with name '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("$tml")) {
            int commaPos = contextExpression.indexOf(",");
            String name = contextExpression.substring(0, commaPos);
            String mediaKey = contextExpression.substring(commaPos + 1);
            WGTMLModule tml = db.getTMLModule(name, mediaKey);
            if (tml != null) {
                return context.getTMLContextForDocument(tml);
            }
            else {
                context.setLastError("Could not retrieve tml with name '" + name + "' and media key '" + mediaKey + "'");
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("$cssjs")) {
            
            String libName = contextExpression;
            String type = null;
            int commaPos = libName.indexOf(",");
            if (commaPos != -1) {
                type = libName.substring(commaPos+1).trim();
                libName = libName.substring(commaPos).trim();
            }
            
            WGCSSJSModule lib;
            if (type != null) {
                lib = db.getCSSJSModule(libName, type);
            }
            else {
                lib = db.getCSSJSModule(libName);
            }
            
            if (lib != null) {
                return context.getTMLContextForDocument(lib);
            }
            else {
                context.setLastError("Could not retrieve css/js module with name '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
    
        else if (contextFunction.equals("$filecontainer")) {
            WGFileContainer cont = db.getFileContainer(contextExpression);
            if (cont != null) {
                return context.getTMLContextForDocument(cont);
            }
            else {
                context.setLastError("Could not retrieve file container with name '" + contextExpression + "'");
                return errorReturnContext;
            }
        }
    
        else {
            context.setLastError("Context function could not be interpreted: " + contextFunction);
            return errorReturnContext;
        }
    }
    
    
}
