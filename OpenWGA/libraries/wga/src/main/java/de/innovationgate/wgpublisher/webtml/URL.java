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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import org.apache.commons.httpclient.URIException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.WGUnresolveableVirtualLinkException;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.filter.WGAVirtualHostingFilter;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

public class URL extends ActionBase implements DynamicAttributes {

	private static final long serialVersionUID = 1L;
    private String type;
    private String db;
    private String doc;
    private String file;
    private String sourceTag;
    private String layout;
    private String medium;
    private String mode;
    private String pkey;
    private String absolute;
    private String protocol;
    private String action;
    private String param1;
    private String param2;
    private String param3;
    private String param4;
    private String param5;
    private String debounce;
    private String keepparams;
    private String derivate;
    
    public String getKeepparams() {
        return getTagAttributeValue("keepparams", keepparams, null);
    }

    public void setKeepparams(String keepparams) {
        this.keepparams = keepparams;
    }

    /**
     * return url as dataurl @see RFC2397
     */
    private String dataurl;
    
    // determines the targetpage on tml:url type="selectpage"
    private String page;

    /**
     * querystring without '?' prefix - appended to the url if type='content' and dataURL='true'
     */
    private String query;
    
    
    public static class Status extends ActionBase.Status implements TMLParameterReceiver {
        private boolean encodeURL;
        private Boolean keepParams = null;
        private Map<String,Object> urlparams = new HashMap<String, Object>();
        private Map<String,Object> varparams = new HashMap<String, Object>();
        
        @Override
        public void initAttributeDelegates(Base tag) {
            super.initAttributeDelegates(tag);
            
            URL urlTag = (URL) tag;
            
            String strKeepParams = urlTag.getKeepparams();
            if (strKeepParams != null) {
                this.keepParams = tag.stringToBoolean(strKeepParams);
            }

        }
        
        
        public void addParam(String name, Object value, String type) {
            if (Param.TYPE_VAR.equals(type)) {
                varparams.put(name, value);
            }
            else {
                urlparams.put(name, value);
            }
        }
    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
    

    
    @Override
    public Status getStatus() {
        return (Status) super.getStatus();
    }

    /**
     * Gets the type
     * 
     * @return Returns a String
     */
    public String getType() {
        String theType = this.getTagAttributeValue("type", type, null);
        if (theType != null) {
            theType = theType.toLowerCase().trim();
        }
        return theType;
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
     * @see Base#tmlStartTag(TMLContext)
     */
    public void tmlStartTag() throws WGException {
        super.tmlStartTag();

        Status status = (Status) getStatus();
        status.encodeURL = true;

        
    }

    private void createHomePageURL(StringBuffer url) throws WGException {
        
        String dbKey = getTMLContext().resolveDBKey(getDb());
        if (dbKey == null) {
            dbKey = getTMLContext().db().getDbReference();
        }
        WGACore _core = WGACore.INSTANCE;
        WGAConfiguration config = _core.getWgaConfiguration();
        VirtualHost vHost = WGAVirtualHostingFilter.findMatchingHost(config, getTMLContext().getrequest());
        if(vHost!=null){
        	String defaultDBKey = WGAVirtualHostingFilter.getDefaultDBKey(_core, vHost);
	        if(vHost.isHideHomepageURL() && dbKey.equalsIgnoreCase(defaultDBKey)){
	        	url.append("/" + getTMLContext().getrequest().getContextPath());
	        	return;
	        }
        }
        url.append(getTMLContext().getURLBuilder().buildHomepageURL(getTMLContext().db(dbKey), getTMLContext().getrequest()));
        
    }



    public void createActionURL(StringBuffer url, String actionID) throws UnsupportedEncodingException, WGException, TMLException {
        
        Status status = (Status) getStatus();
        
        // Action has been stored in tml:action tag
        if (actionID != null) {

            List<Object> params = createParamList();

            TMLAction tmlAction = getTMLContext().getActionByID(actionID, getDesignDBKey());

            if (tmlAction == null) {
                addWarning("Action with id '" + actionID + "' not found.");
                return;
            }

            String actionURL = buildActionURL(tmlAction, buildNamedActionParameters(true), params);
            if (actionURL.startsWith("javascript:")) {
                status.encodeURL = false;
            }
            url.append(actionURL);

        }

        // Action is in body of this tag (Inline action)
        else {
            String actionURL = registerInlineAction(status);
            if (actionURL != null) {
                url.append(actionURL);
            }
        }

        return;
    }

    private List<Object> createParamList() {
        ArrayList<Object> list = new ArrayList<Object>();
        list.add(this.getParam1());
        list.add(this.getParam2());
        list.add(this.getParam3());
        list.add(this.getParam4());
        list.add(this.getParam5());
        return list;
    }

    private void createAjaxActionURL(StringBuffer url, String actionName) throws WGException, TMLException {
        
        Status status = (Status) getStatus();
        
        // ajaxCalls are JS calls - do not encode
        status.encodeURL = false;

        // Action has been stored in tml:action tag
        if (actionName != null) {
            TMLAction action = getTMLContext().getActionByID(actionName, getDesignDBKey());
            if (action != null) {
                url.append("javascript:" + getAjaxJSFunction(action, buildNamedActionParameters(true), createParamList(), status.keepParams));
            }
            else {
                addWarning("Action of id '" + actionName + "' is not defined");
            }
        }
        // Action is in body of this tag (Inline action)
        else {
            String actionURL = registerInlineAction(status);
            if (actionURL != null) {
                url.append(actionURL);
            }
        }
        return;
    }

    public void createPortletmodeURL(StringBuffer url) throws UnsupportedEncodingException {
        try {
            java.net.URL requestURL = new java.net.URL(getStatus().getRequestURL());
            url.append(requestURL.getPath());

            String portletMode = getMode();
            if (portletMode == null) {
            	portletMode = getPortletmode();
            }
            getStatus().urlparams.put(this.getOption("$PORTLETNS") + "mode", portletMode);

        }
        catch (MalformedURLException mue) {
            log.error("Error creating url", mue);
        }
    }

    public void createLogoutURL(WGContent content, StringBuffer url) throws UnsupportedEncodingException, WGException {
        
        String dbKey = getTMLContext().resolveDBKey(getDb());
        if (dbKey == null) {
            dbKey = content.getDatabase().getDbReference();
        }
        
        url.append(getTMLContext().getURLBuilder().buildLogoutURL(getTMLContext().db(dbKey), getTMLContext().getrequest(), getStatus().getRequestURL()));

    }

    public void createLoginURL(WGContent content, StringBuffer url) throws UnsupportedEncodingException, WGException {
        
        String dbKey = getTMLContext().resolveDBKey(getDb());
        if (dbKey == null) {
            dbKey = content.getDatabase().getDbReference();
        }
        
        url.append(getTMLContext().getURLBuilder().buildLoginURL(getTMLContext().db(dbKey), getTMLContext().getrequest(), getStatus().getRequestURL()));
    }

    public void createCssJsURL(StringBuffer url, String type) throws WGException, UnsupportedEncodingException {
        String doc = getDoc();
        if (doc == null) {
            this.addWarning("Attribute 'doc' is missing or empty");
            return;
        }
        
        url.append(getTMLContext().scripturl(getDb(), type, getDoc()));
        return;
    }

    public void createForeachPageURL(StringBuffer url, String type) throws UnsupportedEncodingException, WGException, TMLException {
        ForEach.Status tag = (ForEach.Status) this.getTagStatusById(this.getSourcetag());
        if (tag == null) {
            this.addWarning("Could not find source tag: " + this.getSourcetag(), true);
            return;
        }
        Integer page = (Integer) tag.currentPage;
        if (page == null) {
            this.addWarning("Source tag provides no page information: " + this.getSourcetag(), true);
            return;
        } 
        int targetPage;
        if (type.equals("nextpage")) {
            targetPage = (page.intValue()) + 1;
        }
        else if (type.equals("previouspage") && page.intValue() > 1) {
            targetPage = (page.intValue()) - 1;
        } else if (type.equals("selectpage")) {
            String strPage = getPage();
        	if (strPage != null) {
        		try {
        			targetPage = WGUtils.parseInt(strPage);
        			if (targetPage < 1) {
        				this.addWarning("Invalid page '" + targetPage + "' sourcetag id: '" + getSourcetag() + "'.");
        				targetPage = 1;
        			}
        		} catch (Exception e) {
        			this.addWarning("Unable to parse page attribute as integer");
        			targetPage = page.intValue();
        		}
        	} else {
        		targetPage = page.intValue();
        	}
        }
        else {
            targetPage = page.intValue();
        }

        try {
            String formID = getStatus().getRelevantForm();
            if (formID != null) {
                buildActionForeachURL(url, targetPage);
            }
            else {
                buildStraightForeachURL(url, targetPage);
            }
        }
        catch (java.net.MalformedURLException exc) {
            log.error("Error creating url", exc);
        }

        return;
    }

    private void buildStraightForeachURL(StringBuffer url, int targetPage) throws MalformedURLException, UnsupportedEncodingException {
        
        java.net.URL requestURL = new java.net.URL(getStatus().getRequestURL());
        url.append(requestURL.getPath());
        
        getStatus().keepParams = true;
        getStatus().urlparams.put(this.getSourcetag() + "Page", (new Integer(targetPage)).toString());
    }
    
    private void buildActionForeachURL(StringBuffer url, int targetPage) throws MalformedURLException, UnsupportedEncodingException, WGException, TMLException {
        Status status = (Status) getStatus();
        TMLAction tmlAction = getTMLContext().getActionByID("$" + TMLAction.DEFAULTACTION_SETVAR, getDesignDBKey());
        
        List<Object> params = new ArrayList<Object>();
        params.add("$" + this.getSourcetag() + "Page");
        params.add(String.valueOf(targetPage));
        
        String actionURL = buildActionURL(tmlAction, buildNamedActionParameters(true), params);
        if (actionURL.startsWith("javascript:")) {
            status.encodeURL = false;
        }
        
        url.append(actionURL);
        
    }

    public void createFileURL(WGContent content, StringBuffer url) throws WGException, UnsupportedEncodingException {
        
        Status status = (Status) getStatus();
        
        // Get the correct db for building file URL
        TMLContext fileContext = getTMLContext();

        if (!stringToBoolean(getDataurl())) {
            url.append(getTMLContext().getURLBuilder().buildFileURL(getTMLContext(), getDb(), getDoc(), getFile()));
        } else {
            //render data url (RFC 2397)
            url.append(fileContext.filedataurl(getDb(), getDoc(), getFile(), null));
            status.encodeURL = false;
        }
        return;
    }

    public void createLayoutURL(StringBuffer url) throws UnsupportedEncodingException, WGException {
        String layoutKey = this.getLayout();
        if (layoutKey == null) {
            this.addWarning("Attribut layout needed for tml:url type tml/layout", true);
            return;
        }
        url.append(this.getTMLContext().layouturl(getDb(), getMedium(), getLayout()));
        return;
    }

    public void createContentURL(StringBuffer url) throws WGException, ServletException, IOException {
        Status status = (Status) getStatus();
        if (!stringToBoolean(getDataurl())) {
            url.append(this.getTMLContext().contenturl(this.getMedium(), this.getLayout()));
        } else {
            url.append(this.getTMLContext().contentdataurl(this.getMedium(), this.getLayout(), this.getQuery()));
            status.encodeURL = false;
        }
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
     * Gets the doc
     * 
     * @return Returns a String
     */
    public String getDoc() {
        return this.getTagAttributeValue("doc", doc, null);
    }

    /**
     * Sets the doc
     * 
     * @param doc
     *            The doc to set
     */
    public void setDoc(String doc) {
        this.doc = doc;
    }

    /**
     * Gets the file
     * 
     * @return Returns a String
     */
    public String getFile() {
        return this.getTagAttributeValue("file", file, null);
    }

    public String getSrc() {
        return this.getFile();
    }

    /**
     * Sets the file
     * 
     * @param file
     *            The file to set
     */
    public void setFile(String file) {
        this.file = file;
    }

    public void setSrc(String file) {
        this.setFile(file);
    }

    /**
     * Gets the sourceTag
     * 
     * @return Returns a String
     */
    public String getSourcetag() {
        return this.getTagAttributeValue("sourcetag", sourceTag, "");
    }

    /**
     * Sets the sourceTag
     * 
     * @param sourceTag
     *            The sourceTag to set
     */
    public void setSourcetag(String sourceTag) {
        this.sourceTag = sourceTag;
    }

    /**
     * Gets the name
     * 
     * @return Returns a String
     */
    public String getName() {
        return this.getDoc();
    }

    /**
     * Sets the name
     * 
     * @param name
     *            The name to set
     */
    public void setName(String name) {
        this.setDoc(name);
    }

    /**
     * Gets the layoutkey
     * 
     * @return Returns a String
     */
    public String getLayout() {
        return this.getTagAttributeValue("layout", layout, getName());
    }

    /**
     * Sets the layoutkey
     * 
     * @param layoutkey
     *            The layoutkey to set
     */
    public void setLayout(String layoutkey) {
        this.layout = layoutkey;
    }

    /**
     * Gets the mediakey
     * 
     * @return Returns a String
     */
    public String getMedium() {
        return this.getTagAttributeValue("medium", medium, (String) getStatus().getOption(OPTION_LINK_MEDIUM));
    }

    /**
     * Sets the mediakey
     * 
     * @param mediakey
     *            The mediakey to set
     */
    public void setMedium(String mediakey) {
        this.medium = mediakey;
    }

    /**
     * Returns the mode.
     * @deprecated use portletmode instead
     * @return String
     */
    public String getMode() {
        return this.getTagAttributeValue("mode", mode, null);
    }

    /**
     * Sets the mode.
     * 
     * @param mode
     *           The mode to set
     * @deprecated use portletmode instead
     */
    public void setMode(String mode) {
        this.mode = mode;
    }

    /**
     * Returns the pkey.
     * 
     * @return String
     */
    public String getPkey() {
        return this.getTagAttributeValue("pkey", pkey, null);
    }

    /**
     * Sets the pkey.
     * 
     * @param pkey
     *            The pkey to set
     */
    public void setPkey(String pkey) {
        this.pkey = pkey;
    }

    /**
     * @throws  
     * @throws WGAPIException 
     * @see de.innovationgate.wgpublisher.webtml.Base#tmlEndTag()
     */
    public void tmlEndTag() throws WGException {
        
        Status status = (Status) getStatus();
        
        WGContent content = this.getTMLContext().content();
        StringBuffer url = new StringBuffer();

        String type = this.getType();
        String fileName = this.getFile();
        String actionName = this.getAction();
        String portletMode = this.getPortletmode();
        String portletContext = this.getPortletcontext();
        
        // On a change of portletmode/context we need an action tag. Default the action to $refresh
        if (portletMode != null || portletContext != null) {
            if (actionName == null) {
                actionName = "$refresh";
            }
            if (type == null) {
                type = "action";
            }
        }
        
        // Enforce type default, dependent on the other used attributes
        if (type == null) {
            if (actionName != null) {
                type = "action";
            }
            else if (fileName != null) {
                type = "file";
            }
            else {
                type ="content";
            }
        }
        
        // KeepParams defaulting for actions: If not set default to design setting for AJAX, default to false for Non-AJAX. (#00001697)
        if ("action".equals(type) && status.keepParams == null) {
            if ("norefresh".equals(status.ajax) || WGUtils.stringToBoolean(status.ajax)) {
                status.keepParams = isKeepParamsOnAJAX();
            }
            else {
                status.keepParams = false;
            }
        }
        
        // Look if we need to build an absolute URL
        boolean absoluteURL = stringToBoolean(getAbsolute());
        String absoluteProtocol = getPageContext().getRequest().getScheme();
        int absolutePort = getPageContext().getRequest().getServerPort();

        String protocol = getProtocol();
        if (protocol != null) {
            if (!absoluteProtocol.equalsIgnoreCase(protocol)) {
                absoluteURL = true;
                absoluteProtocol = protocol;
                String targetDBKey = getTMLContext().resolveDBKey(getDb());
                if (targetDBKey == null) {
                    targetDBKey = getTMLContext().db().getDbReference();
                }
                
                WGDatabase targetDB = (WGDatabase) getTMLContext().getwgacore().getContentdbs().get(targetDBKey);
                if (targetDB != null) {
                    absolutePort = getTMLContext().getwgacore().getDefaultPort(targetDB, absoluteProtocol);
                }
                else {
                    addWarning("Cannot resolve target db " + targetDBKey + " to determine default port");
                }            
            }
        }

        // Process URL types
        try {
           
            if (type.equals("content") || type.equals("changelanguage")) {
                createContentURL(url);
            }
            else if (type.equals("tml") || type.equals("layout")) {
                createLayoutURL(url);
            }
            else if (type.equals("file")) {
                createFileURL(content, url);
            }
            else if (type.equals("nextpage") || type.equals("previouspage") || type.equals("selectpage")) {
                status.keepParams = true;
                createForeachPageURL(url, type);
            }
            else if (type.equals("js") || type.equals("css")) {
                createCssJsURL(url, type);
            }
            else if (type.equals("login")) {
                createLoginURL(content, url);
            }
            else if (type.equals("logout")) {
                createLogoutURL(content, url);
            }
            else if (type.equals("portletmode")) {
                createPortletmodeURL(url);
            }
            else if (type.equals("action") && !isAjaxCall()) {
                createActionURL(url, actionName);
            }
            else if (type.equals("action") && isAjaxCall()) {
                createAjaxActionURL(url, actionName);
            }
            else if (type.equals("homepage")) {
                createHomePageURL(url);
            }
            else if (type.equals("tmlform")) {
                createTmlFormURL(url, fileName);
            }
            else if (type.equals("static")) {
                createStaticURL(url, fileName);
            }
            else {
                this.addWarning("Unknown type attribute: " + type, true);
                return;
            }
        }
        catch (WGUnresolveableVirtualLinkException e) {
            throw new TMLException("Unresolveable virtual link on content " + getTMLContext().content().getContentKey(true) + ": " + e.getMessage(), true);
        }
        
        catch (WGUnavailableException e) {
            addWarning("Target database is unavailable", true);
            return;
        }
        catch (WGException e) {
            addWarning(e.getMessage(), true);
            return;
        }
        catch (UnsupportedEncodingException e) {
           addWarning("Cannot create action url because of unsupported encoding: " + e.getMessage());
        }
        catch (ServletException e) {
            addWarning(e.getMessage(), true);
            return;
        }
        catch (IOException e) {
            addWarning(e.getMessage(), true);
            return;
        }
        
        // Post processing
        String completeURL = url.toString();
        boolean isPlainURL = !completeURL.startsWith("javascript:");

        if (isPlainURL) {
            
            // Read params from dynamic attributes
            readDynamicAttributeParams();
            
            // Recover parameters from this request if params are to be kept
            if (status.keepParams != null && status.keepParams.equals(true)) {
                recoverParams(completeURL);
            }
            
            // Add derivate parameter if neccessary
            if (type.equals("file")) {
                completeURL = addDerivateParameter(completeURL, absoluteURL);
            }
            
            // Add parameters to URL
            if (status.urlparams.size() > 0 || status.varparams.size() > 0) {
                completeURL = addParameters(type, completeURL);
            }
            
            // Eventually make absolute
            if (absoluteURL) {
                completeURL = makeAbsolute(status, absoluteProtocol, absolutePort, completeURL);
            }
        }
        
        if (status.encodeURL == true) {
            completeURL = getResponse().encodeURL(completeURL);
        }
        
        setResult(completeURL);
        
    }

    private void createStaticURL(StringBuffer url, String fileName) throws WGAPIException {
        url.append(getTMLContext().meta("request","wgaurl")).append("/static/").append(fileName);
    }

    public String makeAbsolute(Status status, String absoluteProtocol, int absolutePort, String completeURL) {
        try {
            URLBuilder builder = WGA.get(getTMLContext()).urlBuilder(completeURL);
            if (!builder.isAbsoluteUrlGiven()) {
                builder.setProtocol(absoluteProtocol);
                builder.setHost(getPageContext().getRequest().getServerName());
                builder.setPort(absolutePort);
            }
            completeURL = builder.build(true);
        }
        catch (Exception e) {
            addWarning("Exception making URL absolute " + completeURL + " :" + e.getClass().getName() +  " - " + e.getMessage());
            getTMLContext().getlog().error("Exception makingURL absolute: " + completeURL, e);
        }
        return completeURL;
    }

    private void readDynamicAttributeParams() throws WGException {

        Status status = getStatus();
        for (DynamicAttribute att : status.dynamicOptions.values()) {
            
            Object value = att.getDynamicValue(getTMLContext());
            if (value != null) {
                if (att.getPrefix().equals("v")) {
                    status.varparams.put(att.getBaseName(), value);
                }
                else if (att.getPrefix().equals("u")) {
                    status.urlparams.put(att.getBaseName(), value);
                }
            }
            
        }
        
        
    }

    private void createTmlFormURL(StringBuffer url, String fileName) throws TMLException, URIException {

        TMLForm tmlForm = getTMLContext().gettmlform();
        
        String formID = getForm();
        if (formID != null) {
            tmlForm = getTMLContext().tmlformbyid(formID);
            if (tmlForm == null) {
                throw new TMLException("Cannot generate WebTML form URL as WebTML form is not available: " + formID, true);
            }
        }
        else if (tmlForm == null) {
            throw new TMLException("Cannot generate WebTML form URL as WebTML form is not available", true);
        }
        
        if (!tmlForm.getfilenames().contains(fileName)) {
            throw new TMLException("Cannot generate WebTML form URL as file is not available on WebTML form:" + fileName, true);
        }
        
        url.append(tmlForm.fileurl(fileName));
        
    }

    private void recoverParams(String completeURL) throws UnavailableResourceException, WGException {
        
        try {
            Status status = getStatus();
            
            URLBuilder currentURL = WGA.get(getTMLContext()).urlBuilder(completeURL);
            URLBuilder currentRequest = WGA.get(getTMLContext()).urlBuilder(getTMLContext().getrequest().getAttribute(WGACore.ATTRIB_REQUESTURL).toString());
            for (String paramName : currentRequest.getParameterNames()) {
                if (!status.urlparams.containsKey(paramName) && !currentURL.hasParameter(paramName)) {
                    status.urlparams.put(paramName, currentRequest.getParameter(paramName));
                }
            }
            
            @SuppressWarnings("unchecked")
			Map<String,Object> varParams = (Map<String, Object>) getTMLContext().getrequest().getAttribute(WGACore.ATTRIB_VAR_PARAMETERS);
            if (varParams != null) {
                varParams.keySet().removeAll(status.varparams.keySet());
                status.varparams.putAll(varParams);
            }
        }
        catch (Exception e) {
            throw new TMLException("Exception recovering request params", e, true);
        }
        
        
    }

    private String addParameters(String urlType, String completeURL) {
        
        try {
            Status status = getStatus();
            URLBuilder builder = WGA.get(getTMLContext()).urlBuilder(completeURL);
            
            // URL Params
            for (Map.Entry<String,Object> param : status.urlparams.entrySet()) {
                builder.setParameter(param.getKey(), String.valueOf(param.getValue()));
            }
            
            // Var Params
            for (Map.Entry<String,Object> param : status.varparams.entrySet()) {
                builder.setVarParameter(param.getKey(), param.getValue());
            }
            
            return builder.buildLikeGiven();
        }
        catch (Exception e) {
            addWarning("Exception adding parameters to URL " + completeURL + " :" + e.getClass().getName() +  " - " + e.getMessage());
            getTMLContext().getlog().error("Exception adding parameters to URL " + completeURL, e);
            return completeURL;
        }
        
        
    }

    private String addDerivateParameter(String url, boolean absolute) {
        
        try {
            String derivate = getDerivate();
            String predefinedImageQuery = (String) getOption(OPTION_IMAGE_DERIVATES);
            if (derivate == null && predefinedImageQuery == null) {
                return url;
            }
                
            // Look if the predefined image derivate query applies here. For this we must determine the mime type of the file to serve. It must be "image".
            boolean usePredefinedQuery = false;
            if (predefinedImageQuery != null) {
                WGDocument doc = getFileContainerDocument();
                if (doc != null) {
                    WGFileMetaData md = doc.getFileMetaData(getFile());
                    if (md != null && md.getMimeType() != null && md.getMimeType().startsWith("image/")) {
                        usePredefinedQuery = true;
                    }
                }
            }
    
            // Build the query
            DerivateQuery query = null;
            if (usePredefinedQuery) {
                if (derivate != null) {
                    query = getCore().getFileDerivateManager().mergeDerivateQueries(derivate, predefinedImageQuery);
                }
                else {
                    query = getCore().getFileDerivateManager().parseDerivateQuery(predefinedImageQuery);
                }
            }
            else if (derivate != null) {
                query = getCore().getFileDerivateManager().parseDerivateQuery(derivate);
            }
            
            // If built, set as parameter
            if (query != null) {
                
                if (query.isNoDerivate()) {
                    return url;
                }
                
                Status status = getStatus();
                java.net.URL resultURL = new java.net.URL(new java.net.URL(status.getRequestURL()), url);
                URLBuilder builder = new URLBuilder(getTMLContext(), resultURL);
                builder.setParameter(WGPDispatcher.URLPARAM_DERIVATE, query.toString());
                return builder.build(absolute);
            }
            else {
                return url;
            }
        }
        catch (Exception e) {
            addWarning("Exception selecting derivate for URL " + url + " :" + e.getClass().getName() +  " - " + e.getMessage());
            getTMLContext().getlog().error("Exception selecting derivate for URL " + url, e);
            return url;
        }
            
        
    }

    private WGDocument getFileContainerDocument() throws WGException, WGAPIException {
        String dbKey = getDb();
        String docKey = getDoc();
        WGA wga = WGA.get(getTMLContext());
        
        Database db = dbKey != null && !"this".equals(dbKey) ? wga.app(dbKey) : wga.app();
        if (db == null || !db.isOpen()) {
            return null;
        }
        
        WGDocument doc = null; 
        if (docKey == null || docKey.equals("this")) {
            doc = getTMLContext().getdocument();
        }
        else {
            if (db instanceof App) {
                App app = (App) db;
                doc = app.design().resolve(docKey).getFileContainer();
            }
            
            if (doc == null) {
                doc = WGPDispatcher.getContentByAnyKey(docKey, db.db(), getTMLContext().getrequest());
            }
        }
        return doc;
    }

    private String registerInlineAction(Status status) throws WGException, TMLException {
        // Registration of inline action
        try {
            String actionCode = this.getResultString();
            // create actionObject
            TMLAction tmlAction = new TMLAction(actionCode, false, false, stringToBoolean(getDebounce()), TMLAction.ORIGIN_TML);
            tmlAction.setDesignReference(new DesignResourceReference(getDesignDBKey(), getStatus().getTMLModuleName()));
            tmlAction = getTMLContext().registerAction(tmlAction, null, getDesignDBKey());

            this.clearResult();
            String actionURL = "";
            // if we have a ajax call
            if (isAjaxCall()) {
                // build ajax action
                actionURL = "javascript:" + getAjaxJSFunction(tmlAction, buildNamedActionParameters(true), createParamList(), getStatus().keepParams);
            }
            else {
                // build standard action
                actionURL = buildActionURL(tmlAction, buildNamedActionParameters(true), createParamList());
                if (actionURL != null && !actionURL.startsWith("javascript:")) {
                    actionURL = getResponse().encodeURL(actionURL);
                }
            }
            return actionURL;
        }
        catch (UnsupportedEncodingException e) {
            addWarning("Unable to create action url because of unsupported encoding: " + e.getMessage(), true);
            setCancelTag(true);
            return null;
        }
    }

    /**
     * Returns the action.
     * 
     * @return String
     */
    public String getAction() {
        return this.getTagAttributeValue("action", action, null);
    }

    /**
     * Sets the action.
     * 
     * @param action
     *            The action to set
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * Returns the param.
     * 
     * @return String
     */
    public String getParam1() {
        return this.getTagAttributeValue("param1", param1, null);
    }

    /**
     * Sets the param.
     * 
     * @param param
     *            The param to set
     */
    public void setParam1(String param) {
        this.param1 = param;
    }

    /**
     * Returns the param2.
     * 
     * @return String
     */
    public String getParam2() {
        return this.getTagAttributeValue("param2", param2, null);
    }

    /**
     * Returns the param3.
     * 
     * @return String
     */
    public String getParam3() {
        return this.getTagAttributeValue("param3", param3, null);
    }

    /**
     * Sets the param2.
     * 
     * @param param2
     *            The param2 to set
     */
    public void setParam2(String param2) {
        this.param2 = param2;
    }

    /**
     * Sets the param3.
     * 
     * @param param3
     *            The param3 to set
     */
    public void setParam3(String param3) {
        this.param3 = param3;
    }

    /**
     * Returns the param4.
     * 
     * @return String
     */
    public String getParam4() {
        return this.getTagAttributeValue("param4", param4, null);
    }

    /**
     * Returns the param5.
     * 
     * @return String
     */
    public String getParam5() {
        return this.getTagAttributeValue("param5", param5, null);
    }

    /**
     * Sets the param4.
     * 
     * @param param4
     *            The param4 to set
     */
    public void setParam4(String param4) {
        this.param4 = param4;
    }

    /**
     * Sets the param5.
     * 
     * @param param5
     *            The param5 to set
     */
    public void setParam5(String param5) {
        this.param5 = param5;
    }

    public String getDebounce() {
        return this.getTagAttributeValue("debounce", debounce, "true");
    }

    public void setDebounce(String debounce) {
        this.debounce = debounce;
    }

    public String getDataurl() {
        return this.getTagAttributeValue("dataURL", dataurl, "false");
    }

    public void setDataurl(String dataURL) {
        this.dataurl = dataURL;
    }    
    
    public String getQuery() {
        return this.getTagAttributeValue("query", query, null);
    }

    public void setQuery(String query) {
        this.query = query;
    }

    public String getAbsolute() {
        return getTagAttributeValue("absolute", absolute, "false");
    }

    public void setAbsolute(String absolute) {
        this.absolute = absolute;
    }

    public String getProtocol() {
        return getTagAttributeValue("protocol", protocol, null);
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

	public String getPage() {
		return getTagAttributeValue("page", page, null);
	}

	public void setPage(String page) {
		this.page = page;
	}
    
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "a", "v", "u"); 
    }

    public String getDerivate() {
        return getTagAttributeValue("derivate", derivate, null);
    }

    public void setDerivate(String derivate) {
        this.derivate = derivate;
    }    
}
