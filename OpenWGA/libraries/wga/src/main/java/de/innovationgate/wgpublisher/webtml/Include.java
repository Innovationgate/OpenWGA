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
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.tagext.DynamicAttributes;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpVersion;
import org.apache.commons.httpclient.URI;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.params.HttpMethodParams;

import de.innovationgate.utils.URLBuilder;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGTransientPortlet;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.AjaxInfo;
import de.innovationgate.wgpublisher.webtml.utils.RootTagReceptor;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;
import de.innovationgate.wgpublisher.webtml.utils.TMLSilentCancelException;

public class Include extends Base implements DynamicAttributes, PreferredOptionReceiverTag {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String key;
	private String ref;
    private String name;
    private String type;
	private String mediakey;
	private String designdb;
	private String linkaction;
    private String ajax;
    private String keepoptions;
    private String timeout;
    private String encoding;
    private String linkmedium;
    private String tmlscope;
    private String portletmode;
    private String limit;
    
    public static class Status extends BaseTagStatus implements RootTagReceptor, DirectOutputCapableTag {
        String type;
        String ref;
        String designdb;
        public Root.Status rootTag;
        private Set<String> optionsToFilter;
        boolean refAlreadyResolved = false;

    	private long _startTime;
    	private boolean _portletInclude;
    	boolean ajax;
        private AjaxInfo _ajaxInfo;
        public boolean realDirectOutput;
        public Map<String,Object> includedModuleLocalVars = new HashMap<String, Object>();
        
        public TMLContext getChildTagContext() {
            return childTMLContext;
        }
        public void setRootTagStatus(Root.Status root) {
            rootTag = root;            
        }
        
        @Override
        public void setOption(String name, Object value, String scope) {
            super.setOption(name, value, scope);
            
            // Remove options set in include body from options to filter
            if (optionsToFilter.contains(name)) {
                optionsToFilter.remove(name);
            }
        }
        
        public Set<String> getOptionsToFilter() {
            return optionsToFilter;
        }
        
        @Override
        public void initAttributeDelegates(Base tag) {
            
            Include incTag = (Include) tag;
            
            this.type = incTag.getType();
            this.ref = incTag.getRef();
            this.designdb = incTag.getDesigndb();
            
            super.initAttributeDelegates(tag);
        }
        public boolean isPortletInclude() {
            return _portletInclude;
        }
        public AjaxInfo getPortletAJAXInfo() {
            return _ajaxInfo;
        }
        @Override
        public de.innovationgate.wgpublisher.webtml.Root.Status getRootTagStatus() {
            return rootTag;
        }
        @Override
        public void setStartTime(long currentTimeMillis) {
            _startTime = currentTimeMillis;
        }
        @Override
        public long getStartTime() {
            return _startTime;
        }
        
        @Override
        public boolean isDirectOutputCapable() {
            return false;
        }
        @Override
        public Map<String, Object> getLocalVarsToInherit() {
            return includedModuleLocalVars;
        }

    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
	
    public static final String TYPE_TML = "tml";
	public static final String TYPE_STATICTML = "statictml";	
    public static final String TYPE_INNERLAYOUT = "innerlayout";
	public static final String TYPE_URL = "url";
	public static final String TYPE_PORTLET = "portlet";

	public static final String OPTION_PORTLET_TMLMODULE = SYSTEMOPTION_PREFIX +"portletTmlmodule";
    public static final String OPTION_AJAX_DIVTAG_ID = SYSTEMOPTION_PREFIX +"ajaxDivTagId";

    
	public void tmlEndTag() throws TMLException, WGException {

	    Status status = (Status) getStatus();
	    
	    
		WGDatabase database = this.getMainContext().getdocument().getDatabase();
        
		// Set WebTML scope option
        String scope = getTmlscope();
        if (scope != null) {
            status.setOption(Base.OPTION_WEBTML_SCOPE, scope, TMLOption.SCOPE_GLOBAL);
        }
		
		// Set link action option
		String linkAction = this.getLinkaction();
		if (linkAction != null) {
		    TMLAction linkActionDef = getTMLContext().getActionByID(linkAction, getTMLContext().getDesignDBKey());
		    if (linkActionDef != null) {
		        getStatus().setOption(OPTION_LINK_ACTION, new DesignResourceReference(linkActionDef.getModuleDatabase(), linkActionDef.getID()), TMLOption.SCOPE_GLOBAL);
		    }
		}
		
		// Set option "body" by content of include tag
		// (only when no option "body" already available that was set explicitly for this tag or is of global scope)
		TMLOption bodyOption = (TMLOption) getStatus().getTagOptions().get("body");
		if (bodyOption == null || (status.optionsToFilter.contains("body") && bodyOption.getScope().equals(TMLOption.SCOPE_LOCAL))) {
		    status.setOption("body", getResultString(), TMLOption.SCOPE_LOCAL);
		    status.optionsToFilter.remove("body");
		}
        
        // Prepare portlet inclusion
		if (status.type.equals(TYPE_PORTLET)) {
			preparePortletInclusion();
            
            // The portlet inclusion is internally handled like a normal TML module inclusion
			performTMLInclude(TYPE_TML, database);
		}
		
		// Perform individual include type
		else if (status.type.equals(TYPE_TML) || status.type.equals(TYPE_INNERLAYOUT)) {
			performTMLInclude(status.type, database);            
		} 
		
		else if (status.type.equals(TYPE_STATICTML)) {
			performStaticTMLInclude();
		} 
		
		else if (status.type.equals(TYPE_URL)) {
			performURLInclude();
		}
		
	}

    private void performURLInclude() throws TMLException {
        
        Status status = (Status) getStatus();
        
        try {
            
            HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
            HttpMethodParams methodParams = new HttpMethodParams();
            methodParams.setSoTimeout(Integer.parseInt(getTimeout()));
            methodParams.setVersion(HttpVersion.HTTP_1_1);
            
            GetMethod getMethod = new GetMethod();
            getMethod.setURI(new URI(status.ref, false));
            getMethod.setParams(methodParams);
            getMethod.setFollowRedirects(true);
            
            int httpStatus = client.executeMethod(getMethod);
            if (httpStatus != HttpServletResponse.SC_OK) {
                throw new TMLException("Response status " + httpStatus + " (" + getMethod.getStatusText() + ") for included URL " + ref, true);
            }
            
            String encoding = getEncoding();
            if (encoding == null) {
                encoding = getMethod.getResponseCharSet();
                if (encoding == null) {
                    getTMLContext().addwarning("No encoding returned from URL '" + status.ref + "'. Assuming default encoding " + encoding);
                    encoding = getCore().getCharacterEncoding();
                }
            }
            
            Reader reader = new InputStreamReader(getMethod.getResponseBodyAsStream(), encoding);
            StringWriter writer = new StringWriter();
            char[] buf = new char[2048];
            long count = 0;
            
            String limitStr = getLimit();
            long charLimit = 0;
            try {
                charLimit = Math.round(1024 * 1024 * Double.parseDouble(limitStr));
            }
            catch (NumberFormatException e) {
                throw new TMLException("Cannot parse limit attribute as number: " + limitStr);
            }
            
            int len;
            while ((len = reader.read(buf)) != -1) {
                writer.write(buf, 0, len);
                count+=len;
                if (charLimit != 0 && count > charLimit) {
                    throw new TMLException("Include of URL '" + status.ref + "' reaches content limit of " + limitStr + " million characters. Include is cancelled.");
                }
            }
            this.setResult(writer.toString());
            
        	
        } catch (java.io.IOException exc) {
        	log.error("Exception including url", exc);
        	this.addWarning("Exception while including url: " + exc.getMessage());
        }
        
    }









    private void performStaticTMLInclude() throws TMLException {
        
        Status status = (Status) getStatus();
        
        // Locate resource
        String tmlName = status.ref;
        if( tmlName == null || tmlName.equals("") ){
        	throw new TMLException("Could not retrieve result of included static resource. No reference given.");
        }
        
        String jspPath = "/static/tml/" + tmlName + ".jsp";
        
        // Perform include
        String oldDesignDB = (String) getOption(Base.OPTION_DESIGNDB);
        this.pageContext.setAttribute(ATTRIB_INCLUDEPARENT, status, PageContext.REQUEST_SCOPE);
        getStatus().setOption(Base.OPTION_DESIGNDB, oldDesignDB, null);
        try {
       		this.pageContext.include(jspPath);
        } 
        catch (Exception e) {
        	throw new TMLException("Error executing static tml module \"" + tmlName + "\"", e, false);
        }
        finally {
            this.pageContext.setAttribute(ATTRIB_INCLUDEPARENT, null, PageContext.REQUEST_SCOPE);
            getStatus().setOption(Base.OPTION_DESIGNDB, oldDesignDB, null);
        }

        // Import the included root tag result
        if (status.rootTag != null) {
        	this.setResult(status.rootTag.result);
        	status.rootTag.result = null;
        } else {
        	throw new TMLException("Could not retrieve result of included static resource: " + tmlName);
        }
        
    }









    private void performTMLInclude(String type, WGDatabase database) throws WGException, TMLException {
        
        Status status = (Status) getStatus();
        
        
        String mediaKey = this.getMedium();
        if (mediaKey == null) {
        	mediaKey = status.getTMLModuleMediaKey();
        } 
        
        // Fetch the resource to include
        DesignResourceReference includeReference;
        WGA wga = WGA.get(getTMLContext());
        if (type.equals(TYPE_INNERLAYOUT)) {
            
            // Optional inner layout override via option
            if (getTMLContext().hasoption(OPTION_INNER_LAYOUT)) {
                Object innerLayout = getTMLContext().option(OPTION_INNER_LAYOUT);
                if (innerLayout instanceof Inline.Pointer) {
                    performInlineInclude((Inline.Pointer) innerLayout);
                }
                else {
                    this.setResult(String.valueOf(innerLayout));
                }
                return;
            }
            
        	WGContent currentContent =  this.getTMLContext().content();
        	if (currentContent.isDummy() || !currentContent.hasCompleteRelationships()) {
        		throw new TMLException("The inner layout of this content cannot be determined, bc. it is either a dummy document, or it doesnt have complete document relationships.");
        	}
        	Design databaseDesign = wga.design(getTMLContext().content().getDatabase());
            includeReference = databaseDesign.resolve(currentContent.getStructEntry().getContentType().getInnerLayoutName()).getBaseReference();
        	status.designdb = includeReference.getDesignApp();
        } 
        
        else {
        	WGDatabase designdb = database;
        	DesignResourceReference refObj;
        	if (status.refAlreadyResolved) {
        	    includeReference = new DesignResourceReference(status.designdb, status.ref);
        	}
        	else {
        	    includeReference = getTMLContext().resolveDesignResource(status.designdb, status.ref);
        	}
        }
        
        // Got to hijack direct output a bit here. If our parent tag has direct output
        // The included root may have as well. Only the include itself should not have it
        // as its body is needed for option "body".
        if (maySwitchToDirectOutputForInclude(status)) {
            status.directOutput = true;
        }

        if (status.directOutput) { // Must be done here bc. the prefix was modified by preparePortletInclusion() after it was already put out on tag start
            try {
                pageContext.getOut().write(getPrefix());
            }
            catch (IOException e) {
                throw new WGException("Exception writing include prefix", e);
            }
        }

        // Dispatch
        status.result = null;
        getCore().getDispatcher().dispatchToRenderer(wga, wga.design(includeReference.getDesignApp()).resolve(includeReference.getResourceName()), status.childTMLContext, mediaKey);
        
        if (status.directOutput) { // Must be done here bc. the suffix will not be put out automatically after we switch off directOutput as tag result is null.
            try {
                pageContext.getOut().write(getSuffix());
                setResult(null);
            }
            catch (IOException e) {
                throw new WGException("Exception writing include prefix", e);
            }
        }
        
        status.directOutput = false;
        
        
    }

    private boolean maySwitchToDirectOutputForInclude(Status status) {
        if (!status.getParentTag().directOutput) {
            return false;
        }
        
        if (isVarWritten()) {
            return false;
        }
        
        return true;
    }











    private void preparePortletInclusion() throws TMLException, WGException {
        
        Status status = (Status) getStatus();
        status._portletInclude = true;
        
        String type;
        TMLPortlet parentPortlet = getTMLContext().getportlet();
        if (parentPortlet == null) {
            if (getTMLContext().isbotrequest()) {
                throw new TMLSilentCancelException();
            }
            else {
                throw new TMLException("Current user has no portlet registration");
            }
        }
        
        String pKey = getKey();
        String pName = getName();

        //  If no key direcly given try to fetch via portlet name
        WGPortlet pReg = null;
        if (pName != null) {
            TMLPortlet tmlPortlet = parentPortlet.getportletforname(pName);
            if (tmlPortlet != null) {
                pReg = tmlPortlet.getReg();
                pKey = tmlPortlet.getportletkey();
            }
        }
        
        // Fetch registration info if key provided
        else if (pKey != null) {
            pReg = parentPortlet.getPortletRegistration(pKey);
        }

        
        
        // If reg not retrievable and auto registration info available try auto registration, else cancel
        if (pReg == null) {
            if (pName != null && status.ref != null) {
                DesignResourceReference tagRef = getTMLContext().resolveDesignResource(status.designdb, status.ref);
                pKey = parentPortlet.registerportletforname(pName, tagRef, false);
                pReg = parentPortlet.getPortletRegistration(pKey);
            }
            else {
                if (pName != null) {
                    throw new TMLException("Portlet name not registered: " + pName, true);
                }
                else if (pKey != null) {
                    throw new TMLException("Portlet key not registered: " + pKey, true);
                }
                else {
                    throw new TMLException("No portlet key provided.", true);
                }
            }
        }

        // Check if registration matchtes the autoregister TML. If not, reregister with the given TML.
        else if (status.ref != null) {
            
                DesignResourceReference tagRef = getTMLContext().resolveDesignResource(status.designdb, status.ref);
                
                // Design references in the portlet registry should already be resolved. Just expanding local references for backward compatibility (#00001674)
                DesignResourceReference portletRef = new DesignResourceReference(pReg.getDesignDb(), TMLContext.processReferencePath(pReg.getDesign(), getTMLContext().getDesignContext().getBaseReference().getResourceName()));
                
                // If the registration has no design database specified we take the current design db (which is always equal in the following test)
                if (portletRef != null && portletRef.getDesignApp() == null) {
                    portletRef.setDesignApp(status.designdb);
                }
                    
                if (!WGUtils.nullSafeEquals(tagRef.getResourceName(), portletRef.getResourceName(), true) ||
                        !WGUtils.nullSafeEquals(tagRef.getDesignApp(), portletRef.getDesignApp(), true)) {
                    if (pReg instanceof WGTransientPortlet) { // On transient portlets just write the info to prevent re-registration (#00004253)
                        WGTransientPortlet transientReg = (WGTransientPortlet) pReg;
                        pReg.setDesignDb(tagRef.getDesignApp());
                        pReg.setDesign(tagRef.getResourceName());
                    }
                    else {
                        pKey = parentPortlet.registerportletforname(pName, tagRef, true);
                        pReg = parentPortlet.getPortletRegistration(pKey);
                    }
                }
                    
        }
        

        // Set portlet namespace option, setting this portlet for WebTML
        status.setOption(Base.OPTION_PORTLET_NAMESPACE, pKey, null);
        
        // Prepare environment. Load back portlet registration info into tag attribute delegates
        if (pReg.getDesignDb() != null) {
            status.designdb = pReg.getDesignDb();
        }
        status.ref = pReg.getDesign();
        if (status.ref != null && !status.ref.startsWith("::")) { // Backward compatibility with old registrations, which may have made it into registry unresolved (see #00001574) 
            status.refAlreadyResolved = true;
        }

        // Work on the TMLScript portlet object from now on
        TMLPortlet portlet = getTMLContext().getportlet();
        
        // Build basic HTML structures for AJAX processing
        String portletMode = getPortletmode();
        if (status.ajax) {
            createAJAXHTML(status, portlet, (portletMode != null ? portletMode : "view"));
        }   
        
        // If an initial mode is determined set it
        if (portletMode != null) {
            portlet.setmode(portletMode);
            portlet.forcestate();
        }
        
        // Instantiate the controller, if available
        portlet.getState();
    }

    private void createAJAXHTML(Status status, TMLPortlet portlet, String initialMode) throws WGException {
        String uniquePortletID = portlet.getportletkey();
        
        // create prefix buffer
        StringBuffer prefix = new StringBuffer();
        // create suffix buffer
        StringBuffer suffix = new StringBuffer();
        
        // set id as option, so tags from included module can retrieve it for rendering ajaxCall
        status.setOption(OPTION_AJAX_DIVTAG_ID, uniquePortletID, null);
        // set tmlModule as option, so tags from included module can retrieve the tmlmodulename
        status.setOption(OPTION_PORTLET_TMLMODULE, portlet.getDesign(), null); 
                            
        // create ajaxInfo
        AjaxInfo ajaxInfo = new AjaxInfo(uniquePortletID);
        try {
        	URLBuilder builder = new URLBuilder(getTMLContext().getrequest(), getCore().getCharacterEncoding());
        	builder.removeParameter("$action");
        	java.net.URL url = new URL(builder.build(true));
        	ajaxInfo.setQueryString(url.getQuery());
        } catch (Exception e1) {
        	getTMLContext().addwarning("Unable to build request querystring for ajax environment.");
        }
        ajaxInfo.setPortletPath(portlet.getportletpath());
        ajaxInfo.setTmlmodule(portlet.getDesign());
        ajaxInfo.setDesignDB(portlet.gettmldb() != null ? portlet.gettmldb() : getDesigndb());
        ajaxInfo.setMediaKey((String)this.getOption(OPTION_CURRENT_MEDIAKEY));
        ajaxInfo.setContextPath(this.getTMLContext().getpath());        
        
        ajaxInfo.setOptions(new HashMap<String,TMLOption>(getStatus().getTagOptions()));
        ajaxInfo.getOptions().keySet().removeAll(Root.UNRECOVERABLE_OPTIONS);
        
        ajaxInfo.setProfileSessionId(getTMLContext().getmaincontext().getprofile().getProfileSessionId());
        ajaxInfo.setInitialMode(initialMode);
        if (getTMLContext().getprofile() != null) {
            ajaxInfo.setSaveProfileOnEnd(getTMLContext().getprofile().isSavedOnEnd());
        }
        else {
            ajaxInfo.setSaveProfileOnEnd(false);
        }
        ajaxInfo.setSuperform(getStatus().getRelevantForm());
        
        TMLPageImpl page = (TMLPageImpl) WGA.get(getTMLContext()).tmlPage();
        PageConnection pc = page.getPageConnection(false);
        if (pc != null) {
            ajaxInfo.setPageId(pc.getPageId());
        }
        
        status._ajaxInfo = ajaxInfo;
        
        // create javascript object with ajaxInfo                    
        // serialize        
        String serAjaxInfo = getCore().getDefaultSerializer().toXML(ajaxInfo);
        // zip
        byte[] zipped  = WGUtils.zipString(serAjaxInfo);
        String encryptedAjaxInfo = "";
        if (serAjaxInfo != null) {
            // encrypt
            try {
                encryptedAjaxInfo= this.getTMLContext().getwgacore().getSymmetricEncryptionEngine().encryptBase64Web(zipped);
            }
            catch (Exception e) {
               throw new TMLException("Cannot render ajax enabled include because of unsupported encoding: " + e.getMessage(), true);
            }
        }                    
        
        // div tag for ajax paste
        prefix.append("<div id=\"$ajaxDiv_").append(uniquePortletID).append("\">");
        prefix.append("<div id=\"$ajaxContentDiv_").append(uniquePortletID).append("\">");

        prefix.append("<script type=\"text/javascript\">");
        
        // javascript variable ajaxInfo
        prefix.append("var $ajaxInfo_").append(uniquePortletID).append(" = '").append(encryptedAjaxInfo + "';");
        
        prefix.append("</script>");
        suffix.append("</div></div>");
        
        
        // set prefix and suffix
        this.setPrefix(prefix.toString());                                        
        this.setSuffix(suffix.toString());
    }

    public String getKey() {
		return this.getTagAttributeValue("key", this.key, null);
	}

	public void setKey(String name) {
		this.key = name;
	}

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getRef() {
		return this.getTagAttributeValue("ref", ref, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setRef(String name) {
		this.ref = name;
	}
	/**
	 * Gets the type
	 * @return Returns a String
	 */
	public String getType() {
		return this.getTagAttributeValue("type", this.type, "tml");
	}
	/**
	 * Sets the type
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}
	/**
	 * Gets the mediakey
	 * @return Returns a String
	 */
	public String getMedium() {
	    return this.getTagAttributeValue("medium", mediakey, null);
	}
	/**
	 * Sets the mediakey
	 * @param mediakey The mediakey to set
	 */
	public void setMedium(String mediakey) {
		this.mediakey = mediakey;
	}

	/**
	 * Returns the designdb.
	 * @return String
	 */
	public String getDesigndb() {

		return this.getTagAttributeValue("designdb", designdb, getDesignDBKey());

	}

	/**
	 * Sets the designdb.
	 * @param designdb The designdb to set
	 */
	public void setDesigndb(String designdb) {
		this.designdb = designdb;
	}

	/**
	 * Returns the linkaction.
	 * @return String
	 */
	public String getLinkaction() {
		return this.getTagAttributeValue("linkaction", linkaction, null);
	}

	/**
	 * Sets the linkaction.
	 * @param linkaction The linkaction to set
	 */
	public void setLinkaction(String linkaction) {
		this.linkaction = linkaction;
	}

    private String getAjax() {
        return this.getTagAttributeValue("ajax", ajax, "false");
    }

    public void setAjax(String ajax) {
        this.ajax = ajax;
    }

    public boolean isAjaxCall() {
        if (getAjax().equalsIgnoreCase(Base.AJAX_MODE_NO_PORTLET_REFRESH)) {
            return true;
        } else {
            return stringToBoolean(getAjax());
        }
    }

    public String getKeepoptions() {
        return getTagAttributeValue("keepoptions", keepoptions, null);
    }



    public void setKeepoptions(String keepoptions) {
        this.keepoptions = keepoptions;
    }

    public void tmlStartTag() throws WGException {
        Status status = (Status) getStatus();
    	status._startTime = System.currentTimeMillis();
    	status.ajax = stringToBoolean(getAjax());
    	if (getTMLContext().getPortletStateStorage() instanceof TMLPortletStateTransientStorage) {
    	    status.ajax = true;
    	}
    	
        // Keep names of options already available at start of include tag
        // to be able to differ them from options set inside
        
        
        // Build set of options to test for scope in included Root-Tag.
        // Options to test are
        // a) inherited from earlier tags (not set inside include tag)
        // b) not included in attribute keepoptions
        
        status.optionsToFilter = new HashSet<String>(getStatus().getTagOptions().keySet());
        
        String keepOptionsStr = getKeepoptions();
        List<String> keepOptions;
        if (keepOptionsStr != null) {
            keepOptions = WGUtils.deserializeCollection(keepOptionsStr, ",", true);
        }
        else {
            keepOptions = new ArrayList<String>();
        }
        status.optionsToFilter.removeAll(keepOptions);
        
        // Set options injected by dynamic JSP attributes
        for (DynamicAttribute att : status.dynamicOptions.values()) {
            if (att.getPrefix().equals("o")) {
                Object optionValue = att.getDynamicValue(getTMLContext());
                if (optionValue != null) {
                    status.setOption(att.getBaseName(), optionValue, TMLOption.SCOPE_GLOBAL);
                    if (status.optionsToFilter.contains(att.getBaseName())) {
                        status.optionsToFilter.remove(att.getBaseName());
                    }
                }
            }
        }
        
        // Set options by attributes
        String linkMedium = getLinkmedium();
        if (linkMedium != null) {
            status.setOption(OPTION_LINK_MEDIUM, linkMedium, TMLOption.SCOPE_GLOBAL);
        }
        
        status.dynamicOptions.clear();
        
    }



    public String getName() {
        return getTagAttributeValue("name", name, null);
    }

    public void setName(String name) {
        this.name = name;
    }
    
    
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "o"); 
    }

    public String getTimeout() {
        return getTagAttributeValue("timeout", timeout, "10000");
    }

    public void setTimeout(String timeout) {
        this.timeout = timeout;
    }

    public String getEncoding() {
        return getTagAttributeValue("encoding", encoding, null);
    }


    public void setEncoding(String encoding) {
        this.encoding = encoding;
    }

    public String getLinkmedium() {
        return getTagAttributeValue("linkmedium", linkmedium, null);
    }

    public void setLinkmedium(String linkmedium) {
        this.linkmedium = linkmedium;
    }

    public String getTmlscope() {
        return getTagAttributeValue("tmlscope", tmlscope, null);
    }

    public void setTmlscope(String tmlscope) {
        this.tmlscope = tmlscope;
    }

    public String getPortletmode() {
        return getTagAttributeValue("portletmode", portletmode, null);
    }

    public void setPortletmode(String portletmode) {
        this.portletmode = portletmode;
    }

    public String getLimit() {
        return getTagAttributeValue("limit", limit, "10");
    }

    public void setLimit(String limit) {
        this.limit = limit;
    }



}
