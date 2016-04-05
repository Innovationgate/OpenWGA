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

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.DeployerException;
import de.innovationgate.wgpublisher.DummyServletResponse;
import de.innovationgate.wgpublisher.RenderServletRequestWrapper;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.so.ManagedObject;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.Include;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext.ListVarContainer;

public class TMLPageImpl implements TMLPage {
    
    public interface OuterLayoutWriter {
        
        public void write(HttpServletRequest req, HttpServletResponse res) throws HttpErrorException, ServletException, IOException, WGException, DeployerException;
        
    }
    
    private WGA _wga;

    public TMLPageImpl(WGA wga) {
        _wga = wga;
    }
    
    private PageContext getPageContext() throws WGException {
        if (_wga.isTMLContextAvailable()) {
            BaseTagStatus tag = ((TMLContext) _wga.tmlcontext()).getDesignContext().getTag();
            if (tag != null) { 
                return tag.pageContext;
            }
        }
        
        return null;
    }
    
    private TMLDesignContext getDesignContext() throws WGException {
        return ((TMLContext) _wga.tmlcontext()).getDesignContext();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#isAvailable()
     */
    @Override
    public boolean isAvailable() {
        
        try {
            if (!_wga.isRequestAvailable()) {
                return false;
            }
            
            WGPRequestPath path = (WGPRequestPath) _wga.getRequest().getAttribute(WGACore.ATTRIB_REQUESTPATH);
            if (path == null) {
                return false;
            }
            
            if (path.getPathType() != WGPRequestPath.TYPE_TML && path.getPathType() != WGPRequestPath.TYPE_TITLE_PATH) {
                return false;
            }
            
            return true;
        }
        catch (WGException e) {
            return false;
        }
        
        
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#prepareSocket()
     */
    public boolean prepareWebSocket() throws WGException {
        return getOrCreatePageConnection() != null;
    }
    
    public PageConnection getOrCreatePageConnection() throws WGException {
        return getPageConnection(true);
    }
    
    public PageConnection getPageConnection(boolean create) throws WGException {
        if (!isAvailable()) {
            throw new UnavailableResourceException("Page object is not available in this environment");
        }
        
        if ((Boolean) WGACore.INSTANCE.getServicesServerOptionReader().readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_WEBSOCKETS) == false) {
            return null;
        }
        
        PageConnection pc = (PageConnection) _wga.getRequest().getAttribute(WGACore.ATTRIB_PAGECONNECTION);
        if (pc == null && create) {
            pc = _wga.getCore().getPageConnectionManager().newConnection(_wga);
            _wga.getRequest().setAttribute(WGACore.ATTRIB_PAGECONNECTION, pc);
        }
        return pc;
    }
    
    public void restorePageConnectionForAJAX(String pageId) throws WGException {
        PageConnection pc = _wga.getCore().getPageConnectionManager().getConnection(pageId);
        if (pc != null) {
            _wga.getRequest().setAttribute(WGACore.ATTRIB_PAGECONNECTION, pc);
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#render(de.innovationgate.wga.server.api.Design)
     */
    @Override
    public void render(Design design) throws WGException {
        render(design, _wga.call().getMediaKey());
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#render(de.innovationgate.wga.server.api.Design, de.innovationgate.wga.server.api.tml.Context)
     */
    @Override
    public void render(Design design, Context cx) throws WGException {
        render(design, _wga.call().getMediaKey(), cx);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#render(de.innovationgate.wga.server.api.Design, java.lang.String)
     */
    @Override
    public void render(Design design, String mediaKey) throws WGException {
        render(design, mediaKey, null);
    }
    
    @Override
    public void renderDefault() throws WGException {
        renderDefault(null);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#renderDefault()
     */
    @Override
    public void renderDefault(Map<Object,Object> options) throws WGException {
        
        String resourceLocalName = _wga.design().getBaseReference().getResourceLocalName();
        String tmlName = resourceLocalName.substring(0, resourceLocalName.length() - ".renderer".length());
        Design moduleDesign = _wga.design().resolve("::" + tmlName);
        render(moduleDesign, _wga.design().getTmlMedium(), null, options);
        
    }
    
    @Override
    public void render(Design design, String mediaKey, Context cx) throws WGException {
        render(design, mediaKey, cx, null);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#render(de.innovationgate.wga.server.api.Design, java.lang.String, de.innovationgate.wga.server.api.tml.Context)
     */
    @Override
    public void render(Design design, String mediaKey, Context cx, Map<Object,Object> options) throws WGException {
        
        if (cx == null) {
            cx = _wga.tmlcontext();
        }
        
        if (mediaKey == null) {
            mediaKey = _wga.call().getMediaKey();
        }
        
        if (!isAvailable()) {
            throw new UnavailableResourceException("Page object is not available in this environment");
        }
        
        // Locate the JSP resource
        WGTMLModule tmllib = design.getTMLModule(mediaKey);
        if (tmllib == null) {
            throw new TMLException("Unknown WebTML module: " + design.getBaseReference().toString() + " (" + mediaKey + ")");
        }
        String tmlResource = null;
        try {
            tmlResource = _wga.getCore().getDeployer().locateTmlResource(tmllib, _wga.getRequest());
        }
        catch (DeployerException e1) {
            throw new TMLException("Error deploying WebTML resource", e1, true);
        }
        
        if (tmlResource == null) {
            throw new TMLException("Cannot locate requested tml module: " + tmllib.getName() + " (" + tmllib.getMediaKey() + ")");
        }

        // We are either on an include tag or no tag at all
        TMLDesignContext designContext = getDesignContext();
        BaseTagStatus parentTag = designContext.getTag();
        if (parentTag != null && !(parentTag instanceof Include.Status)) {
            throw new WGAServerException("Cannot use render method at this WebTML location");
        }
        Include.Status includeTag = (Include.Status) parentTag;
        
        // Set the include context. Either determine the child context on an include, or determine the thread main context that the
        // absolut root will take over
        if (includeTag != null) {
            includeTag.childTMLContext = (TMLContext) cx;
        }
        else {
            ((TMLContext) cx).makeThreadMainContext();
        }
        
        
        TMLOptionPreserver preserver = new TMLOptionPreserver(designContext);
        
        // In the case of an include we must set include parent, overwrite designdb option
        if (includeTag != null) {
            _wga.getRequest().setAttribute(Include.ATTRIB_INCLUDEPARENT, includeTag);
            preserver.preserve(Base.OPTION_DESIGNDB, tmllib.getDatabase().getDbReference());
        }
        
        // Optionally set options from argument
        if (options != null) {
            for (Map.Entry<Object,Object> option : options.entrySet()) {
                preserver.preserve(String.valueOf(option.getKey()), option.getValue(), TMLOption.SCOPE_PORTLET);
            }
        }
        
        // Optionally instantiate module controller and call prepare
        TMLContext outerLayoutContext = ((TMLContext) cx).designContext(design.getBaseReference().toString());
        ManagedObject controller = outerLayoutContext.fetchModuleController();
        if (controller != null) {
            controller.beforeUsage();
            try {
                preserver.preserve(Base.OPTION_MODULE_CONTROLLER, controller, TMLOption.SCOPE_LOCAL);
                TMLScript tmlscript = WGA.get(outerLayoutContext).tmlscript();
                if (tmlscript.hasProperty(controller.getObject(), "prepare")) {
                    Object result = tmlscript.callMethod(controller.getObject(), "prepare", null, null);
                    if (result != null) {
                        processControllerResultObject(design, cx, includeTag, preserver, tmlscript, result);
                    }
                }
            }
            finally {
                controller.afterUsage();
            }
        }
        
        // Else overwrite the controller downward to be nonexistent
        else {
            preserver.preserve(Base.OPTION_MODULE_CONTROLLER, null, TMLOption.SCOPE_LOCAL);
        }

        // Include
        try {
            doInclude(tmllib, tmlResource);
        } 
        catch (WGException e) {
            throw e;
        }
        catch (Exception e) {
            throw new TMLException("Error executing tml module \"" + tmllib.getName() + "/" + tmllib.getMediaKey() + "\"", e, false);
        }
        finally {
            // Restore old environment
            preserver.restore();
            if (includeTag != null) {
                _wga.getRequest().setAttribute(Include.ATTRIB_INCLUDEPARENT, null);
            }
            else if (cx != null) {
                ((TMLContext) cx).removeThreadMainContext();
            }
        }

        // Transfer result from included root to the parent tag
        if (includeTag != null) {
            if (includeTag.rootTag != null) {
                if (!includeTag.directOutput) {
                    includeTag.appendResult(includeTag.rootTag.getResultString(true, false));
                    includeTag.rootTag.clearResult();
                }
            }
            else {
                throw new TMLException("Could not retrieve result of included resource: " + tmlResource);
            }
        }
        
        
    }

    public void processControllerResultObject(Design design, Context cx, Include.Status includeTag, TMLOptionPreserver preserver, TMLScript tmlscript, Object result) throws WGException {
        
        if (tmlscript.hasProperty(result, "$options")) {
            Object scriptOptions = tmlscript.callMethod(result, "$options");
            if (scriptOptions instanceof Map) {
                try {
                    @SuppressWarnings("unchecked")
                    Map<Object, Object> controllerOptions = (Map<Object,Object>) scriptOptions;
                    for (Map.Entry<Object,Object> option : controllerOptions.entrySet()) {
                        preserver.preserve(String.valueOf(option.getKey()), option.getValue(), TMLOption.SCOPE_PORTLET);
                    }
                }
                catch (Exception e) {
                    _wga.getLog().error("Exception descriptifying result of prepare method on module controller of '" + design.getBaseReference().toString() + "' as Lookup Table", e);
                }
            }
        }
        
        if (tmlscript.hasProperty(result, "$vars")) {
            Object scriptVars = tmlscript.callMethod(result, "$vars");
            if (scriptVars instanceof Map) {
                try {
                    @SuppressWarnings("unchecked")
                    Map<Object, Object> vars = (Map<Object,Object>) scriptVars;
                    for (Map.Entry<Object,Object> var : vars.entrySet()) {
                        if (includeTag != null) {
                            includeTag.includedModuleLocalVars.put(String.valueOf(var.getKey()).toLowerCase(), var.getValue());
                        }
                        else {
                            cx.setvar(String.valueOf(var.getKey()), var.getValue());
                        }
                    }
                }
                catch (Exception e) {
                    _wga.getLog().error("Exception descriptifying result of prepare method on module controller of '" + design.getBaseReference().toString() + "' as Lookup Table", e);
                }
            }
        }
    }

    private void doInclude(final WGTMLModule mod, String tmlResource) throws ServletException, IOException, WGException, HttpErrorException {
        
        PageContext pc = getPageContext();
        if (pc != null) {
            pc.include(tmlResource);
        }
        else {
            doOuterLayout(mod, new OuterLayoutWriter() {
                @Override
                public void write(HttpServletRequest req, HttpServletResponse res) throws HttpErrorException, ServletException, IOException, WGAPIException, DeployerException {
                    String targetJSP = _wga.getCore().getDeployer().locateTmlResource(mod, req);
                    if (targetJSP != null) {
                        _wga.getCore().getDispatcher().getServletContext().getRequestDispatcher(targetJSP).include(req, res);
                    }
                    else {
                        throw new HttpErrorException(500, "WebTML module not active: " + mod.getDocumentKey().toString(), mod.getDatabase().getDbReference());
                    }
                }
            });
        }
        
    }


    
    private void doOuterLayout(WGTMLModule tmllib, OuterLayoutWriter writer) throws WGException, HttpErrorException, ServletException, IOException {
        
        HttpServletRequest request = _wga.getRequest();
        HttpServletResponse response = _wga.getResponse();
        
        // Setup response, cancel if we do not write the body on this type of request
        MediaKey mediaKeyObj = _wga.getCore().getMediaKey(_wga.call().getMediaKey());
        if (!setupResponse(tmllib, request, response, mediaKeyObj)) {
            return;
        }
        
        // Wrapping request/response
        HttpServletRequest dispatchRequest = request;
        HttpServletResponse dispatchResponse = response;
        if (request instanceof RenderServletRequestWrapper) {
            dispatchRequest = (HttpServletRequest) ((HttpServletRequestWrapper) request).getRequest();
        }
        if (mediaKeyObj.isBinary()) {
            dispatchResponse = new DummyServletResponse(response);
        }
        
        // Include
        writer.write(dispatchRequest, dispatchResponse);
        
        
    }

    private boolean setupResponse(WGTMLModule tmllib, HttpServletRequest request, HttpServletResponse response, MediaKey mediaKeyObj) throws WGException {
        
        Boolean setup = (Boolean) request.getAttribute(WGACore.ATTRIB_RESPONSE_SETUP);
        if (setup != null) {
            return setup;
        }
        
        
        response.setContentType(mediaKeyObj.getMimeType());
        request.setAttribute(WGACore.ATTRIB_MIMETYPE, mediaKeyObj.getMimeType());
        request.setAttribute(WGACore.ATTRIB_MEDIAKEY, mediaKeyObj.getKey());
        if (mediaKeyObj.isBinary()) {
            request.setAttribute(WGACore.ATTRIB_SERVLETRESPONSE, response);
        }
        
        // Caching
        boolean continueResponse = true;
        if (tmllib != null && tmllib.isCacheable() && _wga.getCore().isWebTMLCachingEnabled()) {
            response.setHeader("Cache-Control", "must-revalidate");

            long lastModified;
            // determine lastModified
            // - last modified of binary response depends only on resource
            // change date
            // - last change date of textual response additionally depends on
            // character encoding change date
            if (_wga.getCore().getDispatcher().isBinary(request, response)) {
                lastModified = _wga.getCore().getDeployer().getLastChangedOrDeployed(tmllib.getDatabase()).getTime();
            }
            else {
                lastModified = Math.max(_wga.getCore().getDeployer().getLastChangedOrDeployed(tmllib.getDatabase()).getTime(), _wga.getCore().getCharacterEncodingLastModified());
                lastModified = Math.max(lastModified, _wga.getCore().getDesignEncodingLastModified(tmllib.getDatabase().getDbReference()));
            }

            // Test modified since
            if (_wga.getCore().getDispatcher().browserCacheIsValid(request, lastModified, String.valueOf(lastModified))) {
                response.setStatus(HttpServletResponse.SC_NOT_MODIFIED);
                continueResponse = false;
            }
            else {
                response.setDateHeader("Last-Modified", lastModified);
                response.setHeader("ETag", '"' + String.valueOf(lastModified) + '"');
            }
        }
        else {
            response.setHeader("Pragma", "No-Cache");
            
            if (mediaKeyObj.isBinary()) {
                response.setHeader("Cache-Control", "must-revalidate, post-check=0, pre-check=0");
            }
            else {
                response.setHeader("Cache-Control", "No-Cache");
            }
        }
        
        // Obsolete WebTML Header functionality
        if (tmllib != null) {
            String headerModuleName = "wga:header:" + tmllib.getMediaKey() + ":" + tmllib.getName();
            Design headerDesign = _wga.design(tmllib.getDatabase()).resolve(headerModuleName);
            WGScriptModule headerModule = headerDesign.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT); 
            if (headerModule != null) {
                try {                   
                    Context headerModuleContext = _wga.createTMLContext(TMLContext.getThreadMainContext().content(), _wga.design(tmllib.getDatabase()));
                    _wga.tmlscript().runScript(headerModuleContext, headerModule.getCode());             
                } catch (Exception e) {
                    _wga.getCore().getLog().error("Failed to execute header module '" + headerModuleName + "'", e);
                }
            }    
        }
        
        if ("HEAD".equalsIgnoreCase(request.getMethod())) {
            continueResponse = false;
        }
        
        request.setAttribute(WGACore.ATTRIB_RESPONSE_SETUP, continueResponse);
        return continueResponse;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#write(java.lang.String)
     */
    @Override
    public void write(final String out) throws WGException, IOException {
        if (!isAvailable()) {
            throw new UnavailableResourceException("Page object is not available in this environment");
        }
        
        TMLDesignContext designContext = getDesignContext();
        if (designContext.getTag() != null) {
           designContext.getTag().appendResult(out);
        }
        else {
            try {
                doOuterLayout(null, new OuterLayoutWriter() {
                    @Override
                    public void write(HttpServletRequest req, HttpServletResponse res) throws HttpErrorException, ServletException, IOException, WGException, DeployerException {
                        _wga.getResponse().getWriter().write(out);
                    }
                });
            } 
            catch (WGException e) {
                throw e;
            }
            catch (Exception e) {
                throw new TMLException("Error executing writing to out", e, false);
            }
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#write(byte[])
     */
    @Override
    public void write(byte[] out) throws WGException, IOException {
        if (!isAvailable()) {
            throw new UnavailableResourceException("Page object is not available in this environment");
        }
        _wga.getResponse().getOutputStream().write(out);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.Page#write(byte)
     */
    @Override
    public void write(byte out) throws WGException, IOException {
        if (!isAvailable()) {
            throw new UnavailableResourceException("Page object is not available in this environment");
        }
        _wga.getResponse().getOutputStream().write(out);
    }

    @Override
    public void write(InputStream in) throws WGException, IOException {
        WGUtils.inToOut(in, _wga.getResponse().getOutputStream(), 4092);
        
    }

    @Override
    public void write(final Reader in) throws WGException, IOException {
        if (!isAvailable()) {
            throw new UnavailableResourceException("Page object is not available in this environment");
        }
        
        TMLDesignContext designContext = getDesignContext();
        if (designContext.getTag() != null) {
           designContext.getTag().appendResult(in);
        }
        else {
            try {
                doOuterLayout(null, new OuterLayoutWriter() {
                    @Override
                    public void write(HttpServletRequest req, HttpServletResponse res) throws HttpErrorException, ServletException, IOException, WGException, DeployerException {
                        WGUtils.inToOut(in, _wga.getResponse().getWriter(), 4092);
                    }
                });
            } 
            catch (WGException e) {
                throw e;
            }
            catch (Exception e) {
                throw new TMLException("Error executing writing to out", e, false);
            }
        }
        
    }

    @SuppressWarnings("unchecked")
    @Override
    public void setVar(String name, Object value) throws WGException {
        if (value instanceof List<?>) {
            value = new ListVarContainer((List<Object>) value);
         }
        TMLContext.getThreadMainContext().getEnvironment().getPageVars().put(name.toLowerCase(), value);
    }

    @Override
    public boolean hasVar(String name) throws WGException {
        return TMLContext.getThreadMainContext().getEnvironment().getPageVars().containsKey(name.toLowerCase());
    }

    @Override
    public Object getVar(String name) throws WGException {
        return unwrapVar(TMLContext.getThreadMainContext().getEnvironment().getPageVars().get(name.toLowerCase()));
    }

    private Object unwrapVar(Object object) {
        if (object instanceof ListVarContainer) {
           object =((ListVarContainer) object).getList();
        }
        return object;
    }

    @Override
    public Object removeVar(String name) throws WGException {
        return unwrapVar(TMLContext.getThreadMainContext().getEnvironment().getPageVars().remove(name.toLowerCase()));
    }

}
