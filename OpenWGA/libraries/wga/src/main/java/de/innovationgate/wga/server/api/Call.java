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

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.filter.WGAFilter;
import de.innovationgate.wgpublisher.filter.WGAVirtualHostingFilter;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLForm.MultipartFormData;
import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

/**
 * Information about the current HTTP call that initialized the current environment.
 * The object reads data of the request and is able to influence the response, if possible on the environment.  
 * If no call is available on the environment there is no data returned.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Call {
    
    private WGA _wga;
    
    protected Call(WGA wga) {
        _wga = wga;
    }

    /**
     * Returns the (first) value of a request parameter
     * This is either a URL parameter or the form value of a POSTed URL-encoded HTML form
     * @param name Name of the parameter
     * @return Parameter value, null if the param does not exist
     * @throws WGException
     */
    public String getParam(String name) throws WGException {
        try {
            return _wga.getRequest().getParameter(name);
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the names of all available call parameters
     * These are either URL parameters or the form fields of a POSTed URL-encoded HTML form
     * @return Names of parameters
     * @throws WGException
     */
    public List<String> getParamNames() throws WGException {
        try {
            @SuppressWarnings("unchecked")
            List<String> extractEntryList = WGUtils.extractEntryList(_wga.getRequest().getParameterNames());
            return extractEntryList;
        }
        catch (UnavailableResourceException e) {
            return Collections.emptyList();
        }
    }
    
    public TMLForm getPostedForm() throws WGException {
        return (TMLForm) getJavaRequest().getAttribute(WGACore.ATTRIB_POSTED_TMLFORM);
    }
    
    /**
     * Returns all values of a call parameter
     * This is either a URL parameter or the form value of a POSTed URL-encoded HTML form
     * @param name Name of the parameter
     * @return Parameter values, an empty list if the param does not exist
     * @throws WGException
     */
    public List<String> getParamValues(String name) throws WGException {
        try {
            String[] values = _wga.getRequest().getParameterValues(name);
            if (values != null) {
                return Arrays.asList(values);
            }
            else {
                return Collections.emptyList();
            }
        }
        catch (UnavailableResourceException e) {
            return Collections.emptyList();
        }
    }
    
    /**
     * Returns the content of a HTTP header from the request
     * @param name Name of the header
     * @return Header content, null if the header does not exist
     * @throws WGException
     */
    public String getRequestHeader(String name) throws WGException {
        try {
            return _wga.getRequest().getHeader(name);
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the names of all available HTTP headers
     * @throws WGException
     */
    public List<String> getRequestHeaderNames() throws WGException {
        try {
            @SuppressWarnings("unchecked")
            List<String> extractEntryList = WGUtils.extractEntryList(_wga.getRequest().getHeaderNames());
            return extractEntryList;
        }
        catch (UnavailableResourceException e) {
            return Collections.emptyList();
        }
    }
    
    /**
     * Returns a HTTP cookie
     * @param name Name of the cookie
     * @return Cookie object, null if the cookie does not exist
     * @throws WGException
     */
    public Cookie getCookie(String name) throws WGException {
        try {
            return fetchCookies().get(name);
        }
        catch (UnavailableResourceException e) {
            return null;
        }        
    }
    
    /**
     * Returns the names of all available Cookies
     */
    public List<String> getCookieNames() throws WGException {
        try {
            return new ArrayList<String>(fetchCookies().keySet());
        }
        catch (UnavailableResourceException e) {
            return Collections.emptyList();
        }
    }
    
    @SuppressWarnings("unchecked")
    private Map<String,Cookie> fetchCookies() throws WGException {
        return (Map<String,Cookie>) _wga.getRequest().getAttribute(WGACore.ATTRIB_COOKIES);
    }

    /**
     * Returns the complete URL by which this request was called
     * @throws WGException
     */
    public String getURL() throws WGException {
        try {
            String url =  WGPDispatcher.getCompleteRequestURL(_wga.getRequest());
            return _wga.urlBuilder(url).clearPathParameters().build(true);
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the complete original URL by which this request was called
     * Some request filters may modify the URL that was originally sent to OpenWGA before it actually gets parsed.
     * This version is guaranteed to be the URL form before any modification.
     * @throws WGException
     */
    public String getOriginalURL() throws WGException {
        try {
            String originalURL = (String) _wga.getRequest().getAttribute(WGAFilter.REQATTRIB_ORIGINAL_URL);
            String originalQS  = (String) _wga.getRequest().getAttribute(WGAFilter.REQATTRIB_ORIGINAL_QUERYSTRING);
            if (originalURL != null) {
                return WGPDispatcher.getCompleteRequestURL(new StringBuffer(originalURL), originalQS);
            }
            else {
                return null;
            }
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the WebTML media key of the outer layout of this request
     * @throws WGException
     */
    public String getMediaKey() throws WGException {
        try {
            return ((WGPRequestPath) _wga.getRequest().getAttribute(WGACore.ATTRIB_REQUESTPATH)).getMediaKey();
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns then resource name of the outer layout of this request 
     * @throws WGException
     */
    public String getOuterLayout() throws WGException {
        try {
            return (String) _wga.getRequest().getAttribute(WGACore.ATTRIB_OUTER_DESIGN);
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the main WebTML context of this request
     * @throws WGException
     */
    public Context getMainContext() throws WGException {
        try {
            WGContent content = (WGContent) _wga.getRequest().getAttribute(WGACore.ATTRIB_MAINCONTEXT);
            if (content != null) {
                return _wga.createTMLContext(content);
            }
            else {
                return null;
            }
        }
        catch (UnavailableResourceException e) {
            return null;
        }        
    }
    
    /**
     * Returns if an actual call is available
     * If this is false the Call object will not give out any data.
     */
    public boolean isAvailable() {
        return _wga.isRequestAvailable();
    }
    
    /**
     * Returns if the call is an AJAX request
     * @throws WGException
     */
    public boolean isAjax() throws WGException {
        try {
            return (_wga.getRequest().getAttribute(WGACore.ATTRIB_AJAXINFO) != null);
        }
        catch (UnavailableResourceException e) {
            return false;
        }
    }
    
    /**
     * Returns the user agent of the calling client
     * @throws WGException
     */
    public String getUserAgent() throws WGException {
        return getRequestHeader("User-Agent");
    }
    

    /**
     * Returns the referrer URL of this request
     * @throws WGException
     */
    public String getReferrer() throws WGException {
        String url = getRequestHeader("Referer");
        return _wga.urlBuilder(url).clearPathParameters().build(true);
        
    }
    
    /**
     * Returns the query string of this requests URL
     * @throws WGException
     */
    public String getQueryString() throws WGException {
        try {
            return _wga.getRequest().getQueryString();
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the name of the OpenWGA virtual host that was used to handle this request.
     * This is the main host name stored on the virtual host configuration. 
     * @throws WGException
     */
    public String getVirtualHostName() throws WGException {
        try {
            VirtualHost host = (VirtualHost) _wga.getRequest().getAttribute(WGAVirtualHostingFilter.REQUESTATTRIB_VIRTUAL_HOST);
            if (host != null) {
                return host.getServername();
            }
            else {
                return null;
            }
        }
        catch (UnavailableResourceException e) {
            return null;
        }        
    }
    
    /**
     * Returns the TCP port on which OpenWGA received this request
     * @throws WGException
     */
    public int getPort() throws WGException {
        try {
            return _wga.getRequest().getServerPort();
        }
        catch (UnavailableResourceException e) {
            return -1;
        }        
    }
    
    /**
     * Returns the IP adress of the request client
     * @throws WGException
     */
    public String getClient() throws WGException {
        try {
            return _wga.getRequest().getRemoteAddr();
        }
        catch (UnavailableResourceException e) {
            return null;
        }          
    }
    
    /**
     * Returns the names of multipart/form-data fields posted on this request
     * This method can also read fields posted on WebTML forms.
     * @throws WGException
     */
    public List<String> getFormFieldNames() throws WGException {
        try {
            return new ArrayList<String>(fetchFormData(_wga.getRequest()).keySet());
        }
        catch (UnavailableResourceException e) {
            return Collections.emptyList();
        }
    }
    
    /**
     * Returns the value of a multipart/form-data fields posted on this request
     * This method can also read fields posted on WebTML forms.
     * However the WebTML input data type is not enforced on the returned value.
     * This method uses the servers default encoding to decode the field value.
     * @param name Name of the field
     * @return The field value
     * @throws WGException
     */
    public String getFormField(String name) throws WGException {
        return getFormField(name, _wga.server().getDefaultHttpEncoding());
    }
    
    /**
     * Returns the value of a multipart/form-data fields posted on this request
     * This method can also read fields posted on WebTML forms.
     * However the WebTML input data type is not enforced on the returned value.
     * @param name Name of the field
     * @param encoding The encoding used to decode the field value
     * @return The field value
     * @throws WGException
     */
    public String getFormField(String name, String encoding) throws WGException {
        try {
            FileItem fi = fetchFormData(_wga.getRequest()).get(name);
            if (fi != null) {
              return fi.getString(encoding);
            }
            return null;
        }
        catch (UnavailableResourceException e) {
            return null;
        }
        catch (UnsupportedEncodingException e) {
            throw new WGAServerException("Unsupported encoding: " + encoding);
        }
    }

    private Map<String, FileItem> fetchFormData(HttpServletRequest request) {

        Map<String,FileItem> data = new HashMap<String, FileItem>();
        TMLForm.MultipartFormData rawData = (MultipartFormData) request.getAttribute(WGACore.ATTRIB_FORMDATA);
        for (FileItem fi : rawData.getFileItems()) {
            if (fi.isFormField()) {
                data.put(fi.getFieldName(), fi);
            }
        }
        
        return data;
        
    }
    
    /**
     * Returns the JavaEE request object
     * Throws {@link UnavailableResourceException} if no request is available.
     * @throws WGException
     */
    public HttpServletRequest getJavaRequest() throws WGException {
        return _wga.getRequest();
    }
    
    /**
     * Returns the JavaEE response object
     * Throws {@link UnavailableResourceException} if no response is available.
     * @throws WGException
     */
    public HttpServletResponse getJavaResponse() throws WGException {
        return _wga.getResponse();
    }

    /**
     * Returns the HTTP method of the request
     * @throws WGException
     */
    public String getRequestMethod() throws WGException {
        try {
            return _wga.getRequest().getMethod();
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Creates a new completely initialized HTTP cookie, which is not yet assigned to the call.
     * Use {@link #addCookie(Cookie)} to do so and send it to the client.
     * The cookie is initialized with path (the OpenWGA context path), type/maxage (transient),
     * domain (either request host or host from configured server base url) and security
     * flag (true if the current call is HTTPS).
     * @param name Name of the cookie
     * @param value Value of the cookie
     * @return
     * @throws WGException
     */
    public Cookie createCookie(String name, String value) throws WGException {

        URLBuilder baseURL = _wga.urlBuilder(_wga.server().getBaseURL());
        URLBuilder requestURL = _wga.urlBuilder(getURL());
        
        Cookie c = new Cookie();
        c.setName(name);
        c.setValue(value);
        c.setMaxAge(-1);
        c.setPath(baseURL.build(false));
        if (_wga.isRequestAvailable()) {
            c.setDomain(requestURL.getHost());
        }
        else {
            c.setDomain(baseURL.getHost());
        }
        c.setSecure(requestURL.getProtocol().equals("https"));
        
        return c;
        
    }
    
    /**
     * Adds a cookie to the call, so it will be send to the client
     * @param c
     * @throws WGException
     */
    public void addCookie(Cookie c) throws WGException {
        testResponseHeaderWritable();
        getJavaResponse().addCookie(c.toJavaCookie());
        fetchCookies().put(c.getName(), c);
    }
    
    /**
     * Adds a new cookie to the call, so it will be send to the client.
     * This is a shortcut for calling {@link #createCookie(String, String)}
     * followed by {@link #addCookie(Cookie)}. The cookie is initialized
     * according to what is documented on {@link #createCookie(String, String)}.
     * @param name Name of the cookie
     * @param value Value of the cookie
     * @throws WGException
     */
    public void addCookie(String name, String value) throws WGException {
        testResponseHeaderWritable();
        Cookie c = createCookie(name, value);
        addCookie(c);
    }
    
    private void testResponseHeaderWritable() throws WGException {
        if (getJavaResponse().isCommitted()) {
            throw new WGAServerException("The current environment cannot influence HTTP response headers");
        }
//        The environment can currently not determine for sure if this is possible, as the JSP env just prevents this possibility
//        in a transparent way.        
//        if (_wga.isTMLContextAvailable() && _wga.tmlcontext().iswebenvironment()) {
//            throw new WGAServerException("The current environment cannot influence HTTP response headers");
//        }
    }

    /**
     * Removes a cookie from the call, so it will be deleted on the client.
     * @param c The cookie to delete.
     * @throws WGException
     */
    public void removeCookie(Cookie c) throws WGException {
        testResponseHeaderWritable();
        if (!c.isFromClient()) {
            throw new WGAServerException("The cookie does not originate from the client and cannot be removed");
        }
        c.setMaxAge(0);
        getJavaResponse().addCookie(c.toJavaCookie());
        fetchCookies().remove(c.getName());
    }
    
    /**
     * Returns the output to which to write response data, as binary stream
     * @throws WGException
     */
    public OutputStream getOutput() throws WGException, IOException {
        try {
            return getJavaResponse().getOutputStream();
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Returns the output to which to write response data, as character writer
     * @throws WGException
     */
    public Writer getWriter() throws WGException, IOException {
        try {
            return getJavaResponse().getWriter();
        }
        catch (UnavailableResourceException e) {
            return null;
        }
    }
    
    /**
     * Sets a header on the HTTP response
     * @param name Name of the header
     * @param value Value of the header
     * @throws WGException
     */
    public void setResponseHeader(String name, String value) throws WGException {
        testResponseHeaderWritable();
        getJavaResponse().setHeader(name, value);
    }
    
    /**
     * Cancels the current call, as far as possible.
     * @param code The HTTP error code to send.
     * @param message The message for the HTTP error.
     * @throws WGException
     * @throws IOException 
     * @throws HttpErrorException 
     */
    public void cancel(int code, String message) throws WGException, HttpErrorException {
        throw new HttpErrorException(code, message, getMainContext().db().getDbReference());
    }
     
    

}
