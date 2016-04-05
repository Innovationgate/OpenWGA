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

package de.innovationgate.wgpublisher.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Map;

import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.Part;

/**
 * Request facade hiding the service base path from path info and offering it as servlet path.
 */
public class PathHidingRequestFacade implements HttpServletRequest {
    
    private HttpServletRequest _req;
    private String _servletPath;

    public PathHidingRequestFacade(HttpServletRequest req, String servicePath) {
        _req = req;
        _servletPath = servicePath;
    }

    public Object getAttribute(String arg0) {
        return _req.getAttribute(arg0);
    }

    public Enumeration getAttributeNames() {
        return _req.getAttributeNames();
    }

    public String getAuthType() {
        return _req.getAuthType();
    }

    public String getCharacterEncoding() {
        return _req.getCharacterEncoding();
    }

    public int getContentLength() {
        return _req.getContentLength();
    }

    public String getContentType() {
        return _req.getContentType();
    }

    public String getContextPath() {
        return _req.getContextPath();
    }

    public Cookie[] getCookies() {
        return _req.getCookies();
    }

    public long getDateHeader(String arg0) {
        return _req.getDateHeader(arg0);
    }

    public String getHeader(String arg0) {
        return _req.getHeader(arg0);
    }

    public Enumeration getHeaderNames() {
        return _req.getHeaderNames();
    }

    public Enumeration getHeaders(String arg0) {
        return _req.getHeaders(arg0);
    }

    public ServletInputStream getInputStream() throws IOException {
        return _req.getInputStream();
    }

    public int getIntHeader(String arg0) {
        return _req.getIntHeader(arg0);
    }

    public String getLocalAddr() {
        return _req.getLocalAddr();
    }

    public Locale getLocale() {
        return _req.getLocale();
    }

    public Enumeration getLocales() {
        return _req.getLocales();
    }

    public String getLocalName() {
        return _req.getLocalName();
    }

    public int getLocalPort() {
        return _req.getLocalPort();
    }

    public String getMethod() {
        return _req.getMethod();
    }

    public String getParameter(String arg0) {
        return _req.getParameter(arg0);
    }

    public Map getParameterMap() {
        return _req.getParameterMap();
    }

    public Enumeration getParameterNames() {
        return _req.getParameterNames();
    }

    public String[] getParameterValues(String arg0) {
        return _req.getParameterValues(arg0);
    }

    public String getPathInfo() {
        if (_req.getServletPath() != null && _req.getServletPath().startsWith(_servletPath)) {
            return _req.getServletPath().substring(_servletPath.length());
        }
        else {
            return null;
        }
    }

    public String getPathTranslated() {
        return _req.getPathTranslated();
    }

    public String getProtocol() {
        return _req.getProtocol();
    }

    public String getQueryString() {
        return _req.getQueryString();
    }

    public BufferedReader getReader() throws IOException {
        return _req.getReader();
    }

    public String getRealPath(String arg0) {
        return _req.getRealPath(arg0);
    }

    public String getRemoteAddr() {
        return _req.getRemoteAddr();
    }

    public String getRemoteHost() {
        return _req.getRemoteHost();
    }

    public int getRemotePort() {
        return _req.getRemotePort();
    }

    public String getRemoteUser() {
        return _req.getRemoteUser();
    }

    public RequestDispatcher getRequestDispatcher(String arg0) {
        return _req.getRequestDispatcher(arg0);
    }

    public String getRequestedSessionId() {
        return _req.getRequestedSessionId();
    }

    public String getRequestURI() {
        return _req.getRequestURI();
    }

    public StringBuffer getRequestURL() {
        return _req.getRequestURL();
    }

    public String getScheme() {
        return _req.getScheme();
    }

    public String getServerName() {
        return _req.getServerName();
    }

    public int getServerPort() {
        return _req.getServerPort();
    }

    public String getServletPath() {
        return _servletPath;
    }

    public HttpSession getSession() {
        return _req.getSession();
    }

    public HttpSession getSession(boolean arg0) {
        return _req.getSession(arg0);
    }

    public Principal getUserPrincipal() {
        return _req.getUserPrincipal();
    }

    public boolean isRequestedSessionIdFromCookie() {
        return _req.isRequestedSessionIdFromCookie();
    }

    public boolean isRequestedSessionIdFromUrl() {
        return _req.isRequestedSessionIdFromUrl();
    }

    public boolean isRequestedSessionIdFromURL() {
        return _req.isRequestedSessionIdFromURL();
    }

    public boolean isRequestedSessionIdValid() {
        return _req.isRequestedSessionIdValid();
    }

    public boolean isSecure() {
        return _req.isSecure();
    }

    public boolean isUserInRole(String arg0) {
        return _req.isUserInRole(arg0);
    }

    public void removeAttribute(String arg0) {
        _req.removeAttribute(arg0);
    }

    public void setAttribute(String arg0, Object arg1) {
        _req.setAttribute(arg0, arg1);
    }

    public void setCharacterEncoding(String arg0) throws UnsupportedEncodingException {
        _req.setCharacterEncoding(arg0);
    }

    @Override
    public AsyncContext getAsyncContext() {
        return _req.getAsyncContext();
    }

    @Override
    public DispatcherType getDispatcherType() {
        return _req.getDispatcherType();
    }

    @Override
    public ServletContext getServletContext() {
        return _req.getServletContext();
    }

    @Override
    public boolean isAsyncStarted() {
        return _req.isAsyncStarted();
    }

    @Override
    public boolean isAsyncSupported() {
        return _req.isAsyncSupported();
    }

    @Override
    public AsyncContext startAsync() {
        return _req.startAsync();
    }

    @Override
    public AsyncContext startAsync(ServletRequest arg0, ServletResponse arg1) {
        return _req.startAsync(arg0, arg1);
    }

    @Override
    public boolean authenticate(HttpServletResponse arg0) throws IOException, ServletException {
        return _req.authenticate(arg0);
    }

    @Override
    public Part getPart(String arg0) throws IOException, IllegalStateException, ServletException {
        return _req.getPart(arg0);
    }

    @Override
    public Collection<Part> getParts() throws IOException, IllegalStateException, ServletException {
        return _req.getParts();
    }

    @Override
    public void login(String arg0, String arg1) throws ServletException {
        _req.login(arg0, arg1);
    }

    @Override
    public void logout() throws ServletException {
        _req.logout();
    }

}
