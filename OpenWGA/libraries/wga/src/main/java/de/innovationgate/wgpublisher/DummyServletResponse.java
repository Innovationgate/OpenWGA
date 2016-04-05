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
package de.innovationgate.wgpublisher;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.Locale;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;


public class DummyServletResponse implements HttpServletResponse {
	
	private HttpServletResponse realResponse;

	public DummyServletResponse(HttpServletResponse response) {
	 	this.realResponse = response;
	}

	/**
	 * @see javax.servlet.ServletResponse#getOutputStream()
	 */
	public ServletOutputStream getOutputStream() throws IOException {
		return realResponse.getOutputStream();
	}

	/**
	 * @see javax.servlet.ServletResponse#getWriter()
	 */
	public PrintWriter getWriter() throws IOException {
		return new PrintWriter(new StringWriter());
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addCookie(Cookie)
	 */
	public void addCookie(Cookie arg0) {
		this.realResponse.addCookie(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addDateHeader(String, long)
	 */
	public void addDateHeader(String arg0, long arg1) {
		this.realResponse.addDateHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addHeader(String, String)
	 */
	public void addHeader(String arg0, String arg1) {
		this.realResponse.addHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addIntHeader(String, int)
	 */
	public void addIntHeader(String arg0, int arg1) {
		this.realResponse.addIntHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#containsHeader(String)
	 */
	public boolean containsHeader(String arg0) {
		return this.realResponse.containsHeader(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeRedirectUrl(String)
	 * @deprecated
	 */
	public String encodeRedirectUrl(String arg0) {
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeRedirectURL(String)
	 */
	public String encodeRedirectURL(String arg0) {
		return this.realResponse.encodeRedirectURL(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeUrl(String)
	 * @deprecated
	 */
	public String encodeUrl(String arg0) {
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeURL(String)
	 */
	public String encodeURL(String arg0) {
		return this.realResponse.encodeURL(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendError(int, String)
	 */
	public void sendError(int arg0, String arg1) throws IOException {
		this.realResponse.sendError(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendError(int)
	 */
	public void sendError(int arg0) throws IOException {
		this.realResponse.sendError(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendRedirect(String)
	 */
	public void sendRedirect(String arg0) throws IOException {
		this.realResponse.sendRedirect(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setDateHeader(String, long)
	 */
	public void setDateHeader(String arg0, long arg1) {
		this.realResponse.setDateHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setHeader(String, String)
	 */
	public void setHeader(String arg0, String arg1) {
		this.realResponse.setHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setIntHeader(String, int)
	 */
	public void setIntHeader(String arg0, int arg1) {
		this.realResponse.setIntHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setStatus(int, String)
	 * @deprecated
	 */
	public void setStatus(int arg0, String arg1) {
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setStatus(int)
	 */
	public void setStatus(int arg0) {
		this.realResponse.setStatus(arg0);
	}

	/**
	 * @see javax.servlet.ServletResponse#flushBuffer()
	 */
	public void flushBuffer() throws IOException {
		this.realResponse.flushBuffer();
	}

	/**
	 * @see javax.servlet.ServletResponse#getBufferSize()
	 */
	public int getBufferSize() {
		return this.realResponse.getBufferSize();
	}

	/**
	 * @see javax.servlet.ServletResponse#getCharacterEncoding()
	 */
	public String getCharacterEncoding() {
		return this.realResponse.getCharacterEncoding();
	}

	/**
	 * @see javax.servlet.ServletResponse#getLocale()
	 */
	public Locale getLocale() {
		return this.realResponse.getLocale();
	}

	/**
	 * @see javax.servlet.ServletResponse#isCommitted()
	 */
	public boolean isCommitted() {
		return this.realResponse.isCommitted();
	}

	/**
	 * @see javax.servlet.ServletResponse#reset()
	 */
	public void reset() {
		this.realResponse.reset();
	}

	/**
	 * @see javax.servlet.ServletResponse#setBufferSize(int)
	 */
	public void setBufferSize(int arg0) {
		this.realResponse.setBufferSize(arg0);
	}

	/**
	 * @see javax.servlet.ServletResponse#setContentLength(int)
	 */
	public void setContentLength(int arg0) {
		this.realResponse.setContentLength(arg0);
	}

	/**
	 * @see javax.servlet.ServletResponse#setContentType(String)
	 */
	public void setContentType(String arg0) {
		this.realResponse.setContentType(arg0);
	}

	/**
	 * @see javax.servlet.ServletResponse#setLocale(Locale)
	 */
	public void setLocale(Locale arg0) {
		this.realResponse.setLocale(arg0);
	}

	/* (Kein Javadoc)
	 * @see javax.servlet.ServletResponse#resetBuffer()
	 */
	public void resetBuffer() {
		realResponse.resetBuffer();
	}

    public String getContentType() {
        return this.realResponse.getContentType();
    }

    public void setCharacterEncoding(String arg0) {
        this.realResponse.setCharacterEncoding(arg0);        
    }

    @Override
    public String getHeader(String arg0) {
        return this.realResponse.getHeader(arg0);
    }

    @Override
    public Collection<String> getHeaderNames() {
        return this.realResponse.getHeaderNames();
    }

    @Override
    public Collection<String> getHeaders(String arg0) {
        return this.realResponse.getHeaders(arg0);
    }

    @Override
    public int getStatus() {
        return this.realResponse.getStatus();
    }
}
