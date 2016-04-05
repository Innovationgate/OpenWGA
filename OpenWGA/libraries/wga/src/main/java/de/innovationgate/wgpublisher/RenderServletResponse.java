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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.Locale;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

public class RenderServletResponse implements HttpServletResponse {
	
	private HttpServletResponse _response;
    
    private boolean _getOutputStreamCalled = false;
    private boolean _getWriterCalled = false;
    
    ByteArrayOutputStream _byteStream = new ByteArrayOutputStream();
    
    StringWriter _stringWriter = new StringWriter();

    private boolean _flushed;
     
    private ServletOutputStream _outputStream = new ServletOutputStream() {
               
        public void write(int arg0) throws IOException {
            _byteStream.write(arg0);            
        }
        
    };

    private String _contentType = "text/html";

	public RenderServletResponse(HttpServletResponse response) {
	 	this._response = response;
	}
    
    public byte[] getBinaryData() {
        return _byteStream.toByteArray();
    }
    
    public String getStringData() {
        return _stringWriter.toString();
    }

    public boolean isBinary() {
        if (_getOutputStreamCalled) {
            return true;
        } else if (_getWriterCalled) {
            return false;
        } else {
            // nothing written yet
            throw new IllegalStateException("Unable to determine if response is binary - nothing is written yet.");
        }
    }
    
	/**
	 * @see javax.servlet.ServletResponse#getOutputStream()
	 */
	public ServletOutputStream getOutputStream() throws IOException {
        if (_getWriterCalled) {
            throw new IllegalStateException("getWriter() was already called on this response.");
        }
        _getOutputStreamCalled = true;
		return _outputStream;
	}

	/**
	 * @see javax.servlet.ServletResponse#getWriter()
	 */
	public PrintWriter getWriter() throws IOException {
        if (_getOutputStreamCalled) {
            throw new IllegalStateException("getOutputStream() was already called on this response.");
        }
        _getWriterCalled = true;
		return new PrintWriter(_stringWriter);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addCookie(Cookie)
	 */
	public void addCookie(Cookie arg0) {
		// ignore
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addDateHeader(String, long)
	 */
	public void addDateHeader(String arg0, long arg1) {
		// ignore
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addHeader(String, String)
	 */
	public void addHeader(String arg0, String arg1) {
		this._response.addHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addIntHeader(String, int)
	 */
	public void addIntHeader(String arg0, int arg1) {
		this._response.addIntHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#containsHeader(String)
	 */
	public boolean containsHeader(String arg0) {
		return this._response.containsHeader(arg0);
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
		throw new RuntimeException("unsupported");
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
		return this._response.encodeURL(arg0);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendError(int, String)
	 */
	public void sendError(int arg0, String arg1) throws IOException {
        throw new RuntimeException("unsupported");
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendError(int)
	 */
	public void sendError(int arg0) throws IOException {
        throw new RuntimeException("unsupported");
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendRedirect(String)
	 */
	public void sendRedirect(String arg0) throws IOException {
		throw new RuntimeException("unsupported");
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setDateHeader(String, long)
	 */
	public void setDateHeader(String arg0, long arg1) {
		this._response.setDateHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setHeader(String, String)
	 */
	public void setHeader(String arg0, String arg1) {
		this._response.setHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setIntHeader(String, int)
	 */
	public void setIntHeader(String arg0, int arg1) {
		this._response.setIntHeader(arg0, arg1);
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setStatus(int, String)
	 * @deprecated
	 */
	public void setStatus(int arg0, String arg1) {
        //ignore
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setStatus(int)
	 */
	public void setStatus(int arg0) {
		//ignore
	}

	/**
	 * @see javax.servlet.ServletResponse#flushBuffer()
	 */
	public void flushBuffer() throws IOException {
        _flushed = true;
        if (_getOutputStreamCalled) {
            _outputStream.flush();
        }
        if (_getWriterCalled) {
            _stringWriter.flush();
        }
	}

	/**
	 * @see javax.servlet.ServletResponse#getBufferSize()
	 */
	public int getBufferSize() {
		return this._response.getBufferSize();
	}

	/**
	 * @see javax.servlet.ServletResponse#getCharacterEncoding()
	 */
	public String getCharacterEncoding() {
		return this._response.getCharacterEncoding();
	}

	/**
	 * @see javax.servlet.ServletResponse#getLocale()
	 */
	public Locale getLocale() {
		return this._response.getLocale();
	}

	/**
	 * @see javax.servlet.ServletResponse#isCommitted()
	 */
	public boolean isCommitted() {
        return _flushed;	
	}

	/**
	 * @see javax.servlet.ServletResponse#reset()
	 */
	public void reset() {
		throw new RuntimeException("unsupported");
	}

	/**
	 * @see javax.servlet.ServletResponse#setBufferSize(int)
	 */
	public void setBufferSize(int arg0) {
		// ignore - no buffer here
	}

	/**
	 * @see javax.servlet.ServletResponse#setContentLength(int)
	 */
	public void setContentLength(int arg0) {
		// ignore
	}

	/**
	 * @see javax.servlet.ServletResponse#setContentType(String)
	 */
	public void setContentType(String arg0) {
        _contentType = arg0;
	}

	/**
	 * @see javax.servlet.ServletResponse#setLocale(Locale)
	 */
	public void setLocale(Locale arg0) {
		// ignore
	}

	/**
	 * @see javax.servlet.ServletResponse#resetBuffer()
	 */
	public void resetBuffer() {
		// ignore
	}

    public String getContentType() {
        return _contentType;
    }

    public void setCharacterEncoding(String arg0) {
        // ignore        
    }

    public String getHeader(String arg0) {
        return _response.getHeader(arg0);
    }

    public Collection<String> getHeaderNames() {
        return _response.getHeaderNames();
    }

    public Collection<String> getHeaders(String arg0) {
        return _response.getHeaders(arg0);
    }

    public int getStatus() {
        return _response.getStatus();
    }
    
    
}
