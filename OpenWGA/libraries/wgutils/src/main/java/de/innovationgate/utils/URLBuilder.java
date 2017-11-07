/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.httpclient.URI;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;

import de.innovationgate.wga.common.CodeCompletion;

/**
 * Class for building and modifying URLs including their parameters.
 * The URLBuilder can take an existing URL or URL parts. It offers methods to modify
 * every part of the URL separately and can then rebuild a complete URL from it.
 * The path and query part of the URL are decoded an read if an encoding is given to the {@link URLBuilder} constructor.
 * All setter methods are prepared to use "method chaining", as they all return the current URLBuilder instance.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class URLBuilder implements Cloneable {
    
    
    private static Map<String,Integer> _defaultProtocolPorts = new HashMap<String, Integer>();
    static {
        _defaultProtocolPorts.put("http", 80);
        _defaultProtocolPorts.put("https", 443);
        _defaultProtocolPorts.put("ws", 80);
        _defaultProtocolPorts.put("wss", 443);
    }
    
    
    /**
     * Tests if the given port is the default port for the given protocol.
     * This is true if browsers implicitly assume the given port for an URL with the given protocol and do not need it explicitly in the URL.
     * @param port Tested port
     * @param protocol Tested protocol
     * @return true if it is the default port
     */
    public static boolean isDefaultPortForProtocol(int port, String protocol) {
        
        Integer defaultPort = _defaultProtocolPorts.get(protocol);
        if (defaultPort != null && defaultPort.intValue() == port) {
            return true;
        }
        
        return false;
        
    }
    
    /**
     * Returns the default port for the given URL protocol
     * @param protocol The URL protocol
     * @return The default port
     */
    public static Integer getDefaultPortForProtocol(String protocol) {
        return _defaultProtocolPorts.get(protocol.toLowerCase());
    }
	

	protected String _protocol = "";
	protected int _port = 80;
	protected String _host = "";
	protected String _path = "";
	protected String _query = "";
	protected HashMap<String,Object> _parameters = new HashMap<String,Object>();
    protected String _encoding = "UTF-8";
    protected String _pathUndecoded = null;
    protected String _fragment = "";
    
	/**
     * Creates a URLBuilder that parses an existing URL
	 * @param url The URL to parse
	 */
	public URLBuilder(URL url) {
	    this(url.getProtocol(), url.getPort(), url.getHost(), url.getPath(), url.getQuery(), url.getRef(), "UTF-8");
	}
	
	 /**
     * Creates a URLBuilder that parses an existing URL
     * @param url The URL to parse
     * @param encoding The encoding of URL parameters. The URLBuilder will decode them.
     */
    public URLBuilder(URL url, String encoding) {
        this(url.getProtocol(), url.getPort(), url.getHost(), url.getPath(), url.getQuery(), url.getRef(), encoding);
    }
	

    /**
     * Creates a URLBuilder that parses an existing URI
     * @param uri The URI to parse
     */
	public URLBuilder(URI uri) throws URIException {
	    this(uri.getScheme(), uri.getPort(), uri.getHost(), uri.getPath(), uri.getQuery(), uri.getFragment(), "UTF-8");
	}
	
    /**
     * Creates a URLBuilder that parses an existing URI
     * @param uri The URI to parse
     * @param encoding The encoding of URL parameters. The URLBuilder will decode them.
     */
    public URLBuilder(URI uri, String encoding) throws URIException {
        this(uri.getScheme(), uri.getPort(), uri.getHost(), uri.getPath(), uri.getQuery(), uri.getFragment(), encoding);
    }
    
    public URLBuilder clearPathParameters() {
        int ppPos = _path.indexOf(";");
        if (ppPos != -1) {
            _path = _path.substring(0, ppPos);
        }
        return this;
    }
    
    /**
     * Creates a URL Builder that takes various URL parts as parameters
     * @param protocol The protocol URL part
     * @param port The port URL part
     * @param host The host URL part
     * @param path The path URL part. May be left null.
     * @param query The query string URL part. May be left null.
     * @param decode The encoding by which the URL parts will be decoded. Specify null for no decoding.
     */
    public URLBuilder(String protocol, int port, String host, String path, String query, String decode) {
        this(protocol, port, host, path, query, null, decode);
    }
    
    /**
     * Creates a URL Builder that takes various URL parts as parameters
     * @param protocol The protocol URL part
     * @param port The port URL part
     * @param host The host URL part
     * @param path The path URL part. May be left null.
     * @param query The query string URL part. May be left null.
     * @param fragment The fragment part. May be left null.
     * @param decode The encoding by which the URL parts will be decoded. Specify null for no decoding.
     */
    public URLBuilder(String protocol, int port, String host, String path, String query, String fragment, String decode) {
        
        try {
            _protocol = protocol;
            _port = port;
            _host = host;
            
            if (decode != null) {
                _encoding = decode;
            }
            
            if (path != null) {
                _pathUndecoded  = path;
                _path = decode != null ?  WGUtils.decodeURI(path, decode) : path;
            }
             
            if (query != null) {
                _query = decode != null ? WGUtils.decodeURI(query, decode) : query;
                addQueryString(getQuery(), decode);
            }
            
            if (fragment != null) {
                _fragment = decode != null ? WGUtils.decodeURI(fragment, decode) : fragment;
            }

        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        
    }
    
    /**
     * Builds a URL builder for the given URL parts
     * @param protocol 
     * @param port
     * @param host
     * @param path
     * @param query
     */
    public URLBuilder(String protocol, int port, String host, String path, String query) {
        this(protocol, port, host, path, query, "UTF-8");
    }
    
    /**
     * Builds a URL builder taking the request URL of the given request
     * @param request The request
     * @param parameterEncoding The encoding to use in decoding the URL
     * @throws MalformedURLException
     */
    public URLBuilder(HttpServletRequest request, String parameterEncoding) throws MalformedURLException {
        this(new URL(request.getRequestURL().toString()), parameterEncoding);
        if (request.getQueryString() != null) {
            addQueryString(request.getQueryString(), parameterEncoding);
        }
    }
    
    /**
     * Builds a URL builder taking the request URL of the given request, decoding it using UTF-8
     * @param request The request
     * @throws MalformedURLException
     */
    public URLBuilder(HttpServletRequest request) throws MalformedURLException {
        this(request, "UTF-8");
    }

    /**
     * Adds a complete query string to the parameters of this URLBuilder
     * @param queryString The query string
     * @param decode The encoding by which parameters should get decoded. Use null for no decoding.
     */
    public URLBuilder addQueryString(String queryString, String decode) {
        StringTokenizer tokens = new StringTokenizer(queryString);
        String token;
        HashMap<String,Object> params = new HashMap<String,Object>(); 
        while (tokens.hasMoreTokens()) {
            token = tokens.nextToken("&");

            int equalPos = token.indexOf("=");
            if (equalPos == -1) {
                params.put(token, null);
                continue;
            }
            
            String paramName = token.substring(0, equalPos);
            String paramValue = token.substring(equalPos + 1);
            
            if (decode != null) {
                try {
                    paramName = decode != null ? WGUtils.decodeURI(paramName, decode) : paramName;
                    paramValue = decode != null ? WGUtils.decodeURI(paramValue, decode) : paramValue;
                }
                catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
            
            Object currentValue = params.get(paramName);
            if(currentValue!=null){
            	List<Object> values = new ArrayList<Object>();
            	if(currentValue instanceof List){
            		values.addAll((List)currentValue);
            	}
            	else{
            		values.add(currentValue);
            	}
        		values.add(paramValue);
        		params.put(paramName, values);
            }
            else params.put(paramName, paramValue);
            
        }
        
        _parameters.putAll(params);
        
        return this;
    }

	/**
	 * Returns the query part
	 */
	public String getQuery() {
		return _query;
	}
	/**
	 * Sets the query part. You should not use this since the query is rebuilt from its parsed parameters.
     * Manipulating the query here will not be reflected when rebuilding the URL.
     * @deprecated Modify the query via methods {@link #setParameter(String, String)} and {@link #removeParameter(String)}
	 * @param query The query to set
	 */
	@CodeCompletion
	public URLBuilder setQuery(String query) {
		try {
            this._query = URIUtil.encodeQuery(query);
        }
        catch (URIException e) {
            throw new RuntimeException(e);
        }
		return this;
	}

	/**
	 * Returns the path part
	 */
	public String getPath() {
		return _path;
	}
	/**
	 * Sets the path part
	 * @param file The file to set
	 * @throws URIException 
	 */
	public URLBuilder setPath(String file) {
        _path = file;
	    return this;
	}
	
	public URLBuilder setPath(String path, boolean decode) throws UnsupportedEncodingException, MalformedURLException {
		_path = (decode && _encoding!=null) ?  WGUtils.decodeURI(path, _encoding) : path;
		return this;
	}
	
	/**
	 * Appends a new path element to the current path
	 * This method automatically ensures that a single slash is between the current path and the new path element.  
	 * @param pathElement
	 */
	public URLBuilder appendPath(String pathElement) {

	    StringBuilder str = new StringBuilder();
	    str.append(_path);
	    if (!_path.endsWith("/") && !pathElement.startsWith("/")) {
	        str.append("/");
	    }
	    else if (_path.endsWith("/") && pathElement.startsWith("/")) {
	        pathElement = pathElement.substring(1);
	    }
	    
	    str.append(pathElement);
	    _path = str.toString();
	    return this;
	    
	}

	/**
	 * Returns the host part
	 */
	public String getHost() {
		return _host;
	}
	/**
	 * Sets the host
	 * @param host The host to set
	 */
	public URLBuilder setHost(String host) {
		this._host = host;
		return this;
	}

	/**
	 * Gets the protocol
	 */
	public String getProtocol() {
		return _protocol;
	}
	/**
	 * Sets the protocol
	 * @param protocol The protocol to set
	 */
	public URLBuilder setProtocol(String protocol) {
		this._protocol = protocol;
		return this;
	}
	
	/**
     * Rebuilds the URL to a string.
     * @param absolute Specify true for an absolute URL. false will rebuild it only from the path part on, omitting protocol, port and hos.
     * @return The rebuilt URL as string
     * @deprecated Because of inconsistent return type. Use {@link #build(boolean)} instead or use implicit string conversion
     */
	@CodeCompletion
	public String rebuild(boolean absolute) {
	    return build(absolute);
	}
	
	/**
     * Rebuilds the URL to a string.
	 * @param absolute Specify true for an absolute URL. false will rebuild it only from the path part on, omitting protocol, port and host.
	 * @return The rebuilt URL as string
	 */
	public String build(boolean absolute) {

		try {
            StringBuffer newURL = new StringBuffer();
            
            if (absolute) {
            	newURL.append(this._protocol).append("://").append(this._host);
            	if (this._port != -1 && !isDefaultPortForProtocol(this._port, this._protocol)) {
            		newURL.append(":").append(_port);
            	}
            }

            newURL.append(URIUtil.encodePath(_path, _encoding)).append(this.rebuildQuery());
            if (!WGUtils.isEmpty(_fragment)) {
                newURL.append("#").append(URIUtil.encodePath(_fragment, _encoding));
            }
            
            return newURL.toString();
        }
        catch (URIException e) {
            throw new RuntimeException(e);
        }
		

	}
	
	/**
     * Rebuilds the URL in relative form to a string.
     * @return The rebuilt URL as string
     */
	public String build() {
	    return build(false);
	}
	
	/**
     * Rebuilds the absolute URL to an URL object, 
	 * @throws MalformedURLException
	 * @deprecated Because of inconsistent return type. Use {@link #build(boolean)} instead or use implicit string conversion
	 */
	@CodeCompletion
	public URL rebuild() throws MalformedURLException {
        return new URL(rebuild(true));
	}
	
	private String rebuildQuery() {
		
	    Map<String,Object> rebuildParameters = getRebuildParameters();
		Iterator<String> paramKeys = rebuildParameters.keySet().iterator();
		String paramSeparator = "&";
		StringBuffer paramString = new StringBuffer();
		String key;
		Object value;	// may be a List
		while (paramKeys.hasNext()) {
			key = paramKeys.next().toString();
			value = rebuildParameters.get(key);
			
			List<Object> values = new ArrayList<Object>();
			if(value instanceof List)
				values.addAll((List)value);	// add all values
			else values.add(value);			// add single value

			try{
				if (_encoding != null)
					key = URIUtil.encodeWithinQuery(key, _encoding);				
	
				for(Object o: values){
					if (paramString.length() > 0) {
						paramString.append(paramSeparator);
					}
					paramString.append(key);
					if(o==null)
						continue;
					String v = o.toString();
					if (_encoding != null)
	                    v = URIUtil.encodeWithinQuery(v, _encoding);
					if (v != null && !v.trim().equals("")) {
						paramString.append("=");
						paramString.append(v);
					}
				}
			}
			catch(Exception e){
				throw new RuntimeException(e);
			}
			
		}
		
		if (paramString.length() > 0) {
			return "?" + paramString.toString();
		}
		else {
			return "";
		}
		
	}

    protected Map<String,Object> getRebuildParameters() {
        return this._parameters;
    }
	
	/**
     * Returns a URL parameter value. If the parameter is a list the first element is returned.
	 * @param param The parameter name
	 * @return The parameter value
	 */
	@SuppressWarnings("unchecked")
	public String getParameter(String param) {
		if(!_parameters.containsKey(param))
			return null;
		Object value = _parameters.get(param);
		if(value==null)
			return "";
		else if(value instanceof List)
			value = ((List<Object>)value).get(0);
		return value.toString();
	}

	/**
     * Returns a URL parameter value as a List (even if the parameter value is a single value)
     * If the parameter value does not exit null is returned.
     * If the parameter value is null an empty List is returned.
	 * @param param The parameter name
	 * @return The parameter value
	 */
	@SuppressWarnings("unchecked")
	public List<Object> getParameterValues(String param) {
		if(!_parameters.containsKey(param))
			return null;
		Object value = _parameters.get(param);
		if(value==null)
			return new ArrayList<Object>();
		else if(value instanceof List)
			return (List<Object>)value;
		else{
			List<Object> values = new ArrayList<Object>();
			values.add(value);
			return values;
		}
	}

	
    /**
     * Tests for existence of a URL parameter
     * @param param The parameter name
     * @return true, if the parameter is defined
     */
    public boolean hasParameter(String param) {
        return _parameters.containsKey(param);
    }
    
    /**
     * Removes a parameter
     * @param param The parameter name
     */
    public URLBuilder removeParameter(String param) {
        _parameters.remove(param);
        return this;
    }
	
	/**
     * Createds/modifies a parameter
	 * @param param The parameter name
	 * @param value The parameter value
	 */
	public URLBuilder setParameter(String param, Object paramValue) {
		_parameters.put(param, paramValue);
		return this;
	}
	public URLBuilder setParameter(String param) {
		setParameter(param, null);
		return this;
	}
	public URLBuilder setParameter(Map<String,Object> params) {
		_parameters.putAll(params);
		return this;
	}

    /**
     * Returns the encoding used to encode URL parameters. null (default) means no encoding.
     * @deprecated Use {@link #getEncoding()} instead. Encoding is now also used for other URL components.
     */
	@CodeCompletion
    public String getParameterEncoding() {
        return _encoding;
    }

    /**
     * Sets the encoding used to encode URL parameters. The default is null = no encoding
     * @param parameterEncoding The parameterEncoding to set.
     * @deprecated Use {@link #setEncoding(String)} instead. Encoding is now also used for other URL components.
     */
    @CodeCompletion
    public URLBuilder setParameterEncoding(String parameterEncoding) {
        this._encoding = parameterEncoding;
        return this;
    }
    
    /**
     * Clears all url parameters
     */
    public URLBuilder clearParameters() {
        _parameters.clear();
        return this;
    }

    @Override
    public String toString() {
        try {
            return build();
        }
        catch (Exception e) {
            e.printStackTrace();
            return "";
        }
        
    }

    /**
     * Returns the TCP port for this URL
     */
    public int getPort() {
        return _port;
    }

    /**
     * Sets the TCP port for this URL
     */
    public URLBuilder setPort(int port) {
        this._port = port;
        return this;
    }

    /**
     * Returns the names of URL parameters
     */
    public Set<String> getParameterNames() {
        return new HashSet<String>(this._parameters.keySet());
    }

    /**
     * Returns the encoding by which the URL will be encoded when building it using {@link #build()} and variants.
     * Is initialized with the encoding by which the given URL was decoded, if one was given to the URLBuilder constructor that was used. 
     */
    public String getEncoding() {
        return _encoding;
    }

    /**
     * Sets the encoding used to encode the URL on building it using {@link #build()} and variants. 
     * @param encoding The encoding to set.
     */
    public URLBuilder setEncoding(String encoding) {
        _encoding = encoding;
        return this;
    }
    
    @Override
    protected Object clone() throws CloneNotSupportedException {
        URLBuilder clone = (URLBuilder) super.clone();
        clone._parameters = new HashMap<String,Object>(_parameters);
        return clone;
    }

    public String getFragment() {
        return _fragment;
    }

    public URLBuilder setFragment(String fragment) {
        _fragment = fragment;
        return this;
    }

}

