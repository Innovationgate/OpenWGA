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
package de.innovationgate.igutils;

import org.apache.commons.httpclient.HttpClient;

import de.innovationgate.webgate.api.WGFactory;


public abstract class HttpConnector {

	private static final int DEFAULT_HTTP_CONNECTION_TIMEOUT = 5000;
	private static final int DEFAULT_HTTP_TIMEOUT = 5000;
	
	private int _httpConnectionTimeout = DEFAULT_HTTP_CONNECTION_TIMEOUT;
	private int _httpTimeout = DEFAULT_HTTP_TIMEOUT;
	
	private String _proxyHost;
	private int _proxyPort;
	private String _proxyUsername;
	private String _proxyPassword;


    /**
     * @deprecated <code>use WGFactory.getHttpClientFactory().createHttpClient();</code> instead
     */
	@Deprecated
	protected HttpClient createHttpClient() {
		HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();		
//		client.setConnectionTimeout(_httpConnectionTimeout);
//		client.setTimeout(_httpTimeout);
//		if (_proxyHost != null) {
//			client.getHostConfiguration().setProxy(_proxyHost, _proxyPort);
//		}
//		if (_proxyUsername != null) {
//			Credentials credentials = new UsernamePasswordCredentials(_proxyUsername, _proxyPassword);
//			client.getState().setProxyCredentials(null, _proxyHost, credentials);
//		}
		return client;
	}	
	
	@Deprecated
	public void setProxy(String host, int port) {
		setProxy(host, port, null, null);
	}
	
	@Deprecated 
	public void setProxy(String host, int port, String proxyUsername, String proxyPassword) {
		_proxyHost = host;
		_proxyPort = port;
		_proxyUsername = proxyUsername;
		_proxyPassword = proxyPassword;
	}
	
	
	/**
	 * returns the http connect timeout
	 * @return timeout in ms
	 * @deprecated
	 */
	@Deprecated
	public int getHttpConnectionTimeout() {
		return _httpConnectionTimeout;
	}
	
	/**
	 * sets the http connect timeout
	 * @param httpConnectionTimeout timeout in ms
	 * @deprecated (noop)
	 */
	@Deprecated
	public void setHttpConnectionTimeout(int httpConnectionTimeout) {
		_httpConnectionTimeout = httpConnectionTimeout;
	}
	
	/**
	 * returns the http read timeout
	 * @return timeout in ms
	 * @deprecated
	 */
	@Deprecated
	public int getHttpTimeout() {
		return _httpTimeout;
	}
	
	/**
	 * sets the http read timeout
	 * @param httpTimeout timeout in ms
	 * @deprecated (noop)
	 */
	@Deprecated
	public void setHttpTimeout(int httpTimeout) {
		_httpTimeout = httpTimeout;
	}
	
}
