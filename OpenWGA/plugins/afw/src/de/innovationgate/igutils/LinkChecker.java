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

import java.io.IOException;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.URI;
import org.apache.commons.httpclient.methods.GetMethod;

import de.innovationgate.webgate.api.WGFactory;

public class LinkChecker extends HttpConnector {
	
	
	private static final int DEFAULT_MAX_REDIRECTS = 10;

	private int _maxRedirects = DEFAULT_MAX_REDIRECTS;

	
	/**
	 * checks if the given URL is reachable and returns the response code
	 * redirects of type 301, 302, 303 and 307 are followed <code>getMaxRedirects()</code> times automatically 
	 * @param uri the uri to check
	 * @return HTTP reponse code
	 * @throws IOException 
	 * @throws HttpException 
	 * @throws IllegalStateException if max redirects has been reached
	 */
	public int check(String uri) throws HttpException, IOException {
		HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
		return innerCheck(client, new URI(uri, true), 0);
	}
	
	private int innerCheck(HttpClient client, URI uri, int redirectCounter) throws HttpException, IOException {
		if (redirectCounter >= DEFAULT_MAX_REDIRECTS) {
			throw new IllegalStateException("Max redirects '" + DEFAULT_MAX_REDIRECTS + "' reached. Might be an endless redirect.");
		}
		GetMethod targetGET = new GetMethod();
		targetGET.setURI(uri);
		targetGET.setFollowRedirects(false);
		int result = client.executeMethod(targetGET);
		if (result == 301 || result == 302 || result == 303 || result == 307) {
			// follow redirect
			Header locationHeader = targetGET.getResponseHeader("location");
		     if (locationHeader != null) {
		    	 String redirectLocation = locationHeader.getValue();
		    	 targetGET.releaseConnection();
		    	 return innerCheck(client, new URI(uri, redirectLocation, true), redirectCounter+1);
		     } else {
		    	 targetGET.releaseConnection();
		    	 return 404;
		     }
		} else {
			targetGET.releaseConnection();
			return result;
		}
	}
		

	
	/**
	 * returns max redirects for uri checks
	 * @return
	 */
	public int getMaxRedirects() {
		return _maxRedirects;
	}
	
	/**
	 * sets max redirects for uri checks
	 * @param maxRedirects follow redirects n times
	 */
	public void setMaxRedirects(int maxRedirects) {
		_maxRedirects = maxRedirects;
	}
	
	
}
