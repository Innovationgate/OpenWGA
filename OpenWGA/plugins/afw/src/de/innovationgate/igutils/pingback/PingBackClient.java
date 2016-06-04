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
package de.innovationgate.igutils.pingback;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HeaderElement;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.xmlrpc.XmlRpcException;
import org.apache.xmlrpc.client.XmlRpcClient;
import org.apache.xmlrpc.client.XmlRpcClientConfigImpl;
import org.cyberneko.html.parsers.DOMParser;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import de.innovationgate.igutils.HttpConnector;
import de.innovationgate.webgate.api.WGFactory;

public class PingBackClient extends HttpConnector {
	
	private static final int DEFAULT_DOCUMENT_SIZE_LIMIT = 1024*1024*2;

	private int documentSizeLimit = DEFAULT_DOCUMENT_SIZE_LIMIT;

	/**
	 * pings the given target uri
	 * @param sourceURI the source uri for the pingback
	 * @param targetURI the target url to ping
	 * @return pingbackResult as String
	 */
	public String ping(String sourceURI, String targetURI) throws PingBackException {
		// retrieve targetURL and determine pingback uri
		String pingbackURI = determinePingBackURI(targetURI);
		
		try {
			// do the xml rpc
			XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
			config.setServerURL(new URL(pingbackURI));
			XmlRpcClient client = new XmlRpcClient();
			client.setConfig(config);
			Object[] params = new Object[]{sourceURI, targetURI};
			String pingbackResult = (String) client.execute("pingback.ping", params);
			return pingbackResult;
		} catch (MalformedURLException e) {
			throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "Invalid ping back uri.", e);
		} catch (XmlRpcException e) {			
			throw new PingBackException(e.code, "Remote error.", e);
		}
	}
	
	private String determinePingBackURI(String targetURI) throws PingBackException {
		HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
		GetMethod pingbackTargetGET = new GetMethod(targetURI);
		pingbackTargetGET.setFollowRedirects(false);
		try {
			int responseCode = client.executeMethod(pingbackTargetGET);
			if (responseCode != HttpURLConnection.HTTP_OK) {
				if (responseCode == HttpURLConnection.HTTP_FORBIDDEN) {
					throw new PingBackException(PingBackException.ERROR_ACCESS_DENIED, "Access denied on target '" + targetURI + "'." );
				} else if (responseCode == HttpURLConnection.HTTP_BAD_GATEWAY) {
					throw new PingBackException(PingBackException.ERROR_UPSTREAM_SERVER_COMMUNICATION_ERROR, "Unable to determine ping back target for post. Get request on '" + targetURI + "' returned '" + responseCode + "'." );
				} else {
					throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "Unable to determine ping back target for post. Get request on '" + targetURI + "' returned '" + responseCode + "'." );
				}
			}
			
			Header header = pingbackTargetGET.getResponseHeader("X-Pingback");
			if (header != null && header.getValues().length > 0) {
				// retrieve ping back url from header
				HeaderElement headerElement = header.getValues()[0];
				return headerElement.getName();
			} else {
				// retrieve ping back url from link tag
				
				// check for textual content
				checkTextualContentType(pingbackTargetGET);
				
				// retrieve input reader an try to find link tag
				InputStream sourceIn = pingbackTargetGET.getResponseBodyAsStream(); 
				String searchTerm = "<link rel=\"pingback\" href=\"";
				if (sourceIn == null) {
					throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "TargetURL '" + targetURI + "' cannot be parsed for link tag.");
				} else {
					BufferedReader reader = new BufferedReader(new InputStreamReader(pingbackTargetGET.getResponseBodyAsStream(), pingbackTargetGET.getResponseCharSet()));
					String line = reader.readLine();
					while (line != null) {
						String orgLine = line;
						line = line.toLowerCase();						
						int start = line.indexOf(searchTerm);
						if (start != -1) {
							if (start + searchTerm.length() <= line.length()) {
								start = start + searchTerm.length();
								int end = line.indexOf("\"", start);
								if (end != -1) {
									String href = orgLine.substring(start, end);
									href = href.replaceAll("&amp;", "&");
									href = href.replaceAll("&lt;", "<");
									href = href.replaceAll("&gt;", ">");
									href = href.replaceAll("&quot;", "\"");
									return href;
								} else {
									throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "TargetURL '" + targetURI + "' returned an unparsable link-tag");
								}
							} else {
								throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "TargetURL '" + targetURI + "' returned an unparsable link-tag");								
							}
						}
						// if endof head reached - cancel search
						if (line.indexOf("</head>") != -1 || line.indexOf("<body>") != -1) {
							throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "TargetURL '" + targetURI + "' is not pingback-enabled.");							
						}
						line = reader.readLine();
					}
					throw new PingBackException(PingBackException.ERROR_TARGET_URI_CANNOT_BE_USED_AS_TARGET, "TargetURL '" + targetURI + "' is not pingback-enabled.");
				}
			}									
		} catch (HttpException e) {
			throw new PingBackException(PingBackException.ERROR_GENERIC, "Unable to determine ping back target for post." , e);
		} catch (IOException e) {
			throw new PingBackException(PingBackException.ERROR_GENERIC, "Unable to determine ping back target for post." , e);
		}
	}

	/**
	 * checks if the given sourceURI is reachable, has textual content and contains a link to the given target
	 * if present the title of the sourceURI is returned
	 * @param sourceURI - the sourceURI to check
	 * @param targetURI - the targetURI to search as link 
	 * @throws PingBackException - thrown if check fails
	 * @returns title of the sourceURI - null if not present or found
	 */
	public String checkSourceURI(String sourceURI, String targetURI) throws PingBackException {
		HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
		GetMethod sourceGET = new GetMethod(sourceURI);
		sourceGET.setFollowRedirects(false);
		try {
			int responseCode = client.executeMethod(sourceGET);
			if (responseCode != HttpURLConnection.HTTP_OK) {
				if (responseCode == HttpURLConnection.HTTP_FORBIDDEN) {
					throw new PingBackException(PingBackException.ERROR_ACCESS_DENIED, "Access denied on source uri '" + sourceURI + "'." );
				} else if (responseCode == HttpURLConnection.HTTP_BAD_GATEWAY) {
					throw new PingBackException(PingBackException.ERROR_UPSTREAM_SERVER_COMMUNICATION_ERROR, "Get request on source uri '" + sourceURI + "' returned '" + responseCode + "'." );
				} else {
					throw new PingBackException(PingBackException.ERROR_GENERIC, "Source uri is unreachable. Get request on source uri '" + sourceURI + "' returned '" + responseCode + "'." );
				}
			}

			checkTextualContentType(sourceGET);
			
			// search link to target in source
			InputStream sourceIn = sourceGET.getResponseBodyAsStream(); 
			String searchTerm = targetURI.toLowerCase();
			boolean linkFound = false;
			String title = null;
			if (sourceIn == null) {
				throw new PingBackException(PingBackException.ERROR_SOURCE_URI_HAS_NO_LINK, "Source uri contains no link to target '" + targetURI + "'." );
			} else {
				// first of all read response into a fix buffer of 2Mb - all further content will be ignored for doS-reason
				ByteArrayOutputStream htmlPageBuffer = new ByteArrayOutputStream();
				inToOut(sourceGET.getResponseBodyAsStream(), htmlPageBuffer, 1024, documentSizeLimit);				
				
				try {
					// search for title
					DOMParser parser = new DOMParser();					
					parser.parse(new InputSource(new ByteArrayInputStream(htmlPageBuffer.toByteArray())));
					Document doc = parser.getDocument();
					NodeList titleElements = doc.getElementsByTagName("title");
					if (titleElements.getLength() > 0) {
						// retrieve first title
						Node titleNode = titleElements.item(0);
						title = titleNode.getFirstChild().getNodeValue();
					}
				} catch (Exception e) {
					// ignore any parsing exception - title is just a goodie
				}

				// read line per line and search for link
				BufferedReader reader = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(htmlPageBuffer.toByteArray()),sourceGET.getResponseCharSet()));
				String line = reader.readLine();
				while (line != null) {
					line = line.toLowerCase();
					if (line.indexOf(searchTerm) != -1) {
						linkFound = true;
						break;
					}
					line = reader.readLine();
				}
			}
			
			if (!linkFound) {
				throw new PingBackException(PingBackException.ERROR_SOURCE_URI_HAS_NO_LINK, "Source uri '" + sourceURI + "' contains no link to target '" + targetURI + "'.");
			} else {
				return title;
			}
		} catch (HttpException e) {
			throw new PingBackException(PingBackException.ERROR_GENERIC, "Unable to check source uri '" + sourceURI + "'." , e);
		} catch (IOException e) {
			throw new PingBackException(PingBackException.ERROR_GENERIC, "Unable to check source uri '" + sourceURI + "'." , e);
		}
 	}

	private void checkTextualContentType(GetMethod sourceGET) throws PingBackException {
		Header contentType = sourceGET.getResponseHeader("Content-type");
		if (contentType == null) {
			throw new PingBackException(PingBackException.ERROR_GENERIC, "Unable to determine content-type of source uri.");
		} else if (contentType.getValue().indexOf("text/") == -1) {
			throw new PingBackException(PingBackException.ERROR_GENERIC, "Source uri returned none textual content-type '" + contentType.getValue() + "'.");				
		}
	}		
		
	private void inToOut(InputStream in, OutputStream out, int buffer, int limit) throws IOException {
        byte[] buf = new byte[1024];
        int len;
        int written = 0;
        while ((len = in.read(buf)) != -1) {
            out.write(buf, 0, len);
        	written += len;
        	if (written >= limit) {
        		break;
        	}
        }
	}
	
	/**
	 * returns the current size limit in bytes for html document parsing
	 * @return
	 */
	public int getDocumentSizeLimit() {
		return documentSizeLimit;
	}
	
	/**
	 * sets the size limit in bytes for html document parsing
	 * @param documentSizeLimit
	 */
	public void setDocumentSizeLimit(int documentSizeLimit) {
		this.documentSizeLimit = documentSizeLimit;
	}
	
}
