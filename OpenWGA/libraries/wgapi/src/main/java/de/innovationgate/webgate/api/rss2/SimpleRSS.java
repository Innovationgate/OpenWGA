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
package de.innovationgate.webgate.api.rss2;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.LinkedMap;
import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.NTCredentials;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.GetMethod;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.templates.ContentSourceSpecs;
import de.innovationgate.webgate.api.templates.SimpleContentSource;
import de.innovationgate.webgate.api.utils.NativeQueryOptions;
import de.nava.informa.core.CategoryIF;
import de.nava.informa.core.ChannelIF;
import de.nava.informa.core.ItemEnclosureIF;
import de.nava.informa.core.ItemGuidIF;
import de.nava.informa.core.ItemSourceIF;
import de.nava.informa.impl.basic.ChannelBuilder;
import de.nava.informa.impl.basic.Item;
import de.nava.informa.parsers.FeedParser;

public class SimpleRSS extends SimpleContentSource {
    
    public static final String QUERYOPTION_PWD = "pwd";
    public static final String QUERYOPTION_USER = "user";

    public static class ItemWrapper {
        
        private Item _item;

        public ItemWrapper(Item item) {
            _item = item;
        }
        
        public Element getElement() throws DocumentException, IOException {
            
            XMLOutputter outputter = new XMLOutputter();           
            outputter.setFormat(Format.getPrettyFormat());
            StringWriter xml = new StringWriter();
            outputter.output(_item.getItemElement(), xml);
            
            return DocumentHelper.parseText(xml.toString()).getRootElement();
        }
        
        public Element getDocument() throws IOException, DocumentException {
            
            XMLOutputter outputter = new XMLOutputter();           
            outputter.setFormat(Format.getPrettyFormat());
            StringWriter xml = new StringWriter();
            outputter.output(_item.getItemElement().getDocument(), xml);
            
            return DocumentHelper.parseText(xml.toString()).getRootElement();
            
        }
        
        public String getAuthor() {
            return _item.getCreator();
        }
        
        public Date getLastmodified() {
            return _item.getDate();
        }
        
        public Date getCreated() {
            return _item.getDate();
        }
        
        public String getVirtuallink() {
            return String.valueOf(getLink());
        }
        
        public String getVirtuallinktype() {
           return WGContent.VIRTUALLINKTYPE_EXTERNAL;
        }

        public void addCategory(CategoryIF category) {
            _item.addCategory(category);
        }

        public boolean equals(Object arg0) {
            return _item.equals(arg0);
        }

        public String getAttributeValue(String path, String attribute) {
            return _item.getAttributeValue(path, attribute);
        }

        public String[] getAttributeValues(String path, String[] attributes) {
            return _item.getAttributeValues(path, attributes);
        }

        public Collection getCategories() {
            return _item.getCategories();
        }

        public ChannelIF getChannel() {
            return _item.getChannel();
        }

        public URL getComments() {
            return _item.getComments();
        }

        public String getCreator() {
            return _item.getCreator();
        }

        public Date getDate() {
            return _item.getDate();
        }

        public String getDescription() {
            return _item.getDescription();
        }

        public String getElementValue(String path) {
            return _item.getElementValue(path);
        }

        public String[] getElementValues(String path, String[] elements) {
            return _item.getElementValues(path, elements);
        }

        public ItemEnclosureIF getEnclosure() {
            return _item.getEnclosure();
        }

        public Date getFound() {
            return _item.getFound();
        }

        public String getGuid() {
            return _item.getGuid().getLocation();
        }
        
        public boolean getGuid_permalink() {
            return _item.getGuid().isPermaLink();
        }


        public long getId() {
            return _item.getId();
        }

        public URL getLink() {
            return _item.getLink();
        }

        public ItemSourceIF getSource() {
            return _item.getSource();
        }

        public String getSubject() {
            return _item.getSubject();
        }

        public String getTitle() {
            return _item.getTitle();
        }

        public boolean getUnRead() {
            return _item.getUnRead();
        }

        public int hashCode() {
            return _item.hashCode();
        }

        public void removeCategory(CategoryIF category) {
            _item.removeCategory(category);
        }

        public void setCategories(Collection categories) {
            _item.setCategories(categories);
        }

        public void setChannel(ChannelIF channel) {
            _item.setChannel(channel);
        }

        public void setComments(URL comments) {
            _item.setComments(comments);
        }

        public void setCreator(String creator) {
            _item.setCreator(creator);
        }

        public void setDate(Date date) {
            _item.setDate(date);
        }

        public void setDescription(String description) {
            _item.setDescription(description);
        }

        public void setEnclosure(ItemEnclosureIF enclosure) {
            _item.setEnclosure(enclosure);
        }

        public void setFound(Date found) {
            _item.setFound(found);
        }

        public void setId(long id) {
            _item.setId(id);
        }

        public void setLink(URL link) {
            _item.setLink(link);
        }

        public void setSource(ItemSourceIF source) {
            _item.setSource(source);
        }

        public void setSubject(String subject) {
            _item.setSubject(subject);
        }

        public void setTitle(String title) {
            _item.setTitle(title);
        }

        public void setUnRead(boolean val) {
            _item.setUnRead(val);
        }

        public String toString() {
            return _item.toString();
        }
        
    }
	
	public static final String COPTION_PROXY_DOMAIN = "ProxyDomain";
	private String _proxyDomain;
	private String _proxyCredentials;
	public static final String COPTION_PROXY_CREDENTIALS  = "ProxyCredentials";
	private boolean _useProxy = false;
	public static final int PROXY_DEFAULTPORT = 8080;
	private int _proxyPort;

	private String _proxyHost;

	public static final String COPTION_PROXY = "Proxy";

	ThreadLocal _channel = new ThreadLocal();
    
    public ChannelIF getChannel() {
        return (ChannelIF) _channel.get();
    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#login(java.lang.String, java.lang.String)
	 */
	public int login(String user, String pwd) {
		return WGDatabase.ACCESSLEVEL_READER;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#logout()
	 */
	public void logout() {
        _channel.remove();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getTitle()
	 */
	public String getTitle() {
		return "RSS Feed Connector";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getTypeName()
	 */
	public String getTypeName() {
		return "rss/custom";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getCreated()
	 */
	public Date getCreated() {
        ChannelIF channel = getChannel();
        if (channel != null) {
            return channel.getPubDate();
        }
        else {
            return new Date(Long.MIN_VALUE);
        }
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#convertToKey(java.lang.String)
	 */
	public Object convertToKey(String key, String folder) {
		return key;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getContent(java.lang.String, java.lang.Object)
	 */
	public Object getContent(String folder, Object key) {
		
        ChannelIF channel = getChannel();
        if (channel == null) {
            return null;
        }
        
		// folder may be left out blank or null, because its not needed anyway.
		Collection items = channel.getItems();
		Iterator iter = items.iterator();
		
		while(iter.hasNext()){
			Item item = (Item) iter.next();
			if( item.getTitle().equalsIgnoreCase((String) key) ) {
				return new ItemWrapper(item);
			}
		}
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#putContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean insertContent(String folder, Object key, Object bean) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#createContent(java.lang.String, java.lang.Object, java.lang.String)
	 */
	public Object createContent(String folder) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#find(java.lang.String, java.lang.String, java.util.Map)
	 */
	public Map find(String folder, String query, Map parameters) throws WGQueryException {
		
		URL rssURL;
        ChannelIF channel = null;
        NativeQueryOptions nativeOptions = new NativeQueryOptions((String) parameters.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS));
		
		try {
			// rssURL			= new URL(query);
			channel = FeedParser.parse(new ChannelBuilder(), retrievePage(query, nativeOptions));
            if (channel != null) {
                _channel.set(channel);
            }
            else {
                throw new WGQueryException("Unable to retrieve or parse feed", query);
            }
        }  
		catch (Exception e) {
            throw new WGQueryException("Exception retrieving or parsing feed", query, e);
		}
		
		
		List items = new ArrayList(channel.getItems());
		Map itemMap = new LinkedMap();
		Iterator iter = items.iterator();
		
		while(iter.hasNext()){
			Item item = (Item) iter.next();
			itemMap.put( (new Long(item.getId())).toString() , new ItemWrapper(item) );
		  }
		return itemMap;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getLastModified()
	 */
	public Date getLastModified() {
		// return (Date) this.channel.getLastUpdated();
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#removeContent(java.lang.String, java.lang.Object)
	 */
	public void removeContent(String folder, Object key) {
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#init(de.innovationgate.webgate.api.WGDatabase, java.lang.String)
	 */
	public ContentSourceSpecs init(WGDatabase db, String path) {
		
		// conditionally configure proxy
		String proxyConf = (String) db.getCreationOptions().get(COPTION_PROXY);
		if (proxyConf != null) {
			List elements = WGUtils.deserializeCollection(proxyConf, ":");
			_proxyHost = (String) elements.get(0);
			_useProxy = true;
			if (elements.size() == 2) {
				try {
					_proxyPort = Integer.parseInt((String) elements.get(1));
				}
				catch (NumberFormatException e) {
					WGFactory.getLogger().error("Cannot parse proxy port as integer: " + elements.get(1));
					_proxyPort = PROXY_DEFAULTPORT;
				}
				
				_proxyCredentials = (String) db.getCreationOptions().get(COPTION_PROXY_CREDENTIALS);
				_proxyDomain = (String) db.getCreationOptions().get(COPTION_PROXY_DOMAIN);
				
			}
			else {
				_proxyPort = PROXY_DEFAULTPORT;
			}
		}
		
		ContentSourceSpecs specs = new ContentSourceSpecs();
		specs.setBrowseable(false);
		specs.setMaintainsLastChanged(false);
		specs.setQueryable(true);
		specs.setWritable(false);
        specs.setLowerCaseItems(true);
        specs.setServePropertiesAsMetas(true);
        specs.setContentReadProtected(false);
		
		return specs;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#destroy()
	 */
	public void destroy() {
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getFolders()
	 */
	public String[] getFolders() {
		
		// no folders available for RSS-Impl.
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#browse(java.lang.String)
	 */
	public Map browse(String folder) {
	    return null;
	}
	
	private InputStream retrievePage(String url, NativeQueryOptions nativeOptions) throws WGQueryException {
		
		try {
			
			// Retrieve from web
			HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
			client.setConnectionTimeout(10000);
			if (_useProxy) {
				client.getHostConfiguration().setProxy(_proxyHost, _proxyPort);
				if (_proxyCredentials != null) {
					Credentials credentials;
					if (_proxyDomain != null) {
						List elements = WGUtils.deserializeCollection(_proxyCredentials, ":");
						credentials = new NTCredentials((String) elements.get(0), (String) elements.get(1), _proxyHost, _proxyDomain);
					}
					else {
						credentials = new UsernamePasswordCredentials(_proxyCredentials);
						
					}
					client.getState().setProxyCredentials(null, _proxyHost, credentials);
					
				}
			}
			HttpMethod method = new GetMethod(url);
			method.setFollowRedirects(true);
			method.setStrictMode(false);
			
			if (nativeOptions.containsKey(QUERYOPTION_USER) && nativeOptions.containsKey(QUERYOPTION_PWD)) {
			    method.setDoAuthentication(true);
			    client.getParams().setAuthenticationPreemptive(true);
			    client.getState().setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(nativeOptions.get(QUERYOPTION_USER), nativeOptions.get(QUERYOPTION_PWD)));
			}
			
			
			client.executeMethod(method);
			
			// Read response. Wrap content decoder if necessary.
			InputStream inStream = method.getResponseBodyAsStream();

			// Return InputStream from given URL
			return inStream;
		}
		catch (MalformedURLException e) {
			throw new WGQueryException("Malformed feed URL", url, e);
		}
		catch (IOException e) {
			throw new WGQueryException("IO Exception retrieving feed", url, e);
		}
	}	
	
	
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
	 */
	public String getServerName() {
		return "(none)";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#updateContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean updateContent(String folder, Object key, Object bean) {
		return false;
	}

	public void beginUpdate() {
	}

}
