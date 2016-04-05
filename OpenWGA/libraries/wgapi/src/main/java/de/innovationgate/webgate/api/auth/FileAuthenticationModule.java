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
package de.innovationgate.webgate.api.auth;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;

/**
 * This authentication module takes an XML file as base where users and groups are defined:
 * 
 * <!-- This is a demostration file for a definition xml for users and groups. The attribute "allowanonymous" controls, if anonymous access to databases will be allowed -->
 * <users allowanonymous="true">
 *	<!-- 
 *	The user definition:
 *	name: Full distinguished and unique name of this user
 *	password: Her password to sign on
 *	mail: Mail address used for workflow notifications
 *	aliases: name aliases divided by comma. She can also logon using these. You should check for uniqueness of them too
 *	-->
 *	<user name="John Doe" password="mypass" mail="john.doe@mycompany.com" aliases="John,jd"/>
 *	<user name="Jane Doe" password="anotherpass" mail="jane.doe@mycompany.com" aliases="Jane,jad"/>	
 *	
 *	<!--
 *	The group definition:
 *	name: Full distinguished (and only) name of this group.
 *	members: Name of member users. Name aliases will do but you should use the fully distinguished name 
 *	         for lookup speed and readability. You cannot use names of other groups in here.
 *	-->
 *	<group name="admins" members="John Doe,Jane Doe"/>
 * </users>
 */
public class FileAuthenticationModule implements AuthenticationModule, PasswordCachingAuthenticationModule {
    
    public static final String AUTHMODULE_KEY = "file";
    
    public static final Logger LOG = Logger.getLogger("wga.api.auth.file");
	
	/**
	 * Sysproperty to specify a base path for XML config files.
	 */
	public static final String SYSPROPERTY_AUTH_FOLDER = "de.innovationgate.wga.auth.folder";
	private boolean _allowAnonymous;

    private long _lastModified;

	private Map _users = new HashMap();

	private File _file = null;

	private String _fileName;


    
    private List _authenticationSourceListeners = new ArrayList();

	/**
	 * Creation option to specify the XML config file location. 
	 * If sysproperty de.innovationgate.wga.auth.folder is set, this is evaluated relative to it. 
	 */
	public static final String AUTH_FILE =  "auth.file";
	
	class User {
		
		private String _mail;
		private String _pwd;
		private Set _aliases;
		private Set _groups = new HashSet();
		private String _name;

		public User(String name, String pwd, String mail, Set aliases) {
			_name = name;
			_pwd = pwd;
			_mail = mail;
			_aliases = aliases;
		}
		
		/**
		 * @return
		 */
		public Set getAliases() {
			return _aliases;
		}

		/**
		 * @return
		 */
		public String getName() {
			return _name;
		}
		
		public void addGroup(String name) {
			_groups.add(name);
		}

		/**
		 * @return
		 */
		public String getPwd() {
			return _pwd;
		}

		/**
		 * @return
		 */
		public Set getGroups() {
			return _groups;
		}

		/**
		 * @return
		 */
		public String getMail() {
			return _mail;
		}

	}
	
	class Group {
		
		private String _name;

		private List _members;

		public Group(String name, List members) {
			_name = name;
			_members = members;
		}
		
		/**
		 * @return
		 */
		public List getMembers() {
			return _members;
		}

		/**
		 * @return
		 */
		public String getName() {
			return _name;
		}

	}
	
	class Session implements AuthenticationSession {
		
		private Set _names;

		private boolean _valid = true;

		private User _user;

		public Session(User user) {
			_user = user;
			_names = _user.getAliases();
			_names.add(_user.getName());
		}

		/* (Kein Javadoc)
		 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getDistinguishedName()
		 */
		public String getDistinguishedName() {
			return _user.getName();
		}

		/* (Kein Javadoc)
		 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getMailAddress()
		 */
		public String getMailAddress() {
			return _user.getMail();
		}

		/* (Kein Javadoc)
		 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getNames()
		 */
		public Set getNames() {
			return _names;
		}

		/* (Kein Javadoc)
		 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getGroups()
		 */
		public Set getGroups() {
			return _user.getGroups();
		}

		/* (Kein Javadoc)
		 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#logout()
		 */
		public void logout() {
			_valid = false;
		}

		/* (Kein Javadoc)
		 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#isValid()
		 */
		public boolean isValid() {
			return _valid;
		}

        public String getSessionToken() {
            return null;
        }
	}
	
	class AnonymousSession extends Session {
		public AnonymousSession() {
			super(new User(WGDatabase.ANONYMOUS_USER, null, null, new HashSet()));			
		}
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationModule#init(java.util.Map)
	 */
	public void init(Map params, WGDatabase db) throws ConfigurationException {
	
		_fileName = (String) params.get(AUTH_FILE);
		if (_fileName == null) {
		    throw new ConfigurationException("Authentication file not specified");
		}
		
		
		String authFolder = System.getProperty(SYSPROPERTY_AUTH_FOLDER, null);
		if (authFolder != null) {
			_file = new File(new File(authFolder), _fileName);	
		}
		
		if (_file == null || !_file.exists()) {
			_file = new File(_fileName);
		}
		
		if (!_file.exists()) {
		    throw new ConfigurationException("XML authentication file does not exist: " + _fileName);
		}
		
		update(_file);	
	}

	/**
	 * @param _file
	 */
	private synchronized void update(File _file) {
	
		try {			
			// Parse doc
			SAXReader reader = new SAXReader();
			reader.setIncludeExternalDTDDeclarations(false);
			reader.setIncludeInternalDTDDeclarations(false);
			reader.setValidation(false);
			Document doc = reader.read(_file);
			
			// Flags
			_allowAnonymous = Boolean.valueOf(doc.getRootElement().attributeValue("allowanonymous")).booleanValue();
			
			// Read users
			Map users = new HashMap();
			Iterator userNodes = doc.selectNodes("/users/user").iterator();
			Element userNode;
			while (userNodes.hasNext()) {
				userNode = (Element) userNodes.next();
				String name = userNode.attributeValue("name");
				String password = userNode.attributeValue("password");
				String mail = userNode.attributeValue("mail");
				String aliasesStr = userNode.attributeValue("aliases");
				Set aliases = new HashSet();
				if (aliasesStr != null) {
					aliases.addAll(WGUtils.deserializeCollection(aliasesStr, ",", true));
				}
				users.put(name, new User(name, password, mail, aliases));
			}
			_users = users;
			
			// Read Groups
			Map groups = new HashMap();
			Iterator groupNodes = doc.selectNodes("/users/group").iterator();
			Element groupNode;
			while (groupNodes.hasNext()) {
				groupNode = (Element) groupNodes.next();
				String name = groupNode.attributeValue("name");
				List members = WGUtils.deserializeCollection(groupNode.attributeValue("members"), ",", true);
				Group group = new Group(name, members);
				groups.put(name, group);
				notifyMembers(group);
			}
            
            // Call listeners
            synchronized (_authenticationSourceListeners) {
                Iterator listeners = _authenticationSourceListeners.iterator();
                while (listeners.hasNext()) {
                    ((AuthenticationSourceListener) listeners.next()).authenticationDataChanged();
                }
            }
		}
		catch (DocumentException e) {
			WGFactory.getLogger().error("Error parsing authentication file '" + getAuthenticationSource() + "': Error in XML: " + e.getMessage());
		}
	}

	/**
	 * @param group
	 */
	private void notifyMembers(Group group) {
	
		Iterator members = group.getMembers().iterator();
		String memberName;
		User user;
		while (members.hasNext()) {
			memberName = (String) members.next();
			user = findUser(memberName);
			if (user != null) {
				user.addGroup(group.getName());
			}
			else {
				WGFactory.getLogger().error("Error parsing authentication file '" + getAuthenticationSource() + "': User '" + memberName + "' not defined");
			}
			
		}
	
	
	}

	private User findUser(String memberName) {
		
		// First try to find by name - fastest
		User user = (User) _users.get(memberName);
		if (user != null) {
			return user;
		}
		
		// Then try to find by alias - slow
		Iterator users = _users.values().iterator();
		while (users.hasNext()) {
			user = (User) users.next();
			if (user.getAliases().contains(memberName)) {
				return user;
			}
		}
		
		return null;
		
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationModule#login(java.lang.String, java.lang.String)
	 */
	public AuthenticationSession login(String user, Object credentials) throws AuthenticationException {
		
        String password = (String) credentials;
        
		if (user.equals(WGDatabase.ANONYMOUS_USER)) {
			if (_allowAnonymous) {
				return new AnonymousSession();
			}
			else {
			    LOG.warn("Failed login from anonymous user: Anonymous login not allowed (" + getAuthenticationSource() + ")");
				return null;
			}
		}
        
        if (_file.lastModified() > _lastModified) {
            synchronized (this) {
                _lastModified = _file.lastModified();
                update(_file);
            }
        }
		
		User userObj = findUser(user);
		if (userObj == null) {
		    LOG.warn("Failed login for '" + user + "': Unknown user (" + getAuthenticationSource() + ")");
			return null;
		}
		
		if (!userObj.getPwd().equals(password)) {
		    LOG.warn("Failed login for '" + user + "': Wrong password (" + getAuthenticationSource() + ")");
			return null;
		}
		
		return new Session(userObj);

	}



	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationModule#clearCache()
	 */
	public void clearCache() {
		update(_file);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationModule#getAuthenticationSource()
	 */
	public String getAuthenticationSource() {
		return "File-Authentication - " + _file.getAbsolutePath();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationModule#getEMailAddress(java.lang.String)
	 */
	public String getEMailAddress(String user) {
		
		User userObj = findUser(user);
		if (userObj != null) {
			return userObj.getMail();
		}
		else {
			return null;
		}

	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#isPoolable()
     */
    public boolean isPoolable() {
        return true;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#addAuthenticationSourceListener(de.innovationgate.webgate.api.auth.AuthenticationSourceListener)
     */
    public void addAuthenticationSourceListener(AuthenticationSourceListener listener) {
        synchronized (_authenticationSourceListeners) {
            _authenticationSourceListeners.add(listener);
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#removeAuthenticationSourceListener(de.innovationgate.webgate.api.auth.AuthenticationSourceListener)
     */
    public void removeAuthenticationSourceListener(AuthenticationSourceListener listener) {
        synchronized (_authenticationSourceListeners) {
            _authenticationSourceListeners.remove(listener);
        }
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#getAllowedCredentialClasses()
     */
    public Class[] getAllowedCredentialClasses() {
        return new Class[] { String.class };
    }

    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#isQueryable(java.lang.String)
     */
    public boolean isQueryable(String queryType) {
        return false;
    }

    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthenticationModule#query(java.lang.Object, java.lang.String)
     */
    public Object query(Object query, String queryType) {
        return null;
    }

    public void destroy() {
    }

    public boolean isGeneratesSessionToken() {
        return false;
    }

    public void dropPasswordCache(String loginName) {
        clearCache();
    }    

}
