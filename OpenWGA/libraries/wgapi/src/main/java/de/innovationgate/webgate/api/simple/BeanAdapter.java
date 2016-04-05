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
package de.innovationgate.webgate.api.simple;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.templates.BeanKey;
import de.innovationgate.webgate.api.templates.BeanWrapper;
import de.innovationgate.webgate.api.templates.ContentSourceSpecs;
import de.innovationgate.webgate.api.templates.SimpleContentSource;

/**
 * A simple content source that retrieves data from a custom Java bean
 */
public class BeanAdapter extends SimpleContentSource {
	
	static class BeanCache {
		
		private String _password;
		private String _username;
		private Object _bean;

		public BeanCache(Object theBean, String username, String password) {
			_bean = theBean;
			_username = username;
			_password = password;
		}
		
		/**
		 * @return
		 */
		public Object getBean() {
			return _bean;
		}

		/**
		 * @return
		 */
		public String getPassword() {
			return _password;
		}

		/**
		 * @return
		 */
		public String getUsername() {
			return _username;
		}

	}
	


	public static final String USER_BEAN = "userBean";

	private Object _dbBean;
	private BeanKey _dummyBeanKey = new BeanKey("home", new Integer(1));
	public static final String COPTION_ONE_BEAN_PER = "one.bean.per";
	public static final String COPTION_ONE_BEAN_PER_REQUEST = "request";
	public static final String COPTION_ONE_BEAN_PER_USER = "user";
	public static final String COPTION_ONE_BEAN_PER_DB = "db";
	public static final String COPTION_CREATION_HANDLER = "creation.handler";
	
	public static final int ONE_BEAN_PER_REQUEST = 1;
	public static final int ONE_BEAN_PER_USER = 2;
	public static final int ONE_BEAN_PER_DB = 3;
	
	public static final String COPTION_BEAN_JARFILE = "jarfile";
	


	
	private BeanCreationHandler _creationHandler = null;
	private int fetchMode = ONE_BEAN_PER_REQUEST;


	private Class _beanClass;

	private ThreadLocal<BeanWrapper> _bean = new ThreadLocal<BeanWrapper>();


	private String[] _dummyFolderList = new String[] {_dummyBeanKey.getFolder()};
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#login(java.lang.String, java.lang.String)
	 */
	public int login(String user, String pwd) throws WGAPIException {
		Object theBean;
		try {
			theBean = fetchBean(user, pwd);
			_bean.set(createWrapper(_dummyBeanKey, theBean, true));
		}
		catch (WGAuthorisationException e) {
			return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
		}
		
		catch (IllegalArgumentException e) {
			throw new WGCreationException("Cannot instatiate bean bc. of illegal argument");
		}
		catch (InstantiationException e) {
			throw new WGCreationException("Cannot instatiate bean bc. it is an interface or an abstract class");
		}
		catch (IllegalAccessException e) {
			throw new WGCreationException("Cannot instatiate bean bc. cannot access the constructor");
		}
		catch (InvocationTargetException e) {
			if (e.getTargetException() instanceof WGAuthorisationException) {
				return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
			}
			else {
				WGFactory.getLogger().error("Error instantiating bean by bean adapter", e.getTargetException());
				throw new WGCreationException("Cannot instatiate bean bc. of exception in constructor: "+  e.getTargetException().getClass().getName() + " - " + e.getTargetException().getMessage());
			}
		}
		
		return WGDatabase.ACCESSLEVEL_MANAGER;
	}

	private synchronized Object fetchBean(String username, String password) throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException, WGAuthorisationException {
		
		switch (fetchMode) {
			
			case ONE_BEAN_PER_DB: {
				if (_dbBean == null) {
					_dbBean = createBean(username, password);				
				}
				return _dbBean;
			}
			
			case ONE_BEAN_PER_USER: {
				Map userCache = getDb().getUserCache().getMapForUser(username); 
				BeanCache beanCache = (BeanCache) userCache.get(USER_BEAN);
				if (beanCache == null) {
					beanCache = new BeanCache(createBean(username, password), username, password);
					userCache.put(USER_BEAN, beanCache);
				}
                else if (beanCache.getPassword() == null) {
                    if (password != null) {
                        throw new IllegalAccessException("Wrong password");
                    }
                }
				else if (password == null || !beanCache.getPassword().equals(password)) {
					throw new IllegalAccessException("Wrong password");					
				}
				return beanCache.getBean();
			}
			
			default:
				return createBean(username, password);
		}
		
	}

	private Object createBean(String username, String password) throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
	    
	    if (WGDatabase.MASTER_USERNAME.equals(username)) {
	        return _creationHandler.createMasterBean();
	    }
	    else {
	        return _creationHandler.createBean(username, password);
	    }
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#logout()
	 */
	public void logout() {
		
		if (fetchMode == ONE_BEAN_PER_REQUEST) {
			BeanWrapper bean = _bean.get();
			if (bean == null) {
			    return;
			}
			
			try {
				_creationHandler.destroyBean(bean.getNativeObject());
			}
			catch (IllegalArgumentException e) {
				WGFactory.getLogger().warn("Error destroying bean", e);
			}
			catch (IllegalAccessException e) {
				WGFactory.getLogger().warn("Error destroying bean", e);
			}
			catch (InvocationTargetException e) {
				WGFactory.getLogger().warn("Error destroying bean", e.getTargetException());
			}
		}
		
		_bean.remove();
	}


	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTitle()
	 */
	public String getTitle() {
		return "JavaBean Content Source";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
	 */
	public String getTypeName() {
		return "custom/beancs";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getCreated()
	 */
	public Date getCreated() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#convertToKey(java.lang.String)
	 */
	public Object convertToKey(String key, String folder) {
		return _dummyBeanKey;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getContent(java.lang.String, java.lang.Object)
	 */
	public Object getContent(String folder, Object key) {
		return getDummyContent(null);
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
		return browse("home");
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getLastModified()
	 */
	public Date getLastModified() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#removeContent(java.lang.String, java.lang.Object)
	 */
	public void removeContent(String folder, Object key) {}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#init(de.innovationgate.webgate.api.WGDatabase, java.lang.String)
	 */
	public ContentSourceSpecs init(WGDatabase db, String path) throws WGInvalidDatabaseException {

		// Look if a special bean jar was specified
		ClassLoader classLoader = WGFactory.getImplementationLoader();
		if (db.getCreationOptions().containsKey(COPTION_BEAN_JARFILE)) {
			String jarPath = (String) db.getCreationOptions().get(COPTION_BEAN_JARFILE);
			File jarFile = new File(jarPath);
			if (!jarFile.exists()) {
				throw new WGInvalidDatabaseException("Cannot find jarfile '" + jarPath + "'");
			}
			try {
				classLoader = new URLClassLoader(new URL[] { jarFile.toURL() }, WGFactory.getImplementationLoader());
			}
			catch (MalformedURLException e2) {
				throw new WGInvalidDatabaseException("Cannot build URL to jarfile '" + jarPath + "'");
			}
		}
				
		// Fetch the bean class
		try {
			_beanClass = classLoader.loadClass(path);
		}
		catch (ClassNotFoundException e) {
			throw new WGInvalidDatabaseException("Unknown bean class");
		}
		
		if (_beanClass.isInterface()) {
			throw new WGInvalidDatabaseException("Cannot use bean class because it is an interface");
		}
		
		int modifiers = _beanClass.getModifiers();
		if (Modifier.isAbstract(modifiers)) {
			throw new WGInvalidDatabaseException("Cannot use bean class because it is an abstract class");
		}
		
		// Look, if there is creation handler determined. Else use DefaultBeanCreationHandler
		if (db.getCreationOptions().containsKey(COPTION_CREATION_HANDLER)) {
			String creationHandlerStr = (String) db.getCreationOptions().get(COPTION_CREATION_HANDLER);
			try {
				Class creationHandlerClass =  classLoader.loadClass(creationHandlerStr);
				_creationHandler = (BeanCreationHandler) creationHandlerClass.newInstance();
			}
			catch (ClassNotFoundException e1) {
				throw new WGInvalidDatabaseException("Cannot find class for bean creation handler: " + creationHandlerStr);
			} 
			catch (InstantiationException e) {
				throw new WGInvalidDatabaseException("Cannot instantiate bean creation handler because it either is abstract or an interface: " + creationHandlerStr);
			} 
			catch (IllegalAccessException e) {
				throw new WGInvalidDatabaseException("Cannot find class for bean creation handler because the constructor used is not public: " + creationHandlerStr);
			}
		}
		else {
			_creationHandler = new DefaultBeanCreationHandler();
		}
		
		
		// Initialize the creation handler
		try {
			_creationHandler.init(_beanClass, this);
		}
		catch (SecurityException e1) {
			throw new WGInvalidDatabaseException("The java security manager prohibits introspection of the bean class");
		}
		catch (NoSuchMethodException e1) {
			throw new WGInvalidDatabaseException("The needed constructor was not found: " + e1.getMessage());
		}
		
		
		// Fetchmode parsing
		if (db.getCreationOptions().containsKey(COPTION_ONE_BEAN_PER)) {
			String fetchModeStr = (String) db.getCreationOptions().get(COPTION_ONE_BEAN_PER);
			if (fetchModeStr.equals(COPTION_ONE_BEAN_PER_DB)) {
				fetchMode = ONE_BEAN_PER_DB;
			}
			else if (fetchModeStr.equals(COPTION_ONE_BEAN_PER_USER)) {
				fetchMode = ONE_BEAN_PER_USER;
			}
		}
	
		// Create and return specs
		ContentSourceSpecs specs = new ContentSourceSpecs();
		specs.setBrowseable(false);
		specs.setMaintainsLastChanged(false);
		specs.setQueryable(false);
		specs.setWritable(true);
		specs.setUseMasterLogin(true);
		return specs;
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#destroy()
	 */
	public void destroy() {
		
		try {
			switch (fetchMode) {
				
				case ONE_BEAN_PER_DB: {
					if(_dbBean != null) {
						_creationHandler.destroyBean(_dbBean);
					}
				}
				
				case ONE_BEAN_PER_USER: {
					Map userMaps = getDb().getUserCache().getUserMaps();
					Iterator userMapKeys = userMaps.keySet().iterator();
					Map userMap;
					while (userMapKeys.hasNext()) {
						userMap = (Map) userMaps.get(userMapKeys.next());
						BeanCache beanCache = (BeanCache) userMap.remove(USER_BEAN);
						if (beanCache != null) {
							_creationHandler.destroyBean(beanCache.getBean());
						}
					}
				}
			}
		}
		catch (IllegalArgumentException e) {
			WGFactory.getLogger().warn("Error destroying bean", e);
		}
		catch (IllegalAccessException e) {
			WGFactory.getLogger().warn("Error destroying bean", e);
		}
		catch (InvocationTargetException e) {
			WGFactory.getLogger().warn("Error destroying bean", e.getTargetException());
		}
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getFolders()
	 */
	public String[] getFolders() {
		return _dummyFolderList;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#browse(java.lang.String)
	 */
	public Map browse(String folder) {
		Map map = new HashMap();
		if (folder.equals("home")) {
			map.put(_dummyBeanKey, getDummyContent(null));
		}
		return map;
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
	 */
	public String getServerName() {
		return "(none)";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getDummyContent(java.lang.String)
	 */
	public WGDocumentCore getDummyContent(String language) {
		return (WGDocumentCore) _bean.get();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getNativeObject()
	 */
	public Object getNativeObject() {
		return _bean.get().getNativeObject();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#updateContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean updateContent(String folder, Object key, Object bean) {
		return false;
	}
    
    public WGDatabase getDB() {
        return getDb();
    }

}
