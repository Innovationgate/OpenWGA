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
package de.innovationgate.webgate.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGSessionContext.DocumentContext;
import de.innovationgate.webgate.api.locking.Lockable;

/**
 * A user profile, used to store information about and from a web user.
 */

public class WGUserProfile extends WGDocument {
    
    /**
     * Value returned by {@link #getType()} when the type is not determineable
     */
    public static final int PROFILETYPE_UNDEFINED = 0;
	
	public static final String META_NAME = "NAME";
    public static final MetaInfo METAINFO_NAME = new MetaInfo(META_NAME, String.class, null);
    
	public static final String META_TYPE = "TYPE";
    public static final MetaInfo METAINFO_TYPE = new MetaInfo(META_TYPE, Integer.class, new Integer(PROFILETYPE_UNDEFINED));
    
	public static final String META_LASTACCESS = "LASTACCESS";
    public static final MetaInfo METAINFO_LASTACCESS = new MetaInfo(META_LASTACCESS, Date.class, null);
       
	public static final String META_LASTSESSIONID = "LASTSESSIONID";
    public static final MetaInfo METAINFO_LASTSESSIONID = new MetaInfo(META_LASTSESSIONID, String.class, null);
    
	public static final String META_LASTSESSIONDATE = "LASTSESSIONDATE";
    public static final MetaInfo METAINFO_LASTSESSIONDATE = new MetaInfo(META_LASTSESSIONDATE, Date.class, null);
    
	public static final String META_LASTSESSIONHITS= "LASTSESSIONHITS";
    public static final MetaInfo METAINFO_LASTSESSIONHITS = new MetaInfo(META_LASTSESSIONHITS, Integer.class, new Integer(0));
    
	public static final String META_PREVIOUSSESSIONID = "PREVSESSIONID";
    public static final MetaInfo METAINFO_PREVIOUSSESSIONID = new MetaInfo(META_PREVIOUSSESSIONID, String.class, null);
    
	public static final String META_PREVIOUSSESSIONDATE = "PREVSESSIONDATE";
    public static final MetaInfo METAINFO_PREVIOUSSESSIONDATE = new MetaInfo(META_PREVIOUSSESSIONDATE, Date.class, null);
    
	public static final String META_PREVIOUSSESSIONHITS = "PREVSESSIONHITS";
    public static final MetaInfo METAINFO_PREVIOUSSESSIONHITS = new MetaInfo(META_PREVIOUSSESSIONHITS, Integer.class, new Integer(0));
    
	public static final String META_HITS = "HITS";
    public static final MetaInfo METAINFO_HITS = new MetaInfo(META_HITS, Integer.class, new Integer(0));
    
	public static final String META_SESSIONS = "SESSIONS";
    public static final MetaInfo METAINFO_SESSIONS = new MetaInfo(META_SESSIONS, Integer.class, new Integer(0));
    
	public static final String META_CLIENT = "CLIENT";
    public static final MetaInfo METAINFO_CLIENT = new MetaInfo(META_CLIENT, String.class, null);
    static { METAINFO_CLIENT.setInputConverter(
                new MetaConverter() {

                    public Object convert(WGDocument doc, MetaInfo metaInfo, Object value) throws WGAPIException {

                        // Enforce maximum size of 255 chars
                        String str = String.valueOf(value);
                        return WGUtils.reduceUserAgentString(str, 255);
                    
                    }
        
                });
    } ;
    
	public static final String META_LANGUAGES = "LANGUAGES";
    public static final MetaInfo METAINFO_LANGUAGES = new MetaInfo(META_LANGUAGES, String.class, Collections.EMPTY_LIST);
    static { METAINFO_LANGUAGES.setMultiple(true); };
    
	public static final String META_DBLOGIN = "LOGIN";
    public static final MetaInfo METAINFO_DBLOGIN = new MetaInfo(META_DBLOGIN, String.class, WGDatabase.ANONYMOUS_USER);
    
	public static final String META_PASSWORD = "PASSWORD";
    public static final MetaInfo METAINFO_PASSWORD = new MetaInfo(META_PASSWORD, String.class, null);
    
	public static final String META_PORTLETREGISTRY = "PORTLETREGISTRY";
    public static final MetaInfo METAINFO_PORTLETREGISTRY = new MetaInfo(META_PORTLETREGISTRY, WGPortletRegistry.class, null);
    static {
        METAINFO_PORTLETREGISTRY.setCacheable(false);
    }
    
    public static final String META_PORTLETITEMSTORAGE = "PORTLETITEMSTORAGE";
    public static final MetaInfo METAINFO_PORTLETITEMSTORAGE = new MetaInfo(META_PORTLETITEMSTORAGE, WGPortletItemStorage.class, null);
    static {
        METAINFO_PORTLETITEMSTORAGE.setCacheable(false);
    }
    
    public static final String META_LASTIP ="LASTIP";
    public static final MetaInfo METAINFO_LASTIP = new MetaInfo(META_LASTIP, String.class, null);
    static {
        METAINFO_LASTIP.setExtdata(true);
        METAINFO_LASTIP.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    /**
     * Item automatically stored on every user profile denoting the WGA version under which this profile was created
     */
    public static final String ITEM_WGACREATIONVERSION = "$wgacreationversion";

	private String name;

	/**
	 * Constructor. Not to be used outside the WGAPI.
	 * @param db
	 * @param doc
	 * @throws WGAPIException 
	 */
	public WGUserProfile(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
		super(db, doc, flags);
		this.name = this.getName();
	}
	 
	/**
	 * Returns the profile name which also is used as web user name
	 * @throws WGAPIException 
	 */
	public String getName() throws WGAPIException {
		return (String) this.getMetaData(META_NAME);

	}
	
	/**
	 * Returns the profile type, which is the personalisation mode under which it was registered.
	 * @throws WGAPIException 
	 */
	public int getProfileType() throws WGAPIException {
		return ((Integer) this.getMetaData(META_TYPE)).intValue();
	}	
	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.webgate.api.WGDocument#retrieveCore()
	 */
	protected WGDocumentCore retrieveCore() throws WGAPIException {
		return this.db.getPersonalisationCore().getUserProfile(this.name);
	}


	
	/**
	 * Returns the browser client of the user
	 * @throws WGAPIException 
	 */
	public String getClient() throws WGAPIException {
		return (String) this.getMetaData(META_CLIENT);
	}
	
	/**
	 * Sets the browser client name for the current user
	 * @throws WGAPIException 
	 */
	public boolean setClient(String client) throws WGAPIException {
		return this.setMetaData(META_CLIENT, client);
	}
  
	/**
	 * Returns the database login used by the web user
	 * @throws WGAPIException 
	 */
	public String getDBLogin() throws WGAPIException {
		return (String) getMetaData(META_DBLOGIN);
	}
	
	/**
	 * Sets the content db login for the current user
	 * @throws WGAPIException 
	 */
	public boolean setDBLogin(String login) throws WGAPIException {
		return this.setMetaData(META_DBLOGIN, login);
	}
	
	/**
	 * Returns the registered "Hits" (i.e. page impressions) of the current user
	 * @throws WGAPIException 
	 */
	public int getHits() throws WGAPIException {
		return ((Integer) this.getMetaData(META_HITS)).intValue();
	}
	
	/**
	 * Registers a "hit" (page impression) for the current user.
	 * @throws WGAPIException 
	 */
	public boolean addHit() throws WGAPIException {
		boolean result1 = this.setMetaData(META_HITS, new Integer(this.getHits() + 1));
		boolean result2 =  this.setMetaData(META_LASTSESSIONHITS, new Integer(this.getLastSessionHits() + 1));
		return (result1 & result2);
	}
	
	/**
	 * Returns the languages that the users browser is configured to accept
	 * @throws WGAPIException 
	 */
	public List getLanguages() throws WGAPIException {
		return (List) this.getMetaData(META_LANGUAGES);
	}
	
	/**
	 * Sets the accepted languages for the current user
	 * @throws WGAPIException 
	 */
	public boolean setLanguages(List langs) throws WGAPIException {
		return this.setMetaData(META_LANGUAGES, langs);
	}
	
	/**
	 * Returns the last access date.
	 * @throws WGAPIException 
	 */
	public Date getLastAccess() throws WGAPIException {
		return (Date) this.getMetaData(META_LASTACCESS);
	}
	
	/**
	 * Sets the last accessed date.
	 * @throws WGAPIException 
	 */
	public boolean setLastAccess(Date lastAccess) throws WGAPIException {
		return this.setMetaData(META_LASTACCESS, lastAccess);
	}
	
	/**
	 * Returns the date when the last session was opened.
	 * @throws WGAPIException 
	 */
	public Date getLastSessionDate() throws WGAPIException {
		return (Date) this.getMetaData(META_LASTSESSIONDATE);
	}
	
	/**
	 * Returns the hits in the last session.
	 * @throws WGAPIException 
	 */
	public int getLastSessionHits() throws WGAPIException {
		return ((Integer) this.getMetaData(META_LASTSESSIONHITS)).intValue();
	}
	
	/**
	 * Returns the ID of the last session
	 * @throws WGAPIException 
	 */
	public String getLastSessionID() throws WGAPIException {
		return (String) this.getMetaData(META_LASTSESSIONID);
	}
	
	/**
	 * Registers a new session for the user of this profile
	 * @throws WGAPIException 
	 */
	public void addNewSession(HttpServletRequest request, HttpSession session) throws WGAPIException {
		this.setMetaData(META_SESSIONS, new Integer(this.getSessionCount() + 1));
		this.setMetaData(META_PREVIOUSSESSIONDATE, this.getLastSessionDate());
		this.setMetaData(META_PREVIOUSSESSIONHITS, new Integer(this.getLastSessionHits()));
		this.setMetaData(META_PREVIOUSSESSIONID, this.getLastSessionID());
		this.setMetaData(META_LASTSESSIONID, session.getId());
		this.setMetaData(META_LASTSESSIONDATE, new Date());
		this.setMetaData(META_LASTSESSIONHITS, new Integer(0));
		
		if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
		    this.setMetaData(META_LASTIP, request.getRemoteAddr());
		}
		
	}
	
	/**
	 * Returns the hits in the last already closed session
	 * @throws WGAPIException 
	 */
	public int getPreviousSessionHits() throws WGAPIException {
		return ((Integer) this.getMetaData(META_PREVIOUSSESSIONHITS)).intValue();
	}
	
	/**
	 * Returns the ID of the last already closed session
	 * @throws WGAPIException 
	 */
	public String getPreviousSessionID() throws WGAPIException {
		return (String) this.getMetaData(META_PREVIOUSSESSIONID);
	}
	
	/**
	 * Get the number of registered sessions for the current user.
	 * @throws WGAPIException 
	 */
	public int getSessionCount() throws WGAPIException {
		return ((Integer) this.getMetaData(META_SESSIONS)).intValue();
	}
	
	/**
	 * If the personalisation mode is "custom", returns set password for this profile
	 * @throws WGAPIException 
	 */
	public String getPassword() throws WGAPIException {
		return (String) this.getMetaData(META_PASSWORD);
	}
	
	/**
	 * Sets the custom password for this profile, used in personalisation mode "custom" to open this profile.
	 * @param value
	 * @throws WGAPIException 
	 */
	public boolean setPassword(String value) throws WGAPIException {
		return this.setMetaData(META_PASSWORD, value);
	}
	
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
	 */
	public WGDocument createClone(WGDatabase db, WGDocument ref) throws WGAPIException {
	    
	    WGUserProfile newUserProfile = db.createUserProfile(getName(), getType());
        pushData(newUserProfile);
        
        newUserProfile.saveWithGivenTimestamps(getCreated(), getLastModified());
        return newUserProfile;
        
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#remove()
	 */
	protected boolean remove(WGDocument deletionRoot) throws WGAPIException {
		return innerRemove(deletionRoot, true);
	}



    @Override
	protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAPIException {
        
        super.performRemoveCheck(deepCheck, deletionRoot);
        
        if (db.getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_MANAGER) {
			throw new WGAuthorisationException("You are not authorized to delete this user profile", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
		}
    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#dropRelations()
	 */
	protected void dropRelations() {}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGUserProfile newProfile = (WGUserProfile) doc;
        
        newProfile.setClient(getClient());
        newProfile.setDBLogin(getDBLogin());
        newProfile.setMetaData(META_HITS, getHits());
        newProfile.setMetaData(META_LANGUAGES, new ArrayList(getLanguages()));
        newProfile.setLastAccess(getLastAccess());
        newProfile.setMetaData(META_LASTSESSIONDATE, getLastSessionDate());
        newProfile.setMetaData(META_LASTSESSIONHITS, getLastSessionHits());
        newProfile.setMetaData(META_LASTSESSIONID, getLastSessionID());
        newProfile.setPassword(getPassword());
        newProfile.setMetaData(META_PREVIOUSSESSIONDATE, getMetaData(META_PREVIOUSSESSIONDATE));
        newProfile.setMetaData(META_PREVIOUSSESSIONHITS, getPreviousSessionHits());
        newProfile.setMetaData(META_PREVIOUSSESSIONID, getPreviousSessionID());
        newProfile.setMetaData(META_SESSIONS, getSessionCount());
        
        // Clear all items
        if (newProfile.getItemNames().size() > 0) {
            newProfile.removeAllItems();
            newProfile.save();
        }
        
        // Copy Items
        Iterator itemNames = getItemNames().iterator();
        String itemName;
        while (itemNames.hasNext()) {
            itemName = (String) itemNames.next();
            try {
                Object value = getItemValue(itemName);
                if (value instanceof List) {
                    value = new ArrayList((List) value);
                }
                newProfile.setItemValue(itemName, value);
            }
            catch (WGIllegalDataException e) {
                WGFactory.getLogger().warn("Cannot copy item '" + itemName + "' because of unrestorable data", e);
            }
        }
        
        // Migrate portlet registry
        migratePortletRegistry(newProfile);
        
        super.pushData(doc);
        
    }

    private void migratePortletRegistry(WGUserProfile newProfile) throws WGAPIException {
        
        // Registry mode used is injected by user profile migrator
        String portletRegistryMode = (String) getDatabase().getSessionContext().getAttribute("PortletRegistryMode");
        if (portletRegistryMode == null) {
            portletRegistryMode = WGDatabase.PORTLETREGISTRYMODE_PERSISTENT;
        }
        
        
        // Migrate persistent portlets: Use dbkey configuration
        if (WGDatabase.PORTLETREGISTRYMODE_PERSISTENT.equals(portletRegistryMode)) {
            
            WGPortletRegistry newRegistry = newProfile.getPortletRegistry();
            WGPortletRegistry myReg = getPortletRegistry();
        
            // Determine app dbkeys to change when migrating portlet registrations. If application dbs given we assume domain personalisation. We migrate all given dbs to the same keys 
            Map<String, String> appDbMapping = new HashMap<String, String>();
            List<String> appDbs = (List<String>) getDatabase().getSessionContext().getAttribute("PortletRegistryAppDbKeys");
            if (appDbs != null) {
                for (String sourceDb : appDbs) {
                    String targetDb;
                    int equalPos = sourceDb.indexOf("=");
                    if (equalPos != -1) {
                        targetDb = sourceDb.substring(equalPos + 1).trim();
                        sourceDb = sourceDb.substring(0, equalPos).trim();
                    }
                    else {
                        targetDb = sourceDb;
                    }
                    appDbMapping.put(sourceDb, targetDb);
                }
            }
            // Else we assume self personalized. We migrate the from dbkey to the to dbkey
            else {
                appDbMapping.put(getDatabase().getDbReference(), newProfile.getDatabase().getDbReference());
            }

            // Migrate portlet registrations hierarchically from root portlet
            for (String appDb : appDbMapping.keySet()) {
                    
                // Clear portlet registry
                WGPortlet oldPortlet = newRegistry.getOrCreateRootPortlet(appDb);
                if (oldPortlet != null) {
                    newRegistry.clearItems(oldPortlet);
                    for (WGPortlet firstLevel :newRegistry.getChildPortlets(oldPortlet)) {
                        newRegistry.removePortlet(firstLevel);
                    }
                }
                
                // Copy portlet registry
                WGPortlet portlet = myReg.getOrCreateRootPortlet(appDb);
                if (portlet != null) {
                    pushPortletData(appDb, appDbMapping.get(appDb), portlet, myReg, newRegistry, null);
                }
                    
           }                
            
        }
        
        // Migration items of transient portlets: Just copy everything. No dbkey matching necessary as UUID is used    
        else {
            WGPortletItemStorage myStorage = (WGPortletItemStorage) getMetaData(META_PORTLETITEMSTORAGE);
            WGPortletItemStorage newStorage = (WGPortletItemStorage) newProfile.getMetaData(META_PORTLETITEMSTORAGE);
            myStorage.pushData(newStorage);
        }

    }
    
    private void pushPortletData(String sourceDb, String targetDb, WGPortlet portlet, WGPortletRegistry sourceRegistry, WGPortletRegistry targetRegistry, WGPortlet targetParent) throws WGAPIException {
        
        // Push this portlet
        WGPortlet newPortlet;
        if (portlet.isRoot()) {
            newPortlet = targetRegistry.getOrCreateRootPortlet(targetDb);
        }
        else {
            newPortlet = targetRegistry.createPortlet(targetDb, targetParent);
            newPortlet.setName(portlet.getName());
            newPortlet.setDesign(portlet.getDesign());
            
            String newDesignDb = (sourceDb.equals(portlet.getDesignDb()) ? targetDb : portlet.getDesignDb());
            
            newPortlet.setDesignDb(newDesignDb);
            targetRegistry.insertPortlet(newPortlet);
        }
        
        
        for (String itemName : sourceRegistry.getItemNames(portlet)) {
            targetRegistry.setItemValue(newPortlet, itemName, sourceRegistry.getItemValue(portlet, itemName));
        }
        for (WGPortlet child : sourceRegistry.getChildPortlets(portlet)) {
            pushPortletData(sourceDb, targetDb, child, sourceRegistry, targetRegistry, newPortlet);
        }
        targetRegistry.updatePortlet(newPortlet);
        
    }

    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#getParentLockable()
     */
    public Lockable getParentLockable() {
        return getDatabase();
    }
    

    
    /**
     * Returns the registry object for persistently stored portlets on this user profile
     * @throws WGAPIException
     */
    public WGPortletRegistry getPortletRegistry() throws WGAPIException {

        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (WGPortletRegistry) getMetaData(META_PORTLETREGISTRY);
        
    }
    
    /**
     * Returns the storage for persistent portlet items for transient portlets of this profile
     * @throws WGAPIException
     */
    public WGPortletItemStorage getPortletItemStorage() throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_PROVIDE_PORTLETITEM_STORAGE)) {
            throw new WGNotSupportedException("This database is not able to serve a transient portlet registry");
        }
        
        return (WGPortletItemStorage) getMetaData(META_PORTLETITEMSTORAGE);
        
    }

    @Override
    public int getType() {
        return WGDocument.TYPE_USERPROFILE;
    }
    
    @Override
    protected boolean innerSave(WGDocumentCore docCore, Date lastModified, boolean isNewDoc) throws WGAPIException, WGClosedSessionException, WGBackendException {
        
        // We will update the last changed date only, when the core is from this database
        // and there are no pending changes in background
        boolean updateLastChanged = false;
        
        getDatabase().verboseBackendAccess(WGOperationKey.OP_SAVE, getDocumentKeyObj());
        WGDatabaseRevision revision = docCore.save(lastModified);
        
        setEdited(false);

        // This block necessary, if a document goes from temporary to
        // persistent
        if (!this.tempDuplicate) {
            this.temporary = docCore.isTemporary();
            if (!this.temporary) {
                this.temporaryCore = null;
            }
        }

        if (isNewDoc) {
            // In case, some parts of the key were generated on first save
            // Note: Implementations that do this MUST NOT support transactions on WGAPI level!
            WGDocumentKey oldId = uniqueId;
            uniqueId = buildDocumentKey(docCore, getDatabase());
            if (!oldId.equals(uniqueId)) {
                getDatabase().getSessionContext().remapDocumentContext(oldId, uniqueId);
            }
            
            // Replace doc and core in current document context, because there still may be "dummy" instances of these (B00005D32)
            DocumentContext docContext = getDocumentSessionContext();
            docContext.setDocument(this);
            docContext.setCore(docCore);
        }

        if (!getDatabase().getSessionContext().isTransactionActive()) {
            dropCache();
            fastAccessKey = docCore.getFastAccessKey();
        }
        updateBackendCaches(docCore);
        
        return true;
        
    }
    
    /**
     * Returns the IP address under which the last session was started
     * @throws WGAPIException
     */
    public String getLastIP() throws WGAPIException {
        return (String) getMetaData(META_LASTIP);
    }

}
