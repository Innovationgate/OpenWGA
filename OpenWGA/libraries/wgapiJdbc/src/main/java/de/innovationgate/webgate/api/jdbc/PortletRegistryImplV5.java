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

package de.innovationgate.webgate.api.jdbc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.Hibernate;

import de.innovationgate.groq.Groq;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGClosedSessionException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGPortletRegistry;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.ImmutableObject;

public class PortletRegistryImplV5 extends WGPortletRegistry implements ImmutableObject {
    
    private WGDatabaseImpl _parent;
    private WGDocumentImpl _docImpl;
    private Map<String,UserProfilePortlet> _rootPortletsCache = new HashMap<String, UserProfilePortlet>();

    protected PortletRegistryImplV5(WGDatabaseImpl parent, WGDocumentImpl docImpl) {
        _parent = parent;
        _docImpl = docImpl;
    }


    private UserProfile getProfile() {
        return (UserProfile) _docImpl.getNativeObject();
    }
    

    @Override
    public List<WGPortlet> getChildPortlets(WGPortlet portlet) throws WGAPIException {

        List<UserProfilePortlet> childWGPortlets = new ArrayList<UserProfilePortlet>();
        return Groq.selectFrom(getProfile().getPortlets().values()).wherePropertyEquals("parentportlet", portlet).list();
        
    }

    @Override
    public List<String> getItemNames(WGPortlet portlet) throws WGAPIException {
        List<String> names = new ArrayList<String>(((UserProfilePortlet) portlet).getItems().keySet());
        
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get((UserProfilePortlet) portlet);
        if (removedItems != null) {
            names.removeAll(removedItems.keySet());
        }
        
        return names;
    }

    @Override
    public Object getItemValue(WGPortlet portlet, String name) throws WGAPIException {
        
        String lcName = name.toLowerCase().trim();
        
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get((UserProfilePortlet) portlet);
        if (removedItems != null && removedItems.keySet().contains(lcName)) {
            return null;
        }
        
        UserProfileItem item = ((UserProfilePortlet) portlet).getItems().get(lcName);
        if (item != null) {
            return WGDocumentImpl.readItemValue(_parent, _docImpl, item);
        }
        
        // Workaround for wrongly stored items due to bug #00000316
        item = ((UserProfilePortlet) portlet).getItems().get(name);
        if (item != null) {
            return WGDocumentImpl.readItemValue(_parent, _docImpl, item);
        }
        
        return null;
    
    }

    @Override
    public WGPortlet getPortlet(String appDb, String key) throws WGAPIException {
        
        if (key.startsWith(appDb)) {
            return getOrCreateRootPortlet(appDb);
        }
        
        // Then via collection
        return (WGPortlet) getProfile().getPortlets().get(key);
    }

    @Override
    public WGPortlet getOrCreateRootPortlet(String appDb) throws WGAPIException {
        

        // Ask the cache        
        UserProfilePortlet rootPortlet = _rootPortletsCache.get(appDb);
        if (rootPortlet != null) {
            return rootPortlet;
        }
        
        
        for (UserProfilePortlet portlet : getProfile().getPortlets().values()) {
            if (portlet.getParentportlet() == null && portlet.getAppdb().equals(appDb)) {
                rootPortlet = portlet;
                break;
            }
        }
        
        /*GroqFilterSet<UserProfilePortlet> rootPortlets = Groq.selectFrom(getProfile().getPortlets().values())
            .wherePropertyEquals("parentportlet", null)
            .wherePropertyEquals("appdb", dbKey);
        
        if (rootPortlets.hasNext()) {
            return rootPortlets.next();
        }*/

        // Root portlet not yet registered. We create one
        if (rootPortlet == null) {
            startProfileEdit();
            rootPortlet = (UserProfilePortlet) createPortlet(appDb, null);
            getProfile().getPortlets().put(rootPortlet.getKey(), rootPortlet);
            return rootPortlet;
        }
        
        // Put root portlet into cache for reusage in this session
        _rootPortletsCache.put(appDb, rootPortlet);
        
        return rootPortlet;
        
    }

    @Override
    public boolean hasItem(WGPortlet portlet, String name) throws WGAPIException {
        
        String lcName = name.toLowerCase().trim();
        
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get((UserProfilePortlet) portlet);
        if (removedItems != null &&  removedItems.keySet().contains(lcName)) {
            return false;
        }
        
        return ((UserProfilePortlet) portlet).getItems().containsKey(lcName) || ((UserProfilePortlet) portlet).getItems().containsKey(name);
    }

    @Override
    public void insertPortlet(WGPortlet wgPortlet) throws WGAPIException {
        startProfileEdit();
        UserProfilePortlet portlet = (UserProfilePortlet) wgPortlet;
        getProfile().getPortlets().put(portlet.getKey(), portlet);
        
        String parentKey = wgPortlet.getParentPortletKey();
        if (parentKey != null) {
            WGPortlet parentPortlet = getPortlet(wgPortlet.getApplicationDb(), parentKey);
            setItemValue(parentPortlet, PortletRegistryImplV4.PORTLETNAME_ITEM_PREFIX + wgPortlet.getName(), wgPortlet.getKey());
        }
    }

    @Override
    public void prepareNewProfile(WGUserProfile profile) throws WGAPIException {
    }

    @Override
    public void removeItem(WGPortlet portlet, String name) throws WGAPIException {
        
      startProfileEdit();
        
      UserProfilePortlet v5Portlet = (UserProfilePortlet) portlet;
      Hibernate.initialize(v5Portlet.getItems());
      
      Map<String, UserProfileItem> items = v5Portlet.getItems();
      String lcName = name.toLowerCase().trim();
      
      UserProfileItem itemToRemove = null;
      String itemKey = null;
      if (items.containsKey(lcName)) {
          itemToRemove = v5Portlet.getItems().get(lcName);
          itemKey = lcName;
      }
      else if (items.containsKey(name)) {
          itemToRemove = v5Portlet.getItems().get(name);
          itemKey = name;
      } 
      
      if (itemToRemove != null && itemKey != null) {
          Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get(portlet);
          if (removedItems == null) {
              removedItems = new HashMap<String, UserProfileItem>();
              _docImpl._removedPortletItems.put(v5Portlet, removedItems);
          }
          
          removedItems.put(itemKey, itemToRemove);
      }
      
    }

    @Override
    public void removePortlet(WGPortlet portlet) throws WGAPIException {
        
        startProfileEdit();
        
        if (portlet.isRoot()) {
            throw new WGIllegalArgumentException("Cannot remove the root portlet");    
        }
        
        // First unregister child portlets
        Iterator<WGPortlet> children = getChildPortlets(portlet).iterator();
        while (children.hasNext()) {
            WGPortlet child = (WGPortlet) children.next();
            removePortlet(child);
        }
        
        // Remove from root portlet cache, if present there
        if (_rootPortletsCache.containsValue(portlet)) {
            _rootPortletsCache.remove(portlet.getApplicationDb());
        }
        
        // Cleanup all items
        clearItems(portlet);
        
        // Remove from portlets collection
        Hibernate.initialize(getProfile().getPortlets());
        UserProfilePortlet upPortlet = getProfile().getPortlets().remove(portlet.getKey());
        
        // Remove portlet name item from parent portlet
        WGPortlet parentPortlet = getPortlet(portlet.getApplicationDb(), portlet.getParentPortletKey());
        if (parentPortlet != null) {
            removeItem(parentPortlet, PortletRegistryImplV4.PORTLETNAME_ITEM_PREFIX + portlet.getName());
        }

    }


    private void startProfileEdit() throws WGAPIException {
        _docImpl.makeEditable();
        if (_docImpl.getDocument() != null) {
            try {
                _docImpl.getDocument().markEdited();
            }
            catch (WGClosedSessionException e) {
            }
        }
    }

    @Override
    public void setItemValue(WGPortlet wgPortlet, String name, Object value) throws WGAPIException {
        
        startProfileEdit();
        
        name = name.toLowerCase().trim();        
        
        UserProfilePortlet portlet = (UserProfilePortlet) wgPortlet;
        UserProfileItem item = ((UserProfilePortlet) portlet).getItems().get(name);
        if (item == null) {
            item = new UserProfileItem();
            item.setParentprofile(getProfile());
            item.setPortlet(portlet);
            item.setName(name);
            portlet.getItems().put(name, item);
        }
        
        WGDocumentImpl.writeItemValue(_parent, _docImpl, item, value);
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get(portlet);
        if (removedItems != null) {
            removedItems.remove(name);
        }

    }

    @Override
    public void updatePortlet(WGPortlet portlet) throws WGAPIException {
        _docImpl.makeEditable();
    }


    @Override
    public WGPortlet createPortlet(String appDb, WGPortlet parent) {
        UserProfilePortlet portlet = new UserProfilePortlet();

        portlet.setParentprofile(getProfile());
        portlet.setParentportlet((UserProfilePortlet) parent);
        if (parent != null) {
            portlet.setKey(UIDGenerator.generateUID());
        }
        else {
            String rootKey = (appDb.length() > 32 ? appDb.substring(0, 32) : appDb);
            portlet.setKey(rootKey);
        }
        
        portlet.setAppdb(appDb);
        portlet.setItems(new HashMap<String,UserProfileItem>());
        portlet.setExtensionData(new HashMap<String, ExtensionData>());
                
        return portlet;
    }
    

    
    @Override
    public WGPortlet getPortletByName(String appDb, WGPortlet parentPortlet, String name) throws WGAPIException {

        String key = (String) getItemValue(parentPortlet, PortletRegistryImplV4.PORTLETNAME_ITEM_PREFIX + name);
        if (key == null) {
            return null;
        }
        
        return getPortlet(appDb, key);
        
    }


    @Override
    public void preSaveProfile(WGUserProfile profile) throws WGAPIException {
    }


    @Override
    public boolean isTransient() throws WGAPIException {
        return false;
    }


    @Override
    public String getApplicationId(WGDatabase appDb) {
        return appDb.getDbReference();
    }

}
