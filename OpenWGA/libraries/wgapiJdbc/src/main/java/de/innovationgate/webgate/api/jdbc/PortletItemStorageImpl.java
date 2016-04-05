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
import org.hibernate.Query;

import de.innovationgate.webgate.api.PortletItemStorageData;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGClosedSessionException;
import de.innovationgate.webgate.api.WGPortletItemStorage;
import de.innovationgate.webgate.api.WGTransientPortlet;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.ImmutableObject;

/**
 * Item storage implementation using normal CS5 portlet registry tables
 */
public class PortletItemStorageImpl implements WGPortletItemStorage, ImmutableObject {
    
    
    private WGDatabaseImpl _parent;
    private WGDocumentImpl _docImpl;
    public PortletItemStorageImpl(WGDatabaseImpl parent, WGDocumentImpl wgDocumentImpl) {
        _parent = parent;
        _docImpl = wgDocumentImpl;
    }

    @Override
    public void prepareNewProfile(WGUserProfile profile) throws WGAPIException {
    }

    @Override
    public List<String> getItemNames(WGTransientPortlet wgPortlet) throws WGAPIException {

        UserProfilePortlet portlet = getOrCreatePersistentPortlet(wgPortlet);
        List<String> names = new ArrayList<String>(portlet.getItemNames());
        
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get((UserProfilePortlet) portlet);
        if (removedItems != null) {
            names.removeAll(removedItems.keySet());
        }
        
        return names;
        
    }

    @Override
    public boolean hasItem(WGTransientPortlet wgPortlet, String name) throws WGAPIException {

        String lcName = name.toLowerCase().trim();
        
        UserProfilePortlet portlet = getOrCreatePersistentPortlet(wgPortlet);
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get(portlet);
        if (removedItems != null &&  removedItems.keySet().contains(lcName)) {
            return false;
        }
        
        return portlet.getItems().containsKey(lcName) || portlet.getItems().containsKey(name);
        
    }

    @Override
    public Object getItemValue(WGTransientPortlet wgPortlet, String name) throws WGAPIException {

        String lcName = name.toLowerCase().trim();
        
        UserProfilePortlet portlet = getOrCreatePersistentPortlet(wgPortlet);
        Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get(portlet);
        if (removedItems != null && removedItems.keySet().contains(lcName)) {
            return null;
        }
        
        
        return ((UserProfilePortlet) portlet).getItemValue(name);
        
    }

    @Override
    public void setItemValue(WGTransientPortlet wgPortlet, String name, Object value) throws WGAPIException {

        startProfileEdit();
        
        name = name.toLowerCase().trim();        
        
        UserProfilePortlet portlet = getOrCreatePersistentPortlet(wgPortlet);
        writeItem(portlet, name, value);
        
    }

    public void writeItem(UserProfilePortlet portlet, String name, Object value) throws WGAPIException {
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
    public void removeItem(WGTransientPortlet wgPortlet, String name) throws WGAPIException {

        startProfileEdit();
        
        UserProfilePortlet portlet = getOrCreatePersistentPortlet(wgPortlet);
        Hibernate.initialize(portlet.getItems());
        
        Map<String, UserProfileItem> items = portlet.getItems();
        String lcName = name.toLowerCase().trim();
        
        UserProfileItem itemToRemove = null;
        String itemKey = null;
        if (items.containsKey(lcName)) {
            itemToRemove = portlet.getItems().get(lcName);
            itemKey = lcName;
        }
        else if (items.containsKey(name)) {
            itemToRemove = portlet.getItems().get(name);
            itemKey = name;
        } 
        
        if (itemToRemove != null && itemKey != null) {
            Map<String,UserProfileItem> removedItems = _docImpl._removedPortletItems.get(portlet);
            if (removedItems == null) {
                removedItems = new HashMap<String, UserProfileItem>();
                _docImpl._removedPortletItems.put(portlet, removedItems);
            }
            
            removedItems.put(itemKey, itemToRemove);
        }
        
    }

    protected UserProfile getProfile() {
        return (UserProfile) _docImpl.getNativeObject();
    }

    @Override
    public void preSaveProfile(WGUserProfile profile) throws WGAPIException {

        if (_docImpl.getDocument().isEdited()) {
            Iterator<UserProfilePortlet> it = getProfile().getPortlets().values().iterator();
            while (it.hasNext()) {
                UserProfilePortlet p = it.next();
                if (p.getItems().size() == 0) {
                    it.remove();
                }
            }
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
    
    public UserProfilePortlet getOrCreatePersistentPortlet(WGTransientPortlet portlet) {
        return getOrCreatePersistentPortlet(portlet.getKey(), portlet.getApplicationDb(), portlet.getName());
    }
    
    public UserProfilePortlet getOrCreatePersistentPortlet(String portletKey, String appDb, String name) {
        
        Map<String, UserProfilePortlet> portlets = getProfile().getPortlets();
        UserProfilePortlet persistentPortlet = portlets.get(portletKey);
        if (persistentPortlet == null) {
            persistentPortlet = createPersistentPortlet(portletKey, appDb, name);
            portlets.put(portletKey, persistentPortlet);
        }
        return persistentPortlet;

        
    }

    private UserProfilePortlet createPersistentPortlet(String portletKey, String appDb, String name) {
        UserProfilePortlet portlet = new UserProfilePortlet();

        portlet.setParentprofile(getProfile());
        portlet.setKey(portletKey);
        portlet.setAppdb(appDb);
        portlet.setName(name);
        portlet.setItems(new HashMap<String,UserProfileItem>());
        portlet.setExtensionData(new HashMap<String, ExtensionData>());
                
        return portlet;
    }
    
   
    @Override
    public void pushData(WGPortletItemStorage targetStorage) throws WGAPIException {

        Query query = _parent.getSession().createQuery("select portlet from UserProfilePortlet as portlet");
        Iterator<UserProfilePortlet> it = query.iterate();
        while (it.hasNext()) {
            UserProfilePortlet p = it.next();
            targetStorage.receiveData(p);
        }
        
    }

    @Override
    public void receiveData(PortletItemStorageData p) throws WGAPIException {

        UserProfilePortlet newP = createPersistentPortlet(p.getKey(), p.getApplicationDb(), p.getName());
        for (String name : p.getItemNames()) {
            writeItem(newP, name, p.getItemValue(name));
        }
        
    }        
    
    
}
