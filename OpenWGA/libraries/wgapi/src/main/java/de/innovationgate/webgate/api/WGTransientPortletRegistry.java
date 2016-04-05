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
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.wga.common.ImmutableObject;

/**
 * A transient portlet registry which is able to store portlet items on the given profiles portlet item storage
 */
public class WGTransientPortletRegistry extends WGPortletRegistry implements ImmutableObject {
    
    public class RootPortletInformation {
        
        private String _referer;
        private String _portletId;
        
        public RootPortletInformation(String referer, String portletId) {
            _referer = referer;
            _portletId = portletId;
        }
        
    }
    
    private Map<String,WGTransientPortlet> _rootPortlets = new ConcurrentHashMap<String, WGTransientPortlet>();
    private Map<String,WGTransientPortlet> _portletsByKey = new ConcurrentHashMap<String, WGTransientPortlet>();
    private WGDatabase _parent;
    private WGUserProfile _profile;

    public WGTransientPortletRegistry(WGDatabase parent, WGUserProfile profile) {
        _parent = parent;
        _profile = profile;
    }

    @Override
    public void insertPortlet(WGPortlet portlet) throws WGAPIException {
        WGTransientPortlet transientPortlet = (WGTransientPortlet) portlet;
        WGTransientPortlet parentPortlet = transientPortlet.getParentPortlet();
        
        if (parentPortlet != null) {
            parentPortlet.getChildPortlets().put(transientPortlet.getName(), transientPortlet);
        }
        else {
            _rootPortlets.put(portlet.getKey(), transientPortlet);
        }
        
        _portletsByKey.put(portlet.getKey(), transientPortlet);
    }

    @Override
    public WGPortlet getPortlet(String appDb, String key) throws WGAPIException {
        if (key != null) {        
            return  _portletsByKey.get(key);
        }
        else {
            return null;
        }
    }

    @Override
    public WGPortlet getPortletByName(String appDb, WGPortlet parentPortlet, String name) throws WGAPIException {
        
        if (name == null) {
            return null;
        }
        
        WGTransientPortlet transientPortlet = (WGTransientPortlet) parentPortlet;
        return transientPortlet.getChildPortlets().get(name);
    }

    @Override
    public void updatePortlet(WGPortlet portlet) throws WGAPIException {
    }

    @Override
    public void removePortlet(WGPortlet portlet) throws WGAPIException {
        WGTransientPortlet transientPortlet = (WGTransientPortlet) portlet;
        _portletsByKey.remove(portlet.getKey());
        if (portlet.isRoot()) {
            _rootPortlets.remove(portlet.getKey());
        }
        else {
           transientPortlet.getParentPortlet().getChildPortlets().remove(transientPortlet.getName());
        }
    }

    @Override
    public WGPortlet getOrCreateRootPortlet(String appDb) throws WGAPIException {
        
        String rootPortletKey = WGTransientPortlet.generateKey(appDb, null, null);
        WGTransientPortlet rootPortlet = (WGTransientPortlet) getPortlet(appDb,  rootPortletKey);
        if (rootPortlet != null) {
            return rootPortlet;
        }
        
        rootPortlet = new WGTransientPortlet(this, appDb);
        insertPortlet(rootPortlet);
        return rootPortlet;
    }

    @Override
    public void prepareNewProfile(WGUserProfile profile) throws WGAPIException {
        getStorage().prepareNewProfile(profile);
    }

    @Override
    public List<WGPortlet> getChildPortlets(WGPortlet portlet) throws WGAPIException {

        WGTransientPortlet transientPortlet = (WGTransientPortlet) portlet;
        return new ArrayList<WGPortlet>(transientPortlet.getChildPortlets().values());
    }


    @Override
    public WGPortlet createPortlet(String appDb, WGPortlet parent) {
        return new WGTransientPortlet(this, appDb,  (WGTransientPortlet) parent);
    }
   
    @Override
    public boolean isTransient() throws WGAPIException {
        return true;
    }

    @Override
    public void preSaveProfile(WGUserProfile profile) throws WGAPIException {
        getStorage().preSaveProfile(profile);
    }

    private WGPortletItemStorage getStorage() throws WGAPIException {
        return _profile.getPortletItemStorage();
    }

    @Override
    public List<String> getItemNames(WGPortlet portlet) throws WGAPIException {
        return getStorage().getItemNames((WGTransientPortlet) portlet);
    }

    @Override
    public boolean hasItem(WGPortlet portlet, String name) throws WGAPIException {
        return getStorage().hasItem((WGTransientPortlet) portlet, name);
    }

    @Override
    public Object getItemValue(WGPortlet portlet, String name) throws WGAPIException {
        return getStorage().getItemValue((WGTransientPortlet) portlet, name);
    }

    @Override
    public void setItemValue(WGPortlet portlet, String name, Object value) throws WGAPIException {
        getStorage().setItemValue((WGTransientPortlet) portlet, name, value);
    }

    @Override
    public void removeItem(WGPortlet portlet, String name) throws WGAPIException {
        getStorage().removeItem((WGTransientPortlet) portlet, name);
    }
    
    @Override
    public String getApplicationId(WGDatabase db) {
        
        String uuid = db.getUUID();
        if (uuid != null) {
            return uuid;
        }
        else {
            return db.getDbReference();
        }
        
    }

}
