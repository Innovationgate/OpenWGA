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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import de.bannkreis.groq.Groq;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGClosedSessionException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGPortletDefaultBean;
import de.innovationgate.webgate.api.WGPortletRegistry;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.ImmutableObject;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;

public class PortletRegistryImplV4 extends WGPortletRegistry implements ImmutableObject {
    
    public static final DecimalFormat PORTLETINDEX_FORMAT = new DecimalFormat("000");
    
    public static final Pattern PATTERN_OLD_STYLE_PORTLETKEY = Pattern.compile("^p\\d+$");
    
    public static String buildNamespacePrefix(String dbkey) {
        
        dbkey = dbkey.toLowerCase();
        String prefix = StringUtils.left(dbkey, 1) + StringUtils.right(dbkey, 2);
        if (prefix.length() < 3) {
            prefix += StringUtils.repeat("x", 3 - prefix.length());
        }
        
        // Escape non alphanumeric characters as x
        char[] chars = prefix.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (!Character.isLetterOrDigit(chars[i])) {
                chars[i] = 'x';
            }
        }
        
        return new String(chars);
        
    }
    
    
    private WGDatabaseImpl _parent;
    private UserProfile _profile;
    private WGDocumentImpl _docImpl;

    public static final String PORTLETNAME_ITEM_PREFIX = "$pname_";

    public PortletRegistryImplV4(WGDatabaseImpl parent, WGDocumentImpl docImpl) {
        _parent = parent;
        _docImpl = docImpl;
        _profile = (UserProfile) docImpl.getNativeObject();
    }

    public synchronized void insertPortlet(WGPortlet portlet) throws WGAPIException {
        
        startProfileEdit();
        
        if (portlet.isRoot()) {
            throw new WGIllegalArgumentException("Cannot insert a root portlet");    
        }
        
        List portletKeys = getPortletIds(portlet.getApplicationDb());
        if (portlet.getParentPortletKey() != null && !isRootPortletKey(portlet.getParentPortletKey()) && !portletKeys.contains(portlet.getParentPortletKey())) {
            throw new IllegalStateException("Cannot register portlet for parent '" + portlet.getParentPortletKey() + "' because the parent portlet is not registered");
        }
        
        synchronized (this) {
            // Generate portlet key
            String childPortletKey = generateNewPortletKey(portlet.getApplicationDb());
    
            // Register portlet
            WGPortletDefaultBean portletBean = (WGPortletDefaultBean) portlet;
            insertPortletId(portletBean, childPortletKey);
            _profile.getPortletkeys().add(serializePortlet(portlet));
            String parentPortletKey = portlet.getParentPortletKey();
            if (parentPortletKey != null) {
                WGPortlet parentPortlet = getPortlet(portlet.getApplicationDb(), parentPortletKey);
                setItemValue(parentPortlet, PortletRegistryImplV4.PORTLETNAME_ITEM_PREFIX + portlet.getName(), childPortletKey);
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
    

    public WGPortlet getPortlet(String appDb, String key) throws WGAPIException {

        if (isRootPortletKey(key)) {
            return getOrCreateRootPortlet(appDb);
        }
        
        Iterator<String> regs = _profile.getPortletkeys().iterator();
        while (regs.hasNext()) {
            String reg = (String) regs.next();
            WGPortlet portlet = deserializePortlet(reg, appDb);
            
            // We do not need to check appDb here bc. in this portlet registry impl the appDb is implicitly contained in the portlet key
            if (portlet.getKey().equals(key)) {
                return portlet;
            }
        }
        
        return null;
        
    }



    private boolean isRootPortletKey(String key) {
        
        if (key.equals("")) {
            return true;
        }
        
        if (key.length() == 6) {
            try {
                String indexStr = key.substring(3);
                Number index = PORTLETINDEX_FORMAT.parse(indexStr);
                if (index.intValue() == 0) {
                    return true;
                }
            }
            catch (Exception e) {
            }
        }
        
        return false;
    }

    public List<String> getPortletIds(String appDb) throws WGAPIException {
        
        Iterator<String> regs = _profile.getPortletkeys().iterator();
        List<String> keys = new ArrayList<String>();
        
        while (regs.hasNext()) {
            String reg = (String) regs.next();
            if (reg == null) {
                continue;
            }
            
            WGPortlet portlet = deserializePortlet(reg, appDb);
            if (doesPortletKeyMatchAppDb(appDb, portlet)) {
                keys.add(portlet.getKey());
            }
        }
        return keys;
    }
    

    


    private boolean doesPortletKeyMatchAppDb(String appDb, WGPortlet portlet) {
        boolean matchesAppDb = false;
        
        String expectedKeyPrefix = buildNamespacePrefix(appDb);
        
        if (portlet.getKey().startsWith(expectedKeyPrefix)) {
            matchesAppDb = true;
        }
        // Check for old style portlet registrations - Here we cannot determine if the portletkey matches any application
        else if (PATTERN_OLD_STYLE_PORTLETKEY.matcher(portlet.getKey()).matches()) {
            matchesAppDb = true;
        }
        return matchesAppDb;
    }

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
        
        // Cleanup all items
        clearItems(portlet);
        
        // Remove registration
        Iterator<String> regs = _profile.getPortletkeys().iterator();
        while (regs.hasNext()) {
            String reg = (String) regs.next();
            WGPortlet p = deserializePortlet(reg, portlet.getApplicationDb());
            
            // We do not need to check appDb here bc. in this portlet registry impl the appDb is implicitly contained in the portlet key
            if (p.getKey().equals(portlet.getKey())) {
                regs.remove();
            }
        }
        
        // Remove portlet name item. Must traverse all parent portlet items for this.
        WGPortlet parentPortlet = getPortlet(portlet.getApplicationDb(), portlet.getParentPortletKey());
        if (parentPortlet != null) {
            for (String itemName : getItemNames(parentPortlet)) {
                if (itemName.startsWith(PORTLETNAME_ITEM_PREFIX)) {
                    String key = (String) getItemValue(parentPortlet, itemName);
                    if (portlet.getKey().equals(key)) {
                        removeItem(parentPortlet, itemName);
                    }
                }
            }
        }
        
    }
    
    private synchronized String generateNewPortletKey(String appDb) throws WGAPIException {
        
        String charPart = buildNamespacePrefix(appDb);
                
        // Find the last portlet key with this prefix
        int portletIndex = 0;
        List portletKeys = getPortletIds(appDb);
        if (!portletKeys.isEmpty()) {
            Iterator iter = portletKeys.iterator();

            while (iter.hasNext()) {
                String pkey = (String) iter.next();
                if (pkey.startsWith(charPart)) {
                    int tempValue = Integer.parseInt(pkey.substring(charPart.length()));
                    if (portletIndex < tempValue) {
                        portletIndex = tempValue;
                    }
                }
            }
        }
        
        portletIndex++;
        return charPart + PORTLETINDEX_FORMAT.format(portletIndex);
    }
    
    private String serializePortlet(WGPortlet portlet) {
        
        List elements = new ArrayList();
        elements.add(portlet.getKey());
        elements.add(portlet.getParentPortletKey());
        
        if (portlet.getDesignDb() != null) {
            elements.add(portlet.getDesignDb() + "/" + portlet.getDesign());
        }
        else {
            elements.add(portlet.getDesign());
        }
        
        elements.add(portlet.getName());
        return WGUtils.serializeCollection(elements, "|", null, true);
        
    }
    
    private WGPortletDefaultBean deserializePortlet(String portletRegistration, String appDb) {
        
        //  getting Indizes of delimiters
        List regElements = WGUtils.deserializeCollection(portletRegistration, "|");
        if (regElements.size() < 4) {
            throw new IllegalArgumentException("Portlet registration elements are " + regElements.size() + ", must be at least 4: " + portletRegistration);
        }

        // separate string into components
        
        String key  = (String) regElements.get(0);
        String parentPortletId = (String) regElements.get(1);
        WGPortletDefaultBean portlet = createPortletDefaultBean(key, appDb, parentPortletId);
        
        String db = null;
        String design = (String) regElements.get(2);
        
        int slashPos = design.indexOf("/");
        
        // Extract database if available
        if (slashPos != -1) {
            db = design.substring(0, slashPos);
            design = design.substring(slashPos + 1);
        }
        String title = (String) regElements.get(3);
        
        portlet.setDesignDb(db);
        portlet.setDesign(design);
        portlet.setName(title);
        
        return portlet;
        
        
    }

    @Override
    public void updatePortlet(WGPortlet portletToUpdate) throws WGAPIException {

        startProfileEdit();
        
        if (portletToUpdate.isRoot()) {
            throw new WGIllegalArgumentException("Cannot update the root portlet");    
        }
        
        List regs = _profile.getPortletkeys();
        for (int idx=0; idx < regs.size(); idx++) {
            String reg = (String) regs.get(idx);
            WGPortlet portlet = deserializePortlet(reg, portletToUpdate.getApplicationDb());
            
            // We do not need to check appDb here bc. in this portlet registry impl the appDb is implicitly contained in the portlet key
            if (portlet.getKey().equals(portletToUpdate.getKey())) {
                regs.set(idx, serializePortlet(portletToUpdate));
                return;
            }
        }
        
        throw new WGIllegalArgumentException("No portlet of id '" + portletToUpdate.getKey() + "' has yet been registered");
    }

    @Override
    public List<WGPortlet> getChildPortlets(WGPortlet parent) throws WGAPIException {

        List<WGPortlet> children = new ArrayList<WGPortlet>();
        
        List regs = _profile.getPortletkeys();
        for (int idx=0; idx < regs.size(); idx++) {
            String reg = (String) regs.get(idx);
            if (reg == null) {
                continue;
            }
            
            WGPortletDefaultBean portlet = deserializePortlet(reg, parent.getApplicationDb());
            
            if (parent.getKey().equals(portlet.getParentPortletKey())) {
                children.add(portlet);
            }
        }
        
        return children;
        
        
    }

    @Override
    public WGPortlet getOrCreateRootPortlet(String appDb) throws WGAPIException {
        String rootNS = determineRootPortletNamespace(appDb);
        return createPortletDefaultBean(rootNS, appDb, null);
    }
    
    private String determineRootPortletNamespace(String dbKey) throws WGAPIException {
        
        UserProfileItem wgaCreationVersion = (UserProfileItem) _profile.getItems().get(WGUserProfile.ITEM_WGACREATIONVERSION);
        if (wgaCreationVersion != null) {
            Version version = new Version(wgaCreationVersion.getText());
            
            // From WGA Version 4.1 on the root portlet has a defined namespace, based on the dbkey of the main context, plus index number 000.
            if (version.isAtLeast(4, 1)) {
                String nsPrefix = buildNamespacePrefix(dbKey);
                return nsPrefix + "000";
            }
        }
        
        // For earlier WGA versions the root portlet has an empty string namespace
        // (which is problematic when multiple portlet application dbs share the same perso db)
        return "";
    }

    @Override
    public void prepareNewProfile(WGUserProfile profile) throws WGAPIException {
    }

    @Override
    public List<String> getItemNames(WGPortlet portlet) {
        
        String itemPrefix = getItemPrefix(portlet);
        List<String> list = Groq
        .selectFrom(_profile.getItems().keySet())
        .whereStartsWith(itemPrefix)
        .substring(itemPrefix.length());
        return list;
    }

    @Override
    public Object getItemValue(WGPortlet portlet, String name) throws WGAPIException {
        return _docImpl.getItemValue(getItemPrefix(portlet) + name);
    }

    @Override
    public void removeItem(WGPortlet portlet, String name) throws WGAPIException {
        startProfileEdit();
        _docImpl.removeItem(getItemPrefix(portlet) + name);
        
    }

    @Override
    public void setItemValue(WGPortlet portlet, String name, Object value) throws WGAPIException {
        startProfileEdit();
        _docImpl.setItemValue(getItemPrefix(portlet) + name, value);
        
    }
    
    private String getItemPrefix(WGPortlet portlet) {
        return portlet.getKey() + "_";
    }

    @Override
    public boolean hasItem(WGPortlet portlet, String name) throws WGAPIException {
        return _docImpl.hasItem(getItemPrefix(portlet) + name);
    }

    @Override
    public WGPortlet createPortlet(String appDb, WGPortlet parent) {
        return createPortletDefaultBean(null, appDb, (parent != null ? parent.getKey() : null));
    }
    


    
    @Override
    public WGPortlet getPortletByName(String appDb, WGPortlet parentPortlet, String name) throws WGAPIException {

        String key = (String) getItemValue(parentPortlet, PORTLETNAME_ITEM_PREFIX + name);
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
    
    
    protected WGPortletDefaultBean createPortletDefaultBean(String id, String appDb, String parentId) {
        return new WGPortletDefaultBean(appDb, id, parentId);
    }

    protected void insertPortletId(WGPortletDefaultBean portlet, String id) {
        portlet.setKey(id);
    }
    
    @Override
    public String getApplicationId(WGDatabase appDb) {
        return appDb.getDbReference();
    }

}
