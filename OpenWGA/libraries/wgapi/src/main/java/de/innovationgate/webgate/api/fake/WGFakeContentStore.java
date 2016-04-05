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

package de.innovationgate.webgate.api.fake;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.webgate.api.WGACLCore;
import de.innovationgate.webgate.api.WGACLEntry;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureReturnHierarchyCount;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureSequenceProvider;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGPersonalisationDatabaseCore;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.wga.common.Constants;

/**
 * An actual behaviour-empty implementation usable as content store which cannot store any data
 */
public class WGFakeContentStore extends WGFakeDatabase implements WGDatabaseCoreFeatureReturnHierarchyCount, WGDatabaseCoreFeatureSequenceProvider, AuthenticationSourceListener {
    
    public class ACLEntryImpl implements WGACLEntry {

        private String _name;
        private int _type;
        private int _level;
        private String _flags;
        public String getName() {
            return _name;
        }
        public int getType() {
            return _type;
        }
        public int getLevel() {
            return _level;
        }
        public String getFlags() {
            return _flags;
        }
        protected void setName(String name) {
            _name = name;
        }
        protected void setType(int type) {
            _type = type;
        }
        public void setLevel(int level) {
            _level = level;
        }
        public void setFlags(String flags) {
            _flags = flags;
        }
        
        
    }
    
    
    public class ACLImpl implements WGACLCore {
        
        private Map<String,WGACLEntry> _entries = new ConcurrentHashMap<String,WGACLEntry>();

        @Override
        public List<? extends WGACLEntry> getAllEntries() throws WGBackendException {
            return new ArrayList<WGACLEntry>(_entries.values());
        }

        @Override
        public WGACLEntry getEntry(String name) throws WGBackendException {
            return _entries.get(name.toLowerCase());
        }

        @Override
        public WGACLEntry createEntry(String name, int type, int accessLevel) throws WGBackendException {
            ACLEntryImpl entry = new ACLEntryImpl();
            entry.setName(name);
            entry.setType(type);
            entry.setLevel(accessLevel);
            _entries.put(entry.getName().toLowerCase(), entry);
            return entry;
        }

        @Override
        public void remove(WGACLEntry entry) throws WGBackendException {
            _entries.remove(entry.getName().toLowerCase());
        }

        @Override
        public void save(WGACLEntry entry) throws WGBackendException {
            _entries.put(entry.getName().toLowerCase(), entry);
        }
        
    }
    
    private ACLImpl _acl = new ACLImpl();

    @Override
    public WGUserAccess openSession(AuthenticationSession authSession, Object credentials, boolean master) throws WGAPIException {

        if (master) {
            return new WGUserAccess(WGDatabase.MASTER_USERNAME, WGDatabase.ACCESSLEVEL_MANAGER);
        }
        
        // Determine access
        WGUserDetails userDetails;
        try {
            userDetails = enclosingDB.defaultBuildUserDetails(authSession);
        }
        catch (WGBackendException e) {
            try {
                closeSession();
            }
            catch (WGBackendException e1) {
                WGFactory.getLogger().error(e1);
            }
            throw e;
        }
        if (userDetails.getAccessLevel() <= WGDatabase.ACCESSLEVEL_NOACCESS) {
            try {
                closeSession();
            }
            catch (WGBackendException e) {
                WGFactory.getLogger().error(e);
            }
        }

        return userDetails;
        
    }

    @Override
    public void beginUpdate() throws WGBackendException {
        throw new WGBackendException("Not supported");
    }

    @Override
    public boolean isContentTypeUsed(WGContentType ct) throws WGAPIException {
        return false;
    }

    @Override
    public boolean isLanguageUsed(WGLanguage lang) throws WGAPIException {
        return false;
    }

    @Override
    public boolean isBackendServiceSupported(String serviceName) {
        return false;
    }

    @Override
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException {
        throw new WGBackendException("Not supported");
    }
    
    @Override
    public boolean hasFeature(String feature) {

        if (feature.equals(WGDatabase.FEATURE_UNPATCHEABLE)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_GENERATES_STRUCTKEYS)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_HIERARCHICAL)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_LASTCHANGED)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_MULTILANGUAGE)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_PROVIDE_PORTLETITEM_STORAGE)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_QUERYABLE)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_SELF_PERSONALIZABLE)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_ACL_MANAGEABLE)) {
            return true;
        }
        else if (feature.equals(WGDatabase.FEATURE_FIND_UPDATED_DOCS)) {
            return true;
        }
        
        return false;
        
    }
    
    @Override
    public double getContentStoreVersion() throws WGAPIException {
        return WGDatabase.CSVERSION_WGA5;
    }
    
    @Override
    public int getContentStorePatchLevel() throws WGAPIException {
        return 0;
    }
    
    public WGDocumentCore createUserProfile(String name, int type) throws WGAPIException {
        return new WGFakeUserProfile(enclosingDB, name);
    }

    @Override
    public void authenticationDataChanged() {
    }

    @Override
    public boolean initSequence(String name, long startValue, boolean forceInit) throws WGAPIException {
        throw new WGBackendException("Not supported");
    }

    @Override
    public long incrementSequence(String name) throws WGAPIException {
        throw new WGBackendException("Not supported");
    }

    @Override
    public boolean isSequenceInitialized(String name) throws WGAPIException {
        throw new WGBackendException("Not supported");
    }

    @Override
    public long getLastSequenceIncrementValue(String name) throws WGAPIException {
        throw new WGBackendException("Not supported");
    }

    @Override
    public void deleteSequence(String name) throws WGAPIException {
        throw new WGBackendException("Not supported");
    }

    @Override
    public List<String> getUsedSequenceNames() throws WGAPIException {
        return Collections.emptyList();
    }

    @Override
    public int getChildEntryCount(WGStructEntry structEntry) throws WGAPIException {
        return 0;
    }

    @Override
    public int getRootEntryCount(WGArea area) throws WGAPIException {
        return 0;
    }
    
    @Override
    public WGACLCore getACL() {
        return _acl;
    }
    
    @Override
    public List getUpdateLogs(Comparable cutoff) {
        return Collections.emptyList();
    }

}
