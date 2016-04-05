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

package de.innovationgate.wgpublisher.auth;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.utils.security.HashedPassword;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.LabeledNamesProvider;

public class Login implements AuthenticationSession, LabeledNamesProvider {

    private String _distinguishedName;

    private String _password;

    private String _mailAddress;

    private String _documentkey;

    private Object _additionalData;

    private Set<String> _aliases = new HashSet<String>();
    
    private Set<String> _groups = new HashSet<String>();
    
    private Map<String,Object> _labeledNames = new LinkedHashMap<String,Object>();

    private HashedPassword _hashedPassword;
    
    private boolean _cacheable = true;
    
    public Login(String userName, String password) {
        _distinguishedName = userName;
        setPassword(password);
    }

    public Login(String userName, String password, String email, String documentkey, Set<String> aliases) {
        _distinguishedName = userName;
        setPassword(password);
        _mailAddress = email;
        _documentkey = documentkey;
        
        if (aliases != null) {
            _aliases = aliases;
        }
        
        _labeledNames.put("userdocument", new WGDocumentKey(documentkey).getName());
    }
    
    public void setGroups(Set<String> groups) {
        
        if (groups != null) {
            _groups = groups;
        }
        
    }
    
    public void setGroups(List<String> groupsList) {
        setGroups(new HashSet<String>(groupsList));
    }
    
    public void setAliases(List<String> namesList) {
        setAliases(new HashSet<String>(namesList));
    }

    /**
     * @return Returns the password.
     */
    public String getPassword() {
        return _password;
    }

    /**
     * @return Returns the userName.
     */
    public String getDistinguishedName() {
        return _distinguishedName;
    }

    /**
     * @return Returns the documentkey.
     */
    public String getDocumentkey() {
        return _documentkey;
    }

    /**
     * @return Returns the additionalData.
     */
    public Object getAdditionalData() {
        return _additionalData;
    }

    /**
     * @param additionalData
     *            The additionalData to set.
     */
    public void setAdditionalData(Object additionalData) {
        _additionalData = additionalData;
    }

    /**
     * @return Returns the email.
     */
    public String getMailAddress() {
        return _mailAddress;
    }

    /**
     * @return Returns the aliases.
     */
    public Set<String> getNames() {
        Set<String> names = new HashSet<String>(_aliases);
        names.add(_distinguishedName);
        return names;
    }

    public Set<String> getGroups() {
        return _groups;
    }

    public void logout() {
    }

    public boolean isValid() {
        return true;
    }

    /**
     * @param documentkey The documentkey to set.
     */
    public void setDocumentkey(String documentkey) {
        _documentkey = documentkey;
    }

    /**
     * @param mailAddress The mailAddress to set.
     */
    public void setMailAddress(String mailAddress) {
        _mailAddress = mailAddress;
    }

    /**
     * @param names The names to set.
     */
    public void setAliases(Set<String> names) {
        _aliases = names;
    }


    /**
     * @param password The password to set.
     */
    public void setPassword(String password) {
        _password = password;
        if (password != null) {
            _hashedPassword = new HashedPassword(password);
        }
        else {
            _hashedPassword = null;
        }
    }

    /**
     * @return Returns the aliases.
     */
    public Set<String> getAliases() {
        return _aliases;
    }

    public void addGroups(Set<String> set) {
        _groups.addAll(set);        
    }

    public String getSessionToken() {
        return null;
    }

    public Map<String,Object> getLabeledNames() {
        return Collections.unmodifiableMap(_labeledNames);
    }
    
    protected void addLabeledName(String label, Object name) {
        _labeledNames.put(label, name);
    }

    public HashedPassword getHashedPassword() {
        return _hashedPassword;
    }

    protected boolean isCacheable() {
        return _cacheable;
    }

    protected void setCacheable(boolean cacheable) {
        _cacheable = cacheable;
    }

}
