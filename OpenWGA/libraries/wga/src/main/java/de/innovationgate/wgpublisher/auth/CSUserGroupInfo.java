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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.webgate.api.auth.LabeledNamesProvider;
import de.innovationgate.webgate.api.auth.UserGroupInfo;

public class CSUserGroupInfo implements UserGroupInfo, LabeledNamesProvider {
	private Set<String> _aliasNames;
	private Map<Object, Object> _attributes = new HashMap<Object,Object>();
	private Map<String, Object> _labeledNames = new LinkedHashMap<String, Object>(); 
	private String _fullQualifiedName;
	private boolean _isGroup;
	private boolean _isUser;
    public void setAliasNames(Set<String> set) {
		_aliasNames = set;
	}

	public void setAttributes(Map<Object, Object> _attributes) {
		this._attributes = _attributes;
	}

	public void setFullQualifiedName(String qualifiedName) {
		_fullQualifiedName = qualifiedName;
	}

	public void setIsGroup(boolean isGroup) {
		this._isGroup = isGroup;
	}

	public void setIsUser(boolean isUser) {
		this._isUser = isUser;
	}

	public List<String> getAliasNames() {
		return getAliasNames(_aliasNames);
	}
	
	private List<String> getAliasNames(Set<String> aliasNames) {
		return new ArrayList<String>(aliasNames);
	}

	public Map<Object, Object> getAttributes() {
		return _attributes;
	}

	public String getFullQualifiedName() {
		return _fullQualifiedName;
	}

	public boolean isGroup() {
		return _isGroup;
	}

	public boolean isUser() {
		return _isUser;
	}

    public Object getAttributeValue(String name) {
        return _attributes.get(name);
    }

    @SuppressWarnings("unchecked")
    public List<Object> getAttributeValueList(String name) {
    
        Object val = getAttributeValue(name);
        if (val instanceof List<?>) {
            return (List<Object>) val;
        }
        else if (val != null) {
            return Collections.singletonList(val);
        }
        else {
            return Collections.emptyList();
        }
        
        
    }

    public Map<String,Object> getLabeledNames() {
        return _labeledNames;
    }

    public void setLabeledNames(Map<String, Object> labeledNames) {
        _labeledNames = labeledNames;
    }



}
