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

package de.innovationgate.wgpublisher.webtml.portlet;

import java.util.Map;

import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObjectData;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContext;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;

public class TMLPortletProcessContext extends ProcessContext{
    
    private transient Map<String,ScopeObjectData> _scopeObjects;

    public TMLPortletProcessContext(TMLPortletState tmlPortletState, ProcessContextRegistration contexts) {
        super(tmlPortletState.getProcessId(), tmlPortletState.getPortletKey(), contexts);
    }

    public Map<String, ScopeObjectData> getScopeObjects() {
        return _scopeObjects;
    }

    protected void setScopeObjects(Map<String, ScopeObjectData> map) {
        _scopeObjects = map;
    }
    
    public String getPortletKey() {
        return getParentId();
    }

}
