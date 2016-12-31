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

package de.innovationgate.wgpublisher.webtml.utils;

import java.io.Serializable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.utils.TransientWrappedMap;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObjectData;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletProcessContext;

public class ProcessContextRegistration implements Serializable {
    
    public static class RegistrationMap extends TransientWrappedMap<String,ProcessContext> {
        @Override
        protected Map<String, ProcessContext> initWrappedMap() {
            return new ConcurrentHashMap<String, ProcessContext>();
        }
    }
    
    private RegistrationMap _reg = new RegistrationMap();
    
    public String dump() {
        
        StringBuilder out = new StringBuilder();
        int countContextes = 0;
        int countEntries = 0;
        int countScopeObjects = 0;
        
        for (ProcessContext pc : _reg.values()) {
            countContextes++;
            out.append("- ").append(pc.getClass().getName()).append(" ").append(pc.getProcessId()).append(", Parent ID ").append(pc.getParentId()).append("\n");
            for (Map.Entry<String,Object> pcEntry : pc.entrySet()) {
                countEntries++;
                out.append("-- Key '").append(pcEntry.getKey()).append("': ").append(pcEntry.getValue()).append(" (").append(pcEntry.getValue().getClass().getName()).append(")\n");
            }
            if (pc instanceof TMLPortletProcessContext) {
                TMLPortletProcessContext ppc = (TMLPortletProcessContext) pc;
                if (ppc.getScopeObjects() != null) {
                    for (Map.Entry<String,ScopeObjectData> pcEntry : ppc.getScopeObjects().entrySet()) {
                        countScopeObjects++;
                        out.append("-- Design '").append(pcEntry.getKey()).append("'\n");
                    }
                }
            }
        }
        
        out.append(String.format("No. of contextes: %d\n", countContextes));
        out.append(String.format("No. of entries: %d\n", countEntries));
        out.append(String.format("No. of scope objects: %d\n", countScopeObjects));
        
        
        return out.toString();
        
    }
    
    
    public ProcessContext getProcessContext(String id) {
        return _reg.get(id);
    }
    
    public void setProcessContext(String id, ProcessContext pc) {
        _reg.put(id, pc);
    }
    
    public ProcessContext removeProcessContext(String id) {
        return _reg.remove(id);
    }
    
    public int size() {
        return _reg.size();
    }
    
    /**
     * @deprecated For downward compatibility for D&D file upload functionality
     */
    public ProcessContext get(String id) {
        return getProcessContext(id);
    }

}
