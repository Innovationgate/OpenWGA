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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;

public class TMLOptionPreserver {
    
    private TMLDesignContext _tag;
    private Map<String,TMLOption> _options = new HashMap<String,TMLOption>();

    public TMLOptionPreserver(TMLDesignContext tag) {
        _tag = tag;
    }
    
    public void preserve(String name, Object valueToSet, String scope) throws WGException {

        _options.put(name, _tag.getOption(name));
        if (valueToSet != null) {
            _tag.setOption(name, valueToSet, scope);
        }
        else {
            _tag.removeOption(name);
        }
    }
    
    public void preserve(String name, Object valueToSet) throws WGException {
        preserve(name, valueToSet, TMLOption.SCOPE_GLOBAL);
    }
    
    public void preserveKeepingValue(String name) throws WGException {
        _options.put(name, _tag.getOption(name));
    }
    
    public void restore() throws WGException {
        
        Iterator<Map.Entry<String,TMLOption>> tls = _options.entrySet().iterator();
        while (tls.hasNext()) {
            Map.Entry<String,TMLOption> entry = tls.next();
            try {
                String name = (String) entry.getKey();
                TMLOption oldOption = entry.getValue();
                
                if (oldOption != null) {
                    _tag.setOption(name, oldOption.getValue(), oldOption.getScope());
                }
                else {
                    _tag.removeOption(name);
                }
            }
            catch (RuntimeException e) {
                Logger.getLogger("wga.rhino").error("Error restoring WebTML options", e);
                // We don't want individual errors from keeping the preserver to keep on restoring
            }    
            
        }
        
    }
    
}
