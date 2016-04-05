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
package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.log4j.Logger;

public class ThreadLocalPreserver {
    
    private RhinoContext _context;
    private Map _threadlocals = new HashMap();

    public ThreadLocalPreserver(RhinoContext context) {
        _context = context;
    }
    
    public void preserve(String name, Object valueToSet) {

        _threadlocals.put(name, _context.getThreadLocal(name));
        if (valueToSet != null) {
            _context.putThreadLocal(name, valueToSet);
        }
        else {
            _context.removeThreadLocal(name);
        }
    }
    
    public void preserveKeepingValue(String name) {
        _threadlocals.put(name, _context.getThreadLocal(name));
    }
    
    public void restore() {
        
        Iterator tls = _threadlocals.entrySet().iterator();
        while (tls.hasNext()) {
            Map.Entry entry = (Map.Entry) tls.next();
            try {
                String name = (String) entry.getKey();
                Object previousValue = entry.getValue();
                
                if (previousValue != null) {
                    _context.putThreadLocal(name, previousValue);
                }
                else {
                    _context.removeThreadLocal(name);
                }
            }
            catch (RuntimeException e) {
                Logger.getLogger("wga.rhino").error("Error restoring script threadlocals", e);
                // We don't want individual errors from keeping the preserver to keep on restoring
            }    
            
        }
        
    }
    
}
