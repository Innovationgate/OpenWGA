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

import de.innovationgate.wga.server.api.WGA;

public abstract class TMLScriptGlobal {
    
    public static final int TYPE_PACKAGE_OR_CLASS = 1;
    public static final int TYPE_OBJECT = 2;
    public static final int TYPE_MANAGED = 3;
    
    private String _name;
    private Object _ref;
   
    public TMLScriptGlobal(String name, Object ref) {
        _name = name;
        _ref = ref;
        
        if (!Character.isUpperCase(_name.charAt(0))) {
            throw new IllegalArgumentException("TMLScript global name must start with an uppercase letter");
        }
    }

    public String getName() {
        return _name;
    }

    public Object getRef() {
        return _ref;
    }
    
    public abstract Object provide(WGA wga);
    
    public void afterProvisioning(WGA wga) {
    }

}
