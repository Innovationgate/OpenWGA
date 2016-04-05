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

package de.innovationgate.wga.common;

import java.util.HashMap;
import java.util.Map;

public class TMLScriptDefinitions {
    
    public enum MethodModificatorType {
        SYNCHRONIZED,
        LISTENER,
        NONISOLATED
    }
    
    
    /**
     * Additional methods on JS object Function that modify the function definition and return the originating function again
     */
    public static final Map<String,MethodModificatorType> FUNCTION_MODIFICATION_METHODS = new HashMap<String, TMLScriptDefinitions.MethodModificatorType>();
    static {
        FUNCTION_MODIFICATION_METHODS.put("listenToAppEvents", MethodModificatorType.LISTENER);
        FUNCTION_MODIFICATION_METHODS.put("listenToPortletEvents", MethodModificatorType.LISTENER);
        FUNCTION_MODIFICATION_METHODS.put("listenToWebSocketConnect", MethodModificatorType.LISTENER);
        FUNCTION_MODIFICATION_METHODS.put("synced", MethodModificatorType.SYNCHRONIZED);
        FUNCTION_MODIFICATION_METHODS.put("nonIsolated", MethodModificatorType.NONISOLATED);
        FUNCTION_MODIFICATION_METHODS.put("directAccess", MethodModificatorType.NONISOLATED);
    }

}
