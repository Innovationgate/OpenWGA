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
package de.innovationgate.wgpublisher.expressions;

import org.apache.log4j.Logger;

import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;

public abstract class ExpressionEngineFactory {

    public static void createEngines(WGACore core) {
        
        if (rhinoEngine != null) {
            rhinoEngine.close();
            rhinoEngine = null;
        }
        
        try {
            rhinoEngine = (RhinoExpressionEngine) WGACore.getBaseLibraryLoader().loadClass("de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl").newInstance();
            rhinoEngine.init(core);
        }
        catch (Exception e) {
            Logger.getLogger("wga").fatal("TMLScript Engine could not be created", e);
        }
        
    }
    
	public static final String ENGINE_NATIVE = "native";
	public static final String ENGINE_TMLSCRIPT = "tmlscript";

	private static ExpressionEngine nativeEngine;
	private static RhinoExpressionEngine rhinoEngine;
	public static ExpressionEngine getEngine(String type) {

		if (type.equals(ExpressionEngineFactory.ENGINE_NATIVE)) {

			if (ExpressionEngineFactory.nativeEngine == null) {
				ExpressionEngineFactory.nativeEngine = new NativeExpressionEngine();
			}

			return ExpressionEngineFactory.nativeEngine;
		}
        
		else if (type.equals(ENGINE_TMLSCRIPT)) {
			
			return getTMLScriptEngine();
			
		}
        
		else {
			return null;
		}
	}


    public static RhinoExpressionEngine getTMLScriptEngine() {
		return rhinoEngine;
	}
    
    public static void closeEngines() {
        
        if (rhinoEngine != null) {
            rhinoEngine.close();
            rhinoEngine = null;
        }
        
        if (nativeEngine != null) {
            nativeEngine.close();
            nativeEngine = null;
        }
        
    }
    
    public static void clearCaches() {
        if (rhinoEngine != null) {
            rhinoEngine.clearCache();
        }
    }
}
