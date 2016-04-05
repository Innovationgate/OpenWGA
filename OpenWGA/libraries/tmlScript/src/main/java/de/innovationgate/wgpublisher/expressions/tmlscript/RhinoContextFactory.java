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

import org.apache.log4j.Logger;

import de.innovationgate.ext.org.mozilla.javascript.Callable;
import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.ContextFactory;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.WrapFactory;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.wrapping.RhinoWrapFactory;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

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
public class RhinoContextFactory extends ContextFactory {

    private static final String SYSPROPERTY_OPTIMIZATION = "de.innovationgate.wga.tmlscript.optimization";
    public static int defaultOptimizationLevel = 9;
    
    static {
        
        String optLevel = System.getProperty(SYSPROPERTY_OPTIMIZATION);
        if (optLevel != null) {
            try {
               defaultOptimizationLevel = Integer.parseInt(optLevel);
               Logger.getLogger("wga").info("Using TMLScript/Rhino default optimization level " + defaultOptimizationLevel);
            }
            catch (NumberFormatException e) {
               Logger.getLogger("wga").error("Cannot parse system property " + SYSPROPERTY_OPTIMIZATION + " as a number: " + optLevel);
            }
        }
        
    }
    
    private WrapFactory wrapFactory = new RhinoWrapFactory();
    public RhinoContextFactory() {
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.ContextFactory#makeContext()
     */
    protected Context makeContext() {
        RhinoContext cx = new RhinoContext(this);
        cx.setOptimizationLevel(defaultOptimizationLevel);
        cx.setGeneratingDebug(defaultOptimizationLevel <= 0);
        cx.setWrapFactory(wrapFactory);
        cx.setLanguageVersion(Context.VERSION_1_8);
        cx.setInstructionObserverThreshold(100000);
        return cx;
    }
    
    protected void observeInstructionCount(Context cx, int instructionCount)
    {
        RhinoContext rCon = (RhinoContext) cx;
        if (rCon.isWebsiteScript == false) {
            return;
        }
        
        long currentTime = System.currentTimeMillis();
        if (rCon.endTime != 0 && currentTime > rCon.endTime) {
            // More then n seconds from Context creation time:
            // it is time to stop the script.
            // Throw Error instance to ensure that script will never
            // get control back through catch or finally.
            throw new TimeoutError("TMLScript timed out after " + rCon.scriptTimeout + " milliseconds", rCon.scriptTimeout);
        }
        
        TMLContext tmlContext = WGAGlobal.fetchInitialContext(rCon);
        if (tmlContext != null && tmlContext.getrequest() != null) {
            Boolean cancelled = (Boolean)tmlContext.getrequest().getAttribute(WGACore.ATTRIB_REQUEST_CANCELLED);
            if (cancelled != null && cancelled.booleanValue()) {
                throw new RequestCancelledError();
            }
        }
    }
    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.ContextFactory#hasFeature(de.innovationgate.ext.org.mozilla.javascript.Context, int)
     */
    protected boolean hasFeature(Context arg0, int code) {
        
        switch (code) {
        
        	case Context.FEATURE_DYNAMIC_SCOPE: {
        	    return true;
        	}
        	
        	case Context.FEATURE_E4X: {
        	    return true;
        	}
        	
        	case Context.FEATURE_MEMBER_EXPR_AS_FUNCTION_NAME: {
        	    return true;
        	}
        	
        	case Context.FEATURE_LOCATION_INFORMATION_IN_ERROR: {
        	    return true;
        	}
            
        }
        
        return super.hasFeature(arg0, code);
    }
    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.ContextFactory#doTopCall(de.innovationgate.ext.org.mozilla.javascript.Callable, de.innovationgate.ext.org.mozilla.javascript.Context, de.innovationgate.ext.org.mozilla.javascript.Scriptable, de.innovationgate.ext.org.mozilla.javascript.Scriptable, java.lang.Object[])
     */
    protected Object doTopCall(Callable callable, Context cx, Scriptable scope, Scriptable thisObj, Object[] args) {
        RhinoContext rcx = (RhinoContext) cx;
        rcx.startTime = System.currentTimeMillis();
        if (rcx.scriptTimeout != 0) {
            rcx.endTime = rcx.startTime + rcx.scriptTimeout;
        }

        return super.doTopCall(callable, cx, scope, thisObj, args);
    }

}
