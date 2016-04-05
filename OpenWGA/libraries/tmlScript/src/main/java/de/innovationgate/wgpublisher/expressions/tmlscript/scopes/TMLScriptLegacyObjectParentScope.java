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
package de.innovationgate.wgpublisher.expressions.tmlscript.scopes;


import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.WrappedException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * A class to be used as parent scope for TMLScript object definitions and their objects.
 * This will redirect the parent scope of constructed TMLScript objects to the currenty
 * valid initial TMLContext of the current script.
 * This keeps TMLScript-Objects from keeping their initial TMLContext-Object over multiple
 * scripts/requests. 
 */
public class TMLScriptLegacyObjectParentScope  extends TMLScriptObjectParentScope {

    private static final long serialVersionUID = 1L;
    private String _designDb;
    private TMLScriptRootScope _customRootScope = null;
    private boolean _masterSessionObject = false;
    private WGACore _core;
    
    public TMLScriptLegacyObjectParentScope(WGACore core, TMLAction action, String designDBKey, boolean masterSessionObject) throws WGException {
        super(action);
        _core = core;
        _designDb = designDBKey;
        _objectDefinition = action;
        _masterSessionObject = masterSessionObject;
    }
    
    public String getClassName() {
        return "TMLScriptLegacyObjectParentScope";
    }

    public Scriptable getParentScope() {
        Context context = Context.getCurrentContext();
        TMLScriptRootScope rootScope = WGAGlobal.fetchRootScope(context);
        if (rootScope != null) {
            return rootScope;
        }
                
        RhinoExpressionEngineImpl rhinoEngine = (RhinoExpressionEngineImpl) ExpressionEngineFactory.getTMLScriptEngine();

        // We are outside of any script. Might be the usage of a TMLScript object as Java Event Listener
        // We continue to use a custom scope here with designdb master session if the object also has been created under master session
        
        if (_customRootScope == null) {
            WGDatabase designDb = _core.getContentdbs().get(_designDb);
            if (_designDb == null) {
                throw new EvaluatorException("Cannot create root scope for custom TMLScript object because the design app '" + _designDb + "' is no longer connected");
            }
            try {
                if (!designDb.isSessionOpen()) {
                    if (!_masterSessionObject && !rhinoEngine.isDebugEnabled()) {
                        throw new EvaluatorException("An attempt to access a custom TMLScript object outside of TMLScript has been done. This does only work when the object has been created under master session of its design app.");
                    }
                    designDb.openSession();
                }
                TMLContext tmlContext = new TMLContext(designDb.getDummyContent(designDb.getDefaultLanguage()), _core, null, null);
                
                TMLScriptRootScopeData rootScopeData = new TMLScriptRootScopeData(rhinoEngine.getOrCreateSharedScope(context, _core));
                _customRootScope = new TMLScriptRootScope(rootScopeData, tmlContext, false);
            }
            catch (Exception e) {
                throw new WrappedException(e);
            }
            
        }
        return _customRootScope;
        

        
        
    }

    public String getDesignDb() {
        return _designDb;
    }


}
