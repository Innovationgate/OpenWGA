package de.innovationgate.wga.services.rest;

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.v1.resources.RESTEntry;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class CustomCollectionEnhancer implements CollectionEnhancer {
    
    private App _app;
    private String _enhancerName;
    private WGA _wga;

    public CustomCollectionEnhancer(WGA wga, App app, String enhancerName) {
        _wga = wga;
        _app = app;
        _enhancerName = enhancerName;
    }

    @Override
    public void enhance(RESTEntry ref, WGDocument contextDoc) throws Throwable {

        StringBuffer scriptCode = new StringBuffer();
        scriptCode.append("var enhancerMod = WGA.design().resolveSystemScriptModule('rest:collection-enhancers', 'tmlscript', false);");
        scriptCode.append("if (enhancerMod == null) return;");
        scriptCode.append("var enhancers = WGA.createObject(enhancerMod);");         
        scriptCode.append("if (enhancers." + _enhancerName + ") enhancers." + _enhancerName + "(ref, referenceDocument);");
        
        Map<String,Object> objects = new HashMap<String, Object>();
        objects.put("ref", ref);
        if (contextDoc != null) {
            objects.put("referenceDocument", contextDoc);
        }
        
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
        
        Map<String,Object> params = new HashMap<String,Object>();
        params.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "WGA Web Services REST collection enhancer for app '" +  _app.getDbKey() + "'");
        params.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, _app.design().getDesignContext().getBaseReference());
        params.putAll(objects);
        
        Context tmlCx;
        if (contextDoc instanceof WGContent) {
            tmlCx = _wga.createTMLContext((WGContent) contextDoc);
        }
        else {
            tmlCx = _app.createTMLContext();
        }
        
        ExpressionResult result = engine.evaluateExpression(scriptCode.toString(), (TMLContext) tmlCx, RhinoExpressionEngine.TYPE_SCRIPT, params);
        if (result.isError()) {
            if (result.getException().getCause() != null) {
                throw result.getException().getCause();
            }
            else {
                throw result.getException();
            }
        }  
        
    }

}
