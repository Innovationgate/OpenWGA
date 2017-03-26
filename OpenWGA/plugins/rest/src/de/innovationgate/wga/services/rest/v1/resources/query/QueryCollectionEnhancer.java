package de.innovationgate.wga.services.rest.v1.resources.query;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.CollectionEnhancer;
import de.innovationgate.wga.services.rest.v1.resources.RESTEntry;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class QueryCollectionEnhancer implements CollectionEnhancer {

    private WGA _wga;
    private App _app;
    private Object _object;

    public QueryCollectionEnhancer(WGA wga, App app, Object obj) {
        _wga = wga;
        _app = app;
        _object = obj;
    }

    @Override
    public void enhance(RESTEntry ref, WGDocument contextDoc) throws Throwable {

        Map<String,Object> objects = new HashMap<String, Object>();
        objects.put("$ref", ref);
        if (contextDoc != null) {
            objects.put("$referenceDocument", contextDoc);
        }
        
        Context tmlCx;
        if (contextDoc instanceof WGContent) {
            tmlCx = _wga.createTMLContext((WGContent) contextDoc);
        }
        else {
            tmlCx = _app.createTMLContext();
        }
        
        TMLScript tmlScript = WGA.get(tmlCx).tmlscript();
        if (tmlScript.hasProperty(_object, "enhance")) {
            tmlScript.callMethod(_object, "enhance", objects, Arrays.asList(new Object[] {ref, contextDoc}));
        }

    }

}
