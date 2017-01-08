package de.innovationgate.wga.additional_script_langs;

import java.io.Serializable;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.json.JsonObject;

import ro.isdc.wro.config.Context;
import ro.isdc.wro.config.jmx.WroConfiguration;
import ro.isdc.wro.model.group.processor.Injector;
import ro.isdc.wro.model.group.processor.InjectorBuilder;
import ro.isdc.wro.model.resource.processor.impl.css.CssMinProcessor;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.PostProcessor;

public abstract class CssDialectsPostProcessor implements PostProcessor {

    public CssDialectsPostProcessor() {
        super();
    }

    protected abstract String compileDialect(WGA wga, PostProcessData data, String code, Injector injector, PostProcessResult result) throws WGException, WGAPIException;

    @Override
    public PostProcessResult postProcess(WGA wga, PostProcessData data, String code) throws WGException {
    
        ClassLoader oldLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(wga.server().getLibraryLoader());
        try {
            WroConfiguration config = new WroConfiguration();
            Context.set(Context.standaloneContext(), config);
            Injector injector = new InjectorBuilder().build();
            PostProcessResult result = new PostProcessResult();
            result.addIntegratedResource(data.getDocument());
            
            String out = compileDialect(wga, data, code, injector, result);
            
            
            if (isMinimizingActive()) {
                CssMinProcessor minProcessor = new CssMinProcessor();
                injector.inject(minProcessor);
                StringWriter minOut = new StringWriter();
                minProcessor.process(new StringReader(out), minOut);
                out = minOut.toString();
            }
            
            
            result.setCode(out);
            return result;
            
        }
        catch (WGException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception post-processing " + data.getDocument().getDocumentKey(), e);
        }
        finally {
            Context.unset();
            Thread.currentThread().setContextClassLoader(oldLoader);
        }
        
    }

    protected boolean isMinimizingActive() {
        return !WGACore.isDevelopmentModeEnabled();
    }

    @Override
    @SuppressWarnings("unchecked")
    public void prepare(WGA wga, PostProcessData data) throws WGException {

    	ArrayList<Design> cssVariables = wga.design(data.getDocument().getDatabase().getDbReference())
    			.resolveSystemResources("css-variables", WGDocument.TYPE_CSSJS, WGScriptModule.CODETYPE_TMLSCRIPT, false);
    	if(cssVariables.size()==0)
    		return;

    	Collections.reverse(cssVariables);		// execute @base design first followed by overlay design
    	
        Map<String,Object> extraObjects = new HashMap<String, Object>();
        extraObjects.put("cssDocument", data.getDocument());

        Map<String,Object> vars = new HashMap<String, Object>();
    	for (Design design : cssVariables) {
            Object result = wga.tmlscript().runScript(design, wga.createTMLContext(data.getDocument().getDatabase(), design), design.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT).getCode(), extraObjects);
            if (!(result instanceof Map<?,?>)) {
            	wga.getLog().error("The return type of TMLScript module '" + design + "' used to set CSS variables is no lookup table or JS object and will be ignored.");
            	continue;
            }
            if (wga.tmlscript().isNativeObject(result)) {
                vars.putAll((JsonObject) wga.tmlscript().importJsonData(result));
            }
            else vars.putAll((Map<String,Object>)result);
		}
			
        data.setCacheQualifier((Serializable)vars);
    }

}