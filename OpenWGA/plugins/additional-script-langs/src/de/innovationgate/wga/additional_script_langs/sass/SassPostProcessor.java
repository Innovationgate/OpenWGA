package de.innovationgate.wga.additional_script_langs.sass;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.json.JsonObject;
import javax.json.JsonString;

import org.apache.commons.lang3.StringUtils;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.PostProcessor;

public class SassPostProcessor implements PostProcessor {

    @Override
    public PostProcessResult postProcess(WGA wga, PostProcessData data, String code) throws WGException {
    
        ClassLoader oldLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(this.getClass().getClassLoader());
        try {
            PostProcessResult result = new PostProcessResult();
            StringBuilder completeCode = new StringBuilder();
            WGDesignDocument doc = data.getDocument();

            result.addIntegratedResource(doc);
            
            if (data.getCacheQualifier() != null) {
                @SuppressWarnings("unchecked")
                Map<Object,Object> variables = (Map<Object,Object>) data.getCacheQualifier();
                for (Map.Entry<Object,Object> varEntry : variables.entrySet()) {
                    completeCode.append("$").append(String.valueOf(varEntry.getKey())).append(":");
                    completeCode.append(formatValue(varEntry.getValue())).append(";");
                    completeCode.append("\n");
                }
            }
            
            completeCode.append(code);

    		try {
    	        Design design = wga.design(doc.getDatabase().getDbReference()).resolve(doc.getName());
    	        Context cx = wga.createTMLContext(data.getDocument().getDatabase(), design);
    	        SassEngine engine = new SassEngine(wga, design, cx, doc.getDesignReference().toString(), result);
    	        result.setCode(engine.process(completeCode.toString()));
    		} catch (Exception e) {
    			// Unable to process SCSS-Source: don't cache.
    			data.setCacheable(false);
    			result.setCode(StringUtils.EMPTY);
    		}

            return result;
        }
        catch (WGException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception post-processing " + data.getDocument().getDocumentKey(), e);
        }
        finally {
            Thread.currentThread().setContextClassLoader(oldLoader);
        }
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
    	if(wga.getRequest()!=null){
    		String host = wga.getRequest().getServerName().replace(".", "_");
    		//wga.getLog().info("requested host: " + host);
    		vars.put("requested_host", host);
    	}
        data.setCacheQualifier((Serializable)vars);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
	private String formatValue(Object value) {

        if (value instanceof List<?>) {
            return WGUtils.serializeCollection((List) value, " ", new ObjectFormatter() {
                @Override
                public String format(Object obj) throws FormattingException {
                    return formatValue(obj);
                }
            }, false);            
        }
        else if (value instanceof Map<?,?>) {
            return "(" + WGUtils.serializeCollection(((Map<?,?>) value).entrySet(), ", ", new ObjectFormatter() {
                @Override
                public String format(Object obj) throws FormattingException {
                    Map.Entry<?,?> entry = (Map.Entry<?,?>) obj; 
                    return formatValue(entry.getKey()) + ": " + entry.getValue();
                }
            }, false) + ")";
        }
        else if (value instanceof JsonString) {
        	return ((JsonString) value).getString();
        }
        
        return String.valueOf(value);
    }
    
}
