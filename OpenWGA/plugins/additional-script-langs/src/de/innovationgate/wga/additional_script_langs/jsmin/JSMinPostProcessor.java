package de.innovationgate.wga.additional_script_langs.jsmin;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.Iterator;

import javax.json.JsonObject;

import org.apache.commons.io.IOUtils;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.PostProcessor;
import ro.isdc.wro.extensions.processor.js.UglifyJsProcessor;

public class JSMinPostProcessor implements PostProcessor{

	static final String IMPORT_SCRIPT = "//@import";

	@Override
	public void prepare(WGA wga, PostProcessData data) throws WGException {		
	}

	@Override
	public PostProcessResult postProcess(WGA wga, PostProcessData data, String code) throws WGException {

		PostProcessResult result = new PostProcessResult();
		result.addIntegratedResource(data.getDocument());
		
        StringBuffer convertedCode = new StringBuffer();
        WGDesignDocument doc = data.getDocument();
        Design design = wga.design(doc.getDatabase().getDbReference()).resolve(doc.getName());
        ResourceRef base_ref = new ResourceRef(design, ResourceRef.TYPE_JS);
        try {
			Iterator<String> lines = IOUtils.readLines(new StringReader(code)).iterator();
			while(lines.hasNext()){
				String line = lines.next();
				
				if(line.trim().startsWith(IMPORT_SCRIPT)){
					String path = line.trim().substring(IMPORT_SCRIPT.length()).trim();
					ResourceRef ref = new ResourceRef(base_ref, path);
					convertedCode.append("//@import " + ref.toString() + "\n");
					if(ref.getType().equals(ResourceRef.TYPE_TMLSCRIPT)){
						Context ctx = wga.createTMLContext(data.getDocument().getDatabase(), design);
		                Object tmlscript_result = wga.tmlscript().runScript(ctx, ref.getCode());
						if(tmlscript_result!=null){
							if (wga.tmlscript().isNativeObject(tmlscript_result)) {
								JsonObject json = (JsonObject) wga.tmlscript().importJsonData(tmlscript_result);
								convertedCode.append(json.toString());
							}
							else convertedCode.append(tmlscript_result.toString());
							convertedCode.append("\n");
						}
					}
					else{
						String mod_code = ref.getJavaScriptCode(false);
						if(mod_code!=null){
							convertedCode.append(mod_code + "\n");
						}
						else{
							convertedCode.append("// JS Module '" + ref.toString() + "' not found.\n");
							wga.getLog().error("jsmin: unable to find JS module '" + ref.toString() + "'");
						}
					}
					if(ref.getDesignDocument()!=null)
						result.addIntegratedResource(ref.getDesignDocument());
				}
				else convertedCode.append(line + "\n");
			}
		} catch (Exception e) {
			convertedCode.append("// unable to read JSMin module\n");
			wga.getLog().error("jsmin: unable to read module: " + e);
			data.setCacheable(false);
		}

		if(WGACore.isDevelopmentModeEnabled() || !data.isCompress()){
        	result.setCode(convertedCode.toString());
        }
		else{
	        StringWriter minOut = new StringWriter();
	        ClassLoader oldLoader = Thread.currentThread().getContextClassLoader();
	        Thread.currentThread().setContextClassLoader(this.getClass().getClassLoader());	        
	        try {
	        	UglifyJsProcessor engine = new UglifyJsProcessor();
	        	engine.process(new StringReader(convertedCode.toString()), minOut);
	        	result.setCode(minOut.toString());
			} catch (Exception e) {
				wga.getLog().error("UglifyJsProcessor: unable to process JS-module " + design.toString(), e);
    			// Unable to process Source: don't cache.
    			data.setCacheable(false);
				result.setCode(convertedCode.toString());
			}
	        finally{
	        	Thread.currentThread().setContextClassLoader(oldLoader);
	        }	        
		}
		
        return result;
	}

}
