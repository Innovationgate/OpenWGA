package de.innovationgate.wga.additional_script_langs.jsmin;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Iterator;

import org.apache.commons.io.IOUtils;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
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
				convertedCode.append(line + "\n");
				if(line.trim().startsWith(IMPORT_SCRIPT)){
					String path = line.substring(IMPORT_SCRIPT.length()).trim();
					ResourceRef ref = new ResourceRef(base_ref, path);
					String mod_code = ref.getCode();
					if(mod_code!=null){
						convertedCode.append("// @import " + ref.toString() + "\n");
						convertedCode.append(mod_code + "\n");
						if(ref.getDesignDocument()!=null)
							result.addIntegratedResource(ref.getDesignDocument());
					}
					else{
						convertedCode.append("// JS Module '" + ref.toString() + "' not found.\n");
						wga.getLog().error("jsmin: unable to find JS module '" + ref.toString() + "'");
					}
				}
			}
		} catch (IOException e) {
			convertedCode.append("// unable to read JSMin module\n");
			wga.getLog().error("jsmin: unable to read module");
		}

		if(WGACore.isDevelopmentModeEnabled()){
        	result.setCode(convertedCode.toString());
        }
		else{
	        StringWriter minOut = new StringWriter();
	        UglifyJsProcessor engine = new UglifyJsProcessor();
	        ClassLoader oldLoader = Thread.currentThread().getContextClassLoader();
	        Thread.currentThread().setContextClassLoader(wga.server().getLibraryLoader());
	        try {
	        	engine.process(new StringReader(convertedCode.toString()), minOut);
	        	result.setCode(minOut.toString());
			} catch (Exception e) {
				wga.getLog().error("UglifyJsProcessor: unable to process JS-module " + design.toString());
				wga.getLog().error(e.toString());
				result.setCode(convertedCode.toString());
			}
	        finally{
	        	Thread.currentThread().setContextClassLoader(oldLoader);
	        }	        
		}
		
        return result;
	}

}
