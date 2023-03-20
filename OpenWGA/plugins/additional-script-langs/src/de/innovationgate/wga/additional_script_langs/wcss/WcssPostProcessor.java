package de.innovationgate.wga.additional_script_langs.wcss;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.json.JsonObject;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.PostProcessor;

public class WcssPostProcessor implements PostProcessor{

	WGA _wga;
	
	@Override
	public void prepare(WGA wga, PostProcessData data) throws WGException {
			
    	ArrayList<Design> cssVariables = wga.design(data.getDocument().getDatabase().getDbReference())
    			.resolveSystemResources("css-variables", WGDocument.TYPE_CSSJS, WGScriptModule.CODETYPE_TMLSCRIPT, false);

    	Collections.reverse(cssVariables);		// execute @base design first followed by overlay design
    	
        Map<String,Object> extraObjects = new HashMap<String, Object>();
        extraObjects.put("cssDocument", data.getDocument());

        Map<String,Object> vars = new HashMap<String, Object>();
        vars.put("processor", "wcss");
    	if(wga.getRequest()!=null){
    		String host = wga.getRequest().getServerName().replace(".", "_");
    		vars.put("requested_host", host);
    	}

    	for (Design design : cssVariables) {
            Object result = wga.tmlscript().runScript(design, wga.createTMLContext(data.getDocument().getDatabase(), design), design.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT).getCode(), extraObjects);
            
            if (!wga.tmlscript().isNativeObject(result)) {
            	wga.getLog().error("The return type of TMLScript module '" + design + "' used to set CSS variables is no JS-Object and will be ignored.");
            	continue;            	
            }
            else vars.putAll((JsonObject) wga.tmlscript().importJsonData(result));

		}
    	
        data.setCacheQualifier((Serializable) vars);
			
	}

	@Override
	public PostProcessResult postProcess(WGA wga, PostProcessData data, String code) throws WGException {
		
		_wga = wga;
		
		PostProcessResult result = new PostProcessResult();
		result.setCode("");		
		
        WGDesignDocument doc = data.getDocument();
        Design design = wga.design(doc.getDatabase().getDbReference()).resolve(doc.getName());
        ResourceRef ref = new ResourceRef(design, ResourceRef.TYPE_CSS);

        WcssCompiler.registerCustomFunction("wga_file_url", new WGAFileURL());
        WcssCompiler.registerCustomFunction("fileurl", new WGAFileURL());

        if (data.getCacheQualifier() != null) {
        	@SuppressWarnings("unchecked")
			Map<Object,Object> variables = (Map<Object,Object>) data.getCacheQualifier();
        	HashMap<String,String> vars = new HashMap<String,String>();
        	for(Map.Entry<Object, Object> entry: variables.entrySet()){
        		vars.put("$"+entry.getKey().toString(), entry.getValue().toString());
        	}
			WcssCompiler.registerVars(vars);
		}
		        
        WGAResource res = new WGAResource(result, ref);
		WcssCompiler compiler = new WcssCompiler(res);
		
    	if(!WGACore.isDevelopmentModeEnabled() && data.isCompress())
    		compiler.setCompressing(true);

		try {
			result.setCode(compiler.compile());
			result.addIntegratedResource(data.getDocument());
		} catch (IOException e) {
			_wga.getLog().error("unable to compile wcss resource", e);
			data.setCacheable(false);
		}
        
        return result;
        
	}

	private class WGAResource extends WcssResource{

		private PostProcessResult _result;
		private ResourceRef _ref;
		
		WGAResource(PostProcessResult result, ResourceRef ref){
			_result = result;
			_ref = ref;
			try {
				if(_ref.getCode()==null)
					_ref.setResourceName("_"+_ref.getResourceName());
			} 
			catch (WGException | IOException e) {}
		}
		
		public ResourceRef getResourceRef(){
			return _ref;
		}
		
		public String toString(){
			return _ref.toString();
		}
		
		@Override
		public String getCode() {
			try {
				return _ref.getCode();
			} 
			catch (WGException | IOException e) {
				return "";
			}
		}

		@Override
		public String getCSSCode() {
			try {
				return _ref.getCSSCode();
			} 
			catch (WGException | IOException e) {
				return "";
			}
		}
		
		@Override
		public WcssResource resolve(String path) {
			try {
				return new WGAResource(_result, _ref.resolve(path));
			} catch (WGException e) {
				return null;
			}			
		}

		@Override
		public void addIntegratedResource() {
			try {
				_result.addIntegratedResource(_ref.getDesignDocument());
			} catch (WGException | IOException e) {
			}
		}

	}
	
	/*
	 * custom WcssFunction used as fileurl() or wga_file_url()
	 */
	private class WGAFileURL implements WcssFunction{

		@Override
		public String execute(WcssResource res, ArrayList<String> params) {
			String url = "";
			try {
				Design design = ((WGAResource)res).getResourceRef().getDesign();

				//do: design.resolve(db, container).fileURL(name);
				if(params.size()==1){
					url = design.fileURL(params.get(0));
				}
				else if(params.size()==2){
					url = design.resolve(params.get(0)).fileURL(params.get(1));
				}
				else if(params.size()==3){
					Design d = design.resolve(params.get(0), params.get(1));
					if(d==null){
						_wga.getLog().error("wcss-processor: wga_file_url() design not found: dbkey=" + params.get(0) + ", container=" + params.get(1));
					}
					else url = d.fileURL(params.get(2));
				}
				else{
					_wga.getLog().warn("wcss-processor: wga_file_url([[dbkey], container], filename) with wrong parameter");
				}
		
				return url;
				
			} catch (WGException e) {
				return "";
			}

		}
		
	}	
}

