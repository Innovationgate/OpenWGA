package de.innovationgate.wga.additional_script_langs.wcss;

import java.io.IOException;
import java.util.ArrayList;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wga.additional_script_langs.wcss.WcssCompiler.WcssFunction;
import de.innovationgate.wga.additional_script_langs.wcss.WcssCompiler.WcssResource;
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
        
        WGAResource res = new WGAResource(result, ref);
		WcssCompiler compiler = new WcssCompiler(res);
    	if(!WGACore.isDevelopmentModeEnabled() && data.isCompress())
    		compiler.setCompressing(true);

		try {
			result.setCode(compiler.compile());
		} catch (IOException e) {
			_wga.getLog().error("unable to compile wcss resource", e);
		}
        
        return result;
        
	}

	private class WGAResource implements WcssResource{

		private PostProcessResult _result;
		private ResourceRef _ref;
		
		WGAResource(PostProcessResult result, ResourceRef ref){
			_result = result;
			_ref = ref;
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
			} catch (WGException | IOException e) {
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
	 * custom WcssFunction used as wga_file_url()
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

