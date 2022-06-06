package de.innovationgate.wga.additional_script_langs.less;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.json.JsonObject;
import javax.json.JsonValue;

import com.github.sommeri.less4j.Less4jException;
import com.github.sommeri.less4j.LessCompiler;
import com.github.sommeri.less4j.LessCompiler.CompilationResult;
import com.github.sommeri.less4j.LessFunction;
import com.github.sommeri.less4j.LessProblems;
import com.github.sommeri.less4j.LessSource;
import com.github.sommeri.less4j.core.DefaultLessCompiler;
import com.github.sommeri.less4j.core.ast.Expression;
import com.github.sommeri.less4j.core.ast.FunctionExpression;
import com.github.sommeri.less4j.core.ast.IdentifierExpression;

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

public class LessPostProcessor implements PostProcessor{

	WGA _wga;
	
	public class WGAFileURL implements LessFunction{

		Design _design;
		public WGAFileURL(Design design){
			_design = design;
		}
		
		@Override
		public boolean canEvaluate(FunctionExpression call, List<Expression> parameters) {
			return call.getName().equals("wga_file_url");
		}

		@Override
		public Expression evaluate(FunctionExpression call, List<Expression> parameters, Expression evaluatedParameter, LessProblems problems) {

			String url = "";
			try{
				//do: design.resolve(db, container).fileURL(name);
				if(parameters.size()==1){
					url = _design.fileURL(parameters.get(0).toString());
				}
				else if(parameters.size()==2){
					url = _design.resolve(parameters.get(0).toString()).fileURL(parameters.get(1).toString());
				}
				else if(parameters.size()==3){
					Design d = _design.resolve(parameters.get(0).toString(), parameters.get(1).toString());
					if(d==null){
						_wga.getLog().error("less-processor: wga_file_url() design not found");
						problems.addWarning(call, "less-processor: wga_file_url() design not found");
					}
					else url = d.fileURL(parameters.get(2).toString());
				}
				else{
					_wga.getLog().warn("less-processor: wga_file_url([[dbkey], container], filename) with wrong parameter");
					problems.addWarning(call, "wga_file_url([[dbkey], container], filename) with wrong parameter.");
				}
			} catch (WGException e) {
				e.printStackTrace();
			}
			return new IdentifierExpression(call.getUnderlyingStructure(), url);
			
		}
		
	}
	
	public class WGALessSource extends LessSource {

		ResourceRef _ref;
		PostProcessResult _result;
		
		public WGALessSource(ResourceRef ref, PostProcessResult result) {
			_ref = ref;
			_result = result;
		}

		@Override
		public LessSource relativeSource(String path) throws FileNotFound, CannotReadFile, StringSourceException {
			ResourceRef ref;
			try {
				ref = new ResourceRef(_ref, path);
				WGDesignDocument doc = ref.getDesignDocument();
				if(doc!=null)
					_result.addIntegratedResource(doc);
				return new WGALessSource(ref, _result);
			} catch (WGException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return null;
		}

		@Override
		public String getContent() throws FileNotFound, CannotReadFile {
			try {
				String code = _ref.getCode();
				if(code==null){
					_wga.getLog().error("less processor: unable to @import '" + _ref.toString() + "'");
					//throw new FileNotFound();		// breaks complete compilation
					return "";
				}
				return code;
			} catch (WGException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return null;
		}

		@Override
		public byte[] getBytes() throws FileNotFound, CannotReadFile {
			return this.getContent().getBytes();
		}

		@Override
		public String getName() {
			return _ref.toString();
		}
		
	}
	
	@Override
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
            	JsonObject values = (JsonObject) wga.tmlscript().importJsonData(result);
            	for(Entry<String, JsonValue> value: values.entrySet()){
            		vars.put("@"+value.getKey(), value.getValue().toString());
            	}
            }
            else vars.putAll((Map<String,Object>)result);
		}
    	
    	if(wga.getRequest()!=null){
    		String host = wga.getRequest().getServerName().replace(".", "_");
    		vars.put("requested_host", host);
    	}
    	
        data.setCacheQualifier((Serializable)vars);
		
	}

	@SuppressWarnings("unchecked")
	@Override
	public PostProcessResult postProcess(WGA wga, PostProcessData data, String code) throws WGException {
		_wga = wga;
		PostProcessResult result = new PostProcessResult();
		result.setCode("");		
		
        WGDesignDocument doc = data.getDocument();
        Design design = wga.design(doc.getDatabase().getDbReference()).resolve(doc.getName());
        ResourceRef ref = new ResourceRef(design, ResourceRef.TYPE_CSS);
        
        try {
        	LessCompiler compiler = new DefaultLessCompiler();
        	LessCompiler.Configuration configuration = new LessCompiler.Configuration();
        	configuration.getSourceMapConfiguration().setLinkSourceMap(false);
        	configuration.setIeCompatibility(false);

        	_wga.getLog().info("Less4j " + getVersion(compiler) + " postProcessing " + ref.toString());
        	
        	if (data.getCacheQualifier() != null) {
                configuration.addExternalVariables((Map<String,String>) data.getCacheQualifier());
        	}
        	
        	configuration.addCustomFunction(new WGAFileURL(design));
        	
        	if(!WGACore.isDevelopmentModeEnabled() && data.isCompress())
        		configuration.setCompressing(true);
        	
        	CompilationResult compilationResult = compiler.compile(new WGALessSource(ref, result), configuration);
        	
			result.setCode(compilationResult.getCss());
			result.addIntegratedResource(data.getDocument());
		} 
        catch (Less4jException e) {
        	_wga.getLog().error(e.getMessage());
        	data.setCacheable(false);
			return result;
		}
        
		return result;
	}

	  public String getVersion(Object o) {
		    String path = "/version.prop";

		    InputStream stream = o.getClass().getResourceAsStream(path);
		    if (stream == null)
		      return "UNKNOWN";
		    Properties props = new Properties();
		    try {
		      props.load(stream);
		      stream.close();
		      return (String) props.get("version");
		    } catch (IOException e) {
		      return "UNKNOWN";
		    }
	  }

}
