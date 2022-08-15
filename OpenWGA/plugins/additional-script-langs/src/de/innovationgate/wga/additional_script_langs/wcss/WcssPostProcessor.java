package de.innovationgate.wga.additional_script_langs.wcss;

import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Map.Entry;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
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

		WcssCompiler compiler = new WcssCompiler(ref, result);
		try {
			result.setCode(compiler.compile());
		} catch (IOException e) {
			_wga.getLog().error("unable to compile wcss resource", e);
		}
        
        return result;
        
	}

	private interface WcssFunction{
		public String execute(ResourceRef ref, ArrayList<String> params) throws WGException;
	}
	
	/*
	 * custom WcssFunction used as wga_file_url()
	 */
	private class WGAFileURL implements WcssFunction{

		@Override
		public String execute(ResourceRef ref, ArrayList<String> params) throws WGException {
			String url = "";
			Design design = ref.getDesign();

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
		}
		
	}

	private class WcssCompiler{
		
		ResourceRef _ref;
		PostProcessResult _result;

		HashMap<String, WcssFunction> customFunctions = new HashMap<String, WcssFunction>();

		WcssCompiler(ResourceRef ref, PostProcessResult result){
			_ref=ref;
			_result=result;
			
			customFunctions.put("wga_file_url", new WGAFileURL());
		}
		
		public String compile() throws WGException, IOException{
			return compile("");
		}
		
		public String compile(String root_name) throws WGException, IOException{
			
			StreamTokenizer st = new StreamTokenizer(new StringReader(_ref.getCode()));

			st.resetSyntax();
			
			st.wordChars(0x20, 0xFF);
			st.wordChars('\t', '\t');
			
			st.whitespaceChars('\n', '\n');		// needed to get correct line numbers
			
			st.quoteChar('"');
			st.slashSlashComments(true);
			st.slashStarComments(true);
			st.eolIsSignificant(true);
			
			st.ordinaryChar('{');
			st.ordinaryChar('}');
			st.ordinaryChar(';');
			st.ordinaryChar(':');
			st.ordinaryChar('&');
			st.ordinaryChar('/');
			st.ordinaryChar('@');
	        
			StringBuffer code = new StringBuffer();

			StringBuffer prop = new StringBuffer();
			int token;
			while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
				if(token==StreamTokenizer.TT_EOL)
					continue;
				else if(token==StreamTokenizer.TT_WORD)
					prop.append(st.sval);
				else if((char)token == ':'){
					prop.append(":");		// sample: :root{...}
				}
				else if((char)token == '{'){
					String cssClassName = (root_name  + " " + prop.toString()).trim();
					code.append(parseCssClass(cssClassName, st));
					prop = new StringBuffer();
				}
				else if((char)token == '@'){
					code.append(parseCmd(root_name, st));
				}
				else{
					_wga.getLog().error("syntax error: " + st.toString());
				}
			}
			
	        _result.addIntegratedResource(_ref.getDesignDocument());
			return code.toString();
		}

		private String parseCmd(String name, StreamTokenizer st) throws IOException, WGException {
			int token;
			String cmd="";
			String params = "";
			
			while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
				if(token==StreamTokenizer.TT_WORD){
					if(cmd.isEmpty())
						cmd = st.sval.trim();
					else if(params.isEmpty())
						params = st.sval.trim();
				}
				else if((char)token == '"')
					params = st.sval.trim();
				else if((char)token == ';')
					break;
			}
			
			if(cmd.equalsIgnoreCase("import")){
				ResourceRef ref = _ref.resolve(params); 
				if(ref.getDesignDocument()!=null){
					WcssCompiler compiler = new WcssCompiler(ref, _result);
					return compiler.compile(name);
				}
				else _wga.getLog().error("@import: ResourceRef not found: " + ref);
			}
			else _wga.getLog().error("unknown command @" + cmd);
			
			return "";
		}

		private String parseCssClass(String name, StreamTokenizer st) throws IOException, WGException {

			StringBuffer result = new StringBuffer();
			result.append(name);
			result.append("{\n");
			
			StringBuffer subclasses = new StringBuffer();
			String classDivider = " ";
			StringBuffer prop = new StringBuffer();
			int token;
			while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
				if(token==StreamTokenizer.TT_EOL){
					if(!prop.toString().trim().isEmpty()){
						_wga.getLog().error("syntax error parseCssClass " + prop.toString().trim() + ": " + st.toString());
						prop = new StringBuffer();
					}
					continue;
				}
				else if((char)token == '@'){
					subclasses.append(parseCmd(name, st));
				}
				else if(token==StreamTokenizer.TT_WORD)
					prop.append(st.sval);
				else if((char)token == '&'){
					classDivider="";
				}
				else if((char)token == ':'){
					if(prop.toString().trim().isEmpty() || prop.toString().trim().equals(":"))
						prop.append(":");	// usecase: &:hover{...}
					else{
						result.append(parseValue("\t"+prop.toString().trim(), st));		// returns "prop:value;"
						prop = new StringBuffer();
					}
				}
				else if((char)token == '{'){
					subclasses.append(parseCssClass(name + classDivider + prop.toString().trim(), st));
					prop = new StringBuffer();
					classDivider= " ";
				}
				else if((char)token == '}')
					break;
				else {
					_wga.getLog().error("syntax error parseCssClass: " + st.toString());
				}
			}
			if(token == StreamTokenizer.TT_EOF)
				_wga.getLog().error("syntax error parseCssClass: missing }. " + st.toString());
			
			result.append("}\n");
			result.append(subclasses);
			return result.toString();
		}

		private String parseValue(String name, StreamTokenizer st) throws IOException, WGException {
			StringBuffer result = new StringBuffer();
			StringBuffer value = new StringBuffer();
			int token;
			while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
				if(token==StreamTokenizer.TT_WORD)
					value.append(st.sval);
				else if((char)token == '/'){
					value.append("/");		// usecase: background-image: url(/path/to/file)
				}
				else if((char)token == '"'){
					value.append("\"" + st.sval + "\"");	// usecase: :after{content: "text"};
				}
				else if((char)token == '{'){
					// multiple properties
					result.append(parseProps(name, st));	// list of props: background{image:...;position:...}
					break;
				}
				else if(token == StreamTokenizer.TT_EOL){
					result.append(name + ": " + value.toString().trim() + ";\n");
					_wga.getLog().warn(st.toString() + " - semicolon expected after token '" + name.trim() + ":" + value.toString().trim() + "'");
					break;
				}
				else if((char)token == ';'){
					// finished property definition
					String theValue = value.toString().trim();
					
					// parse for custom functions
					theValue = replaceCustomFunctions(theValue);
					
					result.append(name + ": " + theValue + ";\n");
					break;
				}
				else {
					_wga.getLog().error("syntax error parseValue: " + st.toString());
				}
			}
			return result.toString();
		}
		
		private String replaceCustomFunctions(String theValue) throws WGException {
			for(Map.Entry<String,WcssFunction> customFunction : customFunctions.entrySet()){
				String search_pattern = customFunction.getKey() + "\\s*\\(([^\\)]*)\\)";
				Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
	        	Matcher matcher = pattern.matcher(theValue);
	        	StringBuffer sb = new StringBuffer();
	        	while(matcher.find()){
		            if(matcher.groupCount()>0) {
		            	ArrayList<String> params = parseCommasAndQuotes(matcher.group(1));
		            	String replacement = customFunction.getValue().execute(_ref, params);
		            	matcher.appendReplacement(sb, replacement);
		            }
	        	}
	        	matcher.appendTail(sb);
	        	theValue = sb.toString();
			}
			return theValue;
		}

		private String parseProps(String name, StreamTokenizer st) throws IOException, WGException {
			StringBuffer result = new StringBuffer();
			StringBuffer prop = new StringBuffer();
			int token;
			while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
				if(token==StreamTokenizer.TT_EOL)
					continue;
				else if(token==StreamTokenizer.TT_WORD)
					prop.append(st.sval + " ");
				else if((char)token == ':'){
					result.append(name + "-" + parseValue(prop.toString().trim(), st));
					prop = new StringBuffer();
				}
				else if((char)token == '}')
					break;
				else {
					_wga.getLog().error("syntax error parseProps: " + st.toString());
					break;				
				}
			}
			return result.toString();
		}

		/*
		 *  Helper method to prepare parameter
		 *  input is a comma separated string but elements may be quoted and quoted values may also contain commas
		 *  Sample: "cont,ainer", image.jpg 
		 *  Result should be: param-1 [cont,ainer], param-2 [image.jpg]
		 */
		private ArrayList<String> parseCommasAndQuotes(String input){
			ArrayList<String> result = new ArrayList<String>();
			int start = 0;
			boolean inQuotes = false;
			for (int current = 0; current < input.length(); current++) {
			    if (input.charAt(current) == '\"') inQuotes = !inQuotes; // toggle state
			    else if (input.charAt(current) == ',' && !inQuotes) {
			        result.add(input.substring(start, current).replace("\"", "").trim());
			        start = current + 1;
			    }
			}
			result.add(input.substring(start).replace("\"",  "").trim());
			return result;
		}

	}
	
}
