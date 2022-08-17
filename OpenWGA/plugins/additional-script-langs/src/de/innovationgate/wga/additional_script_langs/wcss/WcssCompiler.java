package de.innovationgate.wga.additional_script_langs.wcss;

import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;

public class WcssCompiler {

	public static final Logger LOG = Logger.getLogger("wga.wcss");
	
	ResourceRef _ref;
	PostProcessResult _result;

	public interface WcssFunction{
		public String execute(ResourceRef ref, ArrayList<String> params) throws WGException;
	}

	// map of custom wcss functions
	final private static HashMap<String, WcssFunction> customFunctions = new HashMap<String, WcssFunction>();

	// register a custom function
	public static void registerCustomFunction(String name, WcssFunction func){
		customFunctions.put(name, func);
	}
	
	WcssCompiler(ResourceRef ref, PostProcessResult result){
		_ref=ref;
		_result=result;
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
		//st.ordinaryChar('@');
        
		StringBuffer code = new StringBuffer();

		StringBuffer prop = new StringBuffer();
		int token;
		while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
			if(token==StreamTokenizer.TT_EOL)
				continue;
			else if(token==StreamTokenizer.TT_WORD)
				prop.append(st.sval);
			else if((char)token == ':'){
				if(prop.toString().trim().isEmpty())
					prop.append(":");		// sample: :root{...}
				else{
					code.append(parseValue("", prop.toString().trim(), st));
					prop = new StringBuffer();
				}
			}
			else if((char)token == '{'){
				String cssClassName = (root_name  + " " + prop.toString()).trim();
				code.append(parseCssClass(cssClassName, st));
				prop = new StringBuffer();
			}
			else if((char)token == '@'){
				code.append(parseCmd(root_name, st));
				//prop = new StringBuffer();
			}
			else{
				LOG.error("syntax error: " + st.toString());
			}
		}
		
        _result.addIntegratedResource(_ref.getDesignDocument());
		return code.toString();
	}

	private String parseCmd(String name, StreamTokenizer st) throws IOException, WGException {
		int token;
		StringBuffer cmd = new StringBuffer();
		String params = "";
		
		while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
			if(token==StreamTokenizer.TT_WORD){
				cmd.append(st.sval.trim());
			}
			else if((char)token == ':')
				cmd.append(':');
			else if((char)token == '"')
				params = st.sval.trim();
			else if((char)token == ';'){
				if(cmd.toString().trim().equalsIgnoreCase("import")){
					ResourceRef ref = _ref.resolve(params); 
					if(ref.getDesignDocument()!=null){
						WcssCompiler compiler = new WcssCompiler(ref, _result);
						return compiler.compile(name);
					}
					else LOG.error("@import: ResourceRef not found: " + ref);
				}
				else LOG.error("unknown command @" + cmd);
				
				break;
			}
			else if((char)token == '{')
				return parseCssClass("@"+cmd.toString(), st);
		}
					
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
				/*
				if(!prop.toString().trim().isEmpty()){
					LOG.error("syntax error parseCssClass " + prop.toString().trim() + ": " + st.toString());
					prop = new StringBuffer();
				}
				*/
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
					if(prop.toString().trim().startsWith("@"))
						subclasses.append(parseValue(name, prop.toString().trim(), st));		// returns "prop:value;"
					else result.append(parseValue(name, "\t"+prop.toString().trim(), st));		// returns "prop:value;"
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
				LOG.error("syntax error parseCssClass: " + st.toString());
			}
		}
		if(token == StreamTokenizer.TT_EOF)
			LOG.error("syntax error parseCssClass: missing }. " + st.toString());
		
		result.append("}\n");
		result.append(subclasses);
		return result.toString();
	}

	private String parseValue(String cssClass, String name, StreamTokenizer st) throws IOException, WGException {
		
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
				result.append(parseProps(cssClass, name, st));	// list of props: background{image:...;position:...}
				break;
			}
			else if(token == StreamTokenizer.TT_EOL){
				result.append(name + ": " + value.toString().trim() + ";\n");
				LOG.warn(st.toString() + " - semicolon expected after token '" + name.trim() + ":" + value.toString().trim() + "'");
				break;
			}
			else if((char)token == ';'){
				// finished property definition
				String theValue = value.toString().trim();
				
				// parse for custom functions
				theValue = replaceCustomFunctions(theValue);

				if(name.startsWith("@")){
					LOG.info("execute command "+name + "(" + theValue + ")");
					if(name.equalsIgnoreCase("@import")){
						ResourceRef ref = _ref.resolve(theValue); 
						if(ref.getDesignDocument()!=null){
							WcssCompiler compiler = new WcssCompiler(ref, _result);
							result.append(compiler.compile(cssClass));
						}
						else LOG.error("@import: ResourceRef not found: " + ref);
					}
				}				
				else result.append(name + ": " + theValue + ";\n");
				
				break;
			}
			else {
				LOG.error("syntax error parseValue: " + st.toString());
			}
		}
		return result.toString();
	}
	
	/*
	 * search for pattern function_name(param1, param2, ...) and replace it with result of custom function
	 */
	private String replaceCustomFunctions(String theValue) throws WGException {
		for(Map.Entry<String,WcssFunction> customFunction : customFunctions.entrySet()){
			String search_pattern = customFunction.getKey() + "\\s*\\(([^\\)]*)\\)";	// search for name(params)
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

	private String parseProps(String cssClass, String name, StreamTokenizer st) throws IOException, WGException {
		StringBuffer result = new StringBuffer();
		StringBuffer prop = new StringBuffer();
		int token;
		while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
			if(token==StreamTokenizer.TT_EOL)
				continue;
			else if(token==StreamTokenizer.TT_WORD)
				prop.append(st.sval + " ");
			else if((char)token == ':'){
				result.append(name + "-" + parseValue(cssClass, prop.toString().trim(), st));
				prop = new StringBuffer();
			}
			else if((char)token == '}')
				break;
			else {
				LOG.error("syntax error parseProps: " + st.toString());
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


