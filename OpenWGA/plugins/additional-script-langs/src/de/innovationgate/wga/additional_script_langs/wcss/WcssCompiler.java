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

public class WcssCompiler {

	public static final Logger LOG = Logger.getLogger("wga.wcss");

	// map of custom wcss functions
	final private static HashMap<String, WcssFunction> customFunctions = new HashMap<String, WcssFunction>();

	public interface WcssResource{
		public String getCode();
		public WcssResource resolve(String path);
		public void addIntegratedResource();
	}

	public interface WcssFunction{
		public String execute(WcssResource ref, ArrayList<String> params);
	}
	
	WcssResource _resource;

	WcssCompiler(WcssResource resource){
		_resource = resource;
	}

	// register a custom function
	public static void registerCustomFunction(String name, WcssFunction func){
		customFunctions.put(name, func);
	}
	
	public String compile() throws IOException{
		CssBlock b = new CssBlock(); 
		compile(b);
		return b.getCode("");
	}
	
	private void compile(CssBlock parent) throws IOException{
		StreamTokenizer st = new StreamTokenizer(new StringReader(_resource.getCode()));

		st.resetSyntax();
		
		st.wordChars(0x20, 0xFF);
		st.wordChars('\t', '\t');
		
		st.whitespaceChars('\n', '\n');		// needed to get correct line numbers
		
		st.slashSlashComments(true);
		st.slashStarComments(true);
		st.eolIsSignificant(true);
		
		st.ordinaryChar('{');
		st.ordinaryChar('}');
		st.ordinaryChar(';');
		st.ordinaryChar('/');		// needed for comments
		        
		CssBlock rootCssBlock = new CssBlock("", parent);
		rootCssBlock.parse(st);
		
		_resource.addIntegratedResource();
		
	}
	
	//--------------------

	private class CssBlock{
		
		private ArrayList<CssBlock> _subClases = new ArrayList<CssBlock>();
		private HashMap<String,CssMixinBlock> _mixins = new HashMap<String,CssMixinBlock>();		
		private HashMap<String,String> _props = new HashMap<String,String>();
		private HashMap<String,String> _vars = new HashMap<String,String>();
		private CssBlock _parentBlock = null;
		private String _name;
		private String _sourceInfo;
		
		CssBlock(){}
		
		CssBlock(String name, CssBlock parent){
			_name = name;
			_parentBlock = parent;
			if(parent!=null)
				parent.addSubClass(this);
		}
		
		public String getName(){
			return _name !=null ? _name : "";
		}
		public void setName(String name){
			_name=name;
		}
		public CssBlock getParentBlock(){
			return _parentBlock;
		}
		public ArrayList<CssBlock> getSubBlocks(){
			return _subClases;
		}
		public String getSourceInfo(){
			return _sourceInfo;
		}
		public void setSourceInfo(String info){
			_sourceInfo=info;
		}
		public HashMap<String,String> getProperties(){
			return _props;
		}
		public HashMap<String,String> getVars(){
			return _vars;
		}
		protected HashMap<String,CssMixinBlock> getMixins(){
			return _mixins;
		}
						
		protected void addSubClass(CssBlock subclass){
			_subClases.add(subclass);
		}
		
		public void parse(StreamTokenizer st) throws IOException{
			_sourceInfo = _resource.toString() + " line " + st.lineno();
			int token;
			StringBuffer prop = new StringBuffer();
			while((token = st.nextToken()) != StreamTokenizer.TT_EOF){
				if(token==StreamTokenizer.TT_WORD)
					prop.append(st.sval);
				else if((char)token == '/'){
					prop.append("/");
				}
				else if((char)token == ';'){					
					String propName = prop.toString().trim();
					prop = new StringBuffer();
					if(propName.startsWith("@")){
						CssDirectiveBlock b = new CssDirectiveBlock(propName, this);
						b.parse(st);
					}
					else{
						// search for property name:value
						int index = propName.indexOf(':');
						if(index>0){
							String propPart = propName.substring(0, index);
							String valuePart = propName.substring(index+1);
							if(propPart.startsWith("$")){
								_vars.put(propPart, trim(valuePart));
							}
							else _props.put(propPart, trim(valuePart));
						}
						else LOG.warn("line " + st.lineno() + " ignored: " + propName);
					}					
				}
				else if((char)token == '{'){
					String className = trim(prop.toString());
					prop = new StringBuffer();
					if(className.startsWith("@media")){
						// special handling for @media
						CssBlock parent = new CssMetaBlock(className, this);	// no parent
						CssBlock b = new CssBlock(getPath(), parent);
						b.parse(st);
					}
					else if(className.startsWith("@mixin")){
						CssMixinBlock b = new CssMixinBlock(className);
						if(b.isValid()){
							_mixins.put(b.getName(), b);
						}
						else LOG.error(st.toString() + ": invalid @mixin definition: " + className);
						b.parse(st);	// parse in any case
					}
					else if(className.endsWith(":")){
						className = className.substring(0, className.length()-1);
						CssBlock b = new CssPropertiesBlock(className, this);
						b.parse(st);
					}
					else {
						CssBlock b = new CssBlock(className, this);
						b.parse(st);
					}
				}
				else if((char)token == '}')
					break;
			}
			
		}
		
		public String getCode(String prefix) throws IOException{
			StringBuffer result = new StringBuffer();

			if(!_props.isEmpty() && getSourceInfo()!=null && !getSourceInfo().isEmpty())
				result.append("\n" + prefix + "/* " + getSourceInfo() + " */\n");

			String path = getPath();
			if(!path.isEmpty() && !_props.isEmpty()){
				result.append(prefix);
				result.append(getPath() + "{\n");
			}
			for(Map.Entry<String,String> entry: _props.entrySet()){
				if(!path.isEmpty()){
					result.append(prefix);
					result.append("\t");
				}
				result.append(entry.getKey());
				result.append(": ");
				result.append(replaceCustomFunctions(replaceVars(entry.getValue())));
				result.append(";\n");
			}
			if(!path.isEmpty() && !_props.isEmpty()){
				result.append(prefix);
				result.append("}\n");
			}
			for(CssBlock b: _subClases)
				result.append(b.getCode(prefix));

			return result.toString();
		}

		protected String replaceVars(String theValue) {
			String search_pattern = "\\$[\\w-]+";		// alphanumeric (incl. underscore) plus minus
			Pattern pattern = Pattern.compile(search_pattern);
			Matcher matcher = pattern.matcher(theValue);
        	StringBuffer sb = new StringBuffer();
        	while(matcher.find()){
            	String replacement = matcher.group(0);
            	String var = searchVar(replacement);
            	if(var!=null)
	            	replacement = var;
            	else {
            		LOG.error("variable not found: " + replacement);
            		replacement = "/* not defined: " + Matcher.quoteReplacement(replacement) + " */";
            	}
	            matcher.appendReplacement(sb, replacement);
        	}
        	matcher.appendTail(sb);
        	return sb.toString();
		}
		
		private String searchVar(String var){			
			for(CssBlock block = this; block!=null; block = block.getParentBlock()){
				String value = block.getVars().get(var);
				if(value!=null)
					return value;
			}
			return null;
		}
		
		/*
		 * search for pattern function_name(param1, param2, ...) and replace it with result of custom function
		 */
		protected String replaceCustomFunctions(String theValue) {
			for(Map.Entry<String,WcssFunction> customFunction : customFunctions.entrySet()){
				String search_pattern = customFunction.getKey() + "\\s*\\(([^\\)]*)\\)";	// search for name(params)
				Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
	        	Matcher matcher = pattern.matcher(theValue);
	        	StringBuffer sb = new StringBuffer();
	        	while(matcher.find()){
		            if(matcher.groupCount()>0) {
		            	ArrayList<String> params = parseCommasAndQuotes(matcher.group(1));
		            	String replacement = customFunction.getValue().execute(_resource, params);
		            	matcher.appendReplacement(sb, replacement);
		            }
	        	}
	        	matcher.appendTail(sb);
	        	theValue = sb.toString();
			}
			return theValue;
		}

		/*
		 *  Helper method to prepare parameter
		 *  input is a comma separated string but elements may be quoted and quoted values may also contain commas
		 *  Sample: "cont,ainer", image.jpg 
		 *  Result should be: param-1 [cont,ainer], param-2 [image.jpg]
		 */
		protected ArrayList<String> parseCommasAndQuotes(String input){
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

		protected CssBlock cloneCssBlock(CssBlock parent){
			String name = getName();
			if(this instanceof CssMixinBlock)
				name="";
			CssBlock clone = new CssBlock(name, parent);
			
			// copy properties
			HashMap<String, String> props = clone.getProperties();
			for(Map.Entry<String,String> entry: getProperties().entrySet()){
				props.put(entry.getKey(), entry.getValue());
			}

			// clone sub classes
			for(CssBlock sub: getSubBlocks()){
				sub.cloneCssBlock(clone);
			}
			
			return clone;
		}

		public String getPath(){

			String names[] = getName().trim().split("\\s*,\\s*");
			String parentPath = trim(_parentBlock!=null ? _parentBlock.getPath() : "");
			for(int i=0; i<names.length; i++){
				String name = names[i];
				if(name.startsWith("&"))
					names[i] = trim(parentPath + name.substring(1));
				else names[i] = trim(parentPath + " " + name);
			}
			return String.join(", ", names);
		}

		/*
		 * Utility to remove multiple whitespaces from text
		 */
		private String trim(String text){
			return text.trim().replaceAll("\\s+", " ");
		}
		
		protected CssMixinBlock findMixin(String name){
			for(CssBlock block = this; block!=null; block = block.getParentBlock()){
				CssMixinBlock mixin = block.getMixins().get(name);
				if(mixin!=null)
					return mixin;
			}
			return null;
		}

	}
	
	private class CssPropertiesBlock extends CssBlock{

		CssPropertiesBlock(String name, CssBlock parent) {
			super(name, parent);
		}
		
		public String getCode(String prefix) throws IOException{
			StringBuffer result = new StringBuffer();

			String path = getParentBlock().getPath();
			if(!getProperties().isEmpty()){
				result.append("\n" + prefix + "/* " + getSourceInfo() + " */\n");
				result.append(prefix);
				result.append(path + "{\n");
			}

			for(Map.Entry<String,String> entry: getProperties().entrySet()){
				result.append(prefix);
				result.append("\t");
				result.append(getName() + "-" + entry.getKey());
				result.append(": ");
				result.append(replaceCustomFunctions(entry.getValue()));
				result.append(";\n");
			}
						
			if(!getProperties().isEmpty()){
				result.append(prefix);
				result.append("}\n");
			}
			
			return result.toString();
		}
	}
	
	private class CssDirectiveBlock extends CssBlock{

		CssDirectiveBlock(String name, CssBlock parent) {
			super(name, parent);
		}

		public void parse(StreamTokenizer st) throws IOException{
			setSourceInfo(_resource.toString() + " line " + st.lineno());

			String parts[] = getName().split("\\s+");
			if(parts[0].equalsIgnoreCase("@import") && parts.length>1){	
				ArrayList<String> params = parseCommasAndQuotes(parts[1]);
				WcssResource ref = _resource.resolve(params.get(0));
				if(ref!=null){
					WcssCompiler compiler = new WcssCompiler(ref);
					compiler.compile(getParentBlock());
				}
				else LOG.error("@import: ResourceRef not found: " + ref);
			}
			else if(parts[0].equalsIgnoreCase("@include") && parts.length>1){
				CssMixinBlock mixin = findMixin(parts[1].trim());
				if(mixin!=null)
					mixin.cloneCssBlock(getParentBlock());
				else LOG.error("@mixin not found " + parts[1]);
			}
			else LOG.error("unknown directive " + getName());
		}
		
		public String getCode(String prefix) throws IOException{
			// nothing to do here
			StringBuffer result = new StringBuffer();
			result.append("\n" + prefix + "/* Execute " + getName() + " in " + getSourceInfo() + " */\n");
			return result.toString();
		}
	}
	
	private class CssMetaBlock extends CssBlock{

		CssMetaBlock(String name, CssBlock parent) {
			super(name, parent);
		}
		
		public String getPath(){
			return "";
		}
		
		public String getCode(String prefix) throws IOException{
			StringBuffer result = new StringBuffer();
			result.append(getName() + "{\n");
			for(CssBlock b: getSubBlocks()){
				result.append(b.getCode("\t"));
			}
			result.append("}\n");
			return result.toString();
		}		
	}
	
	private class CssMixinBlock extends CssBlock{

		ArrayList<String> _params;
		boolean _valid=false;

		CssMixinBlock(String name) {
			String search_pattern = "@mixin\\s+(\\S+)\\s*\\(([^\\)]*)\\)";	// search for name(params)
			Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
        	Matcher matcher = pattern.matcher(name);
        	while(matcher.find()){
        		if(matcher.groupCount()==2){
        			setName(matcher.group(1));
        			_params = parseCommasAndQuotes(matcher.group(2));
        			_valid=true;
        		}
        	}
        	if(_valid)
        		LOG.info("@mixin " + getName() + ", params=" + _params);
		}
		
		public boolean isValid(){
			return _valid;
		}
		
	}
}
