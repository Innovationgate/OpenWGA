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
	static{
		customFunctions.put("concat", new WcssFunction(){
			@Override
			public String execute(WcssResource resource, ArrayList<String> params) {
				StringBuffer s = new StringBuffer();
				for(String el: params){
					s.append(el);
				}
				return s.toString();
			}			
		});
	}
	
	// map of custom vars
	final private static HashMap<String, String> customVars = new HashMap<String, String>();

	WcssResource _resource;

	WcssCompiler(WcssResource resource){
		_resource = resource;
	}

	// register a custom function
	public static void registerCustomFunction(String name, WcssFunction func){
		customFunctions.put(name, func);
	}

	// register vars
	public static void registerVars(Map<String, String> vars){
		customVars.putAll(vars);
	}

	private boolean _compress=false;
	public void setCompressing(boolean value){
		_compress=value;
	}
	
	public String compile() throws IOException{
		CssBlock b = new CssBlock(); 
		b.getVars().putAll(customVars);
		compile(b);
		return b.getCode();
	}
	
	private void compile(CssBlock parent) throws IOException{
		StreamTokenizer st = new StreamTokenizer(new StringReader(_resource.getCode()));

		st.resetSyntax();
		
		st.wordChars(0x20, 0xFF);
		st.wordChars('\t', '\t');
		
		st.whitespaceChars('\n', '\n');		// needed to get correct line numbers
		
		st.slashSlashComments(true);
		st.slashStarComments(true);
		
		st.ordinaryChar('{');
		st.ordinaryChar('}');
		st.ordinaryChar(';');
		st.ordinaryChar('/');		// needed for comments
		        
		CssBlock rootCssBlock = new CssBlock(parent);
		rootCssBlock.parse(st);
		
		_resource.addIntegratedResource();

	}
	
	//--------------------

	private class CssBlock{
		
		private ArrayList<CssBlock> _subBlocks = new ArrayList<CssBlock>();
		private HashMap<String,CssMixinBlock> _mixins = new HashMap<String,CssMixinBlock>();		
		private HashMap<String,String> _props = new HashMap<String,String>();
		private HashMap<String,String> _vars = new HashMap<String,String>();
		private CssBlock _parentBlock = null;
		private String _name="";
		private String _sourceInfo;
		
		public boolean last_if_unless_rendered=true;
		
		CssBlock(){}
		
		CssBlock(CssBlock parent){
			this("", parent);
		}
		
		CssBlock(String name, CssBlock parent){
			_name = name;
			_parentBlock = parent;
			if(parent!=null)
				parent.addSubBlock(this);
		}
		
		@SuppressWarnings("unused")
		public String dump(String prefix){
			StringBuffer s = new StringBuffer();
			s.append("\n" + prefix + "'" + _name + "' (" + getClass().getName() + ")");
			for(CssBlock b : getSubBlocks())
				s.append(b.dump(prefix + "\t"));
			return s.toString();
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
			return _subBlocks;
		}
		public String getSourceInfo(){
			return _compress ? null : _sourceInfo;
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
						
		protected void addSubBlock(CssBlock subblock){
			_subBlocks.add(subblock);
		}
		
		public void parse(StreamTokenizer st) throws IOException{
			_sourceInfo = _resource + " line " + st.lineno();
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
								valuePart = replaceVars(valuePart);
								if(valuePart.endsWith("!default")){
									valuePart = valuePart.substring(0, valuePart.indexOf("!default"));
									if(searchVar(propPart)==null)
										_vars.put(propPart, trim(valuePart));
								}
								else _vars.put(propPart, trim(valuePart));
							}
							else _props.put(propPart, trim(valuePart));
						}
						else LOG.warn("missing : in property definition. " + _resource + ": Line " + st.lineno() + " ignored: " + propName);
					}					
				}
				else if((char)token == '{'){
					String className = trim(prop.toString());
					prop = new StringBuffer();
					if(className.startsWith("@media")){
						// special handling for @media
						CssBlock parent = new CssMediaBlock(className, this);	// no parent
						CssBlock b = new CssBlock(getPath(), parent);
						b.parse(st);
					}
					else if(className.startsWith("@include")){
						CssIncludeBlock b = new CssIncludeBlock(className, this);
						b.parse(st);
					}
					else if(className.startsWith("@if") || className.startsWith("@unless") || className.startsWith("@else")){
						CssConditionBlock b = new CssConditionBlock(className, this);
						b.parse(st);
					}
					else if(className.startsWith("@mixin")){
						CssMixinBlock b = new CssMixinBlock(className);
						if(b.isValid()){
							_mixins.put(b.getName(), b);
						}
						else LOG.error(st.toString() + " in " + _resource + ": invalid @mixin definition: " + className);
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
		
		public String getCode() throws IOException{
			return getCode("");
		}
		public String getCode(String prefix) throws IOException{
			StringBuffer result = new StringBuffer();

			if(_compress)
				prefix="";

			if(!_props.isEmpty() && getSourceInfo()!=null && !getSourceInfo().isEmpty())
				result.append("\n" + prefix + "/* " + getSourceInfo() + " */\n");

			String path = getPath();
			if(!path.isEmpty() && !_props.isEmpty()){
				result.append(prefix);
				result.append(path + "{");
				if(!_compress)
					result.append("\n");
			}
			for(Map.Entry<String,String> entry: _props.entrySet()){
				if(!path.isEmpty() && !_compress){
					result.append(prefix);
					result.append("\t");
				}
				result.append(replaceCustomFunctions(replaceVars(entry.getKey())));
				result.append(": ");
				result.append(replaceCustomFunctions(replaceVars(entry.getValue())));
				result.append(";");
				if(!_compress)
					result.append("\n");
			}
			if(!path.isEmpty() && !_props.isEmpty()){
				result.append(prefix);
				result.append("}");
				if(!_compress)
					result.append("\n");
			}
			for(CssBlock b: _subBlocks)
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
            	if(var!=null){
	            	matcher.appendReplacement(sb, Matcher.quoteReplacement(var));
            	}
            	else {
            		LOG.error("variable not found: " + replacement);
            		matcher.appendReplacement(sb, "");
            	}
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
		            	matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement));
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
			if(input==null)
				return result;
			int start = 0;
			boolean inQuotes = false;
			for (int current = 0; current < input.length(); current++) {
			    if (input.charAt(current) == '\"') inQuotes = !inQuotes; // toggle state
			    else if (input.charAt(current) == ',' && !inQuotes) {
			        result.add(input.substring(start, current).replace("\"", "").trim());
			        start = current + 1;
			    }
			}
			result.add(input.substring(start).replace("\"", "").trim());
			return result;
		}

		protected CssBlock cloneCssBlock(String name, CssBlock parent, CssBlock contentBlock){
			CssBlock clone = new CssBlock(name, parent);			
			clone.copyFromBlock(this, contentBlock);
			return clone;
		}
				
		protected void copyFromBlock(CssBlock source, CssBlock contentBlock){
			// copy properties
			HashMap<String, String> props = this.getProperties();
			for(Map.Entry<String,String> entry: source.getProperties().entrySet()){
				props.put(entry.getKey(), entry.getValue());
			}

			// copy vars
			HashMap<String, String> vars = this.getVars();
			for(Map.Entry<String,String> entry: source.getVars().entrySet()){
				vars.put(entry.getKey(), entry.getValue());
			}

			// clone sub blocks
			for(CssBlock sub: source.getSubBlocks()){
				sub.cloneCssBlock(sub.getName(), this, contentBlock);
			}
			
		}

		public String getPath(){

			String parentPath = (_parentBlock!=null ? _parentBlock.getPath() : "");
			String path_parts[] = parentPath.split("\\s*,\\s*");
			String name_parts[] = getName().trim().split("\\s*,\\s*");
			
			ArrayList<String> result = new ArrayList<String>();
			for(int i=0; i<name_parts.length; i++){
				for(int j=0; j<path_parts.length; j++){
					String name = name_parts[i];
					String path = path_parts[j];
					if(name.startsWith("&"))
						result.add(trim(path + name.substring(1)));
					else result.add(trim(path + " " + name));
				}
			}
			String[] r = new String[result.size()];
			return String.join(", ", result.toArray(r));
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

			if(_compress)
				prefix="";
			
			if(!getProperties().isEmpty()){
				if(getSourceInfo()!=null)
					result.append("\n" + prefix + "/* " + getSourceInfo() + " */\n");
				result.append(prefix);
				result.append(getParentBlock().getPath() + "{");
				if(!_compress)
					result.append("\n");
			}

			for(Map.Entry<String,String> entry: getProperties().entrySet()){
				result.append(prefix);
				if(!_compress)
					result.append("\t");
				result.append(replaceCustomFunctions(replaceVars(getName() + "-" + entry.getKey())));
				result.append(": ");
				result.append(replaceCustomFunctions(replaceVars(entry.getValue())));
				result.append(";");
				if(!_compress)
					result.append("\n");
			}
						
			if(!getProperties().isEmpty()){
				result.append(prefix);
				result.append("}");
				if(!_compress)
					result.append("\n");
			}
			
			return result.toString();
		}
	}
	
	private class CssDirectiveBlock extends CssBlock{

		private String _csscode = "";
		
		CssDirectiveBlock(String name, CssBlock parent) {
			super(name, parent);
		}

		public void parse(StreamTokenizer st) throws IOException{
			setSourceInfo(_resource.toString() + " line " + st.lineno());

			String directive="";
			String params_string="";
			// parse for "@directive something"
			String search_pattern = "@(\\S+)\\s*(.*)";
			Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
        	Matcher matcher = pattern.matcher(getName());
        	if(matcher.find()){
        		if(matcher.groupCount()>1){
        			directive = matcher.group(1);
        			params_string = matcher.group(2);
        		}
        	}
			
			if(directive.equalsIgnoreCase("import") && params_string.length()>1){	
				params_string = replaceCustomFunctions(replaceVars(params_string));
				ArrayList<String> params = parseCommasAndQuotes(params_string);
				WcssResource ref = _resource.resolve(params.get(0));
				if(ref!=null && ref.getCode()!=null){
					WcssCompiler compiler = new WcssCompiler(ref);
					compiler.setCompressing(_compress);
					compiler.compile(getParentBlock());
				}
				else LOG.error("@import: ResourceRef not found: " + ref);
			}
			else if(directive.equalsIgnoreCase("importcss") && params_string.length()>1){	
				params_string = replaceCustomFunctions(replaceVars(params_string));
				ArrayList<String> params = parseCommasAndQuotes(params_string);
				WcssResource ref = _resource.resolve(params.get(0));
				if(ref!=null && ref.getCode()!=null){
					_csscode = ref.getCSSCode();
				}
			}
			else if(directive.equalsIgnoreCase("content")){
				new CssContentBlock(getParentBlock());
			}
			else if(directive.equalsIgnoreCase("include") && !params_string.isEmpty()){
				
				String mixin_name="";
				String rest = "";
				ArrayList<String> params = new ArrayList<String>();
				search_pattern = "([\\w-]+)\\s*(.*)";	// search for name
				pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
	        	matcher = pattern.matcher(params_string);
	        	if(matcher.find()){
	        		if(matcher.groupCount()>1){
	        			mixin_name=matcher.group(1);
        				rest = matcher.group(2);
	        		}
	        	}
	        	if(rest.length()>0){
	        		search_pattern = "\\(([^\\)]*)\\)";	// search for (params)
					pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
		        	matcher = pattern.matcher(rest);
		        	if(matcher.find())
	        			params = parseCommasAndQuotes(matcher.group(1));
	        	}
								
				CssMixinBlock mixin = findMixin(mixin_name);
				if(mixin!=null)
					mixin.cloneCssBlock(getParentBlock(), params, null);
				else LOG.error("@mixin not found " + mixin_name);
			}
			else LOG.error("unknown directive " + getName());
		}
		
		protected CssDirectiveBlock cloneCssBlock(String name, CssBlock parent, CssBlock contentBlock){
			CssDirectiveBlock clone = new CssDirectiveBlock(name, parent);
			clone.copyFromBlock(this, contentBlock);
			return clone;
		}

		public String getCode(String prefix) throws IOException{
			String info="";
			if(getSourceInfo()!=null)
				info = "\n" + prefix + "/* " + getName() + " in " + getSourceInfo() + " */\n";
			return info + _csscode;
		}
	}
	
	private class CssMediaBlock extends CssBlock{

		CssMediaBlock(String name, CssBlock parent) {
			super(name, parent);
		}
		
		public String getPath(){
			return "";
		}
		
		public String getCode(String prefix) throws IOException{
			StringBuffer result = new StringBuffer();
			result.append(getName() + "{");
			if(!_compress)
				result.append("\n");
			for(CssBlock b: getSubBlocks()){
				result.append(b.getCode("\t"));
			}
			result.append("}");
			if(!_compress)
				result.append("\n");
			return result.toString();
		}		
	}
	
	private class CssIncludeBlock extends CssBlock{

		String _name;
		
		CssIncludeBlock(String name, CssBlock parent) {
			super(parent);
			_name = name;
		}

		public void parse(StreamTokenizer st) throws IOException{

			super.parse(st);
			
			String mixin_name="";
			ArrayList<String> params = new ArrayList<String>();

			String rest="";
			String search_pattern = "@include\\s+([\\w-]+)\\s*(.*)";	// search for @include name <rest...>
			Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
        	Matcher matcher = pattern.matcher(_name);
        	if(matcher.find()){
        		if(matcher.groupCount()>1){
        			mixin_name = matcher.group(1);
        			rest = matcher.group(2);
        		}
        	}
        	if(rest.length()>0){
        		search_pattern = "\\(([^\\)]*)\\)";	// search for (params)
				pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
	        	matcher = pattern.matcher(rest);
	        	if(matcher.find())
        			params = parseCommasAndQuotes(matcher.group(1));
        	}

			CssMixinBlock mixin = getParentBlock().findMixin(mixin_name);
			if(mixin!=null){
				mixin.cloneCssBlock(getParentBlock(), params, this);
			}
			else LOG.error("mixin not found: " + mixin_name);
		}

		public String getCode(String prefix) throws IOException{
			if(getSourceInfo()!=null)
				return "\n" + prefix + "/* " + _name + " in " + getSourceInfo() + " */\n";
			return "";
		}

	}
	
	private class CssMixinBlock extends CssBlock{

		ArrayList<String> _params = new ArrayList<String>();
		boolean _valid=false;

		CssMixinBlock(String name) {
			String search_pattern = "@mixin\\s+([\\w-]+)\\s*(\\([^\\)]*\\))?";	// search for @mixin name (params) where (params) is optional
			Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
        	Matcher matcher = pattern.matcher(name);
        	if(matcher.find()){
        		if(matcher.groupCount()>0){
        			_valid=true;
        			setName(matcher.group(1));
        		}
    			if(matcher.groupCount()>1 && matcher.group(2)!=null){
    				String params = matcher.group(2);
    				params = params.substring(1, params.length()-1);	// removes ( and )
    				_params = parseCommasAndQuotes(params);
        			for(int i=0; i<_params.size(); i++){
        				String parts[] = _params.get(i).split("\\s*:\\s*");
        				if(parts.length>1){
        					getVars().put(parts[0], parts[1]);
        					_params.set(i, parts[0]);
        				}
        			}
    			}
        	}
		}
		
		public boolean isValid(){
			return _valid;
		}

		public CssBlock cloneCssBlock(CssBlock parent, ArrayList<String> params, CssBlock contentBlock){
			
			CssBlock clone = super.cloneCssBlock("", parent, contentBlock);
			
			// params
			if(params!=null){
				for(int i=0; i<params.size(); i++){
					String parts[] = params.get(i).split("\\s*:\\s*");	// format $var:value
					if(parts.length>1){
						clone.getVars().put(parts[0], parts[1]);
					}
					else if(_params.size()>i){
						String p = _params.get(i);
						clone.getVars().put(p, parts[0]);
					}
				}
			}
			
			return clone;
		}

	}
	
	private class CssContentBlock extends CssBlock{

		CssContentBlock(CssBlock parent) {
			super(parent);
		}

		protected CssBlock cloneCssBlock(String name, CssBlock parent, CssBlock contentBlock){
			if(contentBlock!=null)
				return contentBlock.cloneCssBlock(name, parent, null);
			return null;
		}
		
	}

	private class CssConditionBlock extends CssBlock{

		String _def;
		
		CssConditionBlock(String def, CssBlock parent) {			
			super(parent);
			_def = def;
		}
		
		public String getCode(String prefix) throws IOException{
			String directive="";
			String params_string="";
			String search_pattern = "@(\\S+)\\s*(.*)";		// parse for "@directive something"
			Pattern pattern = Pattern.compile(search_pattern, Pattern.CASE_INSENSITIVE);
	    	Matcher matcher = pattern.matcher(_def);
	    	if(matcher.find()){
	    		if(matcher.groupCount()>1){
	    			directive = matcher.group(1);
	    			params_string = matcher.group(2);
	    			params_string = replaceCustomFunctions(replaceVars(params_string));
	    		}
	    	}
	    	boolean isTrue = !params_string.isEmpty() && !params_string.equalsIgnoreCase("false"); 
	    	boolean should_render = 
	    				(directive.equalsIgnoreCase("if") && isTrue) 
	    				|| (directive.equalsIgnoreCase("unless") && !isTrue)
	    				|| (directive.equalsIgnoreCase("else") && !getParentBlock().last_if_unless_rendered);
			
			if(should_render){
				getParentBlock().last_if_unless_rendered=true;
				return super.getCode(prefix);
			}
			else {
				getParentBlock().last_if_unless_rendered=false;
				return "";
			}
		}

		protected CssConditionBlock cloneCssBlock(String name, CssBlock parent, CssBlock contentBlock){
			CssConditionBlock clone = new CssConditionBlock(_def, parent);
			clone.copyFromBlock(this, contentBlock);
			return clone;
		}

	}
	
}
