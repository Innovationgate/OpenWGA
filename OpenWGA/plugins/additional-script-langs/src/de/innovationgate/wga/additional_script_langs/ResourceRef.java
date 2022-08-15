package de.innovationgate.wga.additional_script_langs;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletContext;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

public class ResourceRef {

	public static final String TYPE_JS = "js";
	public static final String TYPE_CSS = "css";
	public static final String TYPE_FILE = "file";
	public static final String TYPE_STATIC = "static";
	public static final String TYPE_TMLSCRIPT = "tmlscript";
	
	String _type;
	String _db=null;
	String _path="";
	String _resourceName="";
	Design _design;

	public ResourceRef(Design base_design, String default_type) throws WGException{
		_design = base_design;
		_type = default_type;
		_db = _design.db().getDbReference();
		
		_path = _design.getResourceName();
		List<String> path_parts = new ArrayList<String>(Arrays.asList(_path.split(":")));
		_resourceName = path_parts.get(path_parts.size()-1);
		path_parts.remove(_resourceName);
		_path = StringUtils.join(path_parts, ":");
				
	}

	public ResourceRef(String path) throws WGException{
		this(new ResourceRef(WGA.get().design(), TYPE_FILE), path);
	}

	public ResourceRef(ResourceRef parentref, String path) throws WGException {
		String[] parts = path.split("\\s+", 3);
		for(String part: parts){
			String[] q = part.split("=");
			if(q.length==1){	// path only
				_path = part.trim().replace("/", ":");
			}
			else{
				String qname = q[0].trim();
				String qvalue = q[1].trim();
				if(qname.equals("type"))
					_type = qvalue;
				else if(qname.equals("db"))
					_db = qvalue;
				else if(qname.equals("path"))
					_path = qvalue.replace("/", ":");
			}
		}

		if(_type==null)
			_type = parentref.getType();
				
		List<String> path_parts = new ArrayList<String>(Arrays.asList(_path.split(":")));
		_resourceName = path_parts.get(path_parts.size()-1);
		path_parts.remove(_resourceName);
		_path = StringUtils.join(path_parts, ":");

		if((_type.equals(TYPE_CSS) || _type.equals(TYPE_JS)) && _resourceName.contains(".")){			
			_resourceName = _resourceName.substring(0, _resourceName.lastIndexOf("."));	// remove file extension
		}
		
		if(_db==null){
			_db = parentref.getDb();
			if(!_path.startsWith(":") && !_resourceName.contains("@") && !_type.equals(TYPE_STATIC))	// relative Adressierung
				_path = parentref.getPath() + (_path.equals("") ? "" : ":" + _path);
		}
		else if(!_path.equals("") && !_path.startsWith(":")){
			_path = ":"+_path;		// ensure path is absolute if db attribute is given
		}
		
		String pathToResolve = _db + "/" + _path + (!_type.equals(TYPE_FILE) ? ":"+_resourceName : "");
		_design = parentref.getDesign().resolve(pathToResolve);
				
	}
	
	public ResourceRef resolve(String path) throws WGException{
		return new ResourceRef(this, path);
	}
	
	/*
	 * Returns the code "as is" - WITHOUT recursively calling postProcessors
	 */
	public String getCode() throws WGException, IOException{
		switch (_type) {
			case TYPE_CSS:
			case TYPE_JS:
			case TYPE_TMLSCRIPT:
				WGScriptModule mod = (WGScriptModule)getDesignDocument();
				return mod!=null ? mod.getCode() : null;
	
			case TYPE_STATIC:
				ServletContext ctx = WGACore.INSTANCE.getServletContext();
				if(ctx!=null)
					return IOUtils.toString(WGACore.INSTANCE.getServletContext().getResourceAsStream("/static/" + (_path + ":" + _resourceName).replace(":", "/")));
				else return null;

			case TYPE_FILE:
				WGFileContainer c = (WGFileContainer)getDesignDocument();
				return c!=null && c.hasFile(_resourceName) ? IOUtils.toString(c.getFileText(_resourceName, "UTF-8")) : null;

			default:
				return null;
		}
	}
	
	/*
	 * Returns the code recursively calling other postProcessors
	 */
	public String getJavaScriptCode(Boolean compress) throws WGException, IOException{
		if(_type==TYPE_JS)
			return _design.getJavaScriptCode(compress);
		else return getCode();
	}

	/*
	 * Returns the code recursively calling other postProcessors
	 */
	public String getCSSCode() throws WGException, IOException{
		if(_type==TYPE_CSS)
			return _design.getCSSCode();
		else return getCode();
	}

	/*
	 * Returns the code recursively calling other postProcessors
	 */
	public String getTMLScriptCode(Boolean compress) throws WGException, IOException{
		if(_type==TYPE_TMLSCRIPT)
			return _design.getTMLScriptCode();
		else return getCode();
	}

	public WGDesignDocument getDesignDocument() throws WGException, IOException{
		try{
			switch (_type) {
				case TYPE_CSS:
					return _design.getScriptModule(WGScriptModule.CODETYPE_CSS);
		
				case TYPE_JS:
					return _design.getScriptModule(WGScriptModule.CODETYPE_JS);
	
				case TYPE_TMLSCRIPT:
					return _design.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT);

				case TYPE_FILE:
					return _design.getFileContainer();
	
				default:
					return null;
			}
		}
		catch(Exception e){
			return null;
		}		
	}
	
	public Design getDesign() throws WGException{
		return _design;
	}
	
	public String getDb(){
		return _db;
	}
	
	public String getType() {
		return _type;
	}
	
	public String getResourceName() {
		return _resourceName;
	}

	public void setResourceName(String name) throws WGException {
		_resourceName = name;
		if(!_type.equals(TYPE_FILE))
			_design = _design.resolve(_path + ":"+_resourceName);
	}

	public String getPath(){
		return _path;
	}
	
	public String toString(){
		StringBuffer ret = new StringBuffer();
		ret.append("type="+_type).append(" ");
		if(_db!=null && !_type.equals(TYPE_STATIC))
			ret.append("db="+_db).append(" ");
		ret.append(_path);
		ret.append("/" + _resourceName);
		
		return ret.toString().replace(":", "/");
	}
	
}
