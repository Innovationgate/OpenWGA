package de.innovationgate.wgpublisher.webtml.utils;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;

public class TagInfo {

	private BaseTagStatus _tag;
	private static final Logger logger = LogManager.getLogger("taginfo");
	
	public TagInfo(BaseTagStatus tag){
		_tag = tag;
	}

	public TagInfo parent(String tagname){
		
		if(_tag==null)
			return this;
		
		BaseTagStatus parent = _tag.getParentTag();
		if(tagname!=null){ 
			while(parent != null && !parent.tagClass.getName().equalsIgnoreCase("de.innovationgate.wgpublisher.webtml." + tagname)){
				parent = parent.getParentTag();
			}
		}
		return new TagInfo(parent);
	}

	public TagInfo parent(){
		return parent(null);
	}

	public String toString(){
		if(_tag==null)
			return "null";
        String className = _tag.tagClass.getName();
        return "TML Tag [" + _tag.getTMLModuleName() + "/" + className.substring(className.lastIndexOf(".") + 1).toLowerCase() + "] on line " + _tag.sourceLine;
	}
	
	public Object get(String key) throws WGAPIException {
		if(_tag==null){
			logger.error("Taginfo could not be retrieved. TMLTag not found");
			return null;
		}
		return _tag.getTagInfo(key.toLowerCase());
	}

}
