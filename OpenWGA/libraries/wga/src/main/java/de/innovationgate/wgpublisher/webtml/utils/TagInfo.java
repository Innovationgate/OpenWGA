package de.innovationgate.wgpublisher.webtml.utils;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;

public class TagInfo {

	private BaseTagStatus _status;
	
	public TagInfo(BaseTagStatus tagStatus){
		_status = tagStatus;
	}
	
	public Object get(String key) throws WGAPIException {
		return _status.getTagInfo(key.toLowerCase());
	}

}
