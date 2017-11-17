/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wgpublisher.webtml.utils;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;

/**
 * Represents the WebTML tag being currently rendered
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
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
	
	public Object getInfo(String key) throws WGAPIException {
		if(_tag==null){
			logger.error("Taginfo could not be retrieved. TMLTag not found");
			return null;
		}
		return _tag.getTagInfo(key.toLowerCase());
	}

}
