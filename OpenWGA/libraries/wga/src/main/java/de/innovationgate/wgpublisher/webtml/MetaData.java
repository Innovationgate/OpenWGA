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
package de.innovationgate.wgpublisher.webtml;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class MetaData extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class Status extends BaseTagStatus {
        
        protected String mdType;
        
        @Override
        public void initAttributeDelegates(Base tag) {

            MetaData mdTag = (MetaData) tag;
            this.mdType = mdTag.getType();
            super.initAttributeDelegates(tag);
        }
        
    }
    
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }
    
    public Status getStatus() {
        return (Status) super.getStatus();
    }
	
	private String type;
	private String name;
	private String sourceTag;
	
    private String highlight;
    private String highlightprefix;
    private String highlightsuffix;
	
	
	/**
	 * @throws WGAPIException 
	 * @see Base#tmlEndTag(TMLContext)
	 */
	public void tmlEndTag() throws TMLException, WGAPIException {
		
	    Status status = (Status) getStatus();
	    
		Object result = null;
		String type = status.mdType;
		
		if (type.equals("taginfo")) {
			result = this.getTMLContext().taginfo(this.getSourcetag(), this.getName());
		} 
		else if (type.equals("system")) {
			result = System.getProperty(this.getName());
		} 
		else if (type.equals("content") && stringToBoolean(getHighlight())) {
			result = this.getTMLContext().highlightMeta(this.getName(), getHighlightprefix(), getHighlightsuffix(), getStatus().encode);
			getStatus().encode = "none";
		} 
		else {
			result = this.getTMLContext().metalist(this.getType(), this.getName());
		}
				
		if (result == null && this.getTMLContext().getlasterror() != null) {
			this.addWarning(this.getTMLContext().getlasterror());
			return;
		}
		
		this.setResult(result);
	}

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", name, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Gets the type
	 * @return Returns a String
	 */
	public String getType() {
		return this.getTagAttributeValue("type", type, "content");
	}
	/**
	 * Sets the type
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * Gets the tagId
	 * @return Returns a String
	 */
	public String getSourcetag() {
		return this.getTagAttributeValue("sourcetag", sourceTag, null);
	}
	/**
	 * Sets the tagId
	 * @param tagId The tagId to set
	 */
	public void setSourcetag(String tagId) {
		this.sourceTag = tagId;
	}
	
	public String getEncode() {
	    return this.getTagAttributeValue("encode", super.getEncode(), (String) this.getTMLContext().getDesignContext().getDesignDB().getAttribute(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING));
	}


    public String getHighlight() {
        return getTagAttributeValue("highlight", highlight, "false");
    }

    public void setHighlight(String highlight) {
        this.highlight = highlight;
    }

    public String getHighlightprefix() {
        return getTagAttributeValue("highlightprefix", highlightprefix, "<B>");
    }

    public void setHighlightprefix(String highlightprefix) {
        this.highlightprefix = highlightprefix;
    }

    public String getHighlightsuffix() {
        return getTagAttributeValue("highlightsuffix", highlightsuffix, "</B>");
    }

    public void setHighlightsuffix(String highlightsuffix) {
        this.highlightsuffix = highlightsuffix;
    }
	
}

