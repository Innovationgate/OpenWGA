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

import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGUnresolveableVirtualLinkException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Link extends Base implements DynamicAttributes {
	
    private static final long serialVersionUID = 1L;
    private String astext;
	private String layoutkey;
	private String mediakey;

	/**
	 * @throws WGAPIException 
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws TMLException, WGException {
		
	    try {
            TMLContext context = this.getTMLContext();
            WGContent content = context.content();
            String contentURL;
            
            // Build link body
            try {
                contentURL = context.contenturl(this.getMedium(), this.getLayout());
            }
            catch (WGUnresolveableVirtualLinkException e) {
                throw new TMLException("Unresolveable virtual link on content " + content.getContentKey(true) + ": " + e.getMessage(), true);
            } 

            String linkTarget = content.getLinkTarget();
            if (linkTarget != null && !linkTarget.equals("")) {
            	linkTarget = " target=\"" + linkTarget + "\" ";
            }
            else {
            	linkTarget = "";
            }

            String result;
            
            if(getTMLContext().getDesignContext().getVersionCompliance().isAtLeast(7, 7)){
            	// use Tag-Body as link-body
                String linkBody = this.getResultString(false);
                if(linkBody==null){
                	linkBody = getTMLContext().encode("html", content.getTitle());
                }
                result = "<a" + buildDynamicHtmlAttributes() 
                	+ " href=\"" + getResponse().encodeURL(contentURL) + "\"" 
            		+ linkTarget 
            		+ ">" + linkBody + "</a>";
            }
            else{
            	//Use Tag-Body as link attributes
	            String linkBody = getTMLContext().encode("html", content.getTitle());
	            result = "<a" + buildDynamicHtmlAttributes() 
	            	+ " href=\"" + getResponse().encodeURL(contentURL) + "\"" 
	            		+ linkTarget 
	            		+ this.getResultString(false) + ">" + linkBody + "</a>";
            }
            
            this.setResult(result);
        }
        catch (Exception e) {
            throw new TMLException("Exception building link", e, true);
        }

	}

	/**
	 * Gets the astext
	 * @return Returns a String
	 */
	public String getAstext() {
		return this.getTagAttributeValue("astext", this.astext, "false");
	}
	/**
	 * Sets the astext
	 * @param astext The astext to set
	 */
	public void setAstext(String astext) {
		this.astext = astext;
	}

	/**
	 * Gets the layoutkey
	 * @return Returns a String
	 */
	public String getLayout() {
		return this.getTagAttributeValue("layout", layoutkey, null);
	}
	/**
	 * Sets the layoutkey
	 * @param layoutkey The layoutkey to set
	 */
	public void setLayout(String layoutkey) {
		this.layoutkey = layoutkey;
	}

	/**
	 * Gets the mediakey
	 * @return Returns a String
	 */
	public String getMedium() {
		return this.getTagAttributeValue("medium", mediakey, (String) getStatus().getOption(OPTION_LINK_MEDIUM));
	}
	/**
	 * Sets the mediakey
	 * @param mediakey The mediakey to set
	 */
	public void setMedium(String mediakey) {
		this.mediakey = mediakey;
	}
	    
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "html"); 
    }

}

