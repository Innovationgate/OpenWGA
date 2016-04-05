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
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Arrays;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.tagext.BodyTagSupport;

import de.innovationgate.utils.URLBuilder;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;


public class URLParam extends Base {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _name;
	private String _decode;

	/**
	 * @throws TMLException 
	 * @see BodyTagSupport#doStartTag()
	 */
	public void tmlEndTag() throws TMLException {
			
			try {
                HttpServletRequest request = (HttpServletRequest) getPageContext().getRequest();
                
                boolean decode = stringToBoolean(getDecode());
                String[] param;
                
                if (decode) {
                    param = request.getParameterValues(getName());
                    if (param == null) {
                    	setResult("");
                    	return;
                    }
                }
                else {
                    URLBuilder requestURL = new URLBuilder(request, getCore().getCharacterEncoding());
                    param = new String[] {requestURL.getParameter(getName())};
                }
                
                if (param.length == 1) {
                	this.setResult(param[0]);
                }
                else {
                	this.setResult(Arrays.asList(param));
                }
            }
            catch (Exception e) {
                throw new TMLException("Exception retrieving url param", e, true);
            }
	}

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", _name, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this._name = name;
	}
	
	   /**
     * @see de.innovationgate.wgpublisher.webtml.Base#getEncode()
     */
    public String getEncode() {
        return this.getTagAttributeValue("encode", encode, (String) this.getTMLContext().getDesignContext().getDesignDB().getAttribute(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING));
    }

    public String getDecode() {
        return getTagAttributeValue("decode", _decode, "true");
    }

    public void setDecode(String decode) {
        this._decode = decode;
    }

}

