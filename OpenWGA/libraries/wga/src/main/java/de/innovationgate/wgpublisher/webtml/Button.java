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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import org.apache.commons.lang.StringEscapeUtils;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;


public class Button extends ActionBase implements DynamicAttributes {

    private static final long serialVersionUID = 1L;
    private String clickaction = null;
	private String param1 = null;
	private String param2 = null;
	private String param3 = null;
	private String param4 = null;
	private String param5 = null;
	private String cssclass = null;
	private String cssstyle = null;
	private String confirm = null;
	private String type = null;

	/**
	 * Returns the clickaction.
	 * @return String
	 */
	public String getClickaction() {
		return this.getTagAttributeValue("clickaction", clickaction, "$refresh");
	}

	/**
	 * Sets the clickaction.
	 * @param clickaction The clickaction to set
	 */
	public void setClickaction(String clickaction) {
		this.clickaction = clickaction;
	}

	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlEndTag()
	 */
	public void tmlEndTag() throws WGException {
		
		String tagContent = this.getResultString();
		this.clearResult();

		this.appendResult("<button");
		this.appendResult(buildDynamicHtmlAttributes());
		
		String type = getType();
		if(type==null)
			type = "button";
		this.appendResult(" type=\"" + type + "\" ");
        
        // support id attribute - htmlUnit access for tml:button
        String id = this.getId();
        if (id != null) {
            this.appendResult("id=\"" + id + "\" ");
        }
        
        
		String clickaction = this.getClickaction();
		List<Object> params = new ArrayList<Object>();
		params.add(this.getParam1());
		params.add(this.getParam2());
		params.add(this.getParam3());
		params.add(this.getParam4());
		params.add(this.getParam5());
		Map<String,Object> namedParams = buildNamedActionParameters(true);
		
		
		TMLAction action = getTMLContext().getActionByID(clickaction, getDesignDBKey());
        if (action == null) {
            addWarning("Action of id '" + clickaction + "' is not defined", true);
            return;
            
        } 
		
		String actionCallFunction = null;
        if (isAjaxCall()) {
            actionCallFunction = getAjaxJSFunction(action, namedParams, params, isKeepParamsOnAJAX());
        } 
        else {
			actionCallFunction = getJSFunction(action, namedParams, params, isKeepParamsOnNonAJAX());
        }
        
        // Eventually add confirmation functionality
        String confirmMessage = getConfirm();
        if (confirmMessage != null) {
            actionCallFunction = "if (confirm('" + StringEscapeUtils.escapeJavaScript(StringEscapeUtils.unescapeJavaScript(confirmMessage)) + "')) " + actionCallFunction;
        }
		
		this.appendResult("onclick=\"" + actionCallFunction + "; return false;\" ");
                
		
		
		String cssclass = this.getCssclass();
		if (cssclass != null) {
			this.appendResult("class=\"" + cssclass + "\" ");
		}

		String cssstyle = this.getCssstyle();
		if (cssstyle != null) {
			this.appendResult("style=\"" + cssstyle + "\" ");
		}
		
		this.appendResult(">");
		this.appendResult(tagContent);
		this.appendResult("</button>");

	}
	
	@Override
	protected boolean isTrimResultString() {
	    return true;
	}
	

	/**
	 * Returns the param1.
	 * @return String
	 */
	public String getParam1() {
		return this.getTagAttributeValue("param1", param1, null);
	}

	/**
	 * Returns the param2.
	 * @return String
	 */
	public String getParam2() {
		return this.getTagAttributeValue("param2", param2, null);
	}

	/**
	 * Returns the param3.
	 * @return String
	 */
	public String getParam3() {
		return this.getTagAttributeValue("param3", param3, null);
	}

	/**
	 * Returns the param4.
	 * @return String
	 */
	public String getParam4() {
		return this.getTagAttributeValue("param4", param4, null);
	}

	/**
	 * Returns the param5.
	 * @return String
	 */
	public String getParam5() {
		return this.getTagAttributeValue("param5", param5, null);
	}

	/**
	 * Sets the param1.
	 * @param param1 The param1 to set
	 */
	public void setParam1(String param1) {
		this.param1 = param1;
	}

	/**
	 * Sets the param2.
	 * @param param2 The param2 to set
	 */
	public void setParam2(String param2) {
		this.param2 = param2;
	}

	/**
	 * Sets the param3.
	 * @param param3 The param3 to set
	 */
	public void setParam3(String param3) {
		this.param3 = param3;
	}

	/**
	 * Sets the param4.
	 * @param param4 The param4 to set
	 */
	public void setParam4(String param4) {
		this.param4 = param4;
	}

	/**
	 * Sets the param5.
	 * @param param5 The param5 to set
	 */
	public void setParam5(String param5) {
		this.param5 = param5;
	}

	public String getType(){
		return this.getTagAttributeValue("type", type, null);
	}
	public void setType(String type){
		this.type=type;
	}
	
	/**
	 * Returns the cssclass.
	 * @return String
	 */
	public String getCssclass() {
		return this.getTagAttributeValue("cssclass", cssclass, null);
	}

	/**
	 * Returns the cssstyle.
	 * @return String
	 */
	public String getCssstyle() {
		return this.getTagAttributeValue("cssstyle", cssstyle, null);
	}

	/**
	 * Sets the cssclass.
	 * @param cssclass The cssclass to set
	 */
	public void setCssclass(String cssclass) {
		this.cssclass = cssclass;
	}

	/**
	 * Sets the cssstyle.
	 * @param cssstyle The cssstyle to set
	 */
	public void setCssstyle(String cssstyle) {
		this.cssstyle = cssstyle;
	}

    public String getConfirm() {
        return getTagAttributeValue("confirm", confirm, null);
    }

    public void setConfirm(String confirm) {
        this.confirm = confirm;
    }
    
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "a", "html"); 
    }

}
