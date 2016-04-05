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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Action extends ActionBase implements DynamicAttributes {

    private static final long serialVersionUID = 1L;
    private String ref;
	private String param1;
	private String param2;
	private String param3;	
	private String param4;
	private String param5;
	private String master;
	private String async;
    private String debounce;
	private String timeout;

	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlEndTag()
	 */
	public void tmlEndTag() throws WGException {
		
		// If ref given, the result output is always true	
		String ref = this.getRef();
		String theId = this.getId();
        if (ref != null || theId == null) {
			this.setResultOutput(true);
		}
        
        if (theId != null && theId.contains(":")) {
            addWarning("Colon character ':' is invalid for id of page-defined WebTML Actions", true);
            return;
        }
        
        // try to define an action without code
        // designer may have mixed up <tml:action id='<id>'/> and <tml:action ref='<id>'/>
        Object theResult = this.getResult();
        if (theId != null && theResult == null) {
            addWarning("Action with id '" + theId + "' cannot be defined without code block. To reference an already defined action use <tml:action ref='<id>'/>.", false);
            return;
        }
		
		// Get or create action
		TMLAction tmlAction = null;
		if (theResult != null) {
            // create action object
			String actionCode = this.getResultString();
            TMLAction currentAction = 
                new TMLAction(actionCode, stringToBoolean(getMaster()), stringToBoolean(getAsync()), stringToBoolean(getDebounce()), TMLAction.ORIGIN_TML);
            currentAction.setDesignReference(new DesignResourceReference(getDesignDBKey(), getStatus().getTMLModuleName()));
            
            String strTimeout = getTimeout();
            if (strTimeout != null) {
                try {
                    // set custom timeout on action
                    int timeout = Integer.valueOf(strTimeout).intValue();
                    currentAction.setTimeout(timeout);
                } catch (NumberFormatException e) {
                    addWarning("Unable to set timeout value because it cannot be parsed as a number", false);
                }                    
            }
            
            tmlAction = getTMLContext().registerAction(currentAction, getId(), getDesignDBKey());
		}
		else {
            if (ref == null) {
                addWarning("Action id cannot be null when the action body is empty", true);
                return;
            }
            
            DesignResourceReference refObj;
            try {
                refObj = WGA.get(getTMLContext()).design().resolveReference(ref);
            }
            catch (Exception e) {
                throw new TMLException("Exception resolving action reference: " + ref, e, true);
            }
			tmlAction = (TMLAction) getTMLContext().getActionByID(ref, null);
			if (tmlAction == null) {
				this.addWarning("No action defined with id: " + refObj.toString());
				return;
			}
		}
		
		// If output=true, the action link (+ form name) is put out (for use with callAction-Script)
		if (this.isResultOutput()) {
			List<Object> params = new ArrayList<Object>();
			params.add(this.getParam1());
			params.add(this.getParam2());
			params.add(this.getParam3());
			params.add(this.getParam4());
			params.add(this.getParam5());
            TMLActionLink link = tmlAction.createActionLink(buildNamedActionParameters(true), params, this.getTMLContext());
            
            // F00004242
            if (Base.AJAX_MODE_NO_PORTLET_REFRESH.equalsIgnoreCase(getStatus().ajax)) {
                link.setMode(TMLActionLink.MODE_AJAX_NO_PORTLET_REFRESH);
            }
            
            // F00004292
            link.setPortletmode(getPortletmode());
            String portletcontext = getPortletcontext();
            if (portletcontext != null) {
                link.setPortletContextPath(getTMLContext(), portletcontext);
            }
			
			
			try {
                this.setResult(link.getJavascriptLink(getCore(), getStatus().getRelevantForm(), getTMLContext().getDesignContext().getVersionCompliance()));
            }
            catch (UnsupportedEncodingException e) {
                throw new TMLException("Exception creating action link", e, true);
            }
			 
		}
	
	}



    /**
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlStartTag()
	 */
	public void tmlStartTag() throws WGException {
	    super.tmlStartTag();
		this.setResultOutput(false);
	}

	/**
	 * Returns the ref.
	 * @return String
	 */
	public String getRef() {
		return this.getTagAttributeValue("ref", ref, null);
	}

	/**
	 * Sets the ref.
	 * @param ref The ref to set
	 */
	public void setRef(String ref) {
		this.ref = ref;
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

	/**
	 * @return
	 */
	public String getAsync() {
		return getTagAttributeValue("async", async, "false");
	}

	/**
	 * @return
	 */
	public String getMaster() {
		return getTagAttributeValue("master", master, "false");
	}

	/**
	 * @param string
	 */
	public void setAsync(String string) {
		async = string;
	}

	/**
	 * @param string
	 */
	public void setMaster(String string) {
		master = string;
	}

    public String getDebounce() {
        return this.getTagAttributeValue("debounce", debounce, "true");
    }
    public void setDebounce(String debounce) {
        this.debounce = debounce;
    }



    public String getTimeout() {
        return this.getTagAttributeValue("timeout", timeout, null);
    }



    public void setTimeout(String timeout) {
        this.timeout = timeout;
    }
    
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "a"); 
    }
}
