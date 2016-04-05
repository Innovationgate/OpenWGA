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
import java.util.List;
import java.util.Map;


















import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionCallParameters;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

/**
 * Base class for all tags supporting 
 * action-creation and action-calling
 */
public abstract class ActionBase extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class Status extends BaseTagStatus {
        
        private boolean plainLink =  false;
        private String formID;
        public String ajax;
        
        @Override
        public String getRelevantForm() {
            
            if (plainLink) {
                return null;
            }
            
            if (formID != null) {
                // return formId only if form is in edit-mode
                // none editable forms should not be submitted
                BaseTagStatus base = getTagStatusById(formID);
                if (base != null && base instanceof FormBase.FormStatus) {
                    FormBase.FormStatus formBase = (FormBase.FormStatus) base;
                    if (formBase.formInfo.getMode().equals(TMLFormInfo.EDIT_MODE)) {
                        return formID;
                    } else {
                        return null;
                    }
                } else {
                    return formID;
                }            
            }
            else {
                return super.getRelevantForm();
            }
            
            
            
        }

        @Override
        public void initAttributeDelegates(Base tag) {
            ActionBase actionBase = (ActionBase) tag;
            this.ajax = actionBase.getAjax();
            
            if (this.ajax == null) { 
                try {
                    if (tmlContext.getPortletStateStorage() instanceof TMLPortletStateTransientStorage && !tmlContext.getportlet().isroot()) {
                        this.ajax = "true"; // AJAX defaults to true with transient portlet states, as this is more effective in this mode
                    }
                    else {
                        this.ajax = "false";
                    }
                }
                catch (WGAPIException e) {
                    tmlContext.getlog().error("Exception determining AJAX mode", e);
                    this.ajax = "false";
                }
            }
            
            super.initAttributeDelegates(tag);
        }
        
    }
    @Override
    protected BaseTagStatus createTagStatus() {
        Status status = new Status();
        return status;
    }
    
    public Status getStatus() {
        return (Status) super.getStatus();
    }
    
    private String form = null;  
    
    protected String ajax;
    // defines the portletmode to set after the action was called
    private String portletmode;
    
    // defines the portletcontext to set after the action was called
    private String portletcontext;
    
    public void tmlStartTag() throws WGException {
        Status status = (Status) getStatus();
        status.plainLink = stringToBoolean(getPlainlink());
        status.formID = getForm();
        
        // check if we are a child of an portlet include
        Include.Status includeTag = (Include.Status) getStatus().getAncestorIncludeTag("portlet");
        if (includeTag != null) {
            // if we are included without ajax,
            // set ajax to false to ensure actions are rendered correct
            if (!includeTag.ajax) {
                getStatus().ajax = "false";
            } 
        }        
    }

    /* (Kein Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.Base#getRelevantForm()
     */
    

    /**
     * @return
     */
    public String getForm() {
        return this.getTagAttributeValue("form", form, null);
    }

    /**
     * @param string
     */
    public void setForm(String string) {
        form = string;
    }

    protected String plainlink;

    /**
         * @return
         */
    public String getPlainlink() {
        return getTagAttributeValue("plainlink", plainlink, "false");
    }

    /**
         * @param string
         */
    public void setPlainlink(String string) {
        plainlink = string;
    }
    
    public String getAjax() {
        return this.getTagAttributeValue("ajax", ajax, null);
    }

    public void setAjax(String ajax) {
        this.ajax = ajax;
    }     
    
    public boolean isAjaxCall() {
        if (AJAX_MODE_NO_PORTLET_REFRESH.equalsIgnoreCase(getStatus().ajax)) {
            return true;
        }
        else {
            return stringToBoolean(getStatus().ajax);
        }
    }
 
    public String getPortletmode() {
        return this.getTagAttributeValue("portletmode", portletmode, null);
    }

    public void setPortletmode(String mode) {
        this.portletmode = mode;
    }   
    
    public String getPortletcontext() {
        return this.getTagAttributeValue("portletcontext", portletcontext, null);
    }

    public void setPortletcontext(String context) {
        this.portletcontext = context;
    }   

    protected String getAjaxJSFunction(TMLAction action, Map<String,Object> namedParams, List<Object> params, boolean keepParams) throws WGAPIException, TMLException {
    	return getAjaxJSFunction(action, namedParams, params, getActionCallParameters(), keepParams, null);
    }
    
    protected String getJSFunction(TMLAction action, Map<String,Object> namedParams, List<Object> params, boolean keepParams) throws WGAPIException, TMLException {
        return getJSFunction(action, namedParams, params, getActionCallParameters(), keepParams, null);
    }

    public String buildActionURL(TMLAction action, Map<String,Object> namedParams, List<Object> unnamedParams) throws UnsupportedEncodingException, WGException, TMLException {
        return buildActionURL(action, namedParams, unnamedParams, getPortletmode(), getPortletcontext());
    }

    public String buildActionURL(TMLAction action, Map<String,Object> namedParams, List<Object> params, String portletMode, String portletContext) throws UnsupportedEncodingException, WGException, TMLException {
        
        if (!stringToBoolean(getPlainlink()) && (getStatus().getRelevantForm() != null || getTMLContext().getPortletStateStorage() instanceof TMLPortletStateTransientStorage)) {
            return "javascript:" + getJSFunction(action, namedParams, params, getActionCallParameters(), true, null);
        }
        else {
            return getTMLContext().getURLBuilder().buildActionURL(getTMLContext(), action, namedParams, params, portletMode, portletContext);
        }
    }

    protected TMLActionCallParameters getActionCallParameters() {
        
        TMLActionCallParameters callParams = new TMLActionCallParameters();
        callParams.setAjaxMode(getStatus().ajax);
        callParams.setPortletContext(getPortletcontext());
        callParams.setPortletMode(getPortletmode());
        callParams.setRelevantForm(getStatus().getRelevantForm());
        return callParams;
        
    }
    
}
