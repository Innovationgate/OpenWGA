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
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Form extends FormBase implements DynamicAttributes {
    
    public static class Status extends FormBase.FormStatus implements DirectOutputCapableTag {

        @Override
        public boolean isDirectOutputCapable() {
            return (WGUtils.isEmpty(this.divider));
        }
        
    }
    
    private static final long serialVersionUID = 1L;
    // deprecated attribute
    private String editable;
    
    /**
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlEndTag()
	 */
	public void tmlEndTag() throws TMLException, WGAPIException {
        this.appendResult(renderAdditionHiddenFormFields());
        this.appendResult(renderFormInfo(getFormStatus().formInfo, this.getTMLContext()));
        this.appendResult(renderFormEndTag());
	}
	
	@Override
	protected BaseTagStatus createTagStatus() {
	    return new Status();
	}

	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlStartTag()
	 */
	public void tmlStartTag() throws WGException {
		super.tmlStartTag();
		
		FormStatus status = getFormStatus();
		
		// Check for cascaded WebTML-Forms which is an error
		FormStatus parentForm = (FormStatus) getStatus().getAncestorTag(FormBase.class);
		if (parentForm != null) {
		    addWarning("Cannot create form because it is already contained in another form tag. Cascading forms is not supported.", true);
		    return;
		}
		
		// mimetype = html/text
		// this.appendResult("<form method=\"POST\" name=\"" + id + "\" id=\"" + id + "\">");
		// mimetype = multipart/form-data
		// accept-charset does not work on windows
		//this.appendResult("<form method=\"POST\" name=\"" + id + "\" id=\"" + id + "\" enctype=\"multipart/form-data\" accept-charset=\"iso-8859-1\"");
		String formStartTag = renderFormStartTag(this.getId(), this.getOnsubmit(), this.getCssstyle(), this.getCssclass());
        this.appendResult(formStartTag);		        

        // if new attribute mode is set, ignore old attribute editable
        if (status.mode != null) {
           status.formInfo.setMode(status.mode);           
        } else {
            // set Form mode
            if (this.stringToBoolean((this.getEditable()))) {
                status.formInfo.setMode(TMLFormInfo.EDIT_MODE);
            } else {
                status.formInfo.setMode(TMLFormInfo.VIEW_MODE);
            }                    
        }
	} 
            
	@Override
	public boolean isFormEditable() {
        if (getFormStatus().formInfo.getMode().equals(TMLFormInfo.EDIT_MODE)) { 
            return true;
        } else {
            return false;
        }
	}

    /**
	 * Returns the editable.
	 * @return String
	 */
	public String getEditable() {
		return this.getTagAttributeValue("editable", editable, "true");
	}

	/**
	 * Sets the editable.
	 * @param editable The editable to set
	 */
	public void setEditable(String editable) {
		this.editable = editable;
	}



    @Override
    protected String getFormId() {
        return getId();
    }
        
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "html"); 
    }
    
    @Override
    protected boolean isTrimResultString() {
        return true;
    }
}
