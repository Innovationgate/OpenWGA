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
import java.util.Iterator;
import java.util.List;

import de.innovationgate.wgpublisher.webtml.FormBase.FormStatus;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class FormMessages extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static final String SCOPE_ALL = "all";
    private static final String SCOPE_GLOBAL = "global";
    
    private String _scope;
    
    public static class Status extends BaseTagStatus {
        private TMLForm _tmlForm = null;
    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
    
    private String _sourcetag;
   
    
    public void tmlEndTag() throws TMLException {
        
        Status status = (Status) getStatus();
        
        // iterate over messages and write result
        List results = new ArrayList();
        
        
        Iterator<String> messages;
        String scope = getScope();
        if (SCOPE_ALL.equals(scope)) {
            messages = status._tmlForm.getmessages().iterator(); 
        }
        else if (SCOPE_GLOBAL.equals(scope)) {
            messages = status._tmlForm.getglobalmessages().iterator();
        }
        else {
            String msg = status._tmlForm.getmessage(scope);
            List<String> list = new ArrayList<String>();
            if (msg != null) {
                list.add(msg);
            }
            messages = list.iterator();
            
        }
        
        while (messages.hasNext()) {
            String message = (String) messages.next();
            results.add(message);
        }
        setResult(results);
   }

    /**
     * @see de.innovationgate.wgpublisher.webtml.Base#tmlStartTag()
     */
    public void tmlStartTag() throws TMLException {
        
        Status status = (Status) getStatus();
        
        FormStatus formBase = null;
        if (this.getSourcetag() != null) {
            formBase = (FormStatus) this.getTagStatusById(this.getSourcetag(), FormBase.class);
            if (formBase == null) {
                this.getTMLContext().addwarning("Form or Item with id '" + this.getSourcetag() + "' not found.", true);
                return;
            }                
        } else {
            formBase = (FormStatus) getStatus().getAncestorTag(FormBase.class);
        }        
        
        if (formBase == null) {
        	status._tmlForm = getTMLContext().gettmlform();
        } 
        else {
        	String formId = formBase.formInfo.getFormId();        		
        	status._tmlForm = this.getTMLContext().tmlformbyid(formId);
        }
        
        if (status._tmlForm == null) {
            this.getTMLContext().addwarning("Could not retrieve tmlform.", true);
            return;
        }
            
    }

    public String getSourcetag() {
        return this.getTagAttributeValue("sourcetag", _sourcetag, null);
    }

    public void setSourcetag(String sourcetag) {
        _sourcetag = sourcetag;
    }

    public String getScope() {
        return getTagAttributeValue("scope",_scope, SCOPE_ALL);
    }

    public void setScope(String scope) {
        _scope = scope;
    }
   
}
