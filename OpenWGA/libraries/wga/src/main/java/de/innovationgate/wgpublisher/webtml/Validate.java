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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.FormBase.FormStatus;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Validate extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _condition;
    private String _ifnoerror;
    
    private String _sourcetag;
    
    // a comma separated list of fieldnames to clear if this validation fails
    private String _cleariferror;
    


    public void tmlEndTag() throws TMLException {
        String message = this.getResultString(false);
        String condition = this.getCondition();
        String ifnoerror = this.getIfnoerror();
        String cleariferror = this.getCleariferror();
        
        List ifnoerrorList = new ArrayList();
        if (ifnoerror != null) {
            ifnoerrorList = WGUtils.deserializeCollection(ifnoerror, ",");
        }
        
        List cleariferrorList = new ArrayList();
        if (cleariferror != null) {
            cleariferrorList = WGUtils.deserializeCollection(cleariferror, ",");
        }
        

        FormStatus formTag = null;
        if (this.getSourcetag() != null) {
           formTag = (FormStatus) this.getTagStatusById(this.getSourcetag(), FormBase.FormStatus.class);
            if (formTag == null) {
                this.getTMLContext().addwarning("Form with id '" + this.getSourcetag() + "' not found.", true);
                return;
            }                
        }
        else {
            formTag = (FormStatus) getStatus().getAncestorTag(Form.class);
        }        
        
        if (formTag != null) {
            // register with formTag
            formTag.addFormValidation(condition, message, ifnoerrorList, cleariferrorList );
        } else {            
            this.getTMLContext().addwarning("Could not find corresponding formtag.", true);
            return;            
        }                
        
        String formId = formTag.formInfo.getFormId();
        TMLForm _tmlForm = this.getTMLContext().tmlformbyid(formId); 
        // show message in body only if condition failed
        if (!_tmlForm.conditionFailed(condition)) {
            this.setResultOutput(false);
        } else {
            // resolve scriptlets
            RhinoExpressionEngine engine = (RhinoExpressionEngine) ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
            if (engine == null) {
                this.getTMLContext().addwarning("Scriptlets in tml:validate cannot be resolved. Error initializing tmlscript-engine.", false);
            } else {
                try {
                	Map params = new HashMap();
                	params.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_SCRIPTLETS);
                    message = engine.resolveScriptlets(message, this.getTMLContext(), params);
                } 
                catch (WGException e) {
                    this.getTMLContext().addwarning("Scriptlets in tml:validate cannot be resolved bc of exception: " + e.getClass().getName() + " message: " + e.getMessage(), false);
                }
            }
            this.setResult(message);
        }
    }

    public String getCondition() {
        return this.getTagAttributeValue("condition", _condition, null);
    }

    public void setCondition(String condition) {
        _condition = condition;
    }
    
    public String getSourcetag() {
        return this.getTagAttributeValue("sourcetag", _sourcetag, null);
    }

    public void setSourcetag(String sourcetag) {
        _sourcetag = sourcetag;
    }

    public String getIfnoerror() {
        return this.getTagAttributeValue("ifnoerror", _ifnoerror, null);
    }

    public void setIfnoerror(String ifnoerror) {
        _ifnoerror = ifnoerror;
    }
    
    public String getCleariferror() {
        return getTagAttributeValue("cleariferror" , _cleariferror, "");
    }

    public void setCleariferror(String cleariferror) {
        this._cleariferror = cleariferror;
    }    
}
