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
package de.innovationgate.wgpublisher.webtml.form;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.tml.FormInfo;
import de.innovationgate.wgpublisher.so.NoopScopeObjectContextCreator;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;

/**
 * contains information about a form which could not be or should not be submited
 * with a hidden field.
 * The information are stored in the user session by the form tag and restored 
 * by the corresponding TMLForm object.
 *
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE,beanMode=CodeCompletion.BEAN_MODE_ALL,delegate=FormInfo.class)
public class TMLFormInfo implements FormInfo {
    
    private String _definitionModule;
    private String _definitionDatabase;

    /**
     *  contains form field informations (FieldReg), mapped by <fieldname>
     *  linked Hashmap to save order
     */
    private LinkedHashMap<String,FieldReg> _fieldRegistrations;
    
    /** 
     * contains the form source (content, profile, etc.)
     */
    private String _source = "none";        
    
    /** 
     * trim whole form, trim-attribute of form tag
     */
    private boolean _trim = false;
    
    /** 
     * contains global form validations, mapped by condition
     * linked Hashmap to save order
     */
    private LinkedHashMap _formValidations;
    
    /**
     * contains all fieldnames of the last formrendering
     */
    private Set<String> _lastRenderedFormFields;
    
    /**
     * contains the formId
     */
    private String _formId;
    
    /**
     * contains the processId
     */
    private String _processId;
    
    /**
     * contains the path of the target context (if source="content")
     */
    private String _targetContextPath;
    
    /**
     *  html input-tags are allowed on this form (possible values: 'true', 'false', 'ignore'
     */
    private String _htmlInput;
    
    private boolean _syncFiles = false;
    
    /**
     * Link for Default action to be executed whenever the form is submitted without an action explicitly specified
     */
    private String _defaultAction = null;

    /**
     * Content class to use when storing this form as new content
     */
    private String _contentClass = null;
    
    // form mode, "edit,view,readonly"
    private String _mode;
    
    private ScopeObjectRegistry _scopeObjectRegistry;
    
    public static final String EDIT_MODE = "edit";
    public static final String VIEW_MODE = "view";
    public static final String READONLY_MODE = "readonly";
    
    public static final String HTMLINPUT_IGNORE = "ignore";
    public static final String HTMLINPUT_TRUE = Boolean.TRUE.toString();
    public static final String HTMLINPUT_FALSE = Boolean.FALSE.toString();
    
    private boolean _persistent;
    
    // contains fields (instances of TMLForm.TMLFormFields) which are set via tmlscript
    // by the method tmlform.setfield()
    // these fields should also be stored implicitly by tmlform
    private Map<String,TMLFormField> _customFields;
    
    // contains hashedPassword fields (instances of TMLForm.TMLFormFields)
    // which has been rendered with a value from source (content, etc.)
    // in this case the fields are already hashed
    // this map is used to ensure not to hash these fields twice
    private HashMap _hashedPasswordFields;
    
    // flag is set to true if the form was once validated
    private boolean _validated;
    
    // flag if form should keep on validate if validate once
    private boolean _keepOnValidate;
    
    
    private Version _versionCompliance = CSConfig.getComplianceVersion(CSConfig.VERSIONCOMPLIANCE_WGA50);
    
    public static class FormValidation {
        private String _message;
        private List _ifnoerror;
        private List _cleariferror;
        
        public FormValidation(String message, List ifnoerror, List cleariferror) {
            _message = message;
            _ifnoerror = ifnoerror;
            _cleariferror = cleariferror;
        }
        
        /**
         * no-arg constructor for XStream on JRockit-VM
         *
         */
        private FormValidation() {
        }        
        
        public List getIfnoerror() {
            return _ifnoerror;
        }
        public void setIfnoerror(List ifnoerror) {
            _ifnoerror = ifnoerror;
        }
        public String getMessage() {
            return _message;
        }
        public void setMessage(String message) {
            _message = message;
        }

        public List getCleariferror() {
            return _cleariferror;
        }

        public void setCleariferror(List cleariferror) {
            _cleariferror = cleariferror;
        }
        
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#getSource()
     */
    @Override
    @CodeCompletion
    public String getSource() {
        return _source;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#setSource(java.lang.String)
     */
    @Override
    @CodeCompletion
    public void setSource(String source) {
        _source = source;
    }


    public TMLFormInfo(String formId, String htmlInputMode, boolean persistent, Version complianceVersion) {
        this(formId);
        _htmlInput = htmlInputMode;
        _persistent = persistent;
        _versionCompliance = complianceVersion;
        
        // Some version dependent defaults
        if (_versionCompliance.isAtLeast(6, 0)) {
            _syncFiles = true;
        }
    }
    
    private TMLFormInfo(String formId) {
        this();
        _formId = formId;
        _processId = UIDGenerator.generateUID();
        _scopeObjectRegistry = new ScopeObjectRegistry(ObjectScope.FORM, "WebTML Form " + formId, new NoopScopeObjectContextCreator());
    }
    
    /**
     * no-arg constructor for XStream on JRockit VM
     *
     */
    private TMLFormInfo() {
        _fieldRegistrations = new LinkedHashMap();
        _formValidations = new LinkedHashMap();
        _lastRenderedFormFields = new HashSet();
        _mode = EDIT_MODE; 
        _customFields = new HashMap();
        _hashedPasswordFields = new HashMap();
        _scopeObjectRegistry = new ScopeObjectRegistry(ObjectScope.FORM, "WebTML Form", new NoopScopeObjectContextCreator());
    }    
    
    public void addOrMergeFieldReg(FieldReg fieldReg) {
        // if fieldReg already exists, merge
        if (_fieldRegistrations.containsKey(buildKey(fieldReg))) {
            FieldReg existing = (FieldReg) _fieldRegistrations.get(buildKey(fieldReg));
            existing.importFieldReg(fieldReg);
        }
        else {
            // put new registration in map
            _fieldRegistrations.put(buildKey(fieldReg), fieldReg);
        }
        // put name into lastRenderedFormFields
        _lastRenderedFormFields.add(fieldReg.getName());
    }
    
    public void removeFieldReg(String fieldname) {
        _fieldRegistrations.remove(fieldname);
    }
    
    public Collection<FieldReg> getFieldRegistrations() {
        return _fieldRegistrations.values();
    }
    
    public FieldReg getFieldRegistration(String fieldname) {
        return (FieldReg) _fieldRegistrations.get(fieldname);
    }    
    
    public boolean containsFieldRegistration(String fieldname) {
        return _fieldRegistrations.containsKey(fieldname);
    }
    
    public void reset() {
        this._lastRenderedFormFields.clear();
        this._formValidations.clear();
        this._fieldRegistrations.clear();
        //this._source = null;
        //this._trim = false;
        this._validated = false;
        
        //B00004706
        this._customFields.clear();
        this._hashedPasswordFields.clear();
    }
    
    /**
     * merge given forminfo with this
     * @param formInfo
     * @return true if the form info changed so that a reset of the underlying form is neccessary
     */
    public boolean importFormInfo(TMLFormInfo formInfo) {
        
        if (formInfo == null) {
            return false;
        }
        
        boolean reset = false;
        
        if (!this._formId.equals(formInfo.getFormId())) {
            throw new RuntimeException("Error. You tried to merge formInfos of different formIds.");
        }
        
        // merge fieldregistrations
        Iterator fieldRegistrations = formInfo.getFieldRegistrations().iterator();
        while (fieldRegistrations.hasNext()) {
            FieldReg fieldReg = (FieldReg) fieldRegistrations.next();
            this.addOrMergeFieldReg(fieldReg);
        }
        
        //update source
        if (!WGUtils.nullSafeEquals(_source, formInfo.getSource())) {
            _source = formInfo.getSource();
            reset = true;
            
            // Changing the source allows for also changing the target context
            if (!WGUtils.nullSafeEquals(_targetContextPath, formInfo.getTargetContextPath())) {
                _targetContextPath = formInfo.getTargetContextPath();
            }
        }
        
        //update trim
        if (formInfo.isTrim()) {
            _trim = formInfo.isTrim();
        }
        
        //update mode
        if (formInfo.getMode() != null && !formInfo.getMode().trim().equals("")) {
            _mode = formInfo.getMode();
        }
        
        //update default action
        if (formInfo.getDefaultAction() != null && !formInfo.getDefaultAction().trim().equals("")) {
            _defaultAction = formInfo.getDefaultAction();
        }
        
        //update content class
        if (formInfo.getContentClass() != null && !formInfo.getContentClass().trim().equals("")) {
            _contentClass = formInfo.getContentClass();
        }

        return reset;
            
        
    }
    
    private String buildKey(FieldReg fieldReg) {
        return fieldReg.getName();// + "_" + fieldReg.isMeta();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#isTrim()
     */
    @Override
    @CodeCompletion
    public boolean isTrim() {
        return _trim;
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#setTrim(boolean)
     */
    @Override
    @CodeCompletion
    public void setTrim(boolean trim) {
        this._trim = trim;
    }


    public HashMap getFormValidations() {
        return _formValidations;
    }
    
    public void addFormValidation(String validation, String message, List ifnoerrorList, List cleariferrorList) {
        _formValidations.put(validation, new FormValidation(message, ifnoerrorList, cleariferrorList));
    }


    public Set<String> getLastRenderedFormFields() {
        return _lastRenderedFormFields;
    }

    public void clearLastRenderedFormFields() {
        _lastRenderedFormFields.clear();
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#getFormId()
     */
    @Override
    @CodeCompletion
    public String getFormId() {
        return _formId;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#setFormId(java.lang.String)
     */
    @Override
    @CodeCompletion
    public void setFormId(String formId) {
        _formId = formId;
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#getHtmlInput()
     */
    public String getHtmlInput() {    	
        return _htmlInput;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#isHtmlInput()
     */
    @Override
    @CodeCompletion
    public boolean isHtmlInput() {
    	if (_htmlInput.trim().equalsIgnoreCase(HTMLINPUT_IGNORE)) {
    		return false;
    	} else {
    		return WGUtils.stringToBoolean(_htmlInput);
    	}
    }
    
    
    public void setHtmlInput(boolean input) {
        _htmlInput = new Boolean(input).toString();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#getMode()
     */
    @Override
    @CodeCompletion
    public String getMode() {
        return _mode;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#setMode(java.lang.String)
     */
    @Override
    @CodeCompletion
    public void setMode(String mode) throws IllegalArgumentException {
        if (!isCorrectMode(mode)) {
            throw new IllegalArgumentException("Unsupported form-mode '" + mode + "'.");
        }        
        _mode = mode.trim().toLowerCase();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#isPersistent()
     */
    @Override
    @CodeCompletion
    public boolean isPersistent() {
        return _persistent;
    }
    
    public boolean isCorrectMode(String mode) {
        if (mode == null) {
            return false;
        }
        mode = mode.trim().toLowerCase(); 
        if ( !(mode.equals(EDIT_MODE)) &&
             !(mode.equals(READONLY_MODE)) &&   
             !(mode.equals(VIEW_MODE))) {
            return false;
        } else {
            return true;
        }
    }
    
    public void addCustomField(TMLFormField field) {
        _customFields.put(field.getName(), field);
    }
    
    public void removeCustomField(String fieldname) {        
        _customFields.remove(fieldname);
    }
    
    public Map<String,TMLFormField> getCustomFields() {
        return _customFields;
    }
    
    /**
     * removes all fields which where rendered in the last request from customFields,
     * to avoid these fields to be submitted twice (with FormInfo AND with standard formfield)
     * This method should be called, before the formInfo is serialized. 
     */
    public void optimize() {
    	//B00004642
        Iterator fieldnames = _lastRenderedFormFields.iterator();
        while (fieldnames.hasNext()) {
        	String fieldname = (String) fieldnames.next();
            FieldReg fieldReg = (FieldReg) _fieldRegistrations.get(fieldname);
            // only remove enabled fields - other are not submitted or will be ignored if submitted
            // with this its possible to display a field in view mode and also store the value in content
            if (fieldReg != null && fieldReg.getMode().equals(FieldReg.EDIT_MODE)) {
                _customFields.remove(fieldname);
            }
        }
        
    }
    
    public void addHashedPasswordField(TMLFormField field) {
        _hashedPasswordFields.put(field.getName(), field);
    }
    
    public TMLFormField getHashedPasswordField(String fieldname) {       
        return (TMLFormField) _hashedPasswordFields.get(fieldname);
    }


    public boolean isValidated() {
        return _validated;
    }


    public void setValidated(boolean validated) {
        _validated = validated;
    }


    public boolean keepOnValidate() {
        return _keepOnValidate;
    }


    public void setKeepOnValidate(boolean keepOnValidate) {
        _keepOnValidate = keepOnValidate;
    }


    public String getTargetContextPath() {
        return _targetContextPath;
    }


    public void setTargetContextPath(String targetContextPath) {
        _targetContextPath = targetContextPath;
    }


    public String getDefaultAction() {
        return _defaultAction;
    }


    public void setDefaultAction(String defaultAction) {
        _defaultAction = defaultAction;
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#getContentClass()
     */
    @Override
    @CodeCompletion
    public String getContentClass() {
        return _contentClass;
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.TMLFormInfo#setContentClass(java.lang.String)
     */
    @Override
    @CodeCompletion
    public void setContentClass(String contentclass) {
        _contentClass = contentclass;
    }


    public String getDefinitionModule() {
        return _definitionModule;
    }


    public void setDefinitionModule(String definitionModule) {
        _definitionModule = definitionModule;
    }


    public String getDefinitionDatabase() {
        return _definitionDatabase;
    }


    public void setDefinitionDatabase(String definitionDatabase) {
        _definitionDatabase = definitionDatabase;
    }


    public String getProcessId() {
        return _processId;
    }


    public void setProcessId(String processId) {
        _processId = processId;
    }
    
    public Version getVersionCompliance() {
        return _versionCompliance;
    }


    public boolean isSyncFiles() {
        return _syncFiles;
    }

    public void setSyncFiles(boolean syncFiles) {
        _syncFiles = syncFiles;
    }
    

    public void setPersistent(boolean persistent) {
        _persistent = persistent;
    }
    
    @Override
    public void setDefinition(Design design) {
        _definitionDatabase = design.getBaseReference().getDesignApp();
        _definitionModule = design.getBaseReference().getResourceName();
    }

    public ScopeObjectRegistry getScopeObjectRegistry() {
        return _scopeObjectRegistry;
    }
}
