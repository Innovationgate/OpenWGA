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
import java.security.GeneralSecurityException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.utils.Zipper;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.FieldReg;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.form.FormSource;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormField;
import de.innovationgate.wgpublisher.webtml.form.TMLFormParsingException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

/**
 * base class for form functions
 * tml:form and tml:item (BI-customeditor) needs the same functionality and
 * extends this class
 *
 */
public abstract class FormBase extends Base {
    
    private static final long serialVersionUID = 1L;
    private String source;
    private String onsubmit;
    private String persist;
    private String cssclass;
    private String cssstyle;
    private String htmlinput;
    private String mode;
    private String defaultaction;
    private String contentclass;
    private String maxuploadsize;
    private String syncfiles;

    public String getMaxuploadsize() {
        return getTagAttributeValue("maxuploadsize", maxuploadsize, null);
    }

    public void setMaxuploadsize(String maxuploadsize) {
        this.maxuploadsize = maxuploadsize;
    }

    public static class FormStatus extends BaseTagStatus implements FormInputRegistrator {
        protected TMLFormInfo formInfo;
        protected TMLForm thisForm;
        private String source;
        protected String mode;
        
        public String getFormMode() {
            return formInfo.getMode();
        }
        
        public void addFormValidation(String condition, String message, List ifnoerrorList, List cleariferrorList) {
            formInfo.addFormValidation(condition, message, ifnoerrorList, cleariferrorList);
        }
        
        public void addField(FieldReg fieldReg, Object value) throws WGException {             
            // put new fields in formInfo
            formInfo.addOrMergeFieldReg(fieldReg);

            try {
                // put field in tmlForm, so that it can be accessed by tmlscript just in renderTime
                thisForm.setfield(fieldReg.getName(), value);
            }
            catch (TMLFormParsingException e) {
                this.addWarning(e.getMessage(), false);
            }
        }

        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.webtml.FormInputRegistrator#getFieldValue(java.lang.String, boolean, java.lang.String)
         */
        public List getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation) throws WGException {
        
            // Look in form object (when form was posted previously)
            if (thisForm != null && thisForm.hasfield(fieldname)) {
                return thisForm.getEnteredOrParsedValues(fieldname);
            }
            
            // Fetch from form source
            FormSource fs = thisForm.fetchFormSource(tmlContext);
            return fs.getFieldValue(fieldname, meta, defaultvalue, useRelation, formInfo);
            
        }

        public String getId() {
            return formInfo.getFormId();
        }
        
        public void registerHashedPasswordField(String fieldname, String hashedValue) {
            TMLFormField field = new TMLFormField(fieldname);
            field.addValue(hashedValue);
            formInfo.addHashedPasswordField(field);
        }

        @Override
        public void initAttributeDelegates(Base tag) {

            FormBase formBase = (FormBase) tag;
            this.mode = formBase.getMode();
            this.source = formBase.getSource();
            
            super.initAttributeDelegates(tag);
        }

        public void importForm(TMLForm form) throws WGAPIException {
            thisForm = form;
            if (thisForm.getforminfo().importFormInfo(formInfo)) {
                thisForm.reset();
            }
            formInfo = form.getforminfo();
        }
    }
    
    public FormStatus getFormStatus() {
        return (FormStatus) getStatus();
    }
    
    private String keeponvalidate;        
    
    public void tmlStartTag() throws WGException {
		// Look if eventually posted form is this form, or form was persisted
        FormStatus status = (FormStatus) getStatus();
       
        // inititalize formInfo based on current tag attributes
        status.formInfo = new TMLFormInfo(getFormId(), this.getHtmlinput(), stringToBoolean(this.getPersist()), getTMLContext().getDesignContext().getVersionCompliance());
        status.formInfo.setSource(status.source);
        status.formInfo.setTrim(stringToBoolean(this.getTrim()));
        status.formInfo.setKeepOnValidate(stringToBoolean(this.getKeeponvalidate()));
        status.formInfo.setTargetContextPath(getTMLContext().getpath());
        
        if (getSyncfiles() != null) {
            status.formInfo.setSyncFiles(stringToBoolean(getSyncfiles()));
        }

        String contentClass = getContentclass();
        if (contentClass == null && "content".equals(status.source)) {
            WGContent content = getTMLContext().content();
            if (content != null) { // May happen when using non-content documents in WebTML forms
                contentClass = content.getContentClass();
            }
        }
        
        status.formInfo.setContentClass(contentClass);
        status.formInfo.setDefinitionModule(getStatus().getTMLModuleName());
        status.formInfo.setDefinitionDatabase(getDesignDBKey());
        
        updateDefaultAction(status.formInfo);
        
        // Create new form or import form info into existing form
		TMLForm form = this.getTMLContext().tmlformbyid(getFormId());
		if (form != null) {
            status.importForm(form);
		}		
		else {
            status.thisForm = getTMLContext().createform(status.formInfo);
		}
		getPageContext().getRequest().setAttribute(WGACore.ATTRIB_LASTFORM, status.thisForm);    	
    }
    
    protected abstract String getFormId();
    
    private void updateDefaultAction(TMLFormInfo info) throws WGAPIException, TMLException {
        
        String defaultActionID = getDefaultaction();
        if (defaultActionID != null) {
            TMLAction defaultAction = getTMLContext().getActionByID(defaultActionID, getDesignDBKey());
            if (defaultAction != null) {
                info.setDefaultAction(defaultAction.createActionLink(null, null, getTMLContext()).getEncodedString(getCore()));
            }
            else {
                addWarning("Unknown default action '" + defaultActionID + "'");
            }
        }
        
    }
    
    public boolean isFormEditable() {
        if (getFormStatus().formInfo.getMode().equals(TMLFormInfo.EDIT_MODE)) { 
            return true;
        } else {
            return false;
        }
    }
    
    /* (non-Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.FormInputRegistrator#addField(de.innovationgate.wgpublisher.webtml.utils.FieldReg, java.lang.Object)
	 */

    

    
    
    /**
     * Returns the source.
     * @return String
     */
    public String getSource() {
        return this.getTagAttributeValue("source", source, "content");
    }

    /**
     * Sets the source.
     * @param source The source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    /**
     * @return
     */
    public String getOnsubmit() {
        return this.getTagAttributeValue("onsubmit", onsubmit, "");
    }

    /**
     * @param string
     */
    public void setOnsubmit(String string) {
        this.onsubmit = string;
    }

    /**
     * @return
     */
    public String getPersist() {
        return this.getTagAttributeValue("persist", persist, "false");
    }

    /**
     * @param string
     */
    public void setPersist(String string) {
        persist = string;
    }

    /**
     * Returns the cssstyle.
     * @return String
     */
    public String getCssstyle() {
        return this.getTagAttributeValue("cssstyle", cssstyle, null);
    }

    /**
     * Sets the cssstyle.
     * @param cssstyle The cssstyle to set
     */
    public void setCssstyle(String cssstyle) {
        this.cssstyle = cssstyle;
    }
    /**
     * Returns the cssClass.
     * @return String
     */
    public String getCssclass() {
        return this.getTagAttributeValue("cssclass", cssclass, null);
    }

    /**
     * Sets the cssClass.
     * @param cssClass The cssClass to set
     */
    public void setCssclass(String cssClass) {
        this.cssclass = cssClass;
    }

    public String getHtmlinput() {
        return this.getTagAttributeValue("htmlinput", htmlinput, "false");
    }

    public void setHtmlinput(String htmlinput) {
        this.htmlinput = htmlinput;
    }

    public String getMode() {
        return this.getTagAttributeValue("mode", mode, null);
    }

    public void setMode(String mode) {
        this.mode = mode;
    }
    

    
    public String getKeeponvalidate() {
        return this.getTagAttributeValue("keeponvalidate", keeponvalidate, "true");
    }

    public void setKeeponvalidate(String keeponvalidate) {
        this.keeponvalidate = keeponvalidate;
    }     
    

    protected String renderFormStartTag(String id, String onsubmit, String cssStyle, String cssClass) throws WGException {
        StringBuffer buf = new StringBuffer();
        buf.append("<form");
        buf.append(buildDynamicHtmlAttributes());
        buf.append(" method=\"POST\" name=\"" + id + "\" id=\"" + id + "\" enctype=\"multipart/form-data\"");
        
        if (onsubmit != null) {
            if(!onsubmit.equalsIgnoreCase("")){
                buf.append(" onsubmit=\"");
                buf.append(onsubmit);
                buf.append("\"");
            }
        }
        if(cssStyle !=null){
            buf.append(" style=\"" + cssStyle + "\"");                  
        }
        if(cssClass!=null){
            buf.append(" class=\"" + cssClass + "\"");                  
        }
        //F000037B2
        if (getCore().getCharacterEncoding() != null) {
            buf.append(" accept-charset=\"" + getCore().getCharacterEncoding() + "\"");
        }
        buf.append(">\n");
        
        TMLForm.FormParseInfo formParseInfo = new TMLForm.FormParseInfo(getFormStatus().formInfo.getProcessId());
        String maxUploadSize = getMaxuploadsize();
        if (!WGUtils.isEmpty(maxUploadSize)) {
            try {
                formParseInfo.setMaxUploadSize(WGUtils.parseInt(maxUploadSize));
            }
            catch (NumberFormatException e) {
                addWarning("Unable to interpret maxuploadsize as integer number: " + maxUploadSize);
            }
        }
        else {
            formParseInfo.setMaxUploadSize((Integer) getCore().getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_WEBTML_FILEUPLAD_MAXSIZE));
        }
            
        String xml = XStreamUtils.XSTREAM_CLONING.toXML(formParseInfo);
        byte[] zipped = WGUtils.zipString(xml);
        try {
            String encrypted = getCore().getSymmetricEncryptionEngine().encryptBase64Web(zipped);
            buf.append("<input type=\"hidden\" name=\"").append(TMLForm.SYSTEMFIELD_PARSEINFO).append("\" value=\"").append(encrypted).append("\">");
        }
        catch (Exception e) {
            throw new TMLException("Exception serializing form render info", e, true);
        }
        
        buf.append("<script>if (typeof(WGA) != 'undefined') {WGA.b4submit.reset(\"" + this.getId() + "\")};</script>");
        return buf.toString();
    }
    
    protected String renderAdditionHiddenFormFields() {
        StringBuffer additionalFields = new StringBuffer();
        additionalFields.append("<input type=\"hidden\" name=\"").append(TMLForm.SYSTEMFIELD_FORMACTION).append("\" value=\"\">");
        additionalFields.append("<input type=\"hidden\" name=\"$ajaxcallid\" value=\"\">");
        additionalFields.append("<input type=\"hidden\" name=\"$ajaxgraydiv\" value=\"\">");
        additionalFields.append("<input type=\"hidden\" name=\"$ajaxmode\" value=\"\">");
        additionalFields.append("<input type=\"hidden\" name=\"$portletstates\" value=\"\">");
        return additionalFields.toString();         
    }
    
    protected String renderFormInfo(TMLFormInfo formInfo, TMLContext context) {
    	try {
            String serializedInfo = serializeFormInfo(formInfo, context);
            if (serializedInfo != null) {
            	return "<input type=\"hidden\" name=\"" + TMLForm.SYSTEMFIELD_FORMINFO + "\" value=\"" + serializedInfo + "\">";
            } else {
            	return "";
            }
        }
        catch (Exception e) {
            getCore().getLog().error("Exception rendering form info", e);
            addWarning("Unable to render form info. See application log for details.", true);
            return "";
        }
    }
    
    static String serializeFormInfo(TMLFormInfo info, TMLContext context) throws UnsupportedEncodingException, GeneralSecurityException, WGException {
    	// optimze formInfo
    	info.optimize();
        // serialize FormInfo        
        String serFormInfo = context.getwgacore().getLibraryXStream().toXML(info);        
        byte[] zipped  = WGUtils.zipString(serFormInfo);
        if (zipped != null) {
            // encrypt
            return context.getwgacore().getSymmetricEncryptionEngine().encryptBase64Web(zipped);
        }
        return null;
    }
    
    protected String renderFormEndTag() {
        return "</form>";
    }

    public String getDefaultaction() {
        return getTagAttributeValue("defaultaction", defaultaction, null);
    }

    public void setDefaultaction(String defaultaction) {
        this.defaultaction = defaultaction;
    }

    public String getContentclass() {
        return getTagAttributeValue("contentclass", contentclass, null);
    }

    public void setContentclass(String contentclass) {
        this.contentclass = contentclass;
    }

    @Override
    protected BaseTagStatus createTagStatus() {
        return new FormStatus();
    }

    public String getSyncfiles() {
        return getTagAttributeValue("syncfiles", syncfiles, null);
    }

    public void setSyncfiles(String syncfiles) {
        this.syncfiles = syncfiles;
    }

               
}
