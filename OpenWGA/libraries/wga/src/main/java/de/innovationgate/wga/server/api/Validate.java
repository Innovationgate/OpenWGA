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

package de.innovationgate.wga.server.api;

import java.util.Date;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.webtml.form.FieldReg;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;

/**
 * This object offers comfortable functions to validate data in common ways, prominently in WebTML form validations.
 * The methods of this object that have no parameters are provided for TMLScript to be used within WebTML field validation expressions (<tml:input> attribute validation), so they have no use in Java.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Validate {
    
    private WGA _wga;

    /**
     * Public constructor, just for internal purposes
     * @param wga
     */
    public Validate(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Returns if the WebTML form field is empty
     * Provided for TMLScript and not usable in Java
     * @throws WGAServerException
     */
    public boolean isEmpty() throws WGException {
        return isEmpty(retrieveFormFieldName());
    }
    
    /**
     * Returns if the WebTML form field is empty
     * Provided for TMLScript and not usable in Java
     * @throws WGAServerException
     */
    public boolean isFilled() throws WGException {
        return !isEmpty(retrieveFormFieldName());
    }

    private String retrieveFormFieldName() throws WGException {
        
        try {
            String fieldName = (String) _wga.tmlcontext().item(TMLForm.VALIDATIONVAR_FIELDNAME);
            if (fieldName == null) {
                throw new WGAServerException("Validator methods without field name argument can only be used within field validation expression");
            }
            return fieldName;
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception retrieving current form field name", e);
        }
    }
    
    /**
     * Returns if the WebTML form field is empty
     * This method is the same as calling {@link Form#isempty(String)} with the given field name
     * @param fieldName Name of the field
     * @throws WGAServerException
     */
    public boolean isEmpty(String fieldName) throws WGException {
        TMLForm form = (TMLForm) retrieveForm();
        return form.isempty(fieldName);
    }
    
    /**
     * Returns if a WebTML form field is filled
     * This method performs the opposite test as {@link #isEmpty(String)}. You could use "!isEmpty()" instead in your code but using this method makes it easier to read and understand correctly, prevents errors and may have optimisations for this vice-versa test in the future.
     * @param fieldName Field name
     * @throws WGAServerException
     */
    public boolean isFilled(String fieldName) throws WGException {
        return !isEmpty(fieldName);
    }

    private Form retrieveForm() throws WGException {
        Form form = _wga.tmlcontext().gettmlform();
        if (form == null) {
            throw new WGAServerException("No form present to validate");
        }
        return form;
    }

    private FieldReg retrieveFieldReg(String fieldName) throws WGException {
        FieldReg fieldReg = ((TMLFormInfo) _wga.tmlcontext().gettmlform().getforminfo()).getFieldRegistration(fieldName);
        if (fieldReg == null) {
            fieldReg = new FieldReg(fieldName, "text", null, false, false, null, null, null, false, null, null, true, null);
        }
        return fieldReg;
    }
    
    /**
     * Tests if the date is a valid date in the future
     * Provided for TMLScript and not usable in Java
     * @throws WGAServerException
     */
    public boolean isDateInFuture() throws WGException {
        
        
        String fieldName = retrieveFormFieldName();
        FieldReg fieldReg = retrieveFieldReg(fieldName);
        if (!"date".equals(fieldReg.getType())) {
            return false;
        }
        
        Date pValue = (Date)  retrieveForm().parsedvalue(fieldName);
        return isDateInFuture(pValue);
        
        
    }
    
    /**
     * Tests if the date is a valid date in the past
     * Provided for TMLScript and not usable in Java
     * @throws WGAServerException
     */
    public boolean isDateInPast() throws WGException {
        
        
        String fieldName = retrieveFormFieldName();
        FieldReg fieldReg = retrieveFieldReg(fieldName);
        if (!"date".equals(fieldReg.getType())) {
            return false;
        }
        
        Date pValue = (Date)  retrieveForm().parsedvalue(fieldName);
        return isDateInPast(pValue);
        
        
    }
    
    /**
     * Tests if the date is a valid date in the future
     * @param date Date to test
     * @throws WGAServerException
     */
    public boolean isDateInFuture(Date date) throws WGException {

        return date != null && date.getTime() > System.currentTimeMillis();
                
    }
    
    /**
     * Tests if the date is a valid date in the past
     * @param date Date to test
     * @throws WGAServerException
     */
    public boolean isDateInPast(Date date) throws WGException {

        return date != null && date.getTime() < System.currentTimeMillis();
                
    }


}
