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
import java.util.Collections;
import java.util.List;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.form.FieldReg;
import de.innovationgate.wgpublisher.webtml.form.FormSource;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormField;
import de.innovationgate.wgpublisher.webtml.form.TMLFormParsingException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class AjaxFormInputRegistrator implements FormInputRegistrator {
	
	private TMLForm _form;
	private TMLContext _context;

	public AjaxFormInputRegistrator(TMLForm form) {
		_form = form;
		_context = form.gettargetcontext();
	}

	public void addField(FieldReg fieldReg, Object value) throws WGException {
        _form.getforminfo().addOrMergeFieldReg(fieldReg);

        try {
            _form.setfield(fieldReg.getName(), value);
        }
        catch (TMLFormParsingException e) {
            _context.addwarning(e.getMessage(), false);
        }

	}

	public List getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation) throws WGException {
        String source = _form.getsource();

        if (_form.hasfield(fieldname)) {
            return _form.getEnteredOrParsedValues(fieldname);
        }
        
        FormSource fs = _form.fetchFormSource(_context);
        return fs.getFieldValue(fieldname, meta, defaultvalue, useRelation, _form.getforminfo());
        
	}

	public String getFormMode() {
		return _form.getmode();
	}

	public String getId() {
		return _form.getformid();
	}

    public void registerHashedPasswordField(String fieldname, String hashedValue) {
        TMLFormField field = new TMLFormField(fieldname);
        field.addValue(hashedValue);
        _form.getforminfo().addHashedPasswordField(field);
        
    }

}
