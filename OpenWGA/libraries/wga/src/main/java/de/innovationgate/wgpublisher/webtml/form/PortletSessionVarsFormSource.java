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

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class PortletSessionVarsFormSource implements FormSource {

    private TMLPortlet _portlet;

    @Override
    public void init(TMLContext context) throws WGException {
        _portlet = context.getportlet();
        if (_portlet == null) {
            throw new UnavailableResourceException("Cannot find portlet to store form data on");
        }
    }
    

    @Override
    public List<?> getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation, TMLFormInfo formInfo) throws WGException {

        if (_portlet.hassessionvar(fieldname)) {
            return TMLContext.toList(_portlet.getsessionvar(fieldname));
        }
        else if (defaultvalue != null) {
            return (defaultvalue instanceof List<?> ? (List<?>) defaultvalue : Collections.singletonList(defaultvalue));
        }
        else {
            return Collections.EMPTY_LIST;
        }
        
    }

    @Override
    public boolean storeForm(TMLForm form, boolean includeFiles) throws WGException {

        Iterator<String> fieldNames = form.getfieldnames().iterator();
        String fieldName;
        while (fieldNames.hasNext()) {
            fieldName = (String) fieldNames.next();
            FieldReg fieldReg = form.getforminfo().getFieldRegistration(fieldName);
            if (fieldReg != null) {
                if (fieldReg.isStore()) {
                    if (fieldReg.isMultiple()) {
                        _portlet.setsessionvar(fieldName, form.fieldlist(fieldName));
                    }
                    else {
                        _portlet.setsessionvar(fieldName, form.field(fieldName));
                    }
                }
            } else {
                // store fields without fieldreg e.g. customFields                
               _portlet.setsessionvar(fieldName, form.field(fieldName));
            }
            
        }
        return true;
        
    }

}
