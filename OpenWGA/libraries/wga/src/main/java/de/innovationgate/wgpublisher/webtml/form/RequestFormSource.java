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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class RequestFormSource implements FormSource {

    private TMLContext _cx;
    private HttpServletRequest _request;

    @Override
    public void init(TMLContext context) throws WGException {
        _cx = context;
        _request = context.getrequest();
    }

    @Override
    public List<?> getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation, TMLFormInfo formInfo) throws WGException {

    	if(_request!=null){
    		String[] p_values = _request.getParameterValues(fieldname);
    		if(p_values != null){
        		ArrayList<String> values = new ArrayList<String>();
	    		for(String value : p_values){
	    			values.add(value);
	    		}
	    		return values;
    		}
    	}
        if (defaultvalue != null) {
            if (defaultvalue instanceof List) {
                return (List) defaultvalue;
            }
            else {
                return Collections.singletonList(defaultvalue);
            }
        }
        else {
            return _cx.db().getNoItemBehaviour().getForTMLFormEmptyFieldList();
        }
    }

    @Override
    public boolean storeForm(TMLForm form, boolean includeFiles) throws WGException {
        return false;        
    }

}
