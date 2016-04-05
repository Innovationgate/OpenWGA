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

import java.io.IOException;
import java.util.List;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Source of data for WebTML forms.
 * Implementations of this object are instantiated for concrete occasions where fields are read and forms are stored.
 */
public interface FormSource {
    
    /**
     * Init method called before the source is used.
     * @param context The WebTML target context of the WebTML form
     * @throws WGException
     */
    public void init(TMLContext context) throws WGException;
    
    /**
     * Retrieve a field value from the form source for a form field
     * @param fieldname The name of the field
     * @param meta Whether the field is regarded a metadata field
     * @param defaultvalue The default value to return if the field does not exist
     * @param useRelation Whether the field is regarded to originate from a content relation
     * @param formInfo The form info of the retrieving form with eventually necessary status information
     * @return The field value as list. Form fields always store lists.
     * @throws WGException
     */
    public List<?> getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation, TMLFormInfo formInfo) throws WGException;
    
    /**
     * Store the data of the given form to the form source backend.
     * It is in the responsibility of this method to
     * <ul>
     * <li> Generally store the fields according to the available form field registration, like respecting multvalue/non-multivalue fields
     * <li> Especially filter out fields whose field registration mark them as stored="false"
     * </ul>
     * It is not in the responsibility of the form source to validate the form.
     * @param form The form whose data to store
     * @param includeFiles Whether files attached to the form should also be stored, if even possible
     * @return Always true
     * @throws WGException
     */
    public boolean storeForm(TMLForm form, boolean includeFiles) throws WGException;

}
