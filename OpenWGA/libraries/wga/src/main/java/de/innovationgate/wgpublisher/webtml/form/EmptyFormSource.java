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
import java.util.Collections;
import java.util.List;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class EmptyFormSource implements FormSource {

    private TMLContext _cx;

    @Override
    public void init(TMLContext context) throws WGException {
        _cx = context;
    }

    @Override
    public List<?> getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation, TMLFormInfo formInfo) throws WGException {

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

        boolean processed = false;
        
        // Try to use HDBModel to create new content
        String contentClass = form.getforminfo().getContentClass();
        HDBModel model = HDBModel.getModel(_cx.db());
        if (contentClass != null && _cx != null && model != null) {
            if (model.getModelsForContentClass(contentClass, _cx.content(), false).size() > 0) {
                return model.storeForm(form, _cx.content());
            }
        }
        
        // Try to determine a content type for creating a document in a real CS: either from meta field or the only ct available
        if (!processed && _cx.db().hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            WGContentType ct = null;
            
            String ctName = form.getforminfo().getContentClass();
            if (ctName != null) {
                ct = _cx.db().getContentType(ctName);
            }
            else if (_cx.db().getContentTypes().size() == 1) {
                ct = _cx.db().getContentTypes().get(0);
            }
            
            if (ct != null) {
                WGContent newChild = _cx.content().createChildPage(ct, "");
                form.pushFields(newChild);
                
                form.setcreateddoc(newChild);
                _cx = _cx.context(newChild);
                form.getforminfo().setTargetContextPath(_cx.getpath());
                
                // Optionally attach files too
                if (includeFiles) {
                    try {
                        form.pushFiles(newChild);
                        form.retainFiles(newChild);
                    }
                    catch (IOException e) {
                        throw new WGAServerException("IO Exception attaching files", e);
                    }
                }
                
                newChild.publish();
            }
        }
        
        return true;
        
    }

}
