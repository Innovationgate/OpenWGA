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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCancelledException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.hdb.HDBModelException;
import de.innovationgate.wgpublisher.hdb.HDBModelFormValidationFailedException;
import de.innovationgate.wgpublisher.hdb.HDBModelParams;
import de.innovationgate.wgpublisher.webtml.FormBase;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class ContextDocumentFormSource implements FormSource {

    private TMLContext _cx;

    @Override
    public List<?> getFieldValue(String fieldname, boolean meta, Object defaultvalue, boolean useRelation, TMLFormInfo formInfo) throws WGAPIException {

        WGDocument doc = _cx.getdocument();
        
        // Try to retrieve content relation
        if (useRelation && doc instanceof WGContent) {
            WGContent content = (WGContent)doc;
            List<String> values = new ArrayList<String>();
            
            WGRelationData relationData = content.getRelationData(fieldname);
            if (relationData != null) {
                values.add(relationData.getTargetStructkey().toString());
            }
            else {
                for (WGRelationData relData : content.getRelationsDataOfGroup(fieldname)) {
                    values.add(relData.getTargetStructkey().toString());
                }
            }
            
            return values;
        }
        
        // Document metadata
        else if (meta) {
            return doc.getMetaDataList(fieldname);
        } 
        
        // Document item
        else if (doc.hasItem(fieldname)) {
            return doc.getItemValueList(fieldname);
        } 
        
        // Variables and item mappings
        else if (_cx.hasVariable(fieldname) || _cx.hasMappedItemValue(fieldname)) {
            return _cx.itemlist(fieldname);
        } 
        
        // Empty field
        else if (formInfo.getMode().equals(TMLFormInfo.EDIT_MODE)) {
            if (defaultvalue != null) {
                return (defaultvalue instanceof List<?> ? (List<?>) defaultvalue : Collections.singletonList(defaultvalue));
            }
            else {
                return doc.getDatabase().getNoItemBehaviour().getForTMLFormEmptyFieldList();
            }
        } 
        else {
            return Collections.EMPTY_LIST;
        }    
        
    }

    @Override
    public boolean storeForm(TMLForm form, boolean includeFiles) throws WGAServerException, WGAPIException {

        WGDocument doc = _cx.getdocument();
        
        // Look if we rather should store via HDBModel
        if (doc instanceof WGContent) {
            WGContent content = (WGContent) doc;
            if (!content.isDummy() && content.getStructEntry().getContentType()!=null && content.getStructEntry().getContentType().getName().equals(WGHierarchicalDatabase.CONTENT_TYPE)) {
                HDBModel model = HDBModel.getModel(content.getDatabase());
                if (model != null && model.isContent(content)) {
                    return model.storeForm(form, content);
                }
            }
        }
        
        
        // Regular document storage
        form.pushFields(doc);
        boolean result =  doc.save();
        
        // Optionally attach files too
        try {
            if (result == true && includeFiles && doc instanceof WGContent) {
                WGContent targetContent = (WGContent) doc;
                boolean needToSave = form.pushFiles(targetContent);
                needToSave = form.retainFiles(targetContent) || needToSave; 
                if (needToSave) {
                    targetContent.save();
                }
            }
        }
        catch (IOException e) {
            throw new WGAServerException("IO Exception attaching files", e);
        }
        
        return result;
        
        
    }

    @Override
    public void init(TMLContext context) throws WGException {
        _cx = context;
    }

}
