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

package de.innovationgate.wgpublisher.hdb;

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wgpublisher.webtml.utils.UniqueNamePartFormatter;

/**
 * Parameter class for HDBModel operations, collecting all possible parameters
 */
@CodeCompletion
public class HDBModelParams {
    
    private String _type;
    private String _contentClass;
    private Object _customParam;
    private Form _form;
    private String _createContentID;
    private WGContent _refDocument;
    private WGContentKey _refDocumentKey;
    private HDBModelProcess _process;
    private String _cancelMessage = null;
    private Map<String,Object> _ccEventObjects = new HashMap<String, Object>();
    
    protected void resetStatus() {
        _ccEventObjects.clear();
    }
    
    
    /**
     * Return a process to be executed on the operation
     */
    public HDBModelProcess getProcess() {
		return _process;
	}

	/**
	 * Sets a process to be executed on the operation
	 */
	public void setProcess(HDBModelProcess process) {
		_process = process;
	}

	/**
	 * Constructor.
	 * @param type Type of document to update. Use constants TYPE_... on {@link HDBModel}
	 */
	public HDBModelParams(String type) {
        _type = type;
    }

    /**
     * Constructor.
     * @param content Document on which the operation will be performed
     */
    public HDBModelParams(WGContent content) throws WGAPIException {
        _type = content.getItemText(HDBModel.ITEM_TYPE);
        _contentClass = content.getContentClass();
        _refDocument = content;
        _refDocumentKey = content.getContentKey();
    }

    /**
     * Returns the content class of the document on which the operation will be done
     */
    public String getContentClass() {
        return _contentClass;
    }

    /**
     * Sets the content class of the document on which the operation will be done
     */
    public void setContentClass(String contentClass) {
        _contentClass = contentClass;
    }

    /**
     * Returns a custom parameter passed to the operation
     */
    public Object getCustomParam() {
        return _customParam;
    }

    /**
     * Sets a custom parameter that is passed to the operation
     */
    public void setCustomParam(Object customParam) {
        _customParam = customParam;
    }

    /**
     * Returns the type of document on which the operation is performed (Values of constants TYPE_... on {@link HDBModel})
     */
    public String getType() {
        return _type;
    }

    /**
     * Sets a WebTML form to be injected to the operation
     */
    public void setForm(Form form) {
        _form = form;
    }

    /**
     * Returns the WebTML form to be injected to the operation
     */
    public Form getForm() {
        return _form;
    }

    /**
     * Returns a content ID that should be assigned to a created content of the operation
     */
    public String getCreateContentID() {
        return _createContentID;
    }

    /**
     * Sets a content ID that should be assigned to a created content of the operation
     */
    public void setCreateContentID(String createContentID) {
        _createContentID = UniqueNamePartFormatter.INSTANCE.format(createContentID);
    }

    /**
     * Returns the reference document for the operation
     */
    public WGContent getRefDocument() {
        return _refDocument;
    }

    /**
     * Sets the reference document for the operation
     * @throws WGAPIException
     */
    public void setRefDocument(WGContent refDocument) throws WGAPIException {
        _refDocument = refDocument;
        if (refDocument != null) {
            _refDocumentKey = refDocument.getContentKey();
        }
        else {
            _refDocumentKey = null;
        }
    }

    /**
     * Returns the key of the reference document for the operation
     */
    public WGContentKey getRefDocumentKey() {
        return _refDocumentKey;
    }

    /**
     * Sets the key of the reference document for the operation
     */
    public void setRefDocumentKey(WGContentKey refDocumentKey) {
        _refDocumentKey = refDocumentKey;
    }
    
    /**
     * Sets a TMLScript module of the given database to be executed as process on this operation
     * @param db App containing the module
     * @param moduleName Name of a TMLScript module to execute
     */
    public void setUpdateModuleProcess(WGDatabase db, String moduleName) {
        _process = new TMLScriptHDBModelProcess(db, moduleName);
    }

    /**
     * Returns a cancellation message if the operation has been cancelled
     */
    protected String getCancelMessage() {
        return _cancelMessage;
    }

    /**
     * Sets a cancellation message
     */
    protected void setCancelMessage(String cancelMessage) {
        _cancelMessage = cancelMessage;
    }


    protected Map<String,Object> getCcEventObjects() {
        return _ccEventObjects;
    }


}
