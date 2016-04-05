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

package de.innovationgate.webgate.api;

import java.io.File;
import java.io.InputStream;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * A multi-use wrapper for document cores to enhance them with some specialized
 * functionality
 */
public class WGDocumentCoreWrapper implements WGDocumentCore {
    
    private WGDocumentCore _core;

    public WGDocumentCoreWrapper(WGDocumentCore core) {
        _core = core;
    }

    public boolean attachFile(File file) throws WGAPIException {
        return _core.attachFile(file);
    }

    public void dispose() throws WGAPIException {
        _core.dispose();

    }

    public Object evaluateExpression(String expression) throws WGAPIException {
       return _core.evaluateExpression(expression);
    }

    public Date getCreated() throws WGAPIException {
        return _core.getCreated();
    }

    public Object getFastAccessKey() throws WGBackendException {
        return _core.getFastAccessKey();
    }

    public InputStream getFileData(String strFile) throws WGAPIException {
        return _core.getFileData(strFile);
    }

    public List getFileNames() throws WGAPIException {
        return _core.getFileNames();
    }

    public int getFileSize(String strFile) throws WGAPIException {
        return _core.getFileSize(strFile);
    }

    public List getItemNames() throws WGAPIException {
        return _core.getItemNames();
    }

    public Object getItemValue(String strName) throws WGAPIException {
       return _core.getItemValue(strName);
    }

    public Date getLastModified() throws WGAPIException {
        return _core.getLastModified();
    }

    public Object getMetaData(String type) throws WGAPIException {
        return _core.getMetaData(type);
    }

    public Object getNativeObject() throws WGAPIException {
        return _core.getNativeObject();
    }

    public int getType() {
        return _core.getType();
    }

    public boolean hasItem(String strName) throws WGAPIException {
        return _core.hasItem(strName);
    }

    public boolean isDataCacheable() {
        return _core.isDataCacheable();
    }

    public boolean isDeleted() throws WGAPIException {
        return _core.isDeleted();
    }

    public boolean isSaved() throws WGAPIException {
        return _core.isSaved();
    }

    public boolean isTemporary() throws WGAPIException {
        return _core.isTemporary();
    }

    public WGDatabaseRevision remove() throws WGAPIException {
        return _core.remove();
    }

    public boolean removeFile(String name) throws WGAPIException {
        return _core.removeFile(name);
    }

    public boolean removeItem(String Name) throws WGAPIException {
        return _core.removeItem(Name);
    }

    public WGDatabaseRevision save(Date lastModified) throws WGAPIException {
        return _core.save(lastModified);
    }

    public boolean setItemValue(String strName, Object value) throws WGAPIException {
        return _core.setItemValue(strName, value);
    }

    public boolean setMetaData(String name, Object value) throws WGAPIException {
        return _core.setMetaData(name, value);
    }

    public void setWGDocument(WGDocument doc) {
        _core.setWGDocument(doc);
    }

    public String getOriginDatabase() {
        return _core.getOriginDatabase();
    }

	public void renameFile(String oldFileName, String newFileName)
			throws WGAPIException {
		_core.renameFile(oldFileName, newFileName);		
	}

	public WGFileMetaData getFileMetaData(String strFile) throws WGAPIException {
		return _core.getFileMetaData(strFile);
	}

    public WGDocumentCore getRelation(String name) throws WGAPIException {
        return _core.getRelation(name);
    }

    public List<String> getRelationNames() throws WGAPIException {
        return _core.getRelationNames();
    }

    public WGDocumentCore removeRelation(String name) throws WGAPIException {
        return _core.removeRelation(name);
    }

    public boolean hasFileMetadata() throws WGAPIException {
        return _core.hasFileMetadata();
    }

    public boolean hasFile(String file) throws WGAPIException {
        return _core.hasFile(file);
    }

    public Object getExtensionData(String strName) throws WGAPIException {
        return _core.getExtensionData(strName);
    }

    public List getExtensionDataNames() throws WGAPIException {
        return _core.getExtensionDataNames();
    }

    public void removeExtensionData(String strName) throws WGAPIException {
        _core.removeExtensionData(strName);
        
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        _core.writeExtensionData(strName, value);
    }

    public WGRelationData getRelationData(String name) throws WGAPIException {
        return _core.getRelationData(name);
    }

    public WGDocumentCore setRelation(WGRelationData data) throws WGAPIException {
        return _core.setRelation(data);
    }

    public List<String> getRelationNamesOfGroup(String group, WGColumnSet order) throws WGAPIException {
        return _core.getRelationNamesOfGroup(group, order);
    }

    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String strFile) throws WGAPIException {
        return _core.retrieveFileExtensionDataHandler(strFile);
    }
    
    public List<WGFileDerivateMetaData> getFileDerivates(String strFile) throws WGAPIException {
        return _core.getFileDerivates(strFile);
    }

    @Override
    public void markFileMetaDataModified(WGFileMetaData md) throws WGAPIException {
        _core.markFileMetaDataModified(md);
    }

    @Override
    public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String, Object> customMdFields) throws WGAPIException,
            WGNotSupportedException {
        return _core.createFileDerivate(originalFileName, creator, derivateName, in, customMdFields);
    }

    @Override
    public void removeFileDerivate(String id) throws WGAPIException {
        _core.removeFileDerivate(id);
    }

    @Override
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException {
        return _core.getFileDerivateMetaData(id);
    }

    @Override
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException, WGNotSupportedException {
        _core.writeFileDerivateMetaData(md);
    }

    @Override
    public InputStream getFileDerivateData(String id) throws WGAPIException {
        return _core.getFileDerivateData(id);
    }
    
    @Override
    public Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException {
        return _core.getLastUpdates();
    }
    


}
