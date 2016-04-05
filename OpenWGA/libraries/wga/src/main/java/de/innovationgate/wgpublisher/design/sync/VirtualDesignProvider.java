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

package de.innovationgate.wgpublisher.design.sync;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.ehcache.Cache;
import net.sf.ehcache.Element;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGConfigurableTypeDesignProvider;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.WGVirtualDesignProvider;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wgpublisher.design.WGADesignProvider;

public class VirtualDesignProvider extends WGConfigurableTypeDesignProvider implements WGVirtualDesignProvider, WGADesignProvider {

    Logger _log = Logger.getLogger("wga.virtualdesigns");
    
    public class VirtualDocument implements WGDocumentCore {

        private File _filesDirectory = null;
        private List _fileNames = new ArrayList();

        private int _type;

        private String _mediaKey;
        private String _name;

        private String _documentKey;

        public VirtualDocument(int type, String name, String mediaKey) {
            _type = type;
            _created = new Date();
            _lastModified = new Date();
            _name = name;
            _mediaKey = mediaKey;
            _documentKey = WGDesignDocument.buildDesignDocumentKey(type, name, mediaKey);

            Map metas = getMetaMap();
            metas.put(WGDesignDocument.META_NAME, name);
            if (mediaKey != null) {
                metas.put(WGTMLModule.META_MEDIAKEY, mediaKey);
            }
            
            metas.put(WGDocument.META_PASTAUTHORS, new ArrayList());
            metas.put(WGDocument.META_PASTEDITDATES, new ArrayList());
            metas.put(WGDocument.META_REVISION, new Integer(1));
            writeMetaMap(metas);
        }

        private Date _created;

        private Date _lastModified;

        private boolean _deleted = false;

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#attachFile(java.io.File)
         */
        public synchronized boolean attachFile(File file) {

            try {
                File dir = getFilesDirectory();
                File attachedFile = new File(dir, file.getName().toLowerCase());
                if (attachedFile.exists()) {
                    attachedFile.delete();
                }
                attachedFile.createNewFile();

                InputStream in = new BufferedInputStream(new java.io.FileInputStream(file));
                OutputStream out = new BufferedOutputStream(new FileOutputStream(attachedFile));
                WGUtils.inToOut(in, out, 2048);
                in.close();
                out.close();
                _fileNames.add(attachedFile.getName());
                return true;
            }
            catch (FileNotFoundException e) {
                _log.error("Error attaching file to virtual design", e);
                return false;
            }
            catch (IOException e) {
                _log.error("Error attaching file to virtual design", e);
                return false;
            }

        }

        private synchronized File getFilesDirectory() throws IOException {
            if (_filesDirectory == null) {
                _filesDirectory = File.createTempFile("files", "", _tempFolder);;
                _filesDirectory.delete();
                _filesDirectory.mkdir();
            }
            return _filesDirectory;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#dispose(boolean)
         */
        public void dispose() {
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#evaluateExpression(java.lang.String)
         */
        public Object evaluateExpression(String expression) throws WGExpressionException {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getCreated()
         */
        public Date getCreated() {
            return _created;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getFastAccessKey()
         */
        public Object getFastAccessKey() {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getFileData(java.lang.String)
         */
        public InputStream getFileData(String strFile) {
            try {
                File file = new File(getFilesDirectory(), strFile.toLowerCase());
                if (file.exists()) {
                    return new FileInputStream(file);
                }
                else {
                    return null;
                }
            }
            catch (FileNotFoundException e) {
                _log.error("Error serving file data from virtual designs", e);
                return null;
            }
            catch (IOException e) {
                _log.error("Error serving file data from virtual designs", e);
                return null;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getFileNames()
         */
        public List getFileNames() {
            /*try {
                String[] fileList = getFilesDirectory().list();
                if (fileList != null) {
                    return new ArrayList(Arrays.asList(fileList));
                }
                else {
                    return new ArrayList();
                }
            }
            catch (IOException e) {
                _log.error("Error retrieving file names for virtual design", e);
                return null;
            }*/
            return new ArrayList(_fileNames);
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getFileSize(java.lang.String)
         */
        public int getFileSize(String strFile) {

            try {
                File file = new File(getFilesDirectory(), strFile.toLowerCase());
                if (file.exists()) {
                    return (int) file.length();
                }
                else {
                    return -1;
                }
            }
            catch (IOException e) {
                _log.error("Error retrieving file size for virtual provider", e);
                return -1;
            }

        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getItemNames()
         */
        public List getItemNames() {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getItemValue(java.lang.String)
         */
        public Object getItemValue(String strName) {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getLastModified()
         */
        public Date getLastModified() {
            return _lastModified;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getMetaData(java.lang.String)
         */
        public Object getMetaData(String type) {
            
            if (type.equals(WGDocument.META_CREATED)) {
                return _created;
            }
            else if (type.equals(WGDocument.META_LASTMODIFIED)) {
                return _lastModified;
            }
            else {
                return getMetaMap().get(type);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getNativeObject()
         */
        public Object getNativeObject() {
            return this;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#getType()
         */
        public int getType() {
            return _type;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#hasItem(java.lang.String)
         */
        public boolean hasItem(String strName) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#isDataCacheable()
         */
        public boolean isDataCacheable() {
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#isDeleted()
         */
        public boolean isDeleted() {
            return _deleted;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#isTemporary()
         */
        public boolean isTemporary() {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#remove()
         */
        public WGDatabaseRevision remove() throws WGAPIException {
            _deleted = true;
            removeDocument(this);
            fireDesignChangeEvent(this, WGUpdateLog.TYPE_DELETE);
            return null;
        }



        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#removeFile(java.lang.String)
         */
        public synchronized boolean removeFile(String name) {

            try {
                File dir = getFilesDirectory();
                File attachedFile = new File(dir, name.toLowerCase());
                if (attachedFile.exists()) {
                    attachedFile.delete();
                }
                _fileNames.remove(attachedFile.getName());
                return true;
            }
            catch (IOException e) {
                _log.error("Error removing file from virtual provider", e);
                return false;
            }

        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#removeItem(java.lang.String)
         */
        public boolean removeItem(String Name) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#save(java.util.Date)
         */
        public WGDatabaseRevision save(Date lastModified) throws WGAPIException {
            _lastModified = lastModified;
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#setItemValue(java.lang.String,
         *      java.lang.Object)
         */
        public boolean setItemValue(String strName, Object value) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#setMetaData(java.lang.String,
         *      java.lang.Object)
         */
        public boolean setMetaData(String name, Object value) {
            Map metas = getMetaMap();
            metas.put(name, value);
            writeMetaMap(metas);
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see de.innovationgate.webgate.api.WGDocumentCore#setWGDocument(de.innovationgate.webgate.api.WGDocument)
         */
        public void setWGDocument(WGDocument doc) {
        }

        public boolean isSaved() {
            return true;
        }

        /**
         * @return Returns the mediaKey.
         */
        private String getMediaKey() {
            return _mediaKey;
        }

        /**
         * @return Returns the name.
         */
        private String getName() {
            return _name;
        }
        

        
        private Map getMetaMap() {
            
            Element element = _cache.get(_documentKey);
            
            Map metas = null;
            if (element != null) {
                metas = (Map) element.getValue();
            }
            else {
                metas = new HashMap();
            }
           return metas;
            
        }
        
        private void writeMetaMap(Map map) {
            Element element = new Element(_documentKey, map);
            _cache.put(element);
        }

        public String getOriginDatabase() {
            return _db.getDbReference();
        }

		public void renameFile(String oldFileName, String newFileName) throws WGAPIException {
			throw new WGNotSupportedException("renameFile() is not supported on this document implementation.");			
		}
		
		public WGFileMetaData getFileMetaData(String strFile) throws WGAPIException {
			throw new WGNotSupportedException("getFileMetaData() is not supported on this document implementation.");
		}

        public WGDocumentCore getRelation(String name) throws WGAPIException {
            throw new WGNotSupportedException("content relations are not supported on this document implementation.");
        }

        public List getRelationNames() throws WGAPIException {
            throw new WGNotSupportedException("content relations are not supported on this document implementation.");
        }

        public WGDocumentCore removeRelation(String name) throws WGAPIException {
            throw new WGNotSupportedException("content relations are not supported on this document implementation.");
        }

        public WGDocumentCore setRelation(String name, WGDocumentCore target) throws WGAPIException {
            throw new WGNotSupportedException("content relations are not supported on this document implementation.");
        }

        public boolean hasFileMetadata() throws WGAPIException {
            return false;
        }

        public boolean hasFile(String fileName) throws WGBackendException {
            try {
                File file = new File(getFilesDirectory(), fileName.toLowerCase());
                return file.exists();
            }
            catch (FileNotFoundException e) {
                _log.error("Error serving file data from virtual designs", e);
                return false;
            }
            catch (IOException e) {
                _log.error("Error serving file data from virtual designs", e);
                return false;
            }
        }

        public WGRelationData getRelationData(String name) throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }





        public WGDocumentCore setRelation(WGRelationData relAddress) throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }





        public Object getExtensionData(String strName) throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }





        public List getExtensionDataNames() throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }





        public void removeExtensionData(String strName) throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");        
        }





        public void writeExtensionData(String strName, Object value) throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }

        public List<String> getRelationNamesOfGroup(String group, WGColumnSet order) throws WGBackendException {
            throw new WGNotSupportedException("content relations are not supported on this document implementation.");
        }

        @Override
        public WGExtensionDataContainer retrieveFileExtensionDataHandler(String strFile) throws WGAPIException {
            return null;
        }

        @Override
        public List<WGFileDerivateMetaData> getFileDerivates(String strFile) throws WGAPIException {
            return null;
        }
        
        @Override
        public void markFileMetaDataModified(WGFileMetaData md) throws WGAPIException {
        }

        @Override
        public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String, Object> customMdFields) throws WGAPIException,
                WGNotSupportedException {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public void removeFileDerivate(String id) throws WGAPIException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }

        @Override
        public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException {
            return null;
        }

        @Override
        public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException, WGNotSupportedException {
            throw new WGNotSupportedException("This operation is not supported by this database type");
        }

        @Override
        public InputStream getFileDerivateData(String id) throws WGAPIException {
            return null;
        }
        
        @Override
        public Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException {
            return Collections.<WGUpdateLog>emptyList().iterator();
        }


    }



    private File _tempFolder;

    private Map _fileContainers = new HashMap();

    private Map _scriptModules = new HashMap();

    private Map _tmlModules = new HashMap();
    
    private Set _designChangeListeners = new HashSet();

    private String _providerName;

    private WGDatabase _db;

    /**
     * Storage for metadatas of virtual documents
     * We use a cache here for it's overflow-to-disk capability, so we don't
     * store everything in memory
     */
    private Cache _cache;

    private DesignReference _designReference;

    public VirtualDesignProvider(DesignReference ref, String name, WGDatabase db, File tempFolder) throws IOException {

        _designReference = ref;
        List<Integer> providerTypesList = null;
        String optionProviderTypes = (String) db.getCreationOptions().get(WGDatabase.COPTION_DESIGNPROVIDERTYPES);
        if (optionProviderTypes != null) {
            providerTypesList = new ArrayList<Integer>();
            Iterator<String> providerTypes = WGUtils.deserializeCollection(optionProviderTypes, ",", true).iterator();
            while (providerTypes.hasNext()) {
                String providerTypeName = (String) providerTypes.next();
                int providerType = WGDocument.doctypeNameToNumber(providerTypeName);
                if (providerType != 0) {
                    providerTypesList.add(new Integer(providerType));
                }
            }
        }
        initProviderTypes(providerTypesList);
        
        _db = db;
        _providerName = name;
        _tempFolder = File.createTempFile("vdp", ".tmp", tempFolder);
        _tempFolder.delete();
        _tempFolder.mkdir();

        _cache = new Cache(
                "VDPCache_" + _tempFolder.getName(),
                100,
                true,
                true,
                0,
                0);
        
        WGFactory.getInstance().getCacheManager().addCache(_cache);
        
    }

    public List getDesignObjects(int type) {

        switch (type) {
            case WGDocument.TYPE_TML:
                return new ArrayList(_tmlModules.values());

            case WGDocument.TYPE_CSSJS:
                return new ArrayList(_scriptModules.values());

            case WGDocument.TYPE_FILECONTAINER:
                return new ArrayList(_fileContainers.values());

        }
        return null;

    }

    public WGDocumentCore getDesignObject(int type, String name, String mediaKey) {

        switch (type) {
            case WGDocument.TYPE_TML:
                return (WGDocumentCore) _tmlModules.get(name + "/" + mediaKey);

            case WGDocument.TYPE_CSSJS:
                return (WGDocumentCore) _scriptModules.get(name);

            case WGDocument.TYPE_FILECONTAINER:
                return (WGDocumentCore) _fileContainers.get(name);

        }
        return null;

    }



    public String getName() {
        return _providerName;
    }

    public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) {

        VirtualDocument doc = new VirtualDocument(type, name, mediaKey);
        put(doc, name, mediaKey);
        return doc;
    }

    private void put(VirtualDocument doc, String name, String mediaKey) {

        switch (doc.getType()) {
            case WGDocument.TYPE_TML:
                _tmlModules.put(name + "/" + mediaKey, doc);
                break;

            case WGDocument.TYPE_CSSJS:
                _scriptModules.put(name, doc);
                break;

            case WGDocument.TYPE_FILECONTAINER:
                _fileContainers.put(name, doc);
                break;

        }

    }
    
    private void removeDocument(VirtualDocument doc) {

        switch (doc.getType()) {
            case WGDocument.TYPE_TML:
                _tmlModules.remove(doc.getName() + "/" + doc.getMediaKey());
                break;

            case WGDocument.TYPE_CSSJS:
                _scriptModules.remove(doc.getName());
                break;

            case WGDocument.TYPE_FILECONTAINER:
                _fileContainers.remove(doc.getName());
                break;

        }
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignProvider#addDesignChangeListener(de.innovationgate.webgate.api.WGDesignChangeListener)
     */
    public void addDesignChangeListener(WGDesignChangeListener changeListener) {
        _designChangeListeners.add(changeListener);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignProvider#removeDesignChangeListener(de.innovationgate.webgate.api.WGDesignChangeListener)
     */
    public void removeDesignChangeListener(WGDesignChangeListener changeListener) {
        _designChangeListeners.remove(changeListener);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignProvider#dispose()
     */
    public void dispose() {
        _fileContainers.clear();
        _scriptModules.clear();
        _tmlModules.clear();
        _designChangeListeners.clear();
        WGFactory.getInstance().getCacheManager().removeCache(_cache.getName());
        WGUtils.delTree(_tempFolder);
        
        
    }
    
    private void fireDesignChangeEvent(VirtualDocument doc, int type) throws WGAPIException {
        
        List updateLogs = new ArrayList();
        updateLogs.add(new WGUpdateLog(type, new Date(), getName(), WGDocument.buildDocumentKey(doc, _db).toString(), null, null));
        WGDesignChangeEvent event = new WGDesignChangeEvent(this, _db, updateLogs);
         
        Iterator listeners = _designChangeListeners.iterator();
        while (listeners.hasNext()) {
            ((WGDesignChangeListener) listeners.next()).designChanged(event);
        }
    }

    public boolean isProviderCore(WGDocumentCore arg0) {
        return (arg0 instanceof VirtualDocument);
    }

    @Override
    public boolean isNotifying() {
        return true;
    }

    public DesignReference getDesignReference() {
        return _designReference;
    }

    public WGDatabase getConsumerDatabase() {
        return _db;
    }

    public boolean isLookupVariants() {
        return false;
    }

    public void closeSession() {
    }

    public void openSession(WGSessionContext context) {
    }

    public int designHashCode() {
        return _tempFolder.hashCode();
    }
    
    public boolean isReady() {
        return true;
    }
    
    @Override
    public boolean isSynchronizeAccess() {
        return false;
    }
    
    @Override
    public void clearCache() throws WGException {
        _cache.flush();
    }

    @Override
    public String getFileEncoding() {
        return null;
    }

}
