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
package de.innovationgate.webgate.api.jdbc;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import net.sf.cglib.beans.BeanMap;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.DOMWriter;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.NonUniqueObjectException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.exception.ConstraintViolationException;
import org.hibernate.proxy.HibernateProxy;
import org.hibernate.proxy.LazyInitializer;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGClosedSessionException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureSequenceProvider;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGFileMetaDataContext;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGPortletRegistry;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.webgate.api.jdbc.filehandling.CS41FileAttachmentHandler;
import de.innovationgate.webgate.api.jdbc.filehandling.CS5P4FileAttachmentHandler;
import de.innovationgate.webgate.api.jdbc.filehandling.CS5P4FileHandling;
import de.innovationgate.webgate.api.jdbc.filehandling.FileAttachmentHandler;
import de.innovationgate.webgate.api.jdbc.filehandling.FileHandling;

/**
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates. To enable and disable the creation of type
 * comments go to Window>Preferences>Java>Code Generation.
 */
public class WGDocumentImpl implements WGDocumentCore {
    
    private class UpdateLogIterator implements SkippingIterator<WGUpdateLog>, Closeable {
        private final HibernateQueryIterator _it;

        private UpdateLogIterator(HibernateQueryIterator it) {
            _it = it;
        }

        @Override
        public boolean hasNext() {
            return _it.hasNext();
        }

        @Override
        public WGUpdateLog next() {
            Object[] results = (Object[]) _it.next();
            return _parent.readUpdateLog((LogEntry) results[0]);
        }

        @Override
        public void remove() {
            _it.remove();
        }

        @Override
        public int skip(int nrOfElements) {
            return _it.skip(nrOfElements);
        }
        
        @Override
        public void close() throws IOException {
            _it.close();
        }
    }

    public static final XStream XSTREAM_SERIALIZER = new XStream(new Dom4JDriver());
    
    public static final int ATTACHMENT_FILEPART_SIZE = 1024*64-1;

    /**
     * Column to store struct entry workflow names is "WORKFLOW", not "OVERRIDE_WORKFLOW" (which must be kept bc. of other WGACS implemenetations)
     */
    public static final String ENTITYPROP_STRUCTENTRY_WORKFLOW = "WORKFLOW";

    public static final int ITEMTYPE_STRING = 1;

    public static final int ITEMTYPE_NUMBER = 2;

    public static final int ITEMTYPE_DATE = 3;

    public static final int ITEMTYPE_SERIALIZED_CASTOR = 4;
    
    public static final int ITEMTYPE_SERIALIZED_XSTREAM = 5;
    
    public static final int ITEMTYPE_BOOLEAN = 6;
    
    public static final int ITEMTYPE_BINARY = 7;
    
    public static final int ITEMTYPE_JSON = 8;

    private WGDocument _document;

    protected WGDatabaseImpl _parent;

    private int _type;

    protected MainEntity _entity;
    
    protected BeanMap _beanMap = null;
    
    private boolean _editable = false;
    
    protected WGFileMetaDataContext _fileMetadataContext = new WGFileMetaDataContext() {

        @Override
        public void markMetaDataModified(WGFileMetaData md) throws WGAPIException {
            WGDocumentImpl.this.markFileMetaDataModified(md);
        }

        @Override
        public WGDocument getFileParent() {
            return _document;
        }
        
    };
    
    public void makeEditable() throws WGAPIException {        
        
        if (_editable == false) {
            if (_parent.getSessionStatus().isInSaveOperation()) { // Do not make editable if already in save operation
                return;
            }
            
            if (_parent._saveIsolationActive) {
                _entity.loadFully();
                if (_entity instanceof HibernateProxy) { // Unproxying because of #00002040
                    HibernateProxy proxy = (HibernateProxy) _entity;
                    LazyInitializer lazyInitializer = proxy.getHibernateLazyInitializer();
                     if(lazyInitializer.isUninitialized()) {
                         lazyInitializer.initialize();
                     }
                     _entity = (MainEntity) lazyInitializer.getImplementation();
                    _parent.getSession().evict(proxy);
                }
                else {
                    
                _parent.getSession().evict(_entity);
            }
                
            }
            _editable = true;
        }
        
    }
    
    public void undoEditable()throws WGAPIException  {
        if (_editable == true) {
            if (_parent._saveIsolationActive && isSaved()) {
                _entity = (MainEntity) _parent.getSession().load(WGDatabaseImpl.getClassByType(_type), _entity.getId());
            }
            _editable = false;
        }
    }
    
    private BeanMap getBeanMap() {
        
        if (_beanMap == null) {
            BeanMap.Generator gen = new BeanMap.Generator();
            gen.setBean(_entity);
            gen.setUseCache(true);
            _beanMap = gen.create();
        }
        return _beanMap;
        
    }

    protected Content _content;

    protected UserProfile _profile;

    
    private Map<String,ExtensionData> _removedExtData = new HashMap<String,ExtensionData>();
    private List<Entity> _additionalObjectsToSave = new ArrayList<Entity>();
    
    public void addAdditionalEntityToSave(Entity obj) {
        _additionalObjectsToSave.add(obj);
    }
	private Map<String,Item> _removedItems = new HashMap<String,Item>();
	private Map<String,ContentRelation> _removedRelations = new HashMap<String,ContentRelation>();
	protected Map<UserProfilePortlet, Map<String,UserProfileItem>> _removedPortletItems = new HashMap<UserProfilePortlet, Map<String,UserProfileItem>>();

    private WGPortletRegistry _portletRegistry;

    private boolean _deleted = false;

    private FileAttachmentHandler _fileHandler;

    private PortletItemStorageImpl _portletItemStorage;

    protected WGDocumentImpl(WGDatabaseImpl parent, MainEntity entity, int type) {
        _parent = parent;
        _type = type;
        _entity = entity;
        _fileHandler = parent.getFileHandling().createDocumentHandler(this);

        if (type == WGDocument.TYPE_CONTENT) {
            _content = (Content) entity;
        }
        else if (type == WGDocument.TYPE_USERPROFILE) {
            _profile = (UserProfile) entity;
            _portletRegistry = createPortletRegistry();
            if (_parent._ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                _portletItemStorage = new PortletItemStorageImpl(parent, this);
            }
        }
        
        if (_entity.getCreated() == null) {
            _editable = true;
        }
    }

    /**
     * @throws WGIllegalStateException - if method is unsupported on the current document
     * @throws WGIllegalArgumentException - if a file of the same name already exists
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#attachFile(File)
     */
    public boolean attachFile(File file) throws WGAPIException {

        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
        
        makeEditable();
        
        _fileHandler.attachFile(file, _parent.convertFileNameForAttaching(file.getName()));
        return true;
    }

	/**
	 * @throws HibernateException 
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#dispose()
	 */
    public void dispose() throws WGAPIException, HibernateException {
        // Prevents that changes to the entity are again picked up if this document is re-read
        if (isSaved()) {
            getParent().getSession().evict(_entity);
        }
    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#evaluateExpression(String)
     */
    public Object evaluateExpression(String expression) throws WGExpressionException {
        return null;
    }

    /**
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getCreated()
     */
    public Date getCreated() {
        return _entity.getCreated();
    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#getFastAccessKey()
     */
    public Object getFastAccessKey() {
        
        if (!isSaved()) {
            return null;
        }

        if (_parent._ddlVersion >= WGDatabase.CSVERSION_WGA5) {
            return new WGDatabaseImpl.V5FastAccessKey(_type, _entity.getId());
        }
        else {
            if (_content != null) {
                if (_content.getId() != null) {
                    return _content.getId();
                }
                else {
                    return _content.getCuid();
                }
            }
            else {
                return null;
            }
        }

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getFileData(String)
     */
    public InputStream getFileData(String strFile) throws WGAPIException {
        
        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }

        return _fileHandler.getFileData(_parent.convertFileNameForAttaching(strFile));
    	
    }
	
    /**
	 * @throws WGAPIException 
     * @throws WGSystemException
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getFileNames()
	 */
    public List<String> getFileNames() throws WGAPIException {
        
        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
        
        return _fileHandler.getFileNames();

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getFileSize(String)
     */
    public int getFileSize(String strFile) throws WGAPIException {
        
        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
        
    	return _fileHandler.getFileSize(_parent.convertFileNameForAttaching(strFile));
    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#getItemNames()
     */
    public List getItemNames() {

        List itemNames;
        if (_content != null) {
            itemNames = new ArrayList(_content.getItems().keySet());
        }
        else if (_profile != null) {
            itemNames =  new ArrayList(_profile.getItems().keySet());
        }
        else {
            itemNames = new ArrayList();
        }
        
        itemNames.removeAll(_removedItems.keySet());
        return itemNames;

    }

    /**
     * @throws WGSystemException 
     * @throws WGBackendException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getItemValue(String)
     */
    public Object getItemValue(String strName) throws WGAPIException {

        try {
            strName = strName.toLowerCase();
            if (_removedItems.containsKey(strName)) {
                return null;
            }

            Item item = null;
            if (_content != null) {
                item = (Item) _content.getItems().get(strName);
            }
            else if (_profile != null) {
                item = (Item) _profile.getItems().get(strName);
            }

            if (item == null) {
                return null;
            }

            return readItemValue(_parent, this, item);
        }
        catch (HibernateException e) {
            throw new WGBackendException("Exception retrieving item " + strName, e);
        }

    }

    protected static Object readItemValue(WGDatabaseImpl db, WGDocumentImpl doc, Item item) throws WGAPIException {
        switch (item.getType()) {
        
            case ITEMTYPE_SERIALIZED_XSTREAM:
                try {
                    
                    DeserialisationCache cache = item.getDeserialisationCache();
                    int serializedHashCode = item.getText().hashCode();
                    if (cache != null && cache.getSerializedHashCode() == serializedHashCode) {
                        return cache.getDeserializedObject();
                    }
                    
                    Object obj = XSTREAM_SERIALIZER.fromXML(item.getText());
                    cache = new DeserialisationCache(serializedHashCode, obj);
                    item.setDeserialisationCache(cache);
                    return obj;
                    
                    
                }
                catch (Exception e) {
                    throw new WGIllegalDataException("Error deserializing itemvalue of type 'ITEMTYPE_SERIALIZED_XSTREAM'.", e);
                }

            case ITEMTYPE_SERIALIZED_CASTOR:
                Unmarshaller unmarshaller = new Unmarshaller();
                try {

                    // Workaround for castor bug. Primitive wrappers for
                    // boolean, character, long do not contain xsi:type
                    Document domDoc = DocumentHelper.parseText(item.getText());
                    Element root = domDoc.getRootElement();
                    if (root.getName().equals("boolean")) {
                        root.addNamespace("xsi", "http://www.w3.org/2001/XMLSchema-instance");
                        root.addAttribute("xsi:type", "java:java.lang.Boolean");
                    }
                    else if (root.getName().equals("character")) {
                        root.addNamespace("xsi", "http://www.w3.org/2001/XMLSchema-instance");
                        root.addAttribute("xsi:type", "java:java.lang.Boolean");
                    }
                    else if (root.getName().equals("long")) {
                        root.addNamespace("xsi", "http://www.w3.org/2001/XMLSchema-instance");
                        root.addAttribute("xsi:type", "java:java.lang.Long");
                    }
                    else if (root.getName().equals("array-list")) {
                        root.addNamespace("xsi", "http://www.w3.org/2001/XMLSchema-instance");
                        root.addAttribute("xsi:type", "java:java.util.ArrayList");
                    }

                    DOMWriter domWriter = new DOMWriter();
                    return unmarshaller.unmarshal(domWriter.write(domDoc));
                    // return unmarshaller.unmarshal(new
                    // StringReader(item.getText()));
                }
                catch (MarshalException e) {
                    throw new WGIllegalDataException("Error unmarshaling serialized object", e);
                }
                catch (ValidationException e) {
                    throw new WGIllegalDataException("Error unmarshaling serialized object", e);
                }
                catch (DocumentException e) {
                    throw new WGIllegalDataException("Error unmarshaling serialized object", e);
                }

            
            case ITEMTYPE_JSON:
                return new JsonParser().parse(item.getText());
                
            case ITEMTYPE_STRING:
                return item.getText();

            case ITEMTYPE_NUMBER:
                return item.getNumber();

            case ITEMTYPE_DATE:
                return new Date(item.getDate().getTime());

            case ITEMTYPE_BOOLEAN:
                return new Boolean(item.getNumber().doubleValue() == 1);
                
            case ITEMTYPE_BINARY:
                if (item instanceof ExtensionData) {
                    return db.getFileHandling().readBinaryExtensionData(doc, (ExtensionData) item);
                }
                else {
                    throw new WGSystemException("Unsupported itemtype '" + item.getType() + "' for the current entity");
                }
                
                
            default:
                throw new WGSystemException("Unsupported itemtype '" + item.getType() + "'.");

        }
    }

    /**
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getLastModified()
     */
    public Date getLastModified() throws WGSystemException {
        return _entity.getLastmodified();
    }

    /**
     * @throws WGBackendException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getMetaData(String)
     */
    public Object getMetaData(String name) throws WGAPIException {
        
        try {

	        // Special metas
	        if (name.equals(WGDocument.META_PASTAUTHORS)) {
	            return getPastAuthors();
	        }
	        else if (name.equals(WGDocument.META_PASTEDITDATES)) {
	            return getPastEditDates();
	        }
	        else if (name.equals(WGDocument.META_REVISION)) {
	            return new Integer(getRevision());
	        }
	        else if (_entity instanceof StructEntry && name.equals(WGStructEntry.META_WORKFLOW_NAME)) {
	            return getStructWorkflowName();
	        }
	        else if (_entity instanceof StructEntry && name.equals(WGStructEntry.META_PAGESEQUENCE)) {
	            return getPageSequence();
	        }
	        else if (name.equals(WGUserProfile.META_PORTLETREGISTRY)) {
	            return _portletRegistry;
	        }
	        else if (name.equals(WGUserProfile.META_PORTLETITEMSTORAGE)) {
	            return _portletItemStorage;
	        }
	        
	        Object value = getBeanMap().get(name.toLowerCase());;
	        
	        // Return keys to entities
	        if (value instanceof MainEntity) {
	            if (name.equals(WGStructEntry.META_AREA)) {
	                value = ((Area) value).getName();
	            }
	            else if (name.equals(WGStructEntry.META_CONTENTTYPE)) {
	                value = ((ContentType) value).getName();
	            }
	            else if (name.equals(WGContent.META_STRUCTENTRY)) {
	                value = ((StructEntry) value).getKey();
	            }
	            else if (name.equals(WGContent.META_LANGUAGE)) {
	                value = ((Language) value).getName();
	            }
	        }
	        
	        // Convert SQL-special date derivates to a standard date
	        else if (value instanceof Date) {
	            value = new Date(((Date) value).getTime());
	        }
	        return value;
        
        }
        catch (HibernateException e) {
            throw new WGBackendException("Exception retrieving metadata field " + name, e);
        }
        
    }

    /**
     * The struct's workflow name is stored in the column ORIGINALKEY, which was used before
     * WGA 3.3.1 to store the struct key of the original struct, from which this struct was cloned.
     * Some functionality is needed to determine if the column contains an obsolete original
     * key or a workflow name.
     */
    private Object getStructWorkflowName() {
        
        StructEntry struct = (StructEntry) _entity;
        String wfName = struct.getWorkflow();
        
        // If no workflow stored: Either the column is empty, or the original key equals
        // the struct key (since cloned entries always have the same key as their originals,
        // which is why this columns original meaning is obsolete)
        if (wfName == null || wfName.equals(struct.getKey())) {
            return null;
        }
        
        else {
            return wfName;
        }
        
    }

    /**
     * @return
     */
    private int getRevision() throws WGAPIException  {
        
        try {
            Session session = _parent.getSession();
            String queryString = "select count(*) from LogEntry where target=:target ";
            Query query = session.createQuery(queryString);
            query.setParameter("target", _document.getDocumentKey());
            return ((Number) query.iterate().next()).intValue();
        }
        catch (HibernateException e) {
            WGFactory.getLogger().error("Error retrieving log entries for document '" + _document.getDocumentKey() + "'", e);
            return 0;
        }
        
    }

    /**
     * @return
     */
    private Object getPastEditDates() throws WGAPIException {
        Iterator logEntries = getLogEntries().iterator();
        LogEntry logEntry;
        List pastEditDates = new ArrayList();
        while (logEntries.hasNext()) {
            logEntry = (LogEntry) logEntries.next();
            pastEditDates.add(logEntry.getLogtime());
        }
        return pastEditDates;
    }

    /**
     * @return
     */
    private List getPastAuthors() throws WGAPIException {

        Iterator logEntries = getLogEntries().iterator();
        LogEntry logEntry;
        List pastAuthors = new ArrayList();
        while (logEntries.hasNext()) {
            logEntry = (LogEntry) logEntries.next();
            pastAuthors.add(logEntry.getLoguser());
        }
        return pastAuthors;

    }

    /**
     * 
     */
    private List<LogEntry> getLogEntries() throws WGAPIException {

        try {
            Session session = _parent.getSession();
            String queryString = "from LogEntry where target=:target ";
            if (_parent._ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                queryString += "order by id desc";
            }
            else {
                queryString += "order by logtime desc";
            }
            
            Query query = session.createQuery(queryString);
            query.setParameter("target", _document.getDocumentKey());
            query.setMaxResults(100);
            
            @SuppressWarnings("unchecked")
            List<LogEntry> list = query.list();
            Collections.reverse(list);
            return list;
        }
        catch (HibernateException e) {
            WGFactory.getLogger().error("Error retrieving log entries for document '" + _document.getDocumentKey() + "'", e);
            return new ArrayList();
        }

    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#getType()
     */
    public int getType() {
        return this._type;
    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#hasItem(String)
     */
    public boolean hasItem(String strName) {
        strName = strName.toLowerCase().trim();
        
        if (_removedItems.containsKey(strName)) {
            return false;
        }
        
        if (!(_entity instanceof EntityContainingItems)) {
            return false;
        }
        
        EntityContainingItems itemsEntity = (EntityContainingItems) _entity;
        return itemsEntity.getItems().containsKey(strName);

    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#isDataCacheable()
     */
    public boolean isDataCacheable() {
        return true;
    }

    /**
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#isDeleted()
     */
    public boolean isDeleted() throws WGSystemException {
        return _deleted ;
    }

    /**
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#isTemporary()
     */
    public boolean isTemporary() throws WGSystemException {
        return !isSaved();
    }

    /**
     * @throws WGBackendException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#remove()
     */
    public WGDatabaseRevision remove() throws WGAPIException {

        undoEditable();
        
        
        
        try {
        	
            if (_fileHandler != null) {
                _fileHandler.beforeRemove();
            }
        	
            Session session = _parent.getSession();
            

            LogEntry logEntry = null;
            if (getType() != WGDocument.TYPE_USERPROFILE) {
                logEntry = _parent.createLogEntry(session, WGUpdateLog.TYPE_DELETE, _document.getDocumentKey(), _entity.getId());
            }

            // Dereferencings of special object types
            // Manual initializings are workaround for hibernate bug B00005D36
            if (_entity instanceof Content) {
                Content content = (Content) _entity;
                if (_parent.getSession().contains(content.getStructentry())) {
                    Hibernate.initialize(content.getStructentry().getContent());
                    content.getStructentry().getContent().remove(content);
                }
            }
            else if (_entity instanceof StructEntry) {
                StructEntry entry = (StructEntry) _entity;
                if (entry.getParententry() != null) {
                    if (_parent.getSession().contains(entry.getParententry())) {
                        if (_parent._ddlVersion < WGDatabase.CSVERSION_WGA5) {
                            Hibernate.initialize(entry.getParententry().getChildentries());
                            entry.getParententry().getChildentries().remove(entry.getKey());
                        }
                    }
                }
                else if (_parent.getSession().contains(entry.getArea())) {
                    if (_parent._ddlVersion < WGDatabase.CSVERSION_WGA5) {
                        Hibernate.initialize(entry.getArea().getRootentries());
                        entry.getArea().getRootentries().remove(entry.getKey());
                    }
                }
            }

            session.delete(_entity);
            
            if (_fileHandler != null) {
                _fileHandler.afterRemove();
            }
            
            _parent.commitHibernateTransaction();
            _deleted = true;
            session.evict(_entity);
            
            if (logEntry != null) {
                return _parent.getLogRevision(logEntry);
            }
            else {
                return null;
            }
            
        }
        catch (ConstraintViolationException e) {
            throw new WGBackendException("Deletion of document failed because of database constraint violation", e);
        }
        catch (HibernateException e) {
            _parent.rollbackHibernateTransaction(true);
            throw new WGBackendException("Error deleting entity", e);
        }


    }



    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#removeFile(String)
     */
    public boolean removeFile(String name) throws WGAPIException {
        
        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }

        makeEditable();
        
        _fileHandler.removeFile(_parent.convertFileNameForAttaching(name));
        
        return true;

    }

    /**
     * @throws WGIllegalArgumentException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#removeItem(String)
     */
    public boolean removeItem(String name) throws WGAPIException {
        
        makeEditable();
        
        name = name.toLowerCase();
        if (!(_entity instanceof EntityContainingItems)) {
            throw new WGIllegalArgumentException("Cannot remove item on document of type '" + _entity.getClass().getName() + "' - doctype has no items.");
        }

        EntityContainingItems itemsEntity = (EntityContainingItems) _entity;
        Hibernate.initialize(itemsEntity.getItems());
        Item item = (Item) itemsEntity.getItems().get(name);
        if (item != null) {
            _removedItems.put(item.getName(), item);
        }
        return true;

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#save()
     */
    public WGDatabaseRevision save(java.util.Date lastModified) throws WGAPIException {
        
         if (_editable == false) {
            return null;
        }

        if (lastModified == null) {
            lastModified = new Date();
        }

        _parent.getSessionStatus().setInSaveOperation(true);
        try {
            
            // Execute collected element deletions right before storing the entity to prevent effect #00001085
            if (_fileHandler != null) {
                _fileHandler.beforeSave();
            }
            enforceExtensionDataDeletions();
            enforceItemDeletions();
            enforceRelationDeletions();
            enforcePortletItemDeletions();
            
            final Session session = _parent.getSession();
            session.flush();
            
            // Save or update object
            if(getType()==WGDocument.TYPE_STRUCTENTRY)
            	createPageSequence();
            if (getCreated() == null) {
                setCreated(lastModified);
                setLastModified(lastModified);
                session.save(_entity);
                _editable = false;
            }
            else {
                setLastModified(lastModified);
                if (_parent._saveIsolationActive) {
                    
                    performUpdateOperation(new Callable<Boolean>() {
                        @Override
                        public Boolean call() throws HibernateException, WGBackendException {
                            session.update(_entity);
                            return true;
                        }
                    }, session, 1000, _entity);
                }
            }
            
            // Update content relations to this document if it is a content
            if (_content != null && _content.getStatus().equals(WGContent.STATUS_RELEASE)) {
                _parent.updateContentRelations(_content);
            }
            
            // Update log entry
            LogEntry logEntry = null;
            if (getType() != WGDocument.TYPE_USERPROFILE) {
                logEntry = _parent.createLogEntry(session, WGUpdateLog.TYPE_UPDATE, WGDocument.buildDocumentKey(this, _parent.getDb()).toString(), _entity.getId());
            }
            
            // Update file data in optimized file handling
            if (_fileHandler != null) {
                
                // Additional objects
                for (Object obj : _additionalObjectsToSave) {
                    _fileHandler.saveAdditionalObject(obj, logEntry);
                }
                
                _fileHandler.afterSave(logEntry);
            }
            
            
            // Commit transaction
            performUpdateOperation(new Callable<Boolean>() {
                @Override
                public Boolean call() throws HibernateException, WGAPIException {
                    _parent.commitHibernateTransaction();
                    return true;
                }
            }, session, 1000, null);

            // Clear temporary collector objects
            _removedExtData.clear();
            _removedItems.clear();
            _removedRelations.clear();
            _removedPortletItems.clear();
            _additionalObjectsToSave.clear();
            
            if (logEntry != null) {
                return _parent.getLogRevision(logEntry);
            }
            else {
                return null;
            }
        }
        catch (HibernateException e) {
            _parent.rollbackHibernateTransaction(true);
            throw new WGBackendException("Error saving hibernate document on database " + _parent.getDb().getDbReference(), e);
        }
        catch (WGAPIException e) {
            _parent.rollbackHibernateTransaction(true);
            throw e;
        }
        catch (Exception e) {
			_parent.rollbackHibernateTransaction(true);
			throw new WGBackendException("Error saving hibernate document on database " + _parent.getDb().getDbReference(), e);
		}
        finally {
            _parent.getSessionStatus().setInSaveOperation(false);
        }
    
    }

    public void createPageSequence() throws WGAPIException, InstantiationException, IllegalAccessException {
    	if(getType()==WGDocument.TYPE_STRUCTENTRY && getExtensionData("page-sequence")==null){
	    	long seq = _parent.incrementSystemSequence("page-sequence");        	
	    	writeExtensionData("page-sequence", seq);
    	}
	}
    private long getPageSequence() throws WGAPIException{
    	Object seq = getExtensionData("page-sequence");
    	if(seq==null)
    		return 0;
    	return ((Double)seq).longValue();
	}

	private void enforcePortletItemDeletions() {

        if (!(_entity instanceof UserProfile)) {
            return;
        }
        
        UserProfile profile = (UserProfile) _entity;
        
        for (Map.Entry<UserProfilePortlet, Map<String,UserProfileItem>> removedItems : _removedPortletItems.entrySet()) {
            
            UserProfilePortlet portletFromMap = removedItems.getKey();
            UserProfilePortlet portlet = profile.getPortlets().get(portletFromMap.getKey()); // To see if it is still stored
            if (portlet != null) {
                for (String itemName : removedItems.getValue().keySet()) {
                    portlet.getItems().remove(itemName);
                }
            }
            
        }
        
        
    }

    private void enforceRelationDeletions() {
        
        if (_content == null) {
            return;
        }

        Iterator iterator = _removedRelations.values().iterator();
        while (iterator.hasNext()) {
            ContentRelation rel  = (ContentRelation) iterator.next();
            _content.getRelations().remove(rel.getName());
        }
        
    }

    private void enforceExtensionDataDeletions() {

        Iterator<ExtensionData> iterator = _removedExtData.values().iterator();
        while (iterator.hasNext()) {
            ExtensionData extensionData = (ExtensionData) iterator.next();
            _entity.getExtensionData().remove(extensionData.getName());
        }
        
    }
    
    private void enforceItemDeletions() {

        Iterator<Item> iterator =_removedItems.values().iterator();
        while (iterator.hasNext()) {
            Item item  = iterator.next();
            if (_entity instanceof Content) {
                ((Content) _entity).getItems().remove(item.getName());
            }
            else if (_entity instanceof UserProfile) {
                ((UserProfile) _entity).getItems().remove(item.getName());
            }
        }
        
    }

        /**
     * @throws WGIllegalArgumentException 
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#setItemValue(String,
     *      Object)
     */
    public boolean setItemValue(String strName, Object value) throws WGAPIException {

        makeEditable();
        strName = strName.toLowerCase().trim();
        
        if (!(_entity instanceof EntityContainingItems)) {
            throw new WGIllegalArgumentException("Cannot set item on document of type '" + _entity.getClass().getName() + "' - doctype has no items.");
        }
        
        EntityContainingItems itemsEntity = (EntityContainingItems) _entity;

        /* Deactivated per #00001362
        if (value instanceof List) {
            List valueList = (List) value;
            if (valueList.size() == 0) {
                value = null;
            }
            else if (valueList.size() == 1) {
                value = valueList.get(0);
            }
        }*/

        Map<String, ? extends Item> items = itemsEntity.getItems();
        Item item = items.get(strName);
        if (item == null) {
            item = itemsEntity.createNewItem(strName);
        }
        
        writeItemValue(_parent, this, item, value);
        _removedItems.remove(strName);

        return true;
    }

    protected static void writeItemValue(WGDatabaseImpl db, WGDocumentImpl doc, Item item, Object value) throws WGAPIException {
        if (value == null) {
            item.setText(null);
            item.setDate(null);
            item.setNumber(null);
            item.setType(ITEMTYPE_STRING);
        }
        else if (value instanceof String) {
            item.setText((String) value);
            item.setDate(null);
            item.setNumber(null);
            item.setType(ITEMTYPE_STRING);
        }
        else if (value instanceof Number) {
            item.setText(null);
            item.setDate(null);
            Double dValue = (value instanceof Double ? (Double) value : new Double(((Number) value).doubleValue()));
            item.setNumber(dValue);
            item.setType(ITEMTYPE_NUMBER);
        }
        else if (value instanceof Date) {
            item.setText(null);
            item.setDate((Date) value);
            item.setNumber(null);
            item.setType(ITEMTYPE_DATE);
        }
        else if (value instanceof Boolean && db._ddlVersion >= WGDatabase.CSVERSION_WGA5) {
            Boolean bool = (Boolean) value;
            item.setText(null);
            item.setDate(null);
            item.setNumber(new Double(bool.booleanValue() == true ? 1 : 0));
            item.setType(ITEMTYPE_BOOLEAN);
        }
        else if (item instanceof ExtensionData && BinaryFieldData.isBinaryValue(value)) {
            item.setText(null);
            item.setDate(null);
            item.setNumber(null);
            db.getFileHandling().writeBinaryExtensionData(doc, (ExtensionData) item, value);
            item.setType(ITEMTYPE_BINARY);
        }
        else if (value instanceof JsonElement) {
            StringWriter jsonOut = new StringWriter();
            item.setText(value.toString());
            item.setDate(null);
            item.setNumber(null);
            item.setType(ITEMTYPE_JSON);
        }
        else {
            // serialize with xstream
            try {
                String serializedValue = XSTREAM_SERIALIZER.toXML(value);
                item.setText(serializedValue);
                item.setDate(null);
                item.setNumber(null);
                item.setType(ITEMTYPE_SERIALIZED_XSTREAM);
                item.setDeserialisationCache(null); // We can't inject the value here coz it might be altered from outside
            } catch (Exception e) {
                throw new WGBackendException("Unable to serialize item value." , e);
            }
        }
    }



    /**
     * @throws WGIllegalArgumentException 
     * @throws WGSystemException 
     * @throws WGBackendException 
     * @see de.innovationgate.webgate.api.WGDocumentCore#setMetaData(String,
     *      Object)
     */
    public boolean setMetaData(String name, Object value) throws WGIllegalArgumentException, WGAPIException {

        makeEditable();
        
        // If value is an document implementation, extract it's entity
        if (value instanceof WGDocumentImpl) {
            value = ((WGDocumentImpl) value).getEntity();
        }

        else {

            String designDocName = String.valueOf(value).toLowerCase();
            
            // If value SHOULD be an entity (depending on meta name) but is only
            // a key, the entity it is retrieved by the key
            if (name.equals(WGStructEntry.META_AREA)) {
                value = ((WGDocumentImpl) _parent.getDesignObject(WGDocument.TYPE_AREA, (String) designDocName, null)).getEntity();
            }
            else if (name.equals(WGStructEntry.META_CONTENTTYPE)) {
                value = ((WGDocumentImpl) _parent.getDesignObject(WGDocument.TYPE_CONTENTTYPE, (String) designDocName, null)).getEntity();
            }
            else if (name.equals(WGContent.META_STRUCTENTRY)) {
                value = ((WGDocumentImpl) _parent.getStructEntryByKey(value)).getEntity();
            }
            else if (name.equals(WGContent.META_LANGUAGE)) {
                value = ((WGDocumentImpl) _parent.getDesignObject(WGDocument.TYPE_LANGUAGE, (String) designDocName, null)).getEntity();
            }

            // Some more custom conversions
            if (value != null && (name.equals(WGStructEntry.META_POSITION)) && !(value instanceof Integer)) {
                if (value instanceof Number) {
                    value = new Integer(((Number) value).intValue());
                }
                else {
                    try {
                        value = new Integer(String.valueOf(value));
                    }
                    catch (NumberFormatException e) {
                        throw new WGIllegalArgumentException("Cannot parse metadata type'" + name + "' as number. Value:" + value, e);
                    }

                }
            }
            
            // The Structs workflow name is stored to the column ORIGINALKEY
            if (_entity instanceof StructEntry && name.equals(WGStructEntry.META_WORKFLOW_NAME)) {
                name = ENTITYPROP_STRUCTENTRY_WORKFLOW;
            }
        }

        getBeanMap().put(name.toLowerCase(), value);
        return true;
                
    }

    /**
     * @see de.innovationgate.webgate.api.WGDocumentCore#setWGDocument(WGDocument)
     */
    public void setWGDocument(WGDocument doc) {
        _document = doc;
    }

    private void setCreated( Date date) {
        getBeanMap().put(WGDocument.META_CREATED.toLowerCase(), date);
    }

    private void setLastModified(Date date) {
        getBeanMap().put("lastmodified", date);
    }

    public Object getEntity() {
        return _entity;
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocumentCore#isSaved()
     */
    public boolean isSaved() {
        return (getCreated() != null);
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocumentCore#getNativeObject()
     */
    public Object getNativeObject() {
        return _entity;
    }

    public String getOriginDatabase() {
        return _parent.getDb().getDbReference();
    }

	public WGDatabaseImpl getParent() {
		return _parent;
	}

	public void renameFile(String oldFileName, String newFileName) throws WGAPIException {
	    
	    if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
		
	    makeEditable();
	    
        
		_fileHandler.renameFile(_parent.convertFileNameForAttaching(oldFileName), _parent.convertFileNameForAttaching(newFileName));
	}

	public WGFileMetaData getFileMetaData(String filename) throws WGAPIException {
		
	    if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
	    
	    return _fileHandler.getFileMetaData(_parent.convertFileNameForAttaching(filename));
		
	}
	
	public WGExtensionDataContainer retrieveFileExtensionDataHandler(String filename) throws WGIllegalDataException, WGAPIException {
	    
	    if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
	    
	    return _fileHandler.retrieveFileExtensionDataHandler(_parent.convertFileNameForAttaching(filename));
	}

	
    public WGDocumentCore getRelation(String name) throws WGAPIException {
        
        name = name.toLowerCase();
        if (_removedRelations.containsKey(name)) {
            return null;
        }
        
        if (_content != null) {
            Map relations = _content.getRelations();
            if (relations != null) {
                ContentRelation relation = (ContentRelation) relations.get(name);
                if (relation == null) {
                    return null;
                }
                
                return retrieveRelationTarget(relation);
            }
            else {
                return null;
            }
        }
        
        throw new WGNotSupportedException("Content Relations are not supported on this document type");    
        
    }
    
    public WGRelationData getRelationData(String name) throws WGNotSupportedException {
        
        name = name.toLowerCase();
        if (_removedRelations.containsKey(name)) {
            return null;
        }
        
        if (_content != null) {
            Map relations = _content.getRelations();
            if (relations != null) {
                ContentRelation relation = (ContentRelation) relations.get(name);
                if (relation == null) {
                    return null;
                }
                
                return _parent.createWGRelationData(relation);
            }
            else {
                return null;
            }
        }
        
        throw new WGNotSupportedException("Content Relations are not supported on this document type");   
    }

    public WGDocumentCore removeRelation(String name) throws WGAPIException {
        
        makeEditable();
        
        name = name.toLowerCase();
                
        if (_content != null) {
            Hibernate.initialize(_content.getRelations());
            Map relations = _content.getRelations();
            if (relations != null) {
                ContentRelation relation = (ContentRelation) relations.get(name);
                if (relation != null) {
                    _removedRelations.put(relation.getName(), relation);
                    return retrieveRelationTarget(relation);
                }
                else {
                    return null;
                }
            }
        }

        throw new WGNotSupportedException("Content Relations are not supported on this document type");    

    }

    private WGDocumentCore retrieveRelationTarget(ContentRelation relation) throws WGAPIException {
        
        // Try to retrieve the relation in the most direct way by using the target column
        // Might be actually way slower than normal way bc. of the refresh() call
        /*try {
            Content targetContent = relation.getTarget();
            if (targetContent != null && targetContent.getStatus().equals(WGContent.STATUS_RELEASE)) {
                if (targetContent == _entity) {
                    return this;
                }
                else {
                _parent.getSession().refresh(targetContent);
                return _parent.createDocumentImpl(targetContent);
            }
            }
            
        }
        // Relation points to a deleted document
        catch (ObjectNotFoundException e) {
        }*/
        
        // If the relation is "broken" or empty we try to resolve it by using struct and language information
        WGStructEntry entry = _parent.getDb().getStructEntryByKey(relation.getTargetstructentry());
        if (entry == null) {
            return null;
        }
        WGContent content = entry.getReleasedContent(relation.getTargetlanguage());
        if (content != null) {
            return content.getCore();
        }
        else {
            return null;
        }

    }

    public WGDocumentCore setRelation(WGRelationData relData) throws WGAPIException {
        
        makeEditable();
        
        if (_content != null) {
            
            Map relations = _content.getRelations();
            if (relations != null) {
            
                String structKey = String.valueOf(relData.getTargetStructkey());
                Content targetContent = null;

                // Try to retrieve the target content entity and look if it is already available
                Query query = _parent.getSession().createQuery("select content from Content as content where content.structentry.key=:structkey and content.language.name=:lang and content.status='p'");
                query.setParameter("structkey", structKey);
                query.setParameter("lang", relData.getTargetLanguage());
                if (query.list().size() > 0) {
                    targetContent = (Content) query.list().get(0);
                }
                
                String relName = relData.getName().toLowerCase();
                ContentRelation relation = (ContentRelation) relations.get(relName);
                if (relation == null) {
                    relation = new ContentRelation();
                    relation.setName(relName);
                }
                
                relation.setType(relData.getType());
                relation.setParentcontent(_content);
                
                if (targetContent != null) {
                    relation.setTarget(targetContent);
                }
                
                relation.setTargetstructentry(structKey);
                relation.setTargetlanguage(relData.getTargetLanguage());
                if (relData.getGroup() != null) {
                    relation.setGroup(relData.getGroup().toLowerCase());
                }
                
                ContentRelation previousRelation = (ContentRelation) relations.put(relation.getName(), relation);
                _removedRelations.remove(relation.getName());
                if (previousRelation != null) {
                    return retrieveRelationTarget(previousRelation);
                }
                else {
                    return null;
                }
            }
            
        }

        throw new WGNotSupportedException("Content Relations are not supported on this document type");    

    }
    
    public List<String> getRelationNames() throws WGAPIException {
        if (_content != null) {
            Map relations = _content.getRelations();
            List relNames;
            if (relations != null) {
                relNames = new ArrayList(relations.keySet());
            }
            else {
                relNames = Collections.emptyList();
            }
            relNames.removeAll(_removedRelations.keySet());
            return relNames;
        }

        throw new WGNotSupportedException("Content Relations are not supported on this document type");    

    }

    public boolean hasFileMetadata() throws WGAPIException {
        if (_fileHandler == null) {
            return false;
        }
        
        return _fileHandler.isFileMetaDataAvailable();
    }

    public boolean hasFile(String file) throws WGAPIException {
        
        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
        
        return _fileHandler.hasFile(_parent.convertFileNameForAttaching(file));
        
    }

    public Object getExtensionData(String strName) throws WGAPIException {
        
        strName = strName.toLowerCase().trim();
        if (_removedExtData.containsKey(strName)) {
            return null;
        }
        
        return _entity.readExtensionData(_parent, this, strName);
        
    }

    public List<String> getExtensionDataNames() throws WGAPIException {
        List<String> names = new ArrayList<String>(_entity.getExtensionData().keySet());
        names.removeAll(_removedExtData.keySet());
        return names;
    }

    public void removeExtensionData(String name) throws WGAPIException {
        
        makeEditable();
        
        name = name.toLowerCase().trim();
        ExtensionData item = (ExtensionData) _entity.getExtensionData().get(name);
        if (item != null) {
            _removedExtData.put(item.getName(), item);
        }
        
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        
        makeEditable();
        
        strName = strName.toLowerCase().trim();

        _entity.writeExtensionData(_parent, this, strName, value);
        _removedExtData.remove(strName);
        
    }
    
    private WGPortletRegistry createPortletRegistry() {
        
        if (_parent._ddlVersion >= WGDatabase.CSVERSION_WGA5) {
            return new PortletRegistryImplV5(_parent, this);
        }
        else {
            return new PortletRegistryImplV4(_parent, this);
        }
        
    }

    @SuppressWarnings("unchecked")
    public List<String> getRelationNamesOfGroup(String group, WGColumnSet order) throws WGAPIException {
        
        group = group.toLowerCase();
        
        if (_content != null) {
            if (order != null) {
                Map <String,Object> params = new HashMap<String, Object>();
                Query q = _parent.getSession().createQuery("select rel.name from ContentRelation as rel where rel.parentcontent=:content and rel.group=:group order by " + _parent.buildHqlContentOrderClause("rel.target", order, params));
                params.put("content", _entity);
                params.put("group", group);
                for (Map.Entry<String,Object> param : params.entrySet()) {
                    q.setParameter(param.getKey(), param.getValue());
                }
                return (List<String>) q.list();
            }
            else {
                List<String> names = new ArrayList<String>();
                Map<String,ContentRelation> relations = _content.getRelations();
                if (relations != null) {
                    for (ContentRelation rel : relations.values()) {
                        if (group.equals(rel.getGroup())) {
                            names.add(rel.getName());
                        }
                    }
                    names.removeAll(_removedRelations.keySet());
                    return names;
                }
                else {
                    return Collections.emptyList();
                }
            }
        }

        throw new WGNotSupportedException("Content Relations are not supported on this document type");    
    }

    private boolean isEditable() {
        return _editable;
    }

    public WGDocument getDocument() {
        return _document;
    }
    
    /**
     * Performs an operation involving entity updates and catches possibly appearing {@link NonUniqueObjectException}s.
     * Those exceptions are solved by evicting every entity which causes it and retrying the operation.
     * @param task The update operation
     * @param session The hibernate session to use to load the entity
     * @param retries The number of operation retries before a {@link WGBackendException} exception is thrown to cancel the operation.
     * @throws Exception
     */
    private void performUpdateOperation(Callable<Boolean> task, Session session, int retries, Object entity) throws Exception {
        
        int repeatCount = 0;
        while (true) {
            
            repeatCount++;
            if (repeatCount > 1000) {
                throw new WGBackendException("Update of document failed because of persistent duplicates more than 1000 times. Cancelling update.");
            }
            
            try {
                task.call();
                break;
            }
            
            // Another persistent version was loaded somehow. Evict it and try to update again.
            catch (NonUniqueObjectException e) {
                Object otherObject = session.load(e.getEntityName(), e.getIdentifier());
                session.evict(otherObject);
                if (entity != null) {
                    session.evict(entity);
                }
            }
        }
        
    }

    public void pushFiles(WGDocumentImpl docCopy) throws WGAPIException {
        
        if (_fileHandler == null) {
            throw new WGIllegalDataException("This document does not contain file attachments: " + WGDocument.buildDocumentKey(this, _parent.getDb()));
        }
        
        _fileHandler.pushFiles(docCopy);
    }

    public WGExtensionDataContainer createFileExtDataHandler(WGDatabaseImpl parent, Entity metaEntity) {
        return new EntityExtDataHandler(this, parent, metaEntity);
    }

    public FileAttachmentHandler getFileHandler() {
        return _fileHandler;
    }

    @Override
    public List<WGFileDerivateMetaData> getFileDerivates(String strFile) throws WGAPIException {
        if (_fileHandler instanceof CS5P4FileAttachmentHandler) {
            return ((CS5P4FileAttachmentHandler) _fileHandler).getFileDerivates(strFile);
        }
        else {
            return null;
        }
         
    }

    @Override
    public void markFileMetaDataModified(WGFileMetaData md) throws WGAPIException {

        if (_fileHandler instanceof CS41FileAttachmentHandler) {
            WGSessionContext session = _parent.getDb().getSessionContext();
            if (session == null || !session.isActiveCore(this)) {
                throw new WGClosedSessionException("The file metadata object is not attached to the current session");
            }
            
            ((CS41FileAttachmentHandler) _fileHandler).markMetaDataModified(md);
            makeEditable();
            if (_document != null) {
                _document.markEdited();
            }
        }
        
    }
    
    @Override
    public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String, Object> customMdFields) throws WGAPIException {

        WGFileMetaData originalMd = getFileMetaData(originalFileName);
        if (originalMd == null) {
            throw new WGIllegalArgumentException("The file '" + originalFileName + "' does not exist on document '" + WGDocument.buildDocumentKey(this, _parent.getDb()) + "'");
        }
        
        FileHandling fileHandling = _parent.getFileHandling();
        if (fileHandling instanceof CS5P4FileHandling) {
            CS5P4FileHandling cs5fh = (CS5P4FileHandling) fileHandling;
            String id =cs5fh.storeFileDerivate(originalMd, creator, derivateName, in, customMdFields);
            return cs5fh.getFileDerivateMetadata(id);
        }
        else {
            throw new WGNotSupportedException("File derivates are not supported on this database");
        }
        
    }
    
    @Override
    public void removeFileDerivate(String id) throws WGAPIException {
        FileHandling fileHandling = _parent.getFileHandling();
        if (fileHandling instanceof CS5P4FileHandling) {
            CS5P4FileHandling cs5fh = (CS5P4FileHandling) fileHandling;
            cs5fh.removeFileDerivate(id);
        }
        else {
            throw new WGNotSupportedException("File derivates are not supported on this database");
        }   
    }

    
    @Override
    public InputStream getFileDerivateData(String id) throws WGAPIException {
        FileHandling fileHandling = _parent.getFileHandling();
        if (fileHandling instanceof CS5P4FileHandling) {
            return ((CS5P4FileHandling) fileHandling).getFileDerivateData(id);
        }
        else {
            throw new WGNotSupportedException("File derivates are not supported on this database");
        }
    }
    
    @Override
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException {
        FileHandling fileHandling = _parent.getFileHandling();
        if (fileHandling instanceof CS5P4FileHandling) {
            return ((CS5P4FileHandling) fileHandling).getFileDerivateMetadata(id);
        }
        else {
            throw new WGNotSupportedException("File derivates are not supported on this database");   
        }
    }
    
    @Override
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException, WGNotSupportedException {
        FileHandling fileHandling = _parent.getFileHandling();
        if (fileHandling instanceof CS5P4FileHandling) {
            ((CS5P4FileHandling) fileHandling).writeFileDerivateMetaData(md);
        }
        else {
            throw new WGNotSupportedException("File derivates are not supported on this database");   
        }
    }
    
    @Override
    public Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException {

        Query q;
        if (_parent.getContentStoreVersion() >= 5) {
            q = _parent.getSession().createQuery("from LogEntry as log where log.target_id=:id order by log.logtime desc");
            q.setParameter("id", _entity.getId());
        }
        else {
            q = _parent.getSession().createQuery("from LogEntry as log where log.target=:target order by log.logtime desc");
            q.setParameter("target", WGDocument.buildDocumentKey(this, _parent.getDb()).toString());
        }
        
        @SuppressWarnings("resource")
        final HibernateQueryIterator it = new HibernateQueryIterator(q, null);
        return new UpdateLogIterator(it);
        
        
    }

    public WGFileMetaDataContext getFileMetadataContext() {
        return _fileMetadataContext;
    }
    



}
