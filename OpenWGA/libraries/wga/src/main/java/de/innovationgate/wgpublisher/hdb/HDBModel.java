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

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executors;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.log4j.Logger;

import de.bannkreis.groq.Groq;
import de.innovationgate.utils.MD5HashingInputStream;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAbstractResultSet;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGCancelledException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGEventScript;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.common.beans.hdbmodel.DocumentParent;
import de.innovationgate.wga.common.beans.hdbmodel.Filter;
import de.innovationgate.wga.common.beans.hdbmodel.FilterParam;
import de.innovationgate.wga.common.beans.hdbmodel.Item;
import de.innovationgate.wga.common.beans.hdbmodel.ModelDefinition;
import de.innovationgate.wga.common.beans.hdbmodel.Relation;
import de.innovationgate.wga.common.beans.hdbmodel.SingletonContent;
import de.innovationgate.wga.common.beans.hdbmodel.Storage;
import de.innovationgate.wga.common.beans.hdbmodel.WhereClause;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wgpublisher.ManagedDBAttribute;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.events.ContentTypeEvent;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.UniqueNamePartFormatter;

/**
 * The HDBModel API
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE, propertyMode=CodeCompletion.MODE_EXCLUDE)
public class HDBModel implements ManagedDBAttribute, WGDesignChangeListener {
    
    public static final String PARENT_RELATION_PREFIX = "parent-";
    public static final String STORAGE_CONTENTCLASS = "$hdbmodel-storage";
    /**
     * Option containing an extra HQL clause to inject to the retrieval of relation targets or sources
     */
    public static final String OPTION_EXTRACLAUSE = "extraClause";
    /**
     * Option containing a {@link Map} of extra query parameters to give to the retrieval of relation targets or sources, parameter names as map keys, parameter values as map values
     */
    public static final String OPTION_EXTRAPARAMS = "extraParams";
    /**
     * Determines if the currently stored target of the relation should also be included in the retrieval of valid relation targets, even if it is no longer valid 
     */
    public static final String OPTION_INCLUDECURRENT = "currentTarget";
    
    public static final String EVENT_PRE_CREATE = "preCreate";
    public static final String EVENT_POST_CREATE = "postCreate";
    
    public static final String EVENT_PRE_UPDATE = "preUpdate";
    public static final String EVENT_POST_UPDATE = "postUpdate";
    
    public static final String EVENT_ON_CREATE = "onCreate";
    public static final String EVENT_ON_SAVE = "onSave";
    
    public static final String EVENT_PRE_MOVE_FROM = "preMoveFrom";
    public static final String EVENT_PRE_MOVE_TO = "preMoveTo";
    public static final String EVENT_POST_MOVE_FROM = "postMoveFrom";
    public static final String EVENT_POST_MOVE_TO = "postMoveTo";
    
    public static final String EVENT_PRE_DELETE = "preDelete";
    public static final String EVENT_POST_DELETE = "postDelete";
    
    private static final WGEventScript EVENTSCRIPT_CREATE_CONTENT = new WGEventScript("tmlscript", "return HDBModel.onCreate(event);");
    private static final WGEventScript EVENTSCRIPT_SAVE_CONTENT = new WGEventScript("tmlscript", "return HDBModel.onSave(event);");
    
    protected static final Logger LOG = Logger.getLogger("wga.hdbmodel");

    private static final String HDBMODEL_GLOBAL = "HDBModel";

    public static final String ITEM_TYPE = "$HDBModel_type";
    
    public static final String TYPE_STORAGE = "storage";
    public static final String TYPE_CONTENT = "content";

    @CodeCompletion
    public static final String MODEL_FILE = "hdb-model.xml";

    private static final String SYSTEMFC_DOCKEY = (new WGDocumentKey(WGDocument.TYPE_FILECONTAINER, "system", null)).toString();

    public static final String ITEM_CONTENT_ID = "$HDBModel_contentid";
    public static final String ITEM_STORAGE_ID = "$HDBModel_storageid";

    private static final String EXTDATA_MODEL_HASH = "hdbmodel_defhash";
    private static final String EXTDATA_MODEL_HASHES = "hdbmodel_defhashes";
    public static final Object VERSIONING_FULL = "full";
    public static final Object VERSIONING_NONE = "none";
    private static final int OLDHASHES_MAX_SIZE = 1000;

    private volatile ModelDefinition _definition;

    private volatile long _definitionTime;

    private WGDatabase _db;

    private WGHierarchicalDatabase _hdb;

    private WGACore _core;

    private volatile String _definitionHash;

    private boolean _modelVersionChanged = false;
    private boolean _modelReinitRunning = false;
    
    /**
     * Returns if a reinit of the HDBModel in the database is currently taking place
     */
    public boolean isModelReinitRunning() {
        return _modelReinitRunning;
    }

    private volatile String _scriptsPath = HDBModelListener.TMLSCRIPT_LISTENER_FOLDER;
    
    private int _initSessionRefreshCounter;
    
    public static HDBModel getModel(WGDatabase db) {
        return (HDBModel) db.getAttribute(WGACore.DBATTRIB_HDBMODEL);
    }
    
    /**
     * Returns if the given document is a HDBModel storage
     * @param content The document
     * @throws WGAPIException
     */
    public static boolean isStorage(WGContent content) throws WGAPIException {
        
        return TYPE_STORAGE.equals(content.getItemValue("$hdbmodel_type")) || WGHierarchicalDatabase.getOrCreateInstance(content.getDatabase()).isStorage(content);
        
    }

    /**
     * Returns if the given document is a HDBModel content
     * @param content The document
     * @throws WGAPIException
     */
    public static boolean isContent(WGContent content) throws WGAPIException {
        
        return TYPE_CONTENT.equals(content.getItemValue("$hdbmodel_type"));
        
    }
    
    /**
     * Returns if the given content is a HDBModel document
     * @param content
     * @throws WGAPIException 
     */
    public static boolean isDocument(WGContent content) throws WGAPIException {
        return content.hasItem(ITEM_TYPE);
    }

    /**
     * Returns the HDB ID of the given document, the content id for contents, the storage id for storages
     * @param content Document
     * @throws WGAPIException
     */
    public static String getID(WGContent content) throws WGAPIException {
        
        String type = content.getItemText(ITEM_TYPE);
        String idItemName = null;
        if (TYPE_CONTENT.equals(type)) {
            idItemName = ITEM_CONTENT_ID;
        }
        else if (TYPE_STORAGE.equals(type)) {
            idItemName = ITEM_STORAGE_ID;
        }
        
        if (idItemName != null && content.hasItem(idItemName)) {
            return content.getItemText(idItemName);
        }
        
        // Fallback for old HDBModel docs. Retrieve from uname.
        String partUname = content.getUniqueName();
        String partId = null;
        if (partUname != null) {
            List<String> unameParts = WGUtils.deserializeCollection(partUname, WGHierarchicalDatabase.STORAGE_DELIMITER);
            partId = (String) unameParts.get(unameParts.size() - 1);
        }
        return partId;
    }


    protected WGACore getCore() {
		return _core;
	}


	private HDBModel(WGACore core, WGDatabase db, WGFileContainer systemFc) throws WGAPIException, NoSuchAlgorithmException, IOException {

        _core = core;
        _db = db;
        _db.addDesignChangeListener(this);
        _hdb = WGHierarchicalDatabase.getInstance(_db.getDbReference());
        _hdb.setHdbModelMode(true);
        
        // Set flags
        _hdb.setPostUpdateEventBehaviour(WGHierarchicalDatabase.PostUpdateEventBehaviour.RUN_AFTER_SAVE);
        
        // Init event scripts of hdb content type
        WGContentType hdbContentType = _db.getContentType(WGHierarchicalDatabase.CONTENT_TYPE);
        if (!EVENTSCRIPT_CREATE_CONTENT.equals(hdbContentType.getEventContentCreated())) {
            LOG.info("Initializing event script 'onCreateContent' for HDB content type");
            hdbContentType.setEventContentCreated(EVENTSCRIPT_CREATE_CONTENT);
            hdbContentType.save();
        }
        if (!EVENTSCRIPT_SAVE_CONTENT.equals(hdbContentType.getEventContentSaved())) {
            LOG.info("Initializing event script 'onSaveContent' for HDB content type");
            hdbContentType.setEventContentSaved(EVENTSCRIPT_SAVE_CONTENT);
            hdbContentType.save();
        }

        readDefinition(systemFc);
        
        String versioningMode = _definition.getVersioning();
        if (VERSIONING_FULL.equals(versioningMode)) {
            _hdb.setUseVersioning(true);
        }
    }

    /**
     * Reload the HDBModel definition
     */
    public void reloadDefinition() {
        try {
            WGFileContainer model = _db.getFileContainer("system");
            readDefinition(model);
        }
        catch (Exception e) {
            LOG.error("Exception reading HDBModel definition", e);
        }
    }

    private void init() throws WGAPIException {
        String oldHash = null;
        _initSessionRefreshCounter = 0;
        
        if (_core.isRunSingleNodeFunctionalities() || !_core.isClusteredDatabase(_db)) {
            getCore().getLog().info("Initializing HDB model for database " + _db.getDbReference());
            
            // Create basic structure
            Iterator<Storage> children = _definition.getRootStorages().iterator();
            while (children.hasNext()) {
                Storage child = children.next();
                initModel(child, null, false);    
            }

            // Reinit model if changed
            if (_modelVersionChanged) {
                reinitModel();
            }
        }
        else {
            getCore().getLog().info("Skipping HDB model initialisation as this server is not configured to run single node functionalities in a cluster");
        }
    }

    /**
     * Do a complete reinitialisation of the database according to the model
     */
    public void reinitModel() {
        
        if (!_core.isRunSingleNodeFunctionalities()) {
            throw new IllegalStateException("Model reinitialisation in a cluster can only run on the master node");
        }
        
        Executors.newSingleThreadExecutor().execute(new Runnable() {
            @Override
            public void run() {
                _modelReinitRunning = true;
                WGA wga = WGA.get(_core);
                ProblemOccasion occ = wga.server().startProblemOccasion(HDBModel.class, _hdb.getWrappedDB(), "reinitModel");
                try {
                    LOG.info("Performing complete model reinitialisation.");
                    _hdb.getWrappedDB().openSession();
                    Iterator<Storage> children = _definition.getRootStorages().iterator();
                    while (children.hasNext()) {
                        Storage child = children.next();
                        initModel(child, null, true);    
                    }
                    
                    List<String> oldHashes = readOldHashes();
                    oldHashes.add(_definitionHash);
                    writeOldHashes(oldHashes);
                    
                    LOG.info("HDBModel reinitialisation of app '" + _hdb.getWrappedDB().getDbReference() + "' finished.");
                    
                }
                catch (Throwable e) {
                    LOG.error("Exception performing HDBModel reinitialisation", e);
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "exception", ProblemSeverity.HIGH, e));
                }
                finally {
                    _modelVersionChanged = false;
                    _modelReinitRunning = false;
                    WGFactory.getInstance().closeSessions();
                }
            }
            
        });
    }

    private void initModel(Document modelDoc, WGContent parent, boolean completeReinit) throws WGAPIException {
        
        WGContent document;
        
        if (modelDoc instanceof Storage) {
            Storage storage = (Storage) modelDoc;
            HDBModelParams params = new HDBModelParams(TYPE_STORAGE);
            params.setContentClass(storage.getStorageId());
            
            if (parent != null) {
                if (_hdb.isStorage(parent)) {
                    document = _hdb.getOrCreateStorage(parent, storage.getStorageId(), params);
                }
                else {
                    document = _hdb.getOrCreateUIDContent(parent, storage.getStorageId(), params);
                }
            }
            else {
                document = _hdb.getOrCreateStorage(storage.getStorageId(), params);
            }
            
            
            // If the storage does not yet have a listener item we regard it as just created an initialize it
            if (!document.hasItem(WGHierarchicalDatabase.ITEMNAME_LISTENER_CLASS_OR_MODULE)) {
                document.setItemValue(WGHierarchicalDatabase.ITEMNAME_LISTENER_CLASS_OR_MODULE, HDBModelListener.class.getName());
                document.setItemValue(ITEM_STORAGE_ID, storage.getStorageId());
                document.setContentClass(getStorageContentClass(storage));
                document.save();
            }
            
            // Ensure type is set. May be unset for: just created "real" HDB storages, storages from earlier HDBModel versions
            if (!document.hasItem(ITEM_TYPE)) {
                document.setItemValue(ITEM_TYPE, TYPE_STORAGE);
                document.save();
            }
            
            // Ensure correct position among sibling storages
            DocumentParent parentModel = _definition.getDocumentParent(storage);
            List<Storage> siblings = null;
            if (parentModel instanceof Content) {
                siblings = ((Content) parentModel).getChildStorages();
            }
            else if (parentModel instanceof Storage) {
                siblings = ((Storage) parentModel).getChildStorages();
            }
            else if (parentModel instanceof ModelDefinition) {
                siblings = ((ModelDefinition) parentModel).getRootStorages();
            }
            
            WGStructEntry page = document.getStructEntry();
            boolean pageEdited = false;
            
            if (siblings != null) {
                int idx = siblings.indexOf(storage);
                int position = (idx+1) * 10;
                if (page.getPosition().intValue() != position) {
                    page.setPosition(position);
                    pageEdited = true;
                }
            }
            
            // Enforce accessibility rights
            if (storage.getReaders() != null) {
                List<String> readers = WGUtils.deserializeCollection(storage.getReaders(), ",", true);
                if (!readers.equals(page.getReaders())) {
                    page.setReaders(readers);
                    pageEdited = true;
                }
            }
            
            if (storage.getEditors() != null) {
                List<String> childEditors = WGUtils.deserializeCollection(storage.getEditors(), ",", true);
                if (!childEditors.equals(page.getChildEditors())) {
                    page.setChildEditors(childEditors);
                    pageEdited = true;
                }
            }
            
            if (pageEdited) {
                page.save();
            }
    
            // Initialize eventual children
            Iterator<SingletonContent> childSingletonContents = storage.getChildSingletonContents().iterator();
            while (childSingletonContents.hasNext()) {
                SingletonContent child = childSingletonContents.next();
                initModel(child, document, completeReinit);
            }

            Iterator<Storage> childStorages = storage.getChildStorages().iterator();        
            while (childStorages.hasNext()) {
                Storage child = childStorages.next();
                initModel(child, document, completeReinit);
            }
            
            Iterator<Content> childContents = storage.getChildContents().iterator();        
            while (childContents.hasNext()) {
                Content child = childContents.next();
                initModel(child, document, completeReinit);
            }
            
            
        }
        
        else if (modelDoc instanceof SingletonContent) {
            
            SingletonContent sContent = (SingletonContent) modelDoc;
            
            HDBModelParams params = new HDBModelParams(TYPE_CONTENT);
            params.setCreateContentID(sContent.getContentId());
            params.setContentClass(sContent.getContentId());
            document = _hdb.getOrCreateUIDContent(parent, sContent.getContentId() , params);
            
            // If the document does not yet have a type item we regard it as just created an initialize it
            if (!document.hasItem(WGHierarchicalDatabase.ITEMNAME_LISTENER_CLASS_OR_MODULE)) {
                document.setItemValue(WGHierarchicalDatabase.ITEMNAME_LISTENER_CLASS_OR_MODULE, HDBModelListener.class.getName());
                document.save();
            }
            
            Map<String, Object> itemDefaultValues = fetchItemDefaultValues(sContent, sContent.getItems(), document);
            if (initContentItems(document, itemDefaultValues)) {
                LOG.debug("Initializing items on document '" + document.getDocumentKey() + "'");
                document.saveWithGivenTimestamps(document.getCreated(), document.getLastModified());
            }
            
        }
        
        // We only initialize beyond contents if the model version changed
        else if (modelDoc instanceof Content) {
            
            Content content = (Content) modelDoc;
            
            if (parent != null && completeReinit) {
                
                Iterator<WGContent> childContents = parent.getChildContentIterator(10);
                while (childContents.hasNext()) {
                    WGContent childContent = childContents.next();
                    if (content.getContentClass().equals(childContent.getContentClass())) {
                
                        Map<String, Object> itemDefaultValues = fetchItemDefaultValues(content, content.getItems(), childContent);
                        boolean somethingDone = false;
                        somethingDone = initContentItems(childContent, itemDefaultValues);
                        somethingDone = somethingDone || HDBModel.updateParentRelations(childContent);
                        somethingDone = somethingDone || updateContentUniqueName(childContent);
                        
                        if (somethingDone) {
                            LOG.debug("Initializing data on document '" + childContent.getDocumentKey() + "'");
                            childContent.saveWithGivenTimestamps(childContent.getCreated(), childContent.getLastModified());
                        }
                        
                        // Initialize children
                        Iterator<Storage> childStorages = content.getChildStorages().iterator();        
                        while (childStorages.hasNext()) {
                            Storage child = childStorages.next();
                            initModel(child, childContent, completeReinit);
                        }
                        
                    }
                    performReinitSessionRefresh();
                }
                }
            }
            
        }
        
    public boolean updateContentUniqueName(WGContent childContent) throws WGAPIException {
        
        String name = _hdb.createStorageUID(childContent.getParentContent(), UniqueNamePartFormatter.INSTANCE.format(getID(childContent)));
        if (!name.equals(childContent.getUniqueName())) {
            childContent.setUniqueName(name);
            return true;
        }
        
        return false;
        
    }

    private void performReinitSessionRefresh() throws WGAPIException {

        _initSessionRefreshCounter++;
        if (_initSessionRefreshCounter >= 100) {
            LOG.debug("Model (re)initialisation: Clearing session caches");
            _db.getSessionContext().clearCache();
            _initSessionRefreshCounter = 0;
        }
        
    }

    private boolean initContentItems(WGContent con, Map<String, Object> itemDefaultValues) throws WGAPIException {
        
        boolean somethingChanged = false;
        
        // Initialize items
        for (Map.Entry<String,Object> defValue : itemDefaultValues.entrySet()) {
            if (!con.hasItem(defValue.getKey())) {
                con.setItemValue(defValue.getKey(), defValue.getValue());
                somethingChanged = true;
            }
        }
        
        return somethingChanged;
    }

    private Map<String, Object> fetchItemDefaultValues(Document doc, List<Item> items, WGContent parent) throws WGAPIException {
        // Find initial values for items
        Map<String,Object> itemDefaultValues = new HashMap<String, Object>();
        for (Item item : items) {
            String itemName = item.getName();
            String defaultValueExpr = item.getDefaultValueExpression();
            if (!WGUtils.isEmpty(defaultValueExpr)) {
                TMLContext context = new TMLContext(parent, _core, null, null);
                ExpressionResult result = ExpressionEngineFactory.getTMLScriptEngine().evaluateExpression(defaultValueExpr, context, ExpressionEngine.TYPE_EXPRESSION, null);
                if (!result.isError()) {
                    itemDefaultValues.put(itemName, result.getResult());
                }
                else {
                    _core.getLog().error("Exception determining default value for item '" + itemName + "' in document '" + doc.toString() + "' in HDB model", result.getException());
                }
            }
            else {
                itemDefaultValues.put(itemName, null);
            }
        }
        return itemDefaultValues;
    }

    private synchronized void readDefinition(WGFileContainer model) throws WGAPIException {
        
        try {
            
            // Read definition data
            MD5HashingInputStream in = new MD5HashingInputStream(model.getFileData(MODEL_FILE));
            ModelDefinition newDef = ModelDefinition.read(in);
            in.close();
            
            // Redirect definition if redirection specified
            String redirection = newDef.getDefinition();
            if (redirection != null) {
                WGFileContainer redirectModel = model.getDatabase().getFileContainer(redirection);
                readDefinition(redirectModel);
                return;
            }

            // Write definition info to local fields
            _definition = newDef;
            _definitionHash = in.getHash();
            _definitionTime = model.getLastModified().getTime();
            if (_definition.getScripts() != null) {
                _scriptsPath = _definition.getScripts();
            }
            
            // Determine if the model has been changed
            List<String> oldHashes = readOldHashes();
            if (!oldHashes.contains(_definitionHash) && !"false".equals(System.getProperty("de.innovationgate.wga.hdbmodel.reinit"))) {
                _modelVersionChanged = true;
            }
        
        }
        catch (Exception e1) {
            throw new WGBackendException("Exception reading HDB model definition", e1);
        }
        
    }

    public void writeOldHashes(List<String> oldHashes) throws WGAPIException {
        
        if (oldHashes.size() > OLDHASHES_MAX_SIZE) {
            oldHashes.subList(oldHashes.size() - OLDHASHES_MAX_SIZE, oldHashes.size());
        }
        
        _db.writeExtensionData(EXTDATA_MODEL_HASHES, oldHashes);
        
        if (_db.getExtensionData(EXTDATA_MODEL_HASH) != null) {
            _db.removeExtensionData(EXTDATA_MODEL_HASH);
        }
    }

    @SuppressWarnings("unchecked")
    public List<String> readOldHashes() throws WGAPIException {

        if (_db.getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            LOG.warn("Please use HDB model with a OpenWGA content store of version 5 or higher to enable all model features.");
            //TODO: Register Problem
        }
        
        List<String> oldHashes;

        if (_db.getExtensionDataNames().contains(EXTDATA_MODEL_HASHES)) {
            oldHashes = (List<String>) _db.getExtensionData(EXTDATA_MODEL_HASHES);
        }
        
        // Legacy: Only one old hash was stored in an older extdata field (#00003706)
        else if (_db.getExtensionDataNames().contains(EXTDATA_MODEL_HASH)) {
            String oldHash = (String) _db.getExtensionData(EXTDATA_MODEL_HASH);
            oldHashes = new ArrayList<String>();
            oldHashes.add(oldHash);
        }
        else {
            oldHashes = new ArrayList<String>();
        }
        
        return oldHashes;
    }

    /**
     * Returns the model definition for the given document
     * @param content The document
     * @throws WGAPIException
     */
    @CodeCompletion
    public Document getModelForContent(WGContent content) throws WGAPIException {
        
        if (content.isDummy()) {
           return null;
        }

        Iterator<WGContent> contentPath = getContentPath(content).iterator();
        Object currentModelNode = _definition;

        while (contentPath.hasNext()) {
            if (!(currentModelNode instanceof DocumentParent)) {
                return null;
            }
            
            WGContent partContent = (WGContent) contentPath.next();
            
            
            Document foundModel = findChildModelForContent((DocumentParent) currentModelNode, partContent);
            if (foundModel != null) {
                currentModelNode = foundModel;
            }
            else {
                return null;
            }

        }

        return (Document) currentModelNode;

    }
    
    /**
     * Returns a complete path of model definitions for the given document, holding all definitions from root to this document top-down
     * @param content The document
     * @throws WGAPIException
     */
    @CodeCompletion
    public List<Document> getModelPathForContent(WGContent content) throws WGAPIException {
        
        if (content.isDummy()) {
           return null;
        }

        List<Document> path = new ArrayList<Document>();
        Iterator<WGContent> contentPath = getContentPath(content).iterator();
        Object currentModelNode = _definition;

        while (contentPath.hasNext()) {
            if (!(currentModelNode instanceof DocumentParent)) {
                return null;
            }
            
            WGContent partContent = (WGContent) contentPath.next();
            
            
            Document foundModel = findChildModelForContent((DocumentParent) currentModelNode, partContent);
            if (foundModel != null) {
                currentModelNode = foundModel;
                path.add(foundModel);
            }
            else {
                return null;
            }

        }

        return path;

    }

    private Document findChildModelForContent(DocumentParent docParent, WGContent content) throws WGAPIException {

        Iterator<Document> modelCandidates = docParent.getChildDocuments().iterator();
        Document foundModel = null;
        while (modelCandidates.hasNext()) {
            Document modelCandidate = modelCandidates.next();
            if (isModelMatchingContent(content, modelCandidate)) {
                foundModel = modelCandidate;
                break;
            }
        }
        return foundModel;
    }

    private boolean isModelMatchingContent(WGContent content, Document modelDoc) throws WGAPIException {

        boolean modelMatchesContent = false;
        String partContentClass = content.getContentClass();
        String partId = getID(content);

        if (modelDoc instanceof Storage) {
            Storage storage = (Storage) modelDoc;
            if (partId.equals(storage.getStorageId())) {
                modelMatchesContent = true;
            }
        }

        else if (modelDoc instanceof Content && partContentClass != null) {
            Content contentDef = (Content) modelDoc;
            if (partContentClass.equals(contentDef.getContentClass())) {
                modelMatchesContent = true;
            }
        }
        
        else if (modelDoc instanceof SingletonContent) {
            SingletonContent sContent = (SingletonContent) modelDoc;
            if (partId.equalsIgnoreCase(sContent.getContentId())) {
                modelMatchesContent = true;
            }
        }
        
        return modelMatchesContent;
    }
    
    private List<WGContent> getContentPath(WGContent content) throws WGAPIException {

        List<WGContent> contents = new ArrayList<WGContent>();
        while (content != null) {
            contents.add(content);
            content = content.getParentContent();
        }

        Collections.reverse(contents);
        return contents;

    }

    private WGContent getParentForContentClass(String contentClass, WGContent potentialParent, boolean forceRelative) throws WGAPIException {
        
        WGContent nonRelativeParent = null;
        
        Iterator<Content> modelList = getModelsForContentClass(contentClass, potentialParent, forceRelative).iterator();
        while (modelList.hasNext()) {
            Document contentClassModel = (Document) modelList.next();
            DocumentParent parentModel = _definition.getDocumentParent(contentClassModel);
            WGContent parent = findContentForModel(potentialParent, parentModel, false);
            if (parent != null) {
                return parent;
            }
            else if (nonRelativeParent == null) {
                nonRelativeParent = findContentForModel(null, parentModel, false);
            }
    
        }
        
        return nonRelativeParent;
        
    }
    
    /**
     * Tests if the given model is in direct relationship to the reference model position.
     * This is true when model is a subnode of the reference model with no content nodes inbetween them in hierarchy.
     * @param model The model to test
     * @param ref The reference model
     */
    private boolean isDirectRelationship(Document model, Document ref) {
        
        Document child = model;
        while(true) {
            if (child == ref) {
                return true;
            }
            
            // There may be no content nodes between the original child and it's potential parent
            if (child instanceof Content && child != model) {
                return false;
            }
            
            DocumentParent parent = _definition.getDocumentParent(child);
            if (parent instanceof Document) {
                child = (Document) parent;
            }
            else {
                return false;
            }
        }
        
    }
    
    /**
     * Tests if the given mode is a root model. This is the case when there are no content nodes up the hierarchy.
     * @param model The model to test.
     */
    private boolean isRootModel(Document model) {
        
        while(model != null) {
            
            DocumentParent parent = _definition.getDocumentParent(model);
            if (parent instanceof ModelDefinition) {
                return true;
            }
            if (parent instanceof Content) {
                return false;
            }
            model = (Document) parent;
                        
        }
        
        
        return false;
        
    }

    /**
     * Returns all model definitions for the given content class
     * @param contentClass The content class
     */
    @SuppressWarnings("unchecked")
    @CodeCompletion
    public List<Content> getModelsForContentClass(String contentClass) {
        JXPathContext jxPath = JXPathContext.newContext(_definition);
        return jxPath.selectNodes("/rootStorages//childContents[@contentClass='" + contentClass + "']");
    }
    
    
    /**
     * Returns all models for the given content class whose positions are determinable from the given reference document.
     * @param contentClass The content class to search
     * @param ref The reference document. Use null if you do not want any.
     * @param forceRelative Setting to true will exclude "root models"
     * @return A list of models matching the given description. Models with direct relationship to the reference doc come before "root models" which have no relationship to it.
     * @throws WGAPIException
     */
    @CodeCompletion
    public List<Content> getModelsForContentClass(String contentClass, WGContent ref, boolean forceRelative) throws WGAPIException {
        
        List<Content> modelsList = new ArrayList<Content>();
        
        List<Content> allModelsList = getModelsForContentClass(contentClass);
        
        Document refModel = null;
        if (ref != null && !ref.isDummy()) {
            refModel = getModelForContent(ref);
        }
        
        
        Iterator<Content> models = allModelsList.iterator();
        while (models.hasNext()) {
            Content contentClassModel = (Content) models.next();
            
            // Add the model if it is either in direct relationship to the ref model or a root model. First case is to be ordered to the top.
            if (refModel != null && isDirectRelationship(contentClassModel, refModel)) {
                modelsList.add(0,contentClassModel);
            }
            else if (!forceRelative && isRootModel(contentClassModel)) {
                modelsList.add(contentClassModel);
            }
        }
        
        
        return modelsList;
        
    }

    private WGContent findContentForModel(WGContent ref, DocumentParent model, boolean allowContentModels) throws WGAPIException {

        Document refModel = null;
        if (ref != null && !ref.isDummy()) {
           refModel = getModelForContent(ref);
        }
        
        // Ref document is already correct model?
        if (refModel != null && refModel.equals(model)) {
            return ref;
        }
        
        // The given model is no document? Exit
        if (!(model instanceof Document)) {
            return null;
        }
    
        // Retrieve partial model path between refModel and model (or from model to root if no ref given) 
        List<Document> modelPath = new ArrayList<Document>();
        Document currentModel = (Document) model;
        do {
            modelPath.add(currentModel);
            DocumentParent parent = _definition.getDocumentParent(currentModel);
            if (parent instanceof Document) {
                currentModel = (Document) parent;
            }
            else {
                currentModel = null;
            }
        } while (currentModel != null && !currentModel.equals(refModel));
        

        // model is no descendant of refModel - This path is wrong
        if (refModel != null && currentModel == null) {
            return null;
        }

        Collections.reverse(modelPath);
        
        // Traverse the model path and find matching contents
        Iterator<Document> pathIt = modelPath.iterator();
        while (pathIt.hasNext()) {
            Document pathModel = pathIt.next();
            if (!allowContentModels && pathModel instanceof Content) {
                continue;
            }
            
            ref = findChildContentForModel(ref, pathModel);
            if (ref == null) {
                return null;
            }
        }

        return ref;
    }

    private WGContent findChildContentForModel(WGContent ref, Document pathModel) throws WGAPIException {

        Iterator<WGContent> children;

        if (ref != null && !ref.isDummy()) {
            children = ref.getChildContents().iterator();
        }
        else {
            children = _hdb.getRootDocuments().iterator();
        }

        while (children.hasNext()) {
            WGContent child = (WGContent) children.next();
            if (isModelMatchingContent(child, pathModel)) {
                return child;
            }
        }

        return null;

    }

    @CodeCompletion
    public void close() {
        _db.removeDesignChangeListener(this);

    }

    @CodeCompletion
    public void designChanged(WGDesignChangeEvent event) {

        try {

            // Look if the system file container is in update logs
            if (!Groq.selectFrom(event.getUpdateLogs()).wherePropertyEquals("documentKey", SYSTEMFC_DOCKEY).hasNext()) {
                return;
            }
            
            // If so, we reread the definition in a master session task
            MasterSessionTask task = new MasterSessionTask(event.getDatabase()) {
                protected void exec(WGDatabase db) throws Throwable {
                    WGFileContainer system = db.getFileContainer("system");
                    if (system != null && system.getLastModified().getTime() > _definitionTime && system.hasFile(MODEL_FILE)) {
                        _core.getLog().info("Updating HDB model for database " + db.getDbReference());
                        readDefinition(system);
                    }
                }
            };
            
            task.runWithExceptions();
                   
        }
        catch (Throwable e) {
            _core.getLog().error("Error updating HDB model for database" + event.getDatabase().getDbReference(), e);
        }

    }
    

    /**
     * Creates the HDBModel object. Only for internal WGA use.
     * @param core WGA Core
     * @param db The HDBModel database
     * @throws WGAPIException
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    @CodeCompletion
    public static void createModelObject(WGACore core, WGDatabase db) throws WGException, NoSuchAlgorithmException, IOException {
        WGFileContainer system = db.getFileContainer("system");
        if (system == null) {
            return;
        }

        if (!system.hasFile(MODEL_FILE)) {
            return;
        }
    
        HDBModel model = new HDBModel(core, db, system);
        db.setAttribute(WGACore.DBATTRIB_HDBMODEL, model);
        core.getTmlscriptGlobalRegistry().registerAppGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(HDBMODEL_GLOBAL, TMLScriptGlobal.TYPE_OBJECT, model), db);
        
        // Operations that may already trigger events and therefor need a full functional HDBModel object
        model.init();
        
    
    }

    protected void initSubmodel(WGContent newContent) throws WGAPIException, HDBModelException {
        Document model = getModelForContent(newContent);
        if (model == null) {
            throw new HDBModelException("Cannot find model for " + newContent.getDocumentKey());
        }
        
        if (!(model instanceof DocumentParent)) {
            return;
        }
        
        DocumentParent parent = (DocumentParent) model;
        Iterator<Document> childModels = parent.getChildDocuments().iterator();
        while (childModels.hasNext()) {
            Document childModel = (Document) childModels.next();
            initModel(childModel, newContent, false);
        }
        
    }

    /**
     * Create a HDBModel content
     * @param params Parameters for the operation
     * @return The created content
     * @throws HDBModelException
     * @throws WGAPIException
     */
    public WGContent createContent(HDBModelParams params) throws HDBModelException, WGAPIException {

        try {
            
            params.resetStatus();
            
            // Search for the right parent to store this document
            WGContent parent = getParentForContentClass(params.getContentClass(), params.getRefDocument(), false);
            if (parent == null) {
                if (params.getRefDocument() != null) {
                    throw new HDBModelException("HDB model cannot find correct parent for content class '" + params.getContentClass() + "' on ref document '" + params.getRefDocument().getDocumentKey() + "' of class " + params.getRefDocument().getContentClass());
                }
                else {
                    throw new HDBModelException("HDB model cannot find correct parent for content class '" + params.getContentClass() + "' without ref document");
                }
            }
            
            if (params.getCreateContentID() == null) {
                params.setCreateContentID(UIDGenerator.generateUID());
            }
            String title = params.getContentClass() + " " + params.getCreateContentID();
                
            // Create the content
            WGContent newContent = _hdb.createContent(parent, title, params);
            return newContent;
            
        
        }
        catch (WGCancelledException e) {
            if (params.getCancelMessage() != null) {
                throw new WGCancelledException(params.getCancelMessage());
            }
            else {
                throw e;
            }
        }

    }
    
    /**
     * Create a HDBModel content
     * @param contentClass Content class of the content to create
     * @param ref Reference document for the creation below which the content is to be created
     * @return The created content
     * @throws HDBModelException
     * @throws WGAPIException
     */
    public WGContent createContent(String contentClass, WGContent ref) throws WGAPIException, HDBModelException {
        HDBModelParams params = newCreateContentParams(contentClass, ref);
        return createContent(params);
    }

    /**
     * Creates a parameter object for creating a HDBModel content
     * @param contentClass The content class of the content to create
     * @param parent Reference document for the creation below which the content is to be created
     * @throws WGAPIException
     */
    public HDBModelParams newCreateContentParams(String contentClass, WGContent parent) throws WGAPIException {
        HDBModelParams params = new HDBModelParams(HDBModel.TYPE_CONTENT);
        params.setContentClass(contentClass);
        params.setRefDocument(parent);
        return params;
    }
    
    /**
     * Create a HDBModel content
     * @param contentClass Content class of the content to create
     * @param ref Reference document for the creation below which the content is to be created
     * @param param Custom parameter to inject to the operation
     * @return The created content
     * @throws HDBModelException
     * @throws WGAPIException
     */
    public WGContent createContent(String contentClass, WGContent ref, Object param) throws WGAPIException, HDBModelException {
        HDBModelParams params = newCreateContentParams(contentClass, ref);
        params.setCustomParam(param);
        return createContent(params);
    }
    
    /**
     * Create a HDBModel content based on the information from a WebTML form. The form must be of form source appropriate for creating HDBModel contents and have a contentclass specified. 
     * @param form The form
     * @return The created content
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public WGContent createContent(Form form) throws WGAPIException, HDBModelException {
        
        String contentClass = form.getforminfo().getContentClass();
        if (contentClass == null) {
            throw new WGIllegalArgumentException("A WebTML form must declare a content class to be used for HDBModel creation");
        }
        
        Context targetContext = form.gettargetcontext();
        if (targetContext == null) {
            throw new WGIllegalArgumentException("A WebTML form must have a valid target context to be used for HDBModel creation");
        }
        
        HDBModelParams params = newCreateContentParams(contentClass, targetContext.content());
        params.setForm(form);
        return createContent(params);
    }
    
    
    /**
     * Update a HDBModel content
     * @param content The content
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void updateContent(WGContent content) throws WGAPIException, HDBModelException {
        HDBModelParams params = newUpdateContentParams(content);
        updateContent(params);
    }

    /**
     * Create parameters for updating a HDBModel content
     * @param content The content to update
     * @throws WGAPIException
     */
    public HDBModelParams newUpdateContentParams(WGContent content) throws WGAPIException {
        return new HDBModelParams(content);
    }
    
    /**
     * Create parameters for deleting a HDBModel content
     * @param content The content to delete
     * @throws WGAPIException
     */
    public HDBModelParams newDeleteContentParams(WGContent content) throws WGAPIException {
        return new HDBModelParams(content);
    }
    
    /**
     * Update a HDBModel content
     * @param params Parameters for the operation
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void updateContent(HDBModelParams params) throws WGAPIException, HDBModelException {
        
        try {
            params.resetStatus();
            
            _hdb.updateContent(params.getRefDocument(), params);
        }
        catch (WGCancelledException e) {
            if (params.getCancelMessage() != null) {
                throw new WGCancelledException(params.getCancelMessage());
            }
            else {
                throw e;
            }
        }

        
    }
    
    /**
     * Update a HDBModel content
     * @param content The content to update
     * @param process A process to be executed on the update
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void updateContent(WGContent content, HDBModelProcess process) throws WGAPIException, HDBModelException {
        HDBModelParams params = newUpdateContentParams(content);
        params.setProcess(process);
        updateContent(params);
    }

    /**
     * Update a HDBModel content
     * @param content The content to update
     * @param tmlscriptModule Name of a TMLScript module from the design of the HDBModel database to execute as process on the update
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void updateContent(WGContent content, String tmlscriptModule) throws WGAPIException, HDBModelException {
    	HDBModelProcess process = new TMLScriptHDBModelProcess(_db, tmlscriptModule);
    	updateContent(content, process);
    }

    /**
     * Update a HDBModel content
     * @param content The content to update
     * @param tmlscriptModule Name of a TMLScript module from the design of the HDBModel database to execute as process on the update
     * @param param A custom parameter to inject to the operation
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void updateContent(WGContent content, String tmlscriptModule, Object param) throws WGAPIException, HDBModelException {
        
        
        HDBModelParams params = newUpdateContentParams(content);
        
        if (tmlscriptModule != null) {
            HDBModelProcess process = new TMLScriptHDBModelProcess(_db, tmlscriptModule);
            params.setProcess(process);
        }
        
        params.setCustomParam(param);
        updateContent(params);
        
    }
    
    /**
     * Update a HDBModel content based on the information from a WebTML form. The form must be of form source appropriate for updating HDBModel contents and have this content as target context. 
     * @param form The form
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void updateContent(Form form) throws WGAPIException, HDBModelException {
        
        Context targetContext = form.gettargetcontext();
        if (targetContext == null) {
            throw new WGIllegalArgumentException("A WebTML form must have a valid target context to be used for HDBModel updates");
        }

        HDBModelParams params = newUpdateContentParams(targetContext.content());
        params.setForm(form);
        updateContent(params);
    }
    
    /**
     * Delete a HDBModel content
     * @param content The content to delete
     * @throws WGAPIException
     */
    public void deleteContent(WGContent content) throws WGAPIException {
        HDBModelParams params = newDeleteContentParams(content);
        deleteContent(params);
    }

    /**
     * Delete a HDBModel content
     * @param params Parameters for the operation
     * @throws WGAPIException
     */
    public void deleteContent(HDBModelParams params) throws WGAPIException {
        
        try {
            params.resetStatus();
            
            _hdb.deleteContent(params.getRefDocument(), params);
        }
        catch (WGCancelledException e) {
            if (params.getCancelMessage() != null) {
                throw new WGCancelledException(params.getCancelMessage());
            }
            else {
                throw e;
            }
        }
    }
    
    /**
     * Move a HDBModel content
     * @param content The content to move
     * @param ref The new parent of the content
     * @throws WGAPIException
     * @throws HDBModelException
     */
    public void moveContent(WGContent content, WGContent ref) throws WGAPIException, HDBModelException {
        WGContent newParent = getParentForContentClass(content.getContentClass(), ref, true);
        if (newParent == null) {
            throw new HDBModelException("Cannot find valid position for content class '" + content.getContentClass() + "' below move target " + ref.getDocumentKey());
        }
        _hdb.moveContent(content, newParent);
        
        //TODO: Update all parent relations?
    }
    
    /**
     * Retrieve the valid relation targets for a HDBModel relation
     * @param content The content having the relation or a parent content below which the model of this content is defined (in case the content does not yet exist)
     * @param contentClass The content class of the content for which the relation is defined
     * @param relation Name of the relation
     * @return Result set of all valid relation targets 
     * @throws WGException
     * @throws HDBModelException
     */
    @CodeCompletion
    public WGAbstractResultSet getRelationTargets(WGContent content, String contentClass, String relation) throws WGException, HDBModelException {
        
        Map defaultParams = new HashMap();
        defaultParams.put(WGDatabase.QUERYOPTION_CACHERESULT, Boolean.TRUE);
        return getRelationTargets(content, contentClass, relation, defaultParams);
    }

    /**
     * Retrieve the valid relation targets for a HDBModel relation
     * @param content The content having the relation or a parent content below which the model of this content is defined (in case the content does not yet exist)
     * @param contentClass The content class of the content for which the relation is defined
     * @param relation Name of the relation
     * @param options Custom options for the retrieval. Use constants OPTION_... from this class or QUERYOPTION_... from {@link WGDatabase}
     * @return Result set of all valid relation targets 
     * @throws WGException
     * @throws HDBModelException
     */
    @CodeCompletion
    public WGAbstractResultSet getRelationTargets(WGContent content, String contentClass, String relation, Map options) throws WGException, HDBModelException {
        
        Document refNode = getModelForContent(content);
        if (refNode == null) {
            throw new HDBModelException("Cannot find model for content " + content.getContentKey() + " of class " + content.getContentClass());
        }
        
        Content contentNode = null;
        if (refNode instanceof Content && (contentClass == null || contentClass.equals(((Content) refNode).getContentClass()))) {
            contentNode = (Content) refNode;
        }
        else if (refNode instanceof DocumentParent){
            contentNode = findChildModelOfClass((DocumentParent) refNode, contentClass);
            if (contentNode == null) {
                throw new HDBModelException("Cannot find direct child content class " + contentClass + " from ref document '" + content.getContentKey() + "' of class " + content.getContentClass());
            }
        }
        
        JXPathContext jxPath = JXPathContext.newContext(contentNode);
        Relation relationNode =  (Relation) jxPath.selectSingleNode("/relations[name='" + relation + "']");
        if (relationNode == null) {
            throw new HDBModelException("Cannot find relation '" + relation + "' for content class " + content.getContentClass());
        }
        
        String extraClause = (String) options.get(OPTION_EXTRACLAUSE);
        Map extraParams = (Map) options.get(OPTION_EXTRAPARAMS);
        if (extraParams != null) {
            extraParams = new HashMap(extraParams);
        }
        Boolean includeCurrent = WGUtils.getBooleanMapValue(options, OPTION_INCLUDECURRENT, true);
        
        return getRelationTargets(content, contentClass, relationNode, extraClause, extraParams, includeCurrent, options);
        
    }

    private Content findChildModelOfClass(DocumentParent refDoc, String contentClass) {
        
        for (Document child : refDoc.getChildDocuments()) {

            if (child instanceof Content) {
                Content content = (Content) child;
                if (contentClass.equals(content.getContentClass())) {
                    return content;
                }
            }
            
            if (child instanceof DocumentParent) {
                DocumentParent docParent = (DocumentParent) child;
                Content foundModel = findChildModelOfClass(docParent, contentClass);
                if (foundModel != null) {
                    return foundModel;
                }
            }
            
        }
        
        return null;
        
    }

    private WGAbstractResultSet getRelationTargets(WGContent document, String contentClass, Relation relation, String extraClause, Map extraParams, boolean includeCurrentTarget, Map options) throws WGException {
        
        WGContent baseContent = null;
        
        // Get target and base class
        String targetClass = relation.getTargetClass();
        if (targetClass == null) {
            throw new HDBModelException("Missing attribute targetclass on relation '" + relation.getName() + "'");
        }
        
        String baseClass = relation.getBaseClass();
        if (baseClass != null) {
            if (baseClass.equals(document.getContentClass())) {
                baseContent = document;
            }
            else {
                baseContent = document.getRelation(PARENT_RELATION_PREFIX + baseClass);
                if (baseContent == null) {
                    throw new HDBModelException("Cannot find base class '" + baseClass + "' from content " + document.getContentKey());
                }
            }
        }
        
        List<String> conditions = new ArrayList<String>();
        List<String> orders = new ArrayList<String>();
        Map<String,Object> params = new HashMap<String,Object>();
        params.putAll(Database.buildDefaultQueryParams(document));
        
        // Add relation targets as parameters
        for (String relationName : document.getRelationNames()) {
            WGContent relationTarget = document.getRelation(relationName);
            if (relationTarget != null) {
                params.put("relation_" + Database.makeQueryParameterName(relationName), relationTarget);
            }
        }
        

        // Include the current targets of the relation, even if they are no longer returned by the option collection 
        String currentTargetsInclusion = null;
        if (includeCurrentTarget && isContent(document)) {
            
            // The reference content is itself a parent?
            if (!contentClass.equals(document.getContentClass())) {
                params.put("relation_parent_" + Database.makeQueryParameterName(document.getContentClass()), document);
            }
            // If the reference content itself is the content to store the relation on we automatically add all existing relation targets
            else {
                params.put("target_content", document);
                params.put("relation_name", relation.getName());
                if (relation.isGroup()) {
                    currentTargetsInclusion = "content in (select rel.target from ContentRelation rel where rel.group=:relation_name AND rel.parentcontent=:target_content)";
                }
                else {
                    currentTargetsInclusion = "content in (select rel.target from ContentRelation rel where rel.name=:relation_name AND rel.parentcontent=:target_content)";
                }
            }
        }
        
        int paramIdx = 0;
        
        if (baseContent != null) {
            paramIdx++;
            params.put("p" + String.valueOf(paramIdx), baseContent);
            conditions.add("content.relations['parent-" + baseClass + "'].target = :p" + String.valueOf(paramIdx));
        }
        
        // Processing content class(es) including their filters
        List<String> classTerms = new ArrayList();
        List<String> targetClasses = WGUtils.deserializeCollection(targetClass, ",", true);
        for (String singleClass : targetClasses) {
            classTerms.add("content.contentclass = '" + singleClass + "'");
        }
        
        String filter = relation.getFilter();
        if (filter != null) {
            List<String> filterNames = WGUtils.deserializeCollection(filter, ",", true);
            for (int idx=0; idx < filterNames.size(); idx++) {
                String filterName = filterNames.get(idx);
                List<String> filterConditions = new ArrayList<String>();
                if (filterName.equals("none")) {
                    continue;
                }
                if (targetClasses.size() <= idx) {
                    throw new HDBModelException("More filters than target classes on relation '" + relation.getName() + "'");
                }
                
                Document baseModel = getModelForContent(baseContent);
                Filter filterNode = getFilterNode(baseModel, targetClasses.get(idx), filterName);
                if (filterNode == null) {
                    throw new HDBModelException("Cannot find filter '" + filterName + "' for content class '" + targetClasses.get(idx) + "'");
                }
                
                for (WhereClause condition : filterNode.getWhereClauses()) {
                    filterConditions.add("(" + condition.getText().trim() + ")");
                }
            
                // This might be tricky with multiple ordered filters, may still work when used cautious
                if (filterNode.getOrderClause() != null) {
                    orders.add(filterNode.getOrderClause().getText().trim()); 
                }
                
                String classTerm = classTerms.get(idx);
                classTerms.set(idx, classTerm + " AND " + WGUtils.serializeCollection(filterConditions, " AND" ));
            }
        }
        
        conditions.add("(" + WGUtils.serializeCollection(classTerms, " OR ") + ")");
        
        // Add additional filter from relation
 
        for (WhereClause whereClause : relation.getExtraWhereClauses()) {
            conditions.add("(" + whereClause.getText().trim() + ")");
        }
        
        for (FilterParam filterParam : relation.getExtraFilterParams()) {
            
            TMLContext context = new TMLContext(document, _core, null, null);
            ExpressionResult result = ExpressionEngineFactory.getTMLScriptEngine().evaluateExpression(filterParam.getExpression().trim(), context, ExpressionEngine.TYPE_EXPRESSION, null);
            if (!result.isError()) {
                params.put(filterParam.getName(), result.getResult());
            }
            else {
                throw new HDBModelException("Error evaluating filter param " + filterParam.getName(), result.getException());
            }
            
        }
        
        // Extract an optional order by clause from the extra clause
        String extraClauseOrderClause = null;
        if (extraClause != null) {
            int whereClauseEndIdx = extraClause.toLowerCase().indexOf("order by");
            if (whereClauseEndIdx != -1) {
                extraClauseOrderClause = extraClause.substring(whereClauseEndIdx + 8);
                extraClause = extraClause.substring(0, whereClauseEndIdx);
            }
        }
        
        // Extra filter clause/params via parameter
        if (extraClause != null && !extraClause.trim().isEmpty()) {
            conditions.add("(" + extraClause + ")");
        }
        if (extraParams != null) {
            params.putAll(extraParams);
        }
        
        if (extraClauseOrderClause != null) {
            // order tags on relation overwrite other order clauses
            orders.clear();
            orders.add(extraClauseOrderClause);
        }
        else if (relation.getExtraOrderClause() != null) {
            // order tags on relation overwrite the order set from target class filter
            orders.clear();
            orders.add(relation.getExtraOrderClause().getText().trim());
        }
        
        String hql = WGUtils.serializeCollection(conditions, " AND ");
        if (currentTargetsInclusion != null) {
            hql = currentTargetsInclusion + " OR (" + hql + ")";
        }
        
        
        if (orders.size() > 0) {
            hql += " ORDER BY " + WGUtils.serializeCollection(orders, ", ");
        }
        
        //_core.getLog().info(contentClass + "." + relationNode.attributeValue("name") + ": " + hql);
        
        
        Map<String,Object> queryParams = new HashMap<String, Object>();
        queryParams.put(WGDatabase.QUERYOPTION_QUERY_PARAMETERS, params);
        
        // Transport regular query parameters to the internal query
        migrateQueryParameter(WGDatabase.QUERYOPTION_CACHERESULT, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_ROLE, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_MAXRESULTS, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_ENHANCE, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_ONLYRELEASED, options, queryParams);
        
        WGAbstractResultSet resultSet = _db.query("hql", hql, queryParams);
        
        // Migrate output parameters back
        try {
            migrateQueryParameter(WGDatabase.QUERYOPTION_RETURNQUERY, queryParams, options);
        }
        catch (UnsupportedOperationException e) {
            // Silently ignore if options map is unmodifiable
        }
        
        return resultSet;
    }

    private Filter getFilterNode(Document baseModel, String targetClass, String filter) {
        
        if (!(baseModel instanceof DocumentParent)) {
            return null;
        }
        
        Document model = findChildModelOfClass((DocumentParent) baseModel, targetClass);
        JXPathContext jxPath = JXPathContext.newContext(model);
        
        return (Filter) jxPath.selectSingleNode("/filters[name='" + filter + "']");
        
    }
    
    /**
     * Retrieves the source contents for a HDBModel relation that point to a given content
     * @param target The content to which the relations point
     * @param contentClass The content class of the source contents to return. Specify null to return contents of all content classes.
     * @param relation The name of the relation that should point to the target content
     * @param options Custom options for the retrieval. Use constants OPTION_... from this class or QUERYOPTION_... from {@link WGDatabase} 
     * @throws WGAPIException
     * @throws HDBModelException
     */
    @CodeCompletion
    public WGAbstractResultSet getRelationSources(WGContent target, String contentClass, String relation, Map options) throws WGAPIException, HDBModelException {
        
        String extraClause = (String) options.get(OPTION_EXTRACLAUSE);
        
        Map extraParams = (Map) options.get(OPTION_EXTRAPARAMS);
        
        JXPathContext jxPath = JXPathContext.newContext(_definition);
        Relation relationNode =  (Relation) jxPath.selectSingleNode("//childContents[contentClass='" + contentClass + "']/relations[name='" + relation + "']");
        if (relationNode == null) {
            throw new HDBModelException("Cannot find relation '" + relation + "' for content class " + contentClass);
        }
        
        StringBuffer hql = new StringBuffer();
        if (relationNode.isGroup()) {
            hql.append("content.contentclass = :contentclass and :target in (select rel.target from ContentRelation as rel where rel.parentcontent = content and rel.group=:relname)");
        }
        else {
            hql.append("content.contentclass = :contentclass and content.relations[:relname].target = :target");
        }
        if (extraClause != null) {
            // Extract an optional order by clause from the extra clause
            String extraClauseOrderClause = null;
            int whereClauseEndIdx = extraClause.toLowerCase().indexOf("order by");
            if (whereClauseEndIdx != -1) {
                extraClauseOrderClause = extraClause.substring(whereClauseEndIdx + 8);
                extraClause = extraClause.substring(0, whereClauseEndIdx);
            }
            if (!extraClause.trim().isEmpty()) {
                hql.append(" AND (" + extraClause + ")");
            }
            if (extraClauseOrderClause != null) {
                hql.append(" ORDER BY ").append(extraClauseOrderClause);
            }
        }
        
        Map queryParams = new HashMap();
        queryParams.put("contentclass", contentClass);
        queryParams.put("relname", relation);
        queryParams.put("target", target);
        if (extraParams != null) {
            queryParams.putAll(extraParams);
        }
        
        Map parameters = new HashMap();
        parameters.put(WGDatabase.QUERYOPTION_QUERY_PARAMETERS, queryParams);
        
        // Transport regular query parameters to the internal query
        migrateQueryParameter(WGDatabase.QUERYOPTION_CACHERESULT, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_ROLE, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_MAXRESULTS, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_ENHANCE, options, queryParams);
        migrateQueryParameter(WGDatabase.QUERYOPTION_ONLYRELEASED, options, queryParams);
        
        WGAbstractResultSet resultSet = _db.query("hql", hql.toString(), parameters);

        // Migrate output parameters back
        try {
            migrateQueryParameter(WGDatabase.QUERYOPTION_RETURNQUERY, parameters, options);
        }
        catch (UnsupportedOperationException e) {
            // Silently ignore if options map is unmodifiable
        }
                
        return resultSet;
        
    }

    private void migrateQueryParameter(String param, Map source, Map target) {
        if (source.containsKey(param)) {
            target.put(param, source.get(param));
        }
    }
    
    /**
     * Retrieves the source contents for a HDBModel relation that point to a given content
     * @param target The content to which the relations point
     * @param contentClass The content class of the source contents to return. Specify null to return contents of all content classes.
     * @param relation The name of the relation that should point to the target content
     * @throws WGAPIException
     * @throws HDBModelException
     */
    @CodeCompletion
    public WGAbstractResultSet getRelationSources(WGContent target, String contentClass, String relation) throws WGAPIException, HDBModelException {
        Map defaultParams = new HashMap();
        defaultParams.put(WGDatabase.QUERYOPTION_CACHERESULT, Boolean.TRUE);
        return getRelationSources(target, contentClass, relation, defaultParams);
    }
    
    /**
     * Assings a content id to the given content.
     * @param content The content
     * @param contentId The content ID
     * @throws WGAPIException
     */
    public void assignContentID(WGContent content, String contentId) throws WGAPIException {
        
        if (!TYPE_CONTENT.equals(content.getItemValue(ITEM_TYPE))) {
            throw new WGIllegalDataException("Cannot assign content ID to storage " + getID(content));
        }
        content.setItemValue(ITEM_CONTENT_ID, contentId);
        _hdb.assignContentUID(content, UniqueNamePartFormatter.INSTANCE.format(contentId));
    }
    
    /**
     * Event method for executing necessary operations when a HDBModel content is created. Only for internal WGA use.
     * @param event The HDBModel operation event
     * @return False if the event was cancelled
     * @throws Throwable
     */
    @CodeCompletion
    public boolean onCreate(ContentTypeEvent event) throws Throwable {
        
        if (event.getdocument() != null) {
            Object parameter = event.getdatabase().getSessionContext().getAttribute(WGHierarchicalDatabase.SESSIONPARAM_HDBPARAMETER);
            
            // Not Created via HDBModel functionalities - Exit but allow creation
            if (!(parameter instanceof HDBModelParams)) {
                return true;
            }
            
            // Fetch data
            HDBModelParams params = (HDBModelParams) parameter;
            WGContent newContent = event.getdocument();
            newContent.setItemValue(HDBModel.ITEM_TYPE, params.getType());
            
            // Operations for model contents only. Initialisations for storages is done in initModel() to make it in one place for HDBModel storages and "real" HDB storages (which wouldn't work here)
            if (params.getType().equals(HDBModel.TYPE_CONTENT)) {
                
                // Initialize the content itself
                newContent.setContentClass(params.getContentClass());
                newContent.setItemValue(WGHierarchicalDatabase.ITEMNAME_LISTENER_CLASS_OR_MODULE, HDBModelListener.class.getName()); // Only on content. On storages this is done by HDBModel.initModel()
                
                assignContentID(newContent, params.getCreateContentID());

                // Initialize item defaults
                Document docModel = getModelForContent(event.getdocument());
                if (docModel instanceof Content) { // Excludes singleton contents whose items are initialized in initModel()
                    Content contentModel = (Content) docModel;
                    Map<String,Object> defaultValues = fetchItemDefaultValues(contentModel, contentModel.getItems(), event.getdocument());
                    initContentItems(event.getdocument(), defaultValues);
                }

                // Transfer eventual form data
                TMLForm form = (TMLForm) params.getForm();
                if (form != null) {
                    form.pushFields(newContent);
                    form.pushFiles(newContent);
                    form.retainFiles(newContent);
                }
                
                // Execute custom process
                executeCustomProcess(event.getdocument(), getCore(), params);
            }
            
            updateParentRelations(newContent);
            
            
            // Execute event scripts
            if (params != null) {
                HDBModelContentClassEvents listener = HDBModelListener.retrieveContentClassEvents(event.getdatabase(), params.getContentClass());
                if (listener != null) {
                    try {
                        HDBModelEvent hdbModelEvent = new HDBModelContentEvent(event.getdocument(), HDBModel.EVENT_ON_CREATE, params, true);
                        listener.callModuleEventFunction(hdbModelEvent);
                        return true;
                    }
                    catch (WGCancelledException e) {
                        params.setCancelMessage(e.getMessage());
                        return false;
                    }
                }
            }
        }
        
        return true;
        
    }

    protected static void executeCustomProcess(WGContent content, WGACore core, HDBModelParams params) throws Throwable {
        HDBModelProcess process = params.getProcess();
        if (process != null) {
            if (process instanceof WGACoreAwareHDBModelProcess) {
                ((WGACoreAwareHDBModelProcess) process).setCore(core);
            }
            Map<String, Object> processParams = new HashMap<String, Object>(); 
            processParams.put("hdbParams", params);
            process.run(content, processParams);
        }
    }

    private String getStorageContentClass(Storage docModel) {
        return STORAGE_CONTENTCLASS;
    }
    
    /**
     * Event method for executing necessary operations when a HDBModel content is saved. Only for internal WGA use.
     * @param event The HDBModel operation event
     * @return False if the event was cancelled
     * @throws Throwable
     */
    @CodeCompletion
    public boolean onSave(ContentTypeEvent event) throws Throwable {
        
        if (event.getdocument() != null) {
            Object parameter = event.getdatabase().getSessionContext().getAttribute(WGHierarchicalDatabase.SESSIONPARAM_HDBPARAMETER);
            

            if (!(parameter instanceof HDBModelParams)) {
                // Not saved via HDB functionalities -Exit, but allow saving                
                return true;
            }
            
            HDBModelParams hdbParams = (HDBModelParams) parameter;
            HDBModelContentClassEvents listener = HDBModelListener.retrieveContentClassEvents(event.getdatabase(), hdbParams.getContentClass());
            if (listener != null) {
                try {
                    HDBModelEvent hdbModelEvent = new HDBModelContentEvent(event.getdocument(), HDBModel.EVENT_ON_SAVE, hdbParams, true);
                    listener.callModuleEventFunction(hdbModelEvent);
                    return true;
                }
                catch (WGCancelledException e) {
                    hdbParams.setCancelMessage(e.getMessage());
                    return false;
                }
            }
            
            
        }
        
        return true;
        
    }

    protected static boolean updateParentRelations(WGContent newContent) throws WGAPIException {
    
        boolean somethingDone = false;
        
        // Setting relations to parent contents
        Set<String> setRelations = new HashSet<String>();
        WGContent parentContent = newContent.getParentContent();
        while (parentContent != null) {
            if (parentContent.getItemValue(HDBModel.ITEM_TYPE).equals(HDBModel.TYPE_CONTENT)) {
                String relationName = PARENT_RELATION_PREFIX + parentContent.getContentClass().toLowerCase();
                setRelations.add(relationName);
                if (newContent.getRelation(relationName) != parentContent) {
                    newContent.setRelation(relationName, parentContent);
                    somethingDone = true;
                }
            }
            parentContent = parentContent.getParentContent();
        }
        
        // Removing all old parent relations that do not apply any more
        for (String relName : newContent.getRelationNames()) {
            if (relName.startsWith(PARENT_RELATION_PREFIX) && !setRelations.contains(relName)) {
                newContent.removeRelation(relName);
                somethingDone = true;
            }
        }
        
        return somethingDone;
        
    }

    protected String getScriptsPath() {
        return _scriptsPath;
    }

    /**
     * Returns the HDBModel definition
     */
    @CodeCompletion
    public ModelDefinition getDefinition() {
        return _definition;
    }
    

    /**
     * Sets valid editors on a content, keeping inherited rights effective and reduce the rights additionally.
     * A content with whose valid editors are set with this methods can only be edited by users that are contained in the editors list on this document AND the editors list of the parent document.
     * @param con The content
     * @param editors The valid editors (user names, groups or roles) of this content and its subcontents
     * @throws WGAPIException
     */
    public void reduceContentEditors(WGContent con, List editors) throws WGAPIException {
        
        WGStructEntry struct = con.getStructEntry();
        
        editors = new ArrayList(editors);
        if (editors.size() > 0 && editors.get(0).equals(WGStructEntry.EDITORS_INHERIT_AND_REDUCE)) {
            editors.remove(0);
        }
        
        struct.setPageEditors(editors);
        struct.setChildEditors(editors);
        struct.save();
        
    }
    
    /**
     * Sets valid editors on a content
     * @param con The content
     * @param contentEditors The valid editors (user names, groups or roles) of this content
     * @param childEditors The valid editors (user names, groups or roles) for child contents of this content
     * @throws WGAPIException
     */
    public void setContentEditors(WGContent con, List contentEditors, List childEditors) throws WGAPIException {
        WGStructEntry struct = con.getStructEntry();
        struct.setPageEditors(contentEditors);
        struct.setChildEditors(childEditors);
        struct.save();
    }
    
    /**
     * Sets readers for a content document
     * @param con The content document
     * @param readers The readers (user names, groups or roles) of this content
     * @throws WGAPIException
     */
    public void setContentReaders(WGContent con, List readers) throws WGAPIException {
        WGStructEntry struct = con.getStructEntry();
        struct.setReaders(readers);
        struct.save();
    }
    
    /**
     * Stores the data of a WebTML form in into the database. The form source decides if a new HDBModel content is created or an existing is updated.
     * @param form The form
     * @param content Either the content to update or the reference content below which a new content is to be created.
     * @return False if the saving was cancelled
     * @throws WGAServerException
     * @throws WGAPIException
     */
    public boolean storeForm(Form form, WGContent content) throws WGAServerException, WGAPIException {

            // reset createdDoc - necessary for persistent forms
           ((TMLForm) form).setcreateddoc(null);
           
           // Perform update
           String source = form.getforminfo().getSource();
           if (source.equals("content")) {
               
               HDBModelParams params = newUpdateContentParams(content);
               params.setForm(form);
               params.setCustomParam(null);
               try {
                   updateContent(params);
                   return true;
               }
               catch (WGHierarchicalDatabaseEventCanceledException e) {
                   if (!(e instanceof HDBModelFormValidationFailedException) && !WGUtils.isEmpty(e.getMessage())) {
                       form.clearmessages();
                       form.addmessage(e.getMessage());
                   }
                   return false;
               }
               catch (WGCancelledException e) {
                   if (!WGUtils.isEmpty(e.getMessage())) {
                       form.clearmessages();
                       form.addmessage(e.getMessage());
                   }
                   return false;
               }
               catch (HDBModelException e) {
                   throw new WGAServerException("Exception updating HDB model content", e);
               }
           }
           
           // Perform create
           else if (source.equals("none")  || source.equals("newcontent")) {
               
               // Get desired content class
               String contentClass = form.getforminfo().getContentClass();
               if (contentClass == null) {
                   throw new WGAServerException("Cannot use tmlform.storeInHDB() on form " + form.getformid() + " that is defined without attribute 'contentclass'");
               }
               
               HDBModelParams params = newCreateContentParams(contentClass, content);
               params.setForm(form);
               params.setCustomParam(null);
               
               // Create the content
               try {
                   ((TMLForm) form).setcreateddoc(createContent(params));
                   return true;
               }
               catch (WGHierarchicalDatabaseEventCanceledException e) {
                   if (!(e instanceof HDBModelFormValidationFailedException)) {
                       form.addmessage(e.getMessage());
                   }
                   return false;
               }
               catch (HDBModelException e) {
                   throw new WGAServerException("Exception creating HDB model content", e);
               }
           }
           else {
               throw new IllegalStateException("Cannot use tmlform.storeInHDB() when form source is not 'content', 'newcontent' or 'none'");
           }
           
       }
    
    /**
     * Retrieves a root storage by its storage id
     * @param id The storage id
     * @throws WGAPIException
     */
    public WGContent getDocument(String id) throws WGAPIException {
        return getDocument(id, null);
    }
    
    /**
     * Returns a document, either a storage or content, relative to the given content or absolute if omitted.
     * This will only fetch released contents.
     * @param id The document id, either storage or content id
     * @param relContent The parent storage/content of the document to fetch. Specify null to retrieve root storages.
     * @return The storage of the given Id
     * @throws WGAPIException 
     */
    public WGContent getDocument(String id, WGContent relContent) throws WGAPIException {
        
        if (relContent == null) {
            return _db.getContentByName(UniqueNamePartFormatter.INSTANCE.format(id));
        }
        else {
            return _db.getContentByName(relContent.getUniqueName() + "." + UniqueNamePartFormatter.INSTANCE.format(id));
        }
        
    }
    /**
     * Retrieves a document by its struct key. This will fetch the struct entry and automatically choose the most relevant content. This will also respect contents in draft or workflow state.
     * @param structKey The struct key
     * @return The document, either content or storage, or null if not found.
     * @throws WGAPIException
     */
    public WGContent getDocumentByStructKey(Object structKey) throws WGAPIException {
        
        WGStructEntry entry = _db.getStructEntryByKey(structKey);
        if (entry == null) {
            return null;
        }
        
        WGContent content = entry.getReleasedContent(_db.getDefaultLanguage());
        if (content == null) {
            content = entry.getContent(_db.getDefaultLanguage(), WGContent.STATUS_REVIEW);
            if (content == null) {
                content = entry.getContent(_db.getDefaultLanguage(), WGContent.STATUS_DRAFT);
            }
        }
        
        return content;
        
    }

    /**
     * Indicates if the HDBModel definition has changed since the last HDBModel reinit, so the model definition may not match the stored content in the database
     */
    public boolean isModelVersionChanged() {
        return _modelVersionChanged;
    }
    
}

