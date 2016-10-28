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
package de.innovationgate.webgate.api.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGACLEntry;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.wga.config.FileDerivateManagerConfiguration;

/**
 * Tool class to export and import content store dumps between OpenWGA content stores.
 * This class itself does not create the OpenWGA content store dump format ".wgacs" which is essentially a zipped HSQLDB content store. It just performs the data pumping task.
 */
public class ContentStoreDumpManager {
    
    /**
     * Extension data fields on the database which should not be migrated as they represent local database state that should not be transferred
     */
    public static final List<String> UNMIGRATEABLE_DBMETADATA = new ArrayList<String>();
    static {
        UNMIGRATEABLE_DBMETADATA.addAll(WGDatabase.PROTECTED_DATABASE_EXTDATA);
        UNMIGRATEABLE_DBMETADATA.add(WGDatabase.EXTDATA_DERIVATE_CREATORS);
        UNMIGRATEABLE_DBMETADATA.add(WGDatabase.EXTDATA_DERIVATE_REVISION);
    }

    private int _sourceDocCounter;

    private int _targetDocCounter;

    private Logger _log = Logger.getLogger("wga.csdump");

    private WGDatabase _from;

    private WGDatabase _to;
    
    private int _clearCacheEvery = 100;
    
    private List _contentClones = new ArrayList();

    private Map _oldToNewContentKeys = new HashMap();

    private boolean _import = false;

    private boolean _includeACL;
    
    private boolean _includeSystemAreas = false;
    
    private boolean _includeArchivedContents = true;

    /**
     * Construtor
     * @param from The source database
     * @param to The target database
     * @param logger A logger to write the dump log to
     */
    public ContentStoreDumpManager(WGDatabase from, WGDatabase to, Logger logger) {
        _from = from;
        _to = to;
        if (logger != null) {
            _log = logger;
        }
    }

    /**
     * Constructor
     * @param from The source database
     * @param to The target database
     */
    public ContentStoreDumpManager(WGDatabase from, WGDatabase to) {
        this(from, to, null);
    }

    private boolean innerTransport() throws WGUnavailableException {

        _log.info("Content Store Dump Transport starting");
        _log.info("Source database: " + _from.getDbReference());
        _log.info("Target database: " + _to.getDbReference());
        _log.info("include ACL: " + _includeACL);
        _log.info("include archived: " + _includeArchivedContents);
        _log.info("include system areas: " + _includeSystemAreas);
        
        if (!_to.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            _log.warn("Target database is no full-featured WGA Content Store. This is not supported and transport might fail.");
        }
        
        _sourceDocCounter = 0;
        _targetDocCounter = 0;

        // Delete everything old
        try {
            if (_to.getSessionContext().isMasterSession() && _to.isBackendServiceSupported(WGDatabase.BACKENDSERVICE_CLEAR_CONTENT_AND_SCHEMA)) {
                _log.info("Clearing data and schema of target content store...");
                _to.callBackendService(WGDatabase.BACKENDSERVICE_CLEAR_CONTENT_AND_SCHEMA, null);
            }
            else {
                deleteOldContent();
                deleteOldContentTypes();
                deleteOldLanguages();
                deleteOldDbMetadata();
            }
            
            if (_includeACL) {
                deleteOldACL();
            }

            // Clone everything
            cloneDbMetadata();
            cloneLanguages();
            cloneContentTypes();
            cloneContent();
            if (_includeACL) {
                cloneACL();
            }
            _log.info("Content Store Dump Transport finished");
            return true;
        }
        catch (WGAPIException e) {
            _log.fatal("Transport canceled because of exception", e);
            return false;
        }


    }

    private void cloneDbMetadata() throws WGAPIException {
        
        if (_to.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && _from .getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            
            Iterator extDataNames = _from.getExtensionDataNames().iterator();
            if (extDataNames.hasNext()) {
                _log.info("Cloning database metadata...");
                while (extDataNames.hasNext()) {
                    String extDataName = (String) extDataNames.next();
                    if (UNMIGRATEABLE_DBMETADATA.contains(extDataName)) {
                        continue;
                    }
                    
                    Object value = _from.getExtensionData(extDataName);
                    if (value instanceof List) {
                        value = new ArrayList((List) value);
                    }
                    try {
                        _to.writeExtensionData(extDataName, value);
                    }
                    catch (WGAPIException e) {
                        handleException("Error cloning database metadata", e, null);
                    }
                }
            }
        }
        
    }

    private void deleteOldDbMetadata() throws WGAPIException {
        
        if (_to.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            Iterator names = _to.getExtensionDataNames().iterator();
            if (names.hasNext()) {
                _log.info("Deleting old database metadata...");
                while (names.hasNext()) {
                    String name = (String) names.next();
                    if (!UNMIGRATEABLE_DBMETADATA.contains(name)) {
                        _to.removeExtensionData(name);
                    }
                }
            }
        }
        
    }

    private void incSourceDocCounter() throws WGAPIException {
        _sourceDocCounter++;
        if (_sourceDocCounter >= _clearCacheEvery) {
            _log.info("Clearing cache for source database");
            _from.refresh();
            setFromSessionProps();
            _sourceDocCounter = 0;
        }
    }

    private void incTargetDocCounter() throws WGAPIException {
        _targetDocCounter++;
        if (_targetDocCounter >= _clearCacheEvery) {
            _log.info("Clearing cache for target database");
            _to.refresh();
            setToSessionProps();
            _targetDocCounter = 0;
        }
    }

    private void cloneContent() throws WGAPIException {

        _log.info("Cloning areas, struct entries and contents...");
        Iterator areas = _from.getAreas().iterator();
        WGArea area;
        WGArea areaClone;

        // Clone everything
        while (areas.hasNext()) {
            area = (WGArea) areas.next();
            if (area.isSystemArea() && !_includeSystemAreas) {
                continue;
            }
            
            validate(area);
            _log.info("Cloning area '" + area.getName() + "'");
            try {
                areaClone = (WGArea) createDumpClone(area, null);
            }
            catch (WGAPIException e) {
                handleException("Error cloning area", e, area);
                continue;
            }
            
            if (areaClone == null) {
                handleSaveError("Unable to clone area", area);
                continue;
            }

            areaClone.dropCore();
            area.dropCore();
            
            incSourceDocCounter();
            incTargetDocCounter();
            Iterator rootEntries = area.getRootEntries().iterator();
            WGStructEntry rootEntry;
            while (rootEntries.hasNext()) {
                rootEntry = (WGStructEntry) rootEntries.next();
                cloneStructEntryTree(rootEntry, areaClone, _to, _contentClones, _oldToNewContentKeys);
            }
        }

        _log.info("Finished cloning areas, struct entries and contents");

    }

    private WGDocument createDumpClone(WGDocument doc, WGDocument ref) throws WGAPIException {
       WGDocument clone = doc.createClone(_to, ref);
       clone.saveWithGivenTimestamps(doc.getCreated(), doc.getLastModified());
       return clone;
        
    }

    private void handleException(String msg, Exception e, WGDocument doc) {
        
        if (doc != null) {
            _log.error(msg + ". Current document: " + doc.getDocumentKey(), e);
        }
        else {
            _log.error(msg, e);
        }
        
        throw new RuntimeException("Transport failed bc. of exception. Current document: " + doc.getDocumentKey(), e);
    }

    private void deleteOldContent() throws WGAPIException {

        Iterator areas = _to.getAreas().iterator();
        WGArea area;

        if (areas.hasNext()) {

            _log.info("Deleting old areas...");
            while (areas.hasNext()) {
                area = (WGArea) areas.next();
                _log.info("Deleting old area '" + area.getName() + "'");

                Iterator roots = area.getRootEntries().iterator();
                while (roots.hasNext()) {
                    deleteOldStruct((WGStructEntry) roots.next());
                }

                if (!area.remove()) {
                    _log.error("Cannot delete area. See application log for error details");
                    throw new WGBackendException("Canceled transport because of failed deletion: " + area.getDocumentKey());
                }

                incTargetDocCounter();
            }

            _log.info("Finished deleting old areas");

        }
    }

    private void deleteOldStruct(WGStructEntry entry) throws WGAPIException {

        if (entry.isDeleted()) {
            return;
        }

        // Recursely delete child entries
        Iterator children = entry.getChildEntries().iterator();
        while (children.hasNext()) {
            deleteOldStruct((WGStructEntry) children.next());
        }

        // Delete contents
        Iterator contents = entry.getAllContent(true).iterator();
        while (contents.hasNext()) {
            
            WGContent content = (WGContent) contents.next();
            _log.info("Deleting old content '" + content.getContentKey().toString() + "'");
            if (!content.remove()) {
                _log.error("Cannot delete content. See application log for error details");
                throw new WGBackendException("Canceled transport because of failed deletion: " + content.getDocumentKey());
            }


            incTargetDocCounter();
        }

        // Finally delete the struct
        _log.info("Deleting old struct entry '" + String.valueOf(entry.getStructKey()) + "'");
        if (!entry.remove()) {
            _log.error("Cannot delete struct entry. See application log for error details");
            throw new WGBackendException("Canceled transport because of failed deletion: " + entry.getDocumentKey());
        }

        incTargetDocCounter();

    }

    private void cloneLanguages() throws WGAPIException {

        if (_to.getDesignProvider() != null && _to.getDesignProvider().providesType(WGDocument.TYPE_LANGUAGE)) {
            _log.info("Omitted cloning of languages to target database, bc. of design provider");
            return;
        }
        
        _log.info("Cloning languages...");
        Iterator langs = _from.getLanguages().values().iterator();
        WGLanguage lang;
        WGLanguage langClone = null;

        while (langs.hasNext()) {
            lang = (WGLanguage) langs.next();
            validate(lang);
            _log.info("Cloning language '" + lang.getName() + "'");
            try {
                langClone = (WGLanguage) createDumpClone(lang, null);
            }
            catch (WGAPIException e) {
                handleException("Error cloning language", e, langClone);
            }

            if (langClone == null) {
                handleSaveError("Unable to clone language.", lang);
                return;
            }
            
            langClone.dropCore();
            lang.dropCore();

            incSourceDocCounter();
            incTargetDocCounter();
        }

        _log.info("Finished cloning languages");

    }

    private void deleteOldLanguages() throws WGAPIException {
        
        if (_to.getDesignProvider() != null && _to.getDesignProvider().providesType(WGDocument.TYPE_LANGUAGE)) {
            _log.info("Omitted deletion of old languages in target database, bc. of design provider");
            return;
        }
        
        Iterator langs = _to.getLanguages().values().iterator();
        WGLanguage lang;

        if (langs.hasNext()) {
            _log.info("Deleting old languages...");
            while (langs.hasNext()) {
                lang = (WGLanguage) langs.next();
                _log.info("Deleting old language '" + lang.getName() + "'");

                if (!lang.remove()) {
                    _log.error("Cannot delete language. See application log for error details");
                    throw new WGBackendException("Canceled transport because of failed deletion: " + lang.getDocumentKey());
                }

                incTargetDocCounter();
            }

            _log.info("Finished deleting old languages");
        }
    }

    private void cloneContentTypes() throws WGAPIException {
        
        if (_to.getDesignProvider() != null && _to.getDesignProvider().providesType(WGDocument.TYPE_CONTENTTYPE)) {
            _log.info("Omitted cloning of content types to target database, bc. of design provider");
            return;
        }
        
        _log.info("Cloning content types...");
        Iterator cts = _from.getContentTypes().iterator();
        WGContentType ct;
        WGContentType ctClone = null;

        while (cts.hasNext()) {
            ct = (WGContentType) cts.next();
            validate(ct);
            _log.info("Cloning content type '" + ct.getName() + "'");
            try {
                ctClone = (WGContentType) createDumpClone(ct, null);
            }
            catch (WGAPIException e) {
                handleException("Error cloning content type", e, ct);
            }

            if (ctClone == null) {
                handleSaveError("Unable to clone content type.", ct);
                continue;
            }
            
            ctClone.dropCore();
            ct.dropCore();

            incSourceDocCounter();
            incTargetDocCounter();
        }

        _log.info("Finished cloning content types");
    }

    private void deleteOldContentTypes() throws WGAPIException {
        
        if (_to.getDesignProvider() != null && _to.getDesignProvider().providesType(WGDocument.TYPE_CONTENTTYPE)) {
            _log.info("Omitted deletion of old content types in target database, bc. of design provider");
            return;
        }
        
        Iterator cts = _to.getContentTypes().iterator();
        WGContentType ct;
        if (cts.hasNext()) {

            _log.info("Deleting old content types...");
            while (cts.hasNext()) {
                ct = (WGContentType) cts.next();
                _log.info("Deleting old content type '" + ct.getName() + "'");

                if (!ct.remove()) {
                    _log.error("Cannot delete content type. See application log for error details");
                    throw new WGBackendException("Canceled transport because of failed deletion: " + ct.getDocumentKey());
                }

                incTargetDocCounter();
            }

            _log.info("Finished deleting old content types");

        }
    }

    private void cloneStructEntryTree(WGStructEntry rootEntry, WGDocument cloneParent, WGDatabase to, List contentClones, Map oldToNewContentKeys)
            throws WGAPIException {

        // Clone entry
        WGStructEntry entryClone = null;
        validate(rootEntry);
        _log.info("Cloning struct entry '" + rootEntry.getStructKey() + "'");
        try {
            entryClone = (WGStructEntry) createDumpClone(rootEntry, cloneParent);
        }
        catch (WGAPIException e) {
           handleException("Error cloning struct entry", e, rootEntry);
            return;
        }

        if (entryClone == null) {
            handleSaveError("Unable to clone struct entry.", rootEntry);
            return;
        }
        
        rootEntry.dropCore();
        entryClone.dropCore();

        incSourceDocCounter();
        incTargetDocCounter();

        // Clone contents of entry
        Iterator contents = rootEntry.getAllContent(_includeArchivedContents).iterator();
        WGContent content;
        WGContent contentClone;
        while (contents.hasNext()) {
            content = (WGContent) contents.next();
            validate(content);
            _log.info("Cloning content '" + content.getContentKey() + "'");
            try {
                contentClone = (WGContent) createDumpClone(content, entryClone);
            }
            catch (WGAPIException e1) {
                handleException("Error cloning content", e1, content);
                return;
            }
            if (contentClone == null) {
                handleSaveError("Unable to clone content.", content);
                continue;
            }
            
            contentClone.dropCore();
            content.dropCore();

            incSourceDocCounter();
            incTargetDocCounter();

            oldToNewContentKeys.put(content.getContentKey(true).toString(), contentClone.getContentKey().toString());
            if (contentClone.getStatus().equals(WGContent.STATUS_RELEASE)) {
                oldToNewContentKeys.put(content.getContentKey(false).toString(), contentClone.getContentKey(false).toString());
                oldToNewContentKeys.put(content.getStructKey() + "." + content.getLanguage().getName() + ".p", contentClone.getContentKey(false).toString());
            }

            contentClones.add(contentClone);

            entryClone.dropCore();
            rootEntry.dropCore();

        }

        // Proceed with child entries the same way
        Iterator childEntries = rootEntry.getChildEntries().iterator();
        while (childEntries.hasNext()) {
            cloneStructEntryTree((WGStructEntry) childEntries.next(), entryClone, to, contentClones, oldToNewContentKeys);
        }

    }

    private boolean transport(boolean isImport, boolean includeACL, boolean includeSystemAreas, boolean includeArchived) throws WGAPIException {
        
        _import = isImport;
        _includeACL = includeACL;
        _includeSystemAreas = includeSystemAreas;
        _includeArchivedContents = includeArchived;
        
        boolean fromCaching = _from.getSessionContext().isCachingEnabled();
        boolean toCaching = _to.getSessionContext().isCachingEnabled();
        boolean toEvents = _to.getSessionContext().isEventsEnabled();
        boolean toTestUN = _to.getSessionContext().isTestUniqueNames();

        setFromSessionProps();
        setToSessionProps();

        try {
            return innerTransport();
        }
        finally {
            _to.removeAttribute(WGDatabase.ATTRIB_UPDATED);
            _to.getSessionContext().setEventsEnabled(toEvents);
            _from.getSessionContext().setCachingEnabled(fromCaching);
            _to.getSessionContext().setCachingEnabled(toCaching);
            _to.getSessionContext().setTestUniqueNames(toTestUN);
            _to.refresh();
        }
        
    }
    
    /**
     * Imports an OpenWGA content store dump into a content store. Database "from" must be the dump database and "to" the content store.
     * @return true if the dump import succeeeded, false if it was canceled.
     * @throws WGAPIException
     */
    public boolean importDump() throws WGAPIException {
        return importDump(false);
    }

    
    /**
     * Imports an OpenWGA content store dump into a content store. Database "from" must be the dump database and "to" the content store.
     * @param includeACL should the ACL of the source be imported or not
     * @param includeSystemAreas should system areas be imported or not
     * @return true if the dump import succeeeded, false if it was canceled.
     * @throws WGAPIException
     */
    public boolean importDump(boolean includeACL, boolean includeSystemAreas) throws WGAPIException {
        return transport(true, includeACL, includeSystemAreas, true);
    }
    
    /**
     * Imports an OpenWGA content store dump into a content store. Database "from" must be the dump database and "to" the content store.
     * @param includeACL should the ACL of the source be imported or not
     * @return true if the dump import succeeeded, false if it was canceled.
     * @throws WGAPIException
     */
    public boolean importDump(boolean includeACL) throws WGAPIException {
        return importDump(includeACL, false);
    }
    
    /**
     * Exports an OpenWGA content store dump from a content store. Database "from" must be the content store and "to" the soon-to-be dump database.
     * @return true if the dump succeeeded, false if it was canceled
     * @throws WGAPIException
     */
    public boolean exportDump() throws WGAPIException {
        return exportDump(false);
    }
    
    /**
     * Exports an OpenWGA content store dump from a content store. Database "from" must be the content store and "to" the soon-to-be dump database.
     * @param includeACL should the ACL of the source be included in the dump or not
     * @return true if the dump succeeeded, false if it was canceled
     * @throws WGAPIException
     */
    public boolean exportDump(boolean includeACL) throws WGAPIException {
        return exportDump(includeACL, false, true);
    }
    
    /**
     * Exports an OpenWGA content store dump from a content store. Database "from" must be the content store and "to" the soon-to-be dump database.
     * @param includeACL should the ACL of the source be included in the dump or not
     * @param includeSystemAreas should system areas be included in the dump or not
     * @param includeArchived
     * @return true if the dump succeeeded, false if it was canceled
     * @throws WGAPIException
     */
    public boolean exportDump(boolean includeACL, boolean includeSystemAreas, boolean includeArchived) throws WGAPIException {
        return transport(false, includeACL, includeSystemAreas, includeArchived);
    }

    private void setToSessionProps() {
        _to.getSessionContext().setEventsEnabled(true);
        _to.getSessionContext().setContentTypeEventsEnabled(false);
        _to.getSessionContext().setTestUniqueNames(false);
        _to.getSessionContext().setBatchProcess(true);
        _to.getSessionContext().setCachingEnabled(true);
        _to.getSessionContext().setProtectedRelationsEnabled(false);
        _to.setAttribute(WGDatabase.ATTRIB_UPDATED, new Boolean(true));
    }


    private void setFromSessionProps() {
        _from.getSessionContext().setBatchProcess(true);
        _from.getSessionContext().setCachingEnabled(true);
    }

    /**
     * Returns after how much processed documents the database caches are cleared to free memory.
     * The default is 100.
     */
    public int getClearCacheEvery() {
        return _clearCacheEvery;
    }

    /**
     * Sets after how much processed documents the database caches are cleared to free memory.
     * @param i
     */
    public void setClearCacheEvery(int i) {
        _clearCacheEvery = i;
    }
    
    private void handleSaveError(String mainMessage, WGDocument doc) {
        _log.error(mainMessage + ". Causing document: " + doc.getDocumentKey() + ".See application log for details;");
        throw new RuntimeException("Transport failed because of unsaveable data: " + doc.getDocumentKey());
    }
    
    private void validate(WGDocument doc) throws WGAPIException {
        
        if (_import) {
            if (doc.validateMetas(true)) {
                doc.save();
            }
        }
        
    }

    protected boolean isImport() {
        return _import;
    }

    protected void setImport(boolean import1) {
        _import = import1;
    }

    private void deleteOldACL() throws WGAPIException {
        _log.info("Deleting old acl entries...");
        
        Iterator aclEntries = _to.getACL().getAllEntries().iterator();

        while (aclEntries.hasNext()) {
            WGACLEntry entry = (WGACLEntry) aclEntries.next();
            
            _log.info("Deleting old aclentry '" + entry.getName() + "'");
            try {
                _to.getACL().remove(entry);                
            }
            catch (WGAPIException e) {
                _log.error("Cannot delete aclentry. See application log for error details");
                throw new WGBackendException("Canceled because of failed deletion: '" + entry.getName() + "' (ACL-Entry)");
            }

        }
        _log.info("Finished deleting old acl entries");
   }

    private void cloneACL() throws WGAPIException {

        _log.info("Cloning acl...");
        
        Iterator aclEntries = _from.getACL().getAllEntries().iterator();

        while (aclEntries.hasNext()) {
            WGACLEntry entry = (WGACLEntry) aclEntries.next();
            
            _log.info("Cloning aclentry '" + entry.getName() + "'");
            try {
                WGACLEntry clone = _to.getACL().createEntry(entry.getName(), entry.getType(), entry.getLevel());
                clone.setFlags(entry.getFlags());
                _to.getACL().save(clone);
            }
            catch (WGAPIException e) {
                handleException("Error cloning aclentry", e, null);
            }

        }
        
        incSourceDocCounter();
        incTargetDocCounter();
        
        _log.info("Finished cloning acl");

    }
    
}
