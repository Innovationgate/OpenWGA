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

package de.innovationgate.wgpublisher.files.derivates;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.activation.DataSource;

import org.apache.log4j.Logger;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileAnnotator;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.wga.config.FileDerivateManagerConfiguration;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.types.DerivateQueryTermProcessorModuleType;
import de.innovationgate.wga.modules.types.FileDerivateCreatorModuleType;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.ClientHints;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;

public class FileDerivateManager {
    
    public static final Logger LOG = Logger.getLogger("wga.filederivates");
    protected static final String CF_DERIVATERUN = "derivateupdaterun";
    private static Pattern DERIVATE_QUERY_TERM_PATTERN = Pattern.compile("([\\w-]+)(?:([=<>~]+)(.*))?");
    
    public static final String DERIVATEMODE_OFF = "off";
    public static final String DERIVATEMODE_SPECIAL_CREATORS = "specialCreators";
    public static final String DERIVATEMODE_ALL_CREATORS = "allCreators";
    
    public static class DerivateRunProblemOccasion implements ProblemOccasion {

        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return FileDerivateManager.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
        
        @Override
        public boolean equals(Object obj) {
            return obj instanceof DerivateRunProblemOccasion;
        }
        
        @Override
        public int hashCode() {
            return getClass().hashCode();
        }
        
    }
    
    public static class DerivateQueryResult implements Comparable<DerivateQueryResult> {
        
        private float _score;
        private WGFileAnnotations _file;

        public DerivateQueryResult(float score, WGFileAnnotations file) {
            super();
            _score = score;
            _file = file;
        }

        @Override
        public int compareTo(DerivateQueryResult o) {
            try {
            	int comparision = new Float(o.getScore()).compareTo(_score);
                if (comparision != 0) {
                    return comparision;
                }
                // if scores are equal compare file size
                return new Long(o.getFile().getSize()).compareTo(_file.getSize());
            }
            catch (WGAPIException e) {
                throw new IllegalStateException("Exception comparing derivate query results", e);
            }
        }

        public float getScore() {
            return _score;
        }

        public WGFileAnnotations getFile() {
            return _file;
        }
        
    }
    
    public static class DatabaseStatus {
        public Object updateMonitor = new Object();
        public WGDatabaseRevision lastUpdateRevision = null;
    }
        
    public static class DerivateQuery extends HashMap<String,DerivateQueryTerm> {
        
        private boolean _clearPredefinedQuery = false;
        private boolean _noDerivate = false;
        private Float _devicePixelRatioOverride = null;
        private static final long serialVersionUID = 1L;
        public static final String QUERYTERM_CLEAR = "clear";
        public static final String QUERYTERM_DEVICEPIXELRATIO = "dpr";
        public static final String QUERYTERM_NONE = "none";
        public static final String QUERYTERM_USAGE = "usage";
        public static final String QUERYTERM_TYPE = "type";
        public static final String QUERYTERM_HEIGHT = "height";
        public static final String QUERYTERM_WIDTH = "width";
        public static final String QUERYTERM_ID = "id";

        public DerivateQuery(DerivateQuery queryTerms) {
            super(queryTerms);
        }

        public DerivateQuery() {
        }

        @Override
        public String toString() {
            List<String> termStrings = new ArrayList<String>();

            if (_noDerivate) {
                termStrings.add(QUERYTERM_NONE);
            }
            
            if (_devicePixelRatioOverride != null) {
                termStrings.add(QUERYTERM_DEVICEPIXELRATIO + "=" + String.valueOf(_devicePixelRatioOverride));
            }
            
            for (DerivateQueryTerm term : values()) {
                termStrings.add(term.toString());
            }
            return WGUtils.serializeCollection(termStrings, ",");
        }

        public boolean isClearPredefinedQuery() {
            return _clearPredefinedQuery;
        }

        public boolean isNoDerivate() {
            return _noDerivate;
        }

        protected boolean handleSpecialTerm(DerivateQueryTerm term) {
         
            if (term.getName().equals(QUERYTERM_CLEAR)) {
                _clearPredefinedQuery = true;
                return true;
            }
            else if (term.getName().equals(QUERYTERM_NONE)) {
                _noDerivate = true;
                return true;
            }
            else if (term.getName().equals(QUERYTERM_DEVICEPIXELRATIO)) {
                _devicePixelRatioOverride = Float.parseFloat(term.getValue());
                return true;
            }
            
            return false;
            
        }

        public void setNoDerivate(boolean noDerivate) {
            _noDerivate = noDerivate;
        }

        public Float getDevicePixelRatioOverride() {
            return _devicePixelRatioOverride;
        }
        
        public void addTerm(String name, String operator, String value) {
            put(name, new DerivateQueryTerm(name, operator, value));
        }
        
    }
    
    public static class DerivateQueryTerm {
        
        private String _name;
        private String _operator;
        private String _value;

        public DerivateQueryTerm(String term) throws WGInvalidDerivateQueryException {
            Matcher matcher = DERIVATE_QUERY_TERM_PATTERN.matcher(term);
            if (!matcher.matches()) {
                throw new WGInvalidDerivateQueryException("Invalid derivate query term: " + term);
            }
            _name = matcher.group(1);
            _operator = matcher.group(2);
            _value = matcher.group(3);
        }
        
        public DerivateQueryTerm(String name, String operator, String value) {
            _name = name;
            _operator = operator;
            _value = value;
        }

        public String getName() {
            return _name;
        }

        public String getOperator() {
            return _operator;
        }

        public String getValue() {
            return _value;
        }
        
        @Override
        public String toString() {
            return _name + _operator + _value;
        }
        
    }
    
    public class CreatorSelector {
        
        private List<FileDerivateCreator> _enabledCreators = new ArrayList<FileDerivateCreator>();
        private List<String> _forceUpdateCreators = new ArrayList<String>();
        private List<String> _creatorsToRemove = new ArrayList<String>();
        
        public void addEnabledCreator(FileDerivateCreator creator) {
            _enabledCreators.add(creator);
        }
        
        public void addForceUpdateCreator(String className) {
            _forceUpdateCreators.add(className);
        }

        public void addCreatorToRemove(String className) {
            _creatorsToRemove.add(className);
        }

        
        public boolean isForceUpdateForCreator(FileDerivateCreator creator) {
            return _forceUpdateCreators.contains(creator.getClass().getName());
        }
        
        public List<FileDerivateCreator> getEnabledCreators() {
            return _enabledCreators;
        }

        public List<String> getCreatorsToRemove() {
           return _creatorsToRemove; 
        }
        
        public void forceUpdateOnAllCreators() {
            for (FileDerivateCreator creator : _enabledCreators) {
                _forceUpdateCreators.add(creator.getClass().getName());
            }
        }
        
        public List<String> getEnabledCreatorNames() {
            List<String> names = new ArrayList<String>();
            for (FileDerivateCreator creator : _enabledCreators) {
                names.add(creator.getClass().getName());
            }
            Collections.sort(names);
            return names;
        }
        
        public boolean checkForCreatorSetChange(List<String> lastCreators) {
            List<String> currentCreators = getEnabledCreatorNames();
            if (!currentCreators.equals(lastCreators)) {
                for (String currentCreator : currentCreators) {
                    if (!lastCreators.contains(currentCreator)) {
                        _forceUpdateCreators.add(currentCreator);
                    }
                }
                _creatorsToRemove.addAll(lastCreators);
                _creatorsToRemove.removeAll(currentCreators);
                return true;
            }
            else {
                return false;
            }
            
        }

    }
    
    public class CurrentRun implements WGFileAnnotator {

        private WGDatabaseRevision _currentRevision;
        private boolean _somethingDone = false;
        private int _derivatesCreated = 0;
        private int _derivatesDeleted = 0;
        
        
        protected WGDatabaseRevision getCurrentRevision() {
            return _currentRevision;
        }

        protected String getCurrentRevisionStr() {
            return _currentRevisionStr;
        }

        private String _currentRevisionStr;

        public CurrentRun(WGDatabaseRevision databaseRevision) throws IOException {
            _currentRevision = databaseRevision;
            _currentRevisionStr = databaseRevision.serialize();
        }
        
        @Override
        public void annotateFile(DataSource originalfileData, WGFileAnnotations fileAnnotations) throws WGAPIException {
            fileAnnotations.setCustomFieldValue(CF_DERIVATERUN, _currentRevisionStr);
        }
        
        public void addCreatedDerivate() {
            _derivatesCreated++;
            _somethingDone=true;
        }
        
        public void addDeletedDerivate() {
            _derivatesDeleted++;
            _somethingDone=true;
        }

        protected boolean isSomethingDone() {
            return _somethingDone;
        }

        protected void setSomethingDone(boolean somethingDone) {
            _somethingDone = somethingDone;
        }

        @Override
        public int getOrderNr() {
            return 100;
        }

        protected int getDerivatesCreated() {
            return _derivatesCreated;
        }

        protected int getDerivatesDeleted() {
            return _derivatesDeleted;
        }
        
    };
    
    public class DerivateUpdateProcess extends TimerTask {

        private WGA _wga;
        private boolean _running;
        private String _status;
 

        @Override
        public synchronized void run() {
            try {
                Thread.currentThread().setName("WGA File Derivate Update Process");
                _wga = WGA.get(_core);
                updateDerivates();
            }
            catch (Throwable t) {
                LOG.error("Exception running derivate update process", t);
            }
        }

        public void updateDerivates() throws Exception {
            
            try {
                _running = true;
                ProblemOccasion occ = new DerivateRunProblemOccasion();
                _wga.server().getProblemRegistry().clearProblemOccasion(occ);
                _status = "Starting derivate update";
                
                for (WGDatabase db : _core.getContentdbs().values()) {
                                    	
                    DatabaseStatus dbStatus = getDatabaseStatus(db);
                    synchronized (dbStatus.updateMonitor) {
                    
                        try {
                            if (!db.isConnected() || !db.isReady()) {
                                continue;
                            }
                            if (!db.hasFeature(WGDatabase.FEATURE_CONTENT_FILE_DERIVATES)) {
                                continue;
                            }
                            String derivatesMode = (String) db.getAttribute(WGACore.DBATTRIB_FILE_DERIVATES_ENABLED);
                            if (DERIVATEMODE_OFF.equals(derivatesMode)) {
                                continue;
                            }
                            
                            db.openSession();
                            Boolean sync = (Boolean)db.getExtensionData(WGDatabase.EXTDATA_CS_SYNC_RUNNING); 
                        	if(sync!=null && sync==true){
                        		continue;
                        	}

                        	App app = _wga.app(db);
                            
                            WGDatabaseRevision currentRevision = WGDatabaseRevision.forValue(db.getBackendRevision());
                            CurrentRun currentRun = new CurrentRun(currentRevision);
                            
                            // Read data of last run from database ext data, further configuration
                            WGDatabaseRevision lastRevision = dbStatus.lastUpdateRevision;
                            if (lastRevision == null) {
                                String lastRevisionStr = (String) db.getExtensionData(WGDatabase.EXTDATA_DERIVATE_REVISION);
                                if (lastRevisionStr != null) {
                                   lastRevision = WGDatabaseRevision.deserialize(lastRevisionStr);
                                }
                            }
                            
                            @SuppressWarnings("unchecked")
                            List<String> lastCreators = (List<String>) db.getExtensionData(WGDatabase.EXTDATA_DERIVATE_CREATORS);
                            if (lastCreators == null) {
                                lastCreators = new ArrayList<String>();
                            }
                            
                            List<String> enabledCreators = null;
                            if (DERIVATEMODE_SPECIAL_CREATORS.equals(derivatesMode)) {
                                enabledCreators = (List<String>) app.getPublisherOption(WGACore.DBATTRIB_FILE_DERIVATES_CREATORS);
                            }
                            
                            // Compose creator selector
                            
                            CreatorSelector creatorSelector = new CreatorSelector();
                            for (ModuleDefinition creatorDefinition : _core.getModuleRegistry().getModulesForType(FileDerivateCreatorModuleType.class).values()) {
                                if (enabledCreators == null || enabledCreators.contains(creatorDefinition.getImplementationClass().getName())) {
                                    try {
                                        creatorDefinition.testDependencies();
                                        FileDerivateCreator creator = (FileDerivateCreator) _core.getModuleRegistry().instantiate(creatorDefinition);
                                        creatorSelector.addEnabledCreator(creator);
                                    }
                                    catch (ModuleDependencyException e) {
                                        _wga.server().getProblemRegistry().addProblem(Problem.create(occ, "creatorproblem.missing_dependencies", ProblemSeverity.LOW, Problem.var("module", creatorDefinition.getImplementationClass().getName()), e));
                                    }
                                    catch (Throwable e) {
                                        _wga.server().getProblemRegistry().addProblem(Problem.create(occ, "creatorproblem.exception", ProblemSeverity.LOW, Problem.var("module", creatorDefinition.getImplementationClass().getName()), e));
                                    }
                                }
                            }
                            
                            // Determine update mode

                            // No revision of last run: Complete re-init
                            if (lastRevision == null) {
                            	LOG.info("performInitialDerivateCreation bc. no lastRevision");
                                performInitialDerivateCreation(db, currentRun, creatorSelector);
                                currentRun.setSomethingDone(true);
                            }
                            
                            // Creator set changed: Perform reinit with the set of changed creators
                            else if (creatorSelector.checkForCreatorSetChange(lastCreators)) {
                            	LOG.info("performInitialDerivateCreation (reinit) bc. CreatorSet Changed");
                            	LOG.info("lastCreators: " + lastCreators.toString());
                            	LOG.info("EnabledCreatorNames: " + creatorSelector.getEnabledCreatorNames());
                                performInitialDerivateCreation(db, currentRun, creatorSelector);
                                currentRun.setSomethingDone(true);
                            }
                            
                            // Incremental update if revision changed
                            else if (currentRevision.compareTo(lastRevision) > 0) {
                                performIncrementalDerivateUpdate(db, currentRun, lastRevision, creatorSelector);
                            }
                            
                            
                            // Write run data if something has actually been stored
                            if (currentRun.isSomethingDone()) {
                                if (currentRun.getDerivatesCreated() > 0 || currentRun.getDerivatesDeleted() > 0) {
                                    LOG.info("Created " + WGUtils.DECIMALFORMAT_STANDARD.format(currentRun.getDerivatesCreated()) + " and deleted " + WGUtils.DECIMALFORMAT_STANDARD.format(currentRun.getDerivatesDeleted()) + " content file derivates for app '" + db.getDbReference() + "'");
                                }
                                writeLastRunData(db, currentRevision, creatorSelector.getEnabledCreatorNames());
                            }
                            dbStatus.lastUpdateRevision = currentRevision;
                            db.closeSession();
                            
                        }
                        catch (Throwable t) {
                            LOG.error("Exception creating file derivates for database '" + db.getDbReference() + "'", t);
                            db.closeSession();
                        }
                        
                    }
                    
                }
            }
            finally {
                WGFactory.getInstance().closeSessions();
                _running = false;
            }
            
            
            
            
        }

        protected void writeLastRunData(WGDatabase db, WGDatabaseRevision currentRevision, List<String> currentCreators) throws WGAPIException, IOException {
            db.writeExtensionData(WGDatabase.EXTDATA_DERIVATE_REVISION, currentRevision.serialize());
            db.writeExtensionData(WGDatabase.EXTDATA_DERIVATE_CREATORS, currentCreators);
        }

        private void performIncrementalDerivateUpdate(WGDatabase db, CurrentRun currentRun, WGDatabaseRevision lastRevision, CreatorSelector selector) throws WGException, IOException, NoSuchAlgorithmException {
            
            int count = 0;
            List<WGUpdateLog> updates = db.getUpdatedDocumentsSince(lastRevision);
            for(WGUpdateLog update : updates) {
                if (update.getType() == WGUpdateLog.TYPE_UPDATE) {
                    
                    _status = "Updating derivates for app '" + db.getDbReference() + "' (" + WGUtils.DECIMALFORMAT_STANDARD.format(count+1) + " of " + WGUtils.DECIMALFORMAT_STANDARD.format(updates.size()) + " documents)";

                    count++;
                    if (count % 100 == 0) {
                        currentRun.setSomethingDone(true); // If we have that many revisions worked through we always want to persist the state
                        db.getSessionContext().clearCache();
                    }
                    
                    try{
	                    WGDocumentKey docKey = new WGDocumentKey(update.getDocumentKey());
	                    if (!docKey.isRealDocumentKey()) {
	                        continue;
	                    }
	                    
	                    // Always store revision if actual documents were processed
	                    currentRun.setSomethingDone(true);
	                    
	                    if (docKey.getDocType() != WGDocument.TYPE_CONTENT) {
	                        continue;
	                    }

	                    WGContent content = (WGContent) db.getDocumentByKey(update.getDocumentKey());
	                    if (content != null) {                        
	                        performDerivateUpdate(content, currentRun, selector);
	                    }
                    }
                    catch(Exception e){
                    	LOG.error("Unable to perform derivate update", e);
                    }
                }
            }
            
        }

        private boolean performInitialDerivateCreation(WGDatabase db, CurrentRun currentRun, CreatorSelector selector) throws WGException, NoSuchAlgorithmException, UnsupportedEncodingException {

            
            boolean somethingDone = false;
            Iterator<WGContent> contents = db.getAllContent(false);
            int count = 0;
            int contentCount = db.getAllContentKeys(false).size();
            while (contents.hasNext()) {
                WGContent content = contents.next();
                
                _status = "Initial derivate creation for app '" + db.getDbReference() + "' (" + WGUtils.DECIMALFORMAT_STANDARD.format(count) + " of approx. " + WGUtils.DECIMALFORMAT_STANDARD.format(contentCount) + " documents)";
                
                performDerivateUpdate(content, currentRun, selector);
                count++;
                if (count % 100 == 0) {
                    db.getSessionContext().clearCache();
                }
                
            }
            return somethingDone;
            
        }

        private void performDerivateUpdate(WGContent content, CurrentRun currentRun, CreatorSelector selector) throws WGException, NoSuchAlgorithmException, UnsupportedEncodingException {

            for (String fileName : content.getFileNames()) {
                WGFileMetaData md = content.getFileMetaData(fileName);
                if (md.getMimeType() == null || md.getSha512Checksum() == null) { // Derivates only supported for files with determined mime type, using distinct file contents for storage
                    continue;
                }
                
                List<WGFileDerivateMetaData> existingDerivates = content.getFileDerivates(fileName);
                
                // Iterate over creators
                for (FileDerivateCreator creator : selector.getEnabledCreators()) {
                    performDerivateUpdateForCreator(content, md, creator, currentRun, selector, existingDerivates);
                }
                
                // See if we need to remove derivates
                List<String> creatorsToRemove = selector.getCreatorsToRemove();
                if (creatorsToRemove != null) {
                    for (String creatorToRemove : creatorsToRemove) {
                        String creatorMd5 = WGUtils.createMD5HEX(creatorToRemove.getBytes("UTF-8"));
                        Map<String,WGFileDerivateMetaData> derivatesOfCreator = fetchDerivatesForCreator(existingDerivates, creatorMd5);
                        for (WGFileDerivateMetaData derivateToRemove : derivatesOfCreator.values()) {
                            content.removeFileDerivate(derivateToRemove.getId());
                            currentRun.addDeletedDerivate();
                        }
                    }
                }
            }
            
        }

        private void performDerivateUpdateForCreator(WGContent content, final WGFileMetaData md, FileDerivateCreator creator, CurrentRun currentRun, CreatorSelector selector,
                List<WGFileDerivateMetaData> existingDerivates) throws NoSuchAlgorithmException, UnsupportedEncodingException, WGException {
            String creatorMd5 = WGUtils.createMD5HEX(creator.getClass().getName().getBytes("UTF-8"));

            WGDatabase db = content.getDatabase();
            Map<String, WGFileDerivateMetaData> derivatesFromCreator = fetchDerivatesForCreator(existingDerivates, creatorMd5);
            boolean forceCreation = false;
            if (selector != null) {
                forceCreation = selector.isForceUpdateForCreator(creator);
            }
            
            if (!forceCreation && db.getContentStorePatchLevel() >= 5) {
                // Check if the file was actually updated since the time the earliest derivates was created. If not we can skip everything here
                if (md.getUpdateRevision() == null) {
                    return;
                }
                
                WGDatabaseRevision earliestRevision = null;
                for (WGFileDerivateMetaData derivateMd : existingDerivates) { // Collect latest derivate revision
                    WGDatabaseRevision derivateRev = derivateMd.getOriginalRevision();
                    if (derivateRev == null) { // If the revision on one derivate is missing we need to do the update
                        earliestRevision = null;
                        break;
                    }
                    
                    if (earliestRevision == null || derivateRev.compareTo(earliestRevision) < 0) {
                        earliestRevision = derivateRev;
                    }
                }
                
                // If the earliest derivate was created for a revision equal or newer than the file update revision, then we can skip
                if (earliestRevision != null && earliestRevision.compareTo(md.getUpdateRevision()) >= 0) {
                    return;
                }
            }

            Set<DerivateInfo> derivateInfos = null;
            try {
                derivateInfos = creator.getDerivateInfos(_wga, content, md);
            }
            catch (Throwable e) {
                LOG.error("Exception retrieving derivate infos from creator '" + creator.getClass().getName() + "' for file '" + md.getName() + "' on document '" + content.getDocumentKey() + "' (" + content.getDatabase().getDbReference() + ")", e);
                currentRun.setSomethingDone(true); // To prevent from eternally running into this error (#00003674)
                return;
            }                
            
            if (derivateInfos != null) {
                for (DerivateInfo derivateInfo : derivateInfos) {
                    
                    // Check if the derivate exists/needs update
                    WGFileDerivateMetaData existingMd = derivatesFromCreator.remove(derivateInfo.getName());
                    if (existingMd != null && !forceCreation) {
                        WGDatabaseRevision derivateRev = existingMd.getOriginalRevision();
                        WGDatabaseRevision fileRev = md.getUpdateRevision();
                        if (derivateRev != null && derivateRev.compareTo(fileRev) >=0) {
                            continue;
                        }
                    }
                    
                    // Create the derivate and store it
                    try {
                        TemporaryFile  tempFile = new TemporaryFile("derivate.bin", null, WGFactory.getTempDir());
                        try {
                            OutputStream out = new BufferedOutputStream(new FileOutputStream(tempFile.getFile()));
                            LOG.debug(creator.getClass().getName() + ": creating " + derivateInfo.getUsage() + "/" + derivateInfo.getName() + " for " + md.getName() + " in " + md.getContext().getFileParent().getDocumentKey());
                            creator.createDerivate(_wga, content, md, derivateInfo, out);
                            out.flush();
                            out.close();
                            
                            List<WGFileAnnotator> annotators = new ArrayList<WGFileAnnotator>();
                            annotators.add(currentRun);
                            annotators.add(new WGFileAnnotator() {

                                @Override
                                public void annotateFile(DataSource originalfileData, WGFileAnnotations fileAnnotations) throws WGAPIException {
                                    if (md.getUpdateRevision() != null) {
                                        ((WGFileDerivateMetaData) fileAnnotations).setOriginalRevision(md.getUpdateRevision());
                                    }
                                }
                                
                                @Override
                                public int getOrderNr() {
                                    return 0;
                                }
                                
                            });
                            if (derivateInfo.getAdditionalAnnotators() != null) {
                                annotators.addAll(derivateInfo.getAdditionalAnnotators());
                            }
                            
                            if (existingMd != null) {
                                content.removeFileDerivate(existingMd.getId());
                            }
                            if(tempFile.getFile().length()>0) {
	                            WGFileDerivateMetaData derivateMd = content.createFileDerivate(md.getName(), creatorMd5, derivateInfo.getName(), tempFile.getFile(), derivateInfo.getUsage(), annotators);
	                            currentRun.addCreatedDerivate();
                            }
                        }
                        finally {
                            tempFile.delete();
                        }
                    }
                    catch (Throwable e) {
                        LOG.error("Exception creating derivate '" + derivateInfo.getName() + "' from creator '" + creator.getClass().getName() + "' for file '" + md.getName() + "' on document '" + content.getDocumentKey() + "' (" + content.getDatabase().getDbReference() + ")", e);
                    }
                    currentRun.setSomethingDone(true);
                }
            }
           
            // Remove all existing derivates that are no longer created by this creator
            for (WGFileDerivateMetaData derivateToDelete : derivatesFromCreator.values()) {
                content.removeFileDerivate(derivateToDelete.getId());
                currentRun.setSomethingDone(true);
            }

        }

        private Map<String, WGFileDerivateMetaData> fetchDerivatesForCreator(List<WGFileDerivateMetaData> existingDerivates, String creatorMd5) {
            Map<String,WGFileDerivateMetaData> derivatesFromCreator = new HashMap<String,WGFileDerivateMetaData>();
            for (WGFileDerivateMetaData derivateMd : existingDerivates) {
                if (derivateMd.getCreator().equals(creatorMd5)) {
                    derivatesFromCreator.put(derivateMd.getName(), derivateMd);
                }
            }
            return derivatesFromCreator;
        }

        public boolean isRunning() {
            return _running;
        }

        public String getStatus() {
            return _status;
        }

        
    }
    
    private WGACore _core;
    private Timer _timer = null;
    private FileDerivateManagerConfiguration _config;
    private DerivateUpdateProcess _updateProcess;
    private Map<String,DerivateQueryTermProcessor> _defaultQueryTermProcessors = new HashMap<String, DerivateQueryTermProcessor>();
    private Map<String,DatabaseStatus> _databaseStatus = Collections.synchronizedMap(new HashMap<String, DatabaseStatus>()); 

    public FileDerivateManager(WGACore core) {
        _core = core;
        _defaultQueryTermProcessors.put(DerivateQuery.QUERYTERM_ID, new IdDerivateQueryTermProcessor());
        _defaultQueryTermProcessors.put(DerivateQuery.QUERYTERM_WIDTH, new DisplaySizeQueryTermProcessor());
        _defaultQueryTermProcessors.put(DerivateQuery.QUERYTERM_HEIGHT, new DisplaySizeQueryTermProcessor());
        _defaultQueryTermProcessors.put(DerivateQuery.QUERYTERM_TYPE, new TypeQueryTermProcessor());
        _defaultQueryTermProcessors.put(DerivateQuery.QUERYTERM_USAGE, new UsageQueryTermProcessor());
    }
    
    public void init(WGAConfiguration config) {
        _config = config.getFileDerivateManagerConfiguration();
        if (_timer != null) {
            _timer.cancel();
            _timer = null;
            _updateProcess = null;
        }
        if (_config.getUpdateProcessEnabled().equals(FileDerivateManagerConfiguration.ENABLED) || (_config.getUpdateProcessEnabled().equals(FileDerivateManagerConfiguration.ONLY_MASTER_NODE) && _core.isRunSingleNodeFunctionalities())) {
            _timer = new Timer();
            _updateProcess = new DerivateUpdateProcess();
            _timer.schedule(_updateProcess, _config.getUpdateProcessInterval(), _config.getUpdateProcessInterval());
        }
    }

    public void stop() {
        if (_timer != null) {
            _timer.cancel();
        }
    }

    public WGFileAnnotations queryDerivate(WGDocument container, String fileName, DerivateQuery derivateQuery, ClientHints clientHints, boolean includeOriginal) throws WGException {
        if (!container.hasFile(fileName)) {
            return null;
        }
        return executeDerivateQuery(container, fileName, derivateQuery, clientHints, includeOriginal);
    }

    private WGFileAnnotations executeDerivateQuery(WGDocument container, String fileName, DerivateQuery queryTerms, ClientHints clientHints, boolean includeOriginal) throws WGAPIException, WGException {
        
        // Client hints override
        if (queryTerms.getDevicePixelRatioOverride() != null) {
            clientHints.setDevicePixelRatio(queryTerms._devicePixelRatioOverride);
        }
        
        // Collect all files that may be used as results
        List<DerivateQueryResult> results = new ArrayList<DerivateQueryResult>();
        List<WGFileAnnotations> fileAnnotations = new ArrayList<WGFileAnnotations>();
        fileAnnotations.addAll(container.getFileDerivates(fileName));
        
        // Determine possible results and their score
        for (WGFileAnnotations derivate : fileAnnotations) {
            nextAnnotation: {
                float scoreSum = 0;
                int scoreCount = 0;
                for (DerivateQueryTerm queryTerm : queryTerms.values()) {
                    float rating = testDerivateQueryTerm(derivate, queryTerm, clientHints);
                    if (rating  == 0) {
                        break nextAnnotation;
                    }
                    scoreSum+=rating;
                    scoreCount++;
                }
                float averageScore = scoreSum / scoreCount;
                results.add(new DerivateQueryResult(averageScore, derivate));
            }
        }
        
        if (results.size() > 0) {
        	// use best match (highest score / smallest file)   
            Collections.sort(results);
            return results.get(0).getFile();
        }
        else if (includeOriginal) {
        	WGFileAnnotations original = container.getFileMetaData(fileName);
            for (DerivateQueryTerm queryTerm : queryTerms.values()) {
                float rating = testDerivateQueryTerm(original, queryTerm, clientHints);
                if (rating == 0) {
                	return null;	// original doesn't match query
                }
            }
            return original;
        }
        
        return null;
    
    }

    
    public static DerivateQuery parseDerivateQuery(String derivateQuery) throws WGInvalidDerivateQueryException {
    	return parseDerivateQuery(derivateQuery, true);
    }
    public static DerivateQuery parseDerivateQuery(String derivateQuery, boolean add_usage) throws WGInvalidDerivateQueryException {
        DerivateQuery queryTerms = new DerivateQuery();
        for (String queryTermStr : WGUtils.deserializeCollection(derivateQuery, ",", true)) {
        	if(queryTermStr.isEmpty())
        		continue;
            DerivateQueryTerm queryTerm = new DerivateQueryTerm(queryTermStr);
            
            // Handle special terms that go into special properties of the derivate query
            if (queryTerms.handleSpecialTerm(queryTerm)) {
                continue;
            }
            
            queryTerms.put(queryTerm.getName(), queryTerm);
        }
        
        // Defaulting "usage" if not present and also ne special derivate queried via ID (#00003667)
        if (add_usage && !queryTerms.containsKey(DerivateQuery.QUERYTERM_USAGE) && !queryTerms.containsKey(DerivateQuery.QUERYTERM_ID)) {
            queryTerms.put("usage", new DerivateQueryTerm("usage=" + WGFileAnnotations.USAGE_POSTER));
        }
        
        return queryTerms;
    }
    
    private float testDerivateQueryTerm(WGFileAnnotations fileData, DerivateQueryTerm queryTerm, ClientHints clientHints) throws WGException {
        
        try {
            // Default term
            DerivateQueryTermProcessor processor = _defaultQueryTermProcessors.get(queryTerm.getName());
            
            // Registered term processor
            if (processor == null) {
                ModuleDefinition processorDef = _core.getModuleRegistry().getModuleDefinitionByKey(DerivateQueryTermProcessorModuleType.class, queryTerm.getName());
                if (processorDef != null) {
                    processor = (DerivateQueryTermProcessor) _core.getModuleRegistry().instantiate(processorDef);
                }
            }
            
            // Fallback: custom field query term
            if (processor == null) {
                processor = new CustomFieldQueryTermProcessor();
            }
            
            return processor.matchTermToFileAnnotations(fileData, queryTerm, clientHints);
        }
        catch (ModuleInstantiationException e) {
            throw new WGException("Exception testing query term: " + queryTerm.toString(), e);
        }
        
        
    }

    public void updateAllDerivates(WGDatabase db) throws WGAPIException {
        if (_updateProcess == null) {
            throw new IllegalStateException("This operation must be performed from the server on which the derivate update process runs");
        }
        resetDerivateStatus(db);
    }
    
    public void updateDerivatesOfCreators(WGDatabase db, final List<String> creators) throws Exception {
        if (_updateProcess == null) {
            throw new IllegalStateException("This operation must be performed from the server on which the derivate update process runs");
        }
        
        synchronized (getDatabaseStatus(db).updateMonitor) {
            @SuppressWarnings("unchecked")
            List<String> lastCreators = (List<String>) db.getExtensionData(WGDatabase.EXTDATA_DERIVATE_CREATORS);
            if (lastCreators == null) {
                lastCreators = new ArrayList<String>();
            }
            
            for (String creatorClassName : creators) {
                lastCreators.remove(creatorClassName);
            }
            
            db.writeExtensionData(WGDatabase.EXTDATA_DERIVATE_CREATORS, lastCreators);
        }
       
    }

    public static DerivateQuery mergeDerivateQueries(String fileDerivates, String existingDerivates) throws WGInvalidDerivateQueryException {

        DerivateQuery derivateQuery = parseDerivateQuery(fileDerivates, false);
        DerivateQuery existingDerivateQuery = parseDerivateQuery(existingDerivates);
        
        if (derivateQuery.isClearPredefinedQuery()) {
            return derivateQuery;
        }
        
        existingDerivateQuery.keySet().removeAll(derivateQuery.keySet());
        derivateQuery.putAll(existingDerivateQuery);
        return derivateQuery;
    }

    private void resetDerivateStatus(WGDatabase db) throws WGAPIException {
        synchronized (getDatabaseStatus(db).updateMonitor) {
            db.removeExtensionData(WGDatabase.EXTDATA_DERIVATE_CREATORS);
            db.removeExtensionData(WGDatabase.EXTDATA_DERIVATE_REVISION);
        }
    }
    
    public WGDatabaseRevision getDerivateUpdateRevision(WGDatabase db) throws WGAPIException, IOException {
        String serializedRevision = (String) db.getExtensionData(WGDatabase.EXTDATA_DERIVATE_REVISION);
        if (serializedRevision != null) {
            return WGDatabaseRevision.deserialize(serializedRevision);
        }
        else {
            return null;
        }
    }
    
    public boolean isProcessServer() {
        return _updateProcess != null;
    }
    
    public boolean runUpdateProcess() {
        
        if (_updateProcess == null) {
            return false;
        }
        
        Runnable updateIndexRunnable = new Runnable() {
            @Override
            public void run() {
                //_core.getLog().info("Manually running update process");
                _updateProcess.run();
                
            }
        };
        Thread t = new java.lang.Thread(updateIndexRunnable);
        t.start();
        return true;
        
    }

    private synchronized DatabaseStatus getDatabaseStatus(WGDatabase db) {
        DatabaseStatus status = _databaseStatus.get(db.getDbReference());
        if (status == null) {
            status = new DatabaseStatus();
            _databaseStatus.put(db.getDbReference(), status);
        }
    
        return status;
    }

    public DerivateUpdateProcess getUpdateProcess() {
        return _updateProcess;
    }
    
    public WGDatabaseRevision getLocalUpdateStatus(WGDatabase db) {
        
        DatabaseStatus status = _databaseStatus.get(db.getDbReference());
        if (status != null) {
            return status.lastUpdateRevision;
        }
        else {
            return null;
        }
        
    }

}
