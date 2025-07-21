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

package de.innovationgate.wgpublisher.design;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.vfs2.FileSystemException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.OverlayStatus.ChangeType;
import de.innovationgate.wgpublisher.design.OverlayStatus.ChangedDocument;
import de.innovationgate.wgpublisher.design.db.PluginDesignProvider;
import de.innovationgate.wgpublisher.design.db.PluginDesignSource;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignSource;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;
import de.innovationgate.wgpublisher.problems.AdditiveProblem;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.MessageVariableProvider;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.Problem.Vars;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemPath;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemText;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.problems.UseSpecialProblemImplementation;

public class OverlayDesignProvider implements WGADesignProvider {
    
    public static final String OVERLAY_FOLDER = "overlay";
    public static final String OVERLAY_PREFIX = OVERLAY_FOLDER + ":";
    public static final String OVERLAY_DATA_FILE = "overlay.xml";
    
    
    private WGADesignProvider _overlay;
    private PluginDesignProvider _original;
    private WGDatabase _consumer;
    private Version _initialBaseVersion = null;
    
    private OverlayStatus _status;
    
    public static class ApplyOverlayDesignOccasion implements ProblemOccasion, UseSpecialProblemImplementation, MessageVariableProvider {
        
        private String _dbKey;
        private String _baseDesign;
        private String _overlayDesign;

        public ApplyOverlayDesignOccasion(String dbKey, String baseDesign, String overlayDesign) {
            _dbKey = dbKey;
            _baseDesign = baseDesign;
            _overlayDesign = overlayDesign;
        }

        @Override
        public ProblemScope getDefaultScope() {
            return new DatabaseScope(_dbKey);
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return OverlayDesignProvider.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return true;
        }

        @Override
        public Problem createProblem(ProblemPath problemPath, ProblemText text, ProblemSeverity severity, ProblemOccasion occasion, Throwable throwable, List<MessageVariableProvider> providers) {
            if (problemPath.getKey().equals(OverlayDesignProvider.class.getName() + ".applyOverlayProblem.resourceConflict")) {
                return new ConflictProblem(problemPath, text, severity, occasion, throwable, (List<String>) Problem.getVariable("conflictresources", providers), providers);
            }
            else {
                return new Problem(problemPath, text, severity, occasion, throwable, providers);
            }
        }

        @Override
        public Vars getDefaultMessageVariables() {
            return Problem.var("basedesign", _baseDesign).var("overlaydesign", _overlayDesign);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_dbKey == null) ? 0 : _dbKey.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            ApplyOverlayDesignOccasion other = (ApplyOverlayDesignOccasion) obj;
            if (_dbKey == null) {
                if (other._dbKey != null)
                    return false;
            }
            else if (!_dbKey.equals(other._dbKey))
                return false;
            return true;
        }
        
    }
    
    public static class ConflictProblem extends Problem implements AdditiveProblem<ConflictProblem> {

        /**
         * 
         */
        private static final long serialVersionUID = 1L;

        public ConflictProblem(ProblemPath path, ProblemText text, ProblemSeverity severity, ProblemOccasion occasion, Throwable throwable, List<String> conflictResources, List<MessageVariableProvider> providers) {
            super(path, text, severity, occasion, throwable, providers);
            _conflictResources.addAll(conflictResources);
        }
        
        @Override
        public Object getVariable(String name) {

            if ("conflicts".equals(name)) {
                StringBuilder msg = new StringBuilder();
                for (String resource  : _conflictResources) {
                    msg.append(" - " + resource + "\n");
                }
                return msg.toString();
            }
            
            return super.getVariable(name);
        }

        private List<String> _conflictResources = new ArrayList<String>();
        
        @Override
        public void addProblem(ConflictProblem p) {
            _conflictResources.addAll(p.getConflictResources());            
        }

        public List<String> getConflictResources() {
            return _conflictResources;
        }
        
    }
    
    public static final void applyOverlayDesign(WGACore core, WGDatabase db, WGADesign originalDesign, Map<String,String> originalOptions, WGADesign overlayDesign, Map<String,String> overlayOptions) throws WGException {
     
        ApplyOverlayDesignOccasion occ = new ApplyOverlayDesignOccasion(db.getDbReference(), originalDesign.createDesignReference().toString(), overlayDesign.createDesignReference().toString());
        core.getProblemRegistry().clearProblemOccasion(occ);
        
        try {
            if (!(originalDesign.getSource() instanceof PluginDesignSource)) {
                throw new WGADesignConfigurationException("Overlay designs can only be used with a plugin design as basis");
            }
            if (!(overlayDesign.getSource() instanceof FileSystemDesignSource) && !(overlayDesign.getSource() instanceof PluginDesignSource)) {
                throw new WGADesignConfigurationException("Overlay designs can only be file system or plugin designs");
            }
            
            WGADesignProvider overlay = overlayDesign.createDesignProvider(db, overlayOptions);
            boolean editableOverlay = false;
            if (overlay instanceof FileSystemDesignProvider) {
               editableOverlay = ((FileSystemDesignProvider) overlay).isEditable();
            }
            else if (overlay instanceof PluginDesignProvider) {
               editableOverlay = ((PluginDesignProvider) overlay).getSourceDesignProvider().isEditable();
            }
            
            // Test designs compatibility. Overlay must either correctly point to the base plugin or be completely empty.
            OverlayData overlayData = overlayDesign.getOverlayData();
            boolean testOverlayForEmptiness = false;
            if (overlayData != null) {
                String baseDesignName = originalDesign.getConfig().getPluginConfig().getId().getUniqueName();
                if (!baseDesignName.equals(overlayData.getBasepluginName())) {
                    throw Problem.create(occ, "applyOverlayProblem.wrongBase", ProblemSeverity.HIGH, Problem.var("usedbase", baseDesignName).var("correctbase", overlayData.getBasepluginName()));
                }
                if (!WGUtils.isEmpty(overlayData.getBasepluginVersion())) {
                    Version overlayBaseVersion = new Version(overlayData.getBasepluginVersion());
                    Version currentBaseVersion = originalDesign.getConfig().getPluginConfig().getId().getVersion();
                    if (!currentBaseVersion.isAtLeast(overlayBaseVersion)) {
                        throw new WGABaseDesignNotAvailableException(overlayData.getBasepluginName(), overlayBaseVersion);
                    }
                }
            }
            else {
                if (overlay.getDesignObjects(WGDocument.TYPE_TML).size() > 0 || overlay.getDesignObjects(WGDocument.TYPE_CSSJS).size() > 0) {
                    throw Problem.create(occ, "applyOverlayProblem.invalidOverlay", ProblemSeverity.HIGH);
                }
            }
            
            PluginDesignProvider original = (PluginDesignProvider) originalDesign.createDesignProvider(db, originalOptions);
            String initialBaseVersion = (overlayData != null ? overlayData.getInitialBasepluginVersion() : null);
            OverlayDesignProvider provider = new OverlayDesignProvider(core, db, overlay, original, initialBaseVersion);
            db.setDesignProvider(provider);
            db.setAllowDesignModification(false);
            
            core.getLog().info("Application " + db.getDbReference() + " uses design \"" + original.getName() + "\" with overlay \"" + overlay.getName() + "\"");
            
            if (editableOverlay) {
                for (ChangedDocument doc : provider.getStatus().getChangedDocuments().values()) {
                    
                    if (doc.getChangeType() == ChangeType.CONFLICT) {
                        core.getLog().warn("App '" + db.getDbReference() + "': a conflict was detected on resource '" + doc.getDocumentKey() + "' between base and overlay design. The design might not work correctly.");
                        Problem.Vars vars = Problem.var("baseversion", overlayData.getBasepluginVersion()).var("currentversion", original.getPluginID().getVersion().toString()).var("basedesign", overlayData.getBasepluginName());
                        if (doc.getInvolvedFiles() != null && doc.getInvolvedFiles().size() > 0) {
                            List<String> resources = new ArrayList<String>();
                            for (String file : doc.getInvolvedFiles()) {
                                resources.add(doc.getDocumentKey().toString() + " file " + file);
                            }
                            vars.var("conflictresources", resources);
                        }
                        else {
                            vars.var("conflictresources", Collections.singletonList(doc.getDocumentKey().toString()));
                        }
                        
                        core.getProblemRegistry().addProblem(Problem.create(occ, "applyOverlayProblem.resourceConflict", ProblemSeverity.LOW, vars));
                    }
                    
                }
            }
        }
        catch (Problem p) {
            throw p;
        }
        catch (Exception e) {
            throw new WGADesignConfigurationException("Exception applying overlay design", e);
        }

    }

    public OverlayDesignProvider(WGACore core, WGDatabase consumer, WGADesignProvider overlayProvider, PluginDesignProvider originalProvider, String initialBaseVersionStr) throws Exception {
        _consumer = consumer;
        _overlay = overlayProvider;
        _original = originalProvider;
        
        if (!WGUtils.isEmpty(initialBaseVersionStr)) {
            _initialBaseVersion = new Version(initialBaseVersionStr);
        }
        
        FileSystemDesignProvider overlayFsProvider = getOverlayFSProvider();
        _status = overlayFsProvider.determineOverlayStatus(_original);
        if (_status.isNewOverlay()) {
            overlayFsProvider.upgradeOverlay(_original, _status);
        }
        
    }

    private FileSystemDesignProvider getOverlayFSProvider() throws WGADesignConfigurationException {
        FileSystemDesignProvider overlayFsProvider;
        if (_overlay instanceof FileSystemDesignProvider) {
            overlayFsProvider = (FileSystemDesignProvider) _overlay;;
        }
        else if (_overlay instanceof PluginDesignProvider){
            PluginDesignProvider pluginProvider = (PluginDesignProvider) _overlay;
            overlayFsProvider = pluginProvider.getSourceDesignProvider();
        }
        else {
            throw new WGADesignConfigurationException("Cannot determine overlay status with a non-filesystem design provider");
        }
        return overlayFsProvider;
    }

    public WGDatabase getConsumerDatabase() {
        return _consumer;
    }

    public boolean isLookupVariants() {
        return false;
    }

    public void addDesignChangeListener(WGDesignChangeListener changeListener) {
        _original.addDesignChangeListener(changeListener);
        _overlay.addDesignChangeListener(changeListener);
    }
    

    public void closeSession() throws WGBackendException {
        _overlay.closeSession();
        _original.closeSession();
    }

    public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAuthorisationException, WGCreationException {
        throw new WGCreationException("This design cannot create design documents");
    }

    public int designHashCode() {
        return (new HashCodeBuilder()).append(_overlay.designHashCode()).append(_original.designHashCode()).toHashCode();
    }

    public void dispose() {
        _overlay.dispose();
        _original.dispose();
    }

    public WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException {

        // Determine the correct provider
        WGDesignProvider provider = getDesignProvider(type, name, strMediaKey);

        if (provider == _overlay) {
            name = name.substring(OVERLAY_PREFIX.length());
        }
        WGDocumentCore core = provider.getDesignObject(type, name, strMediaKey);
        if (core != null) {
                if (provider == _overlay) {
                    return new DesignProviderCoreWrapper(core, this, false, true);
                }
                else {
                    return core;
                }
        }
        else {
            return null;
        }
    
    }

    public WGDesignProvider getDesignProvider(int type, String name, String strMediaKey) {
        WGDesignProvider provider = _original;

        if (name.startsWith(OVERLAY_PREFIX)) {
            
            if (type != 0) {
                // Test the resource status and which version to prefer
                WGDocumentKey resourceKey = new WGDocumentKey(type, name, strMediaKey);
                OverlayStatus.ChangedDocument changedResource = _status.getChangedDocuments().get(resourceKey);
                if (changedResource == null || changedResource.getChangeType().equals(OverlayStatus.ChangeType.CONFLICT)) {
                    provider = _overlay;
                }
            }
            else { // type 0 means this is a generic request to see where this name normally points to
                provider = _overlay;
            }
            
        }
        return provider;
    }

    public List getDesignObjects(int type) throws WGAPIException {
        
        Map<WGDocumentKey,WGDocumentCore> designs = new HashMap<WGDocumentKey, WGDocumentCore>(); 
        
        for (WGDocumentCore core : _original.getDesignObjects(type)) {
            WGDocumentKey key = WGDocument.buildDocumentKey(core, _original.getSlaveDB());
            designs.put(key, core);
        }
        
        for (WGDocumentCore core : (List<WGDocumentCore>) _overlay.getDesignObjects(type)) {
            core = new DesignProviderCoreWrapper(core, this, false, true);
            WGDocumentKey key = WGDocument.buildDocumentKey(core, _overlay.getConsumerDatabase());
            OverlayStatus.ChangedDocument changedResource = _status.getChangedDocuments().get(key);
            if (changedResource == null || changedResource.getChangeType().equals(OverlayStatus.ChangeType.CONFLICT)) {
                designs.put(key, core);
            }
        }
        

        return new ArrayList(designs.values());
        
    }

    public String getName() {
        return _original.getName() + " with overlay \"" + _overlay.getName() + "\""; 
    }

    public boolean isNotifying() {
        return _original.isNotifying() && _overlay.isNotifying();
    }

    public boolean isProviderCore(WGDocumentCore core) {
        return false;
    }

    public void openSession(WGSessionContext context) throws WGBackendException {
        _original.openSession(context);
        _overlay.openSession(context);

    }

    public boolean providesType(int type) {
        return _original.providesType(type);
    }

    public void removeDesignChangeListener(WGDesignChangeListener changeListener) {
        _original.removeDesignChangeListener(changeListener);
        _overlay.removeDesignChangeListener(changeListener);
    }

    public WGADesignProvider getOverlay() {
        return _overlay;
    }

    public PluginDesignProvider getOriginal() {
        return _original;
    }

    public boolean isReady() {
        return _original.isReady() && _overlay.isReady();
    }
    
    @Override
    public boolean isSynchronizeAccess() {
        return _original.isSynchronizeAccess() || _overlay.isSynchronizeAccess();
    }
    
    public boolean isUpgradeable() throws WGADesignConfigurationException, FileSystemException, WGDesignSyncException {
        if (!_status.isUpdatedBaseDesign()) {
            return false;
        }
        
        if (!isOverlayWriteable()) {
            return false;
        }
        
        return true;
    }
    
    public boolean isOverlayWriteable() throws WGADesignConfigurationException, FileSystemException, WGDesignSyncException {
        FileSystemDesignProvider fsProvider = getOverlayFSProvider();
        return (fsProvider.getBaseFolder().isWriteable());
    }

    public OverlayStatus getStatus() {
        return _status;
    }
    
    public void upgradeOverlay() throws Exception {
        
        if (!isUpgradeable()) {
            throw new WGDesignSyncException("Overlay design is in no upgradeable state");
        }
        
        // Determine the overlay status again. May have changed since connecting
        FileSystemDesignProvider fsProvider = getOverlayFSProvider();
        _status = fsProvider.determineOverlayStatus(_original);
        fsProvider.upgradeOverlay(_original, _status);
        
    }
    
    public void createOverlayDowngradeFiles() throws Exception {
        
        if (!isOverlayWriteable()) {
            throw new WGDesignSyncException("Overlay design is not writeable");
        }
        
        // Determine the overlay status again. May have changed since connecting
        FileSystemDesignProvider fsProvider = getOverlayFSProvider();
        _status = fsProvider.determineOverlayStatus(_original);
        fsProvider.createDowngradeFiles(getOriginal(), _status.getOverlayData());
        
    }
    
    public Version getInitialBaseVersion() {
        return _initialBaseVersion;
    }
    
    @Override
    public void clearCache() throws WGException {
        _original.clearCache();
        _overlay.clearCache();
    }

    @Override
    public String getFileEncoding() {
        return _original.getFileEncoding();
    }
}
