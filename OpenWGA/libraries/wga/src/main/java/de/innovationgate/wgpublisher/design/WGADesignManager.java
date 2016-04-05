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

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.common.LocalizedInformation;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.Design;
import de.innovationgate.wga.config.DesignConfiguration;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.config.DesignSource;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.properties.DesignSourceProperties;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;
import de.innovationgate.wga.modules.types.DesignSourceModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.db.DBDesignSource;
import de.innovationgate.wgpublisher.design.db.PluginDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;

public class WGADesignManager {
    
    public static final DesignReference createDesignReference(WGDesignDocument doc) throws WGAPIException {
        
        DesignReference ref = doc.getDesignReference();
        if (ref != null) {
            return ref;
        }
        else {
            return DBDesignSource.createDefaultDesignReference(doc);
        }

    }
    
    private WGACore _core;
    private Map<String,WGADesignSource> _designSources = new HashMap<String,WGADesignSource>();

    public WGADesignManager(WGACore core, List<DesignSource> designSources) {
        _core = core;

        // Initialize singleton design sources
        Iterator<ModuleDefinition> singletons = core.getModuleRegistry().getModulesForType(DesignSourceModuleType.class).values().iterator();
        while (singletons.hasNext()) {
            ModuleDefinition sourceDefinition = singletons.next();
            try {
                sourceDefinition.testDependencies();
                final DesignSourceProperties properties = (DesignSourceProperties) sourceDefinition.getProperties();
                if (properties != null && properties.isSingleton()) {
                    try {
                        LocalizedInformation locInfo = new LocalizedInformation() {
                            
                            public String getTitle(Locale locale) {
                                return properties.getSingletonTitle(locale); 
                            }
                            
                            public String getDescription(Locale locale) {
                                return properties.getSingletonDescription(locale);
                            }
                        };
                        
                        WGADesignSource designSource = (WGADesignSource) core.getModuleRegistry().instantiate(sourceDefinition);
                        designSource.init(_core, properties.getSingletonUID(), locInfo, new HashMap<String,String>());
                        core.getLog().info("Registering design source '" + designSource.getTitle(Locale.getDefault()) + "' (Automatically created)");
                        _designSources.put(properties.getSingletonUID(), designSource);
                    }
                    catch (Exception e) {
                        core.getLog().error("Exception registering design source " + sourceDefinition.getTitle(Locale.getDefault()), e);
                    }
                }
            }
            catch (ModuleDependencyException e) {
                core.getLog().warn("Design source " + sourceDefinition.getTitle(Locale.getDefault()) + " deactivated in current WGA runtime: " + e.getMessage());
            }
        }
        
        
        // Initialize configured design sources
        Iterator<DesignSource> collections = designSources.iterator();
        while (collections.hasNext()) {
            final DesignSource designSource = (DesignSource) collections.next();
            try {
                String colClassName = designSource.getImplClassName();
                Class colClass = WGACore.getLibraryLoader().loadClass(colClassName);
                
                if (!(WGADesignSource.class.isAssignableFrom(colClass))) {
                    _core.getLog().error("Error registering design source " + designSource.toString() + ". Class " + colClassName + " is no design source implementation.");
                    continue;
                }
                
                LocalizedInformation locInfo = new LocalizedInformation() {
                    public String getTitle(Locale locale) {
                        return designSource.getTitle(); 
                    }
                    
                    public String getDescription(Locale locale) {
                        return designSource.getDescription();
                    }
                };
                
                WGADesignSource col = (WGADesignSource) core.getModuleRegistry().instantiate(colClass);
                col.init(_core, designSource.getUid(), locInfo, designSource.getOptions());
                _core.getLog().info("Registering design source '" + designSource.toString() + "' of type '" + designSource.getImplClassName() + "'");
                _designSources.put(designSource.getUid(), col);
                
            }
            catch (Exception e) {
                _core.getLog().error("Error initializing design source " + designSource.toString(), e);
            }
            
        }
    }

    public Map<String, WGADesignSource> getDesignSources() {
        return _designSources;
    }

    
    
    public WGADesign resolveDesignReference(DesignReference ref) throws URISyntaxException, WGADesignRetrievalException {
        
        WGADesignSource collection = _designSources.get(ref.getSourceName());
        if (collection == null) {
            return null;
        }
        
        return collection.getDesign(ref.getDesignName());
        
    }
    
    public WGADesign resolveDesignReference(String refStr) throws URISyntaxException, WGADesignRetrievalException {
        return resolveDesignReference(new DesignReference(refStr));
    }
    
    public void applyDesign(WGDatabase db, ContentStore cs, ProblemOccasion occ) throws Problem {
        
        String designName = new DesignReference(cs.getDesign()).toString();
        
        try {
            Design designConfig = cs.getDesign();
            WGADesign design = getDesignForConfig(designConfig);
            if (design == null) {
                throw Problem.create(occ, "applyDesignProblem.unknownDesign", ProblemSeverity.HIGH, Problem.var("design", designName));
            }
                
            // Determine if we have an overlay configuration
            Design overlayConfig = cs.getOverlay();
            
            // If we have an overlay: Use special overlay design provider
            if (overlayConfig != null) {
                DesignReference ref = new DesignReference(overlayConfig.getSource(), overlayConfig.getName());
                WGADesign overlay = resolveDesignReference(ref);
                if (overlay != null) {
                    OverlayDesignProvider.applyOverlayDesign(_core, db, design, designConfig.getOptions(), overlay, overlayConfig.getOptions());
                    return;
                }
                else {
                    throw Problem.create(occ, "applyDesignProblem.unknownOverlayDesign", ProblemSeverity.HIGH, Problem.var("design", ref.toString()));
                }
            }
             
            // Basic way
            design.applyDesign(db, designConfig.getOptions());

        }
        catch (Problem p) {
            throw p;
        }
        catch (WGABaseDesignNotAvailableException e) {
            throw Problem.create(occ, "applyDesignProblem.baseVersionTooLow", ProblemSeverity.HIGH, Problem.var("basedesign", e.getDesignName()).var("version", e.getNeededVersion().toString()));
        }
        catch (Exception e) {
            throw Problem.create(occ, "applyDesignProblem.exception", ProblemSeverity.HIGH, Problem.var("design", designName), e);
        }
        
        
    }

    public WGADesign getDesignForConfig(Design designConfig) throws URISyntaxException, WGADesignRetrievalException {
        DesignReference ref = new DesignReference(designConfig.getSource(), designConfig.getName());
        WGADesign design = resolveDesignReference(ref);
        return design;
    }
    
    public boolean isDesignConsumerOfDatabase(WGDesignProvider provider, WGDatabase designdb) throws WGAPIException {
        
        if (provider instanceof OverlayDesignProvider) {
            OverlayDesignProvider overlayProvider = (OverlayDesignProvider) provider;
            return isDesignConsumerOfDatabase(overlayProvider.getOriginal(), designdb) || isDesignConsumerOfDatabase(overlayProvider.getOverlay(), designdb); 
        }
        
        if (provider instanceof PluginDesignProvider) {
            
            PluginDesignProvider pluginProvider = (PluginDesignProvider)  provider;
            if (pluginProvider.getDesignDB() == designdb) {
                return true;
            }
            
        }
        
        return false;
    }


    
}
