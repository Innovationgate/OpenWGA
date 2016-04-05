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

package de.innovationgate.wgpublisher.design.fs;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.VFS;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.LocalizedInformation;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.config.DesignSource;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.SystemContainerManager;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.OverlayData;
import de.innovationgate.wgpublisher.design.WGADesign;
import de.innovationgate.wgpublisher.design.WGADesignCreationException;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.WGADesignSource;
import de.innovationgate.wgpublisher.design.WGADesignConfigurationException;
import de.innovationgate.wgpublisher.design.WGADesignRetrievalException;
import de.innovationgate.wgpublisher.design.sync.DesignSyncManager;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class FileSystemDesignSource implements WGADesignSource {
    
    private static final String OPTIONPREFIX_ADDITIONALDIR = "additionaldir.";

    public static final String DESIGNNAMEPREFIX_ADDITIONALDIR = "additional:";
    
    public static final String DESIGNNAMEPREFIX_ARCHIVE = "archive:";
    
    public static final String ARCHIVED_DESIGN_EXTENSION = "wgadesign";
    
    private FileObject _dir;
    private WGACore _core;
    private String _name;
    private Map<String,String> _additionalDirs = new HashMap<String,String>();

    private LocalizedInformation _locInfo;

    public List<String> getDesignNames() throws WGADesignRetrievalException {
        
        try {
            List<String> designs = new ArrayList<String>();
            
            // Add child design directories - All with a syncinfo/design.xml or those that are completely empty and can be initialized
            _dir.refresh();
            FileObject[] children = _dir.getChildren();
            for (int i = 0; i < children.length; i++) {
                FileObject child = children[i];
                if (child.getType().equals(FileType.FOLDER)) {
                    FileObject resolvedChild = WGUtils.resolveDirLink(child);
                    if (resolvedChild.getType().equals(FileType.FOLDER)) {
                        FileObject syncInfo = DesignDirectory.getDesignDefinitionFile(resolvedChild);
                        if (syncInfo != null || child.getChildren().length == 0) {
                            designs.add(child.getName().getBaseName());
                        }
                    }
                }
                else if (child.getType().equals(FileType.FILE) && child.getName().getExtension().equalsIgnoreCase(ARCHIVED_DESIGN_EXTENSION)) {
                    designs.add(DESIGNNAMEPREFIX_ARCHIVE + child.getName().getBaseName().substring(0, child.getName().getBaseName().lastIndexOf(".")));
                }
            }
            
            // Add additional directories
            Iterator<Map.Entry<String,String>> dirs = _additionalDirs.entrySet().iterator();
            while (dirs.hasNext()) {
                Map.Entry<String,String> entry = dirs.next();
                FileObject dir = VFS.getManager().resolveFile((String) entry.getValue());
                if (dir.exists() && dir.getType().equals(FileType.FOLDER)) {
                    FileObject syncInfo = DesignDirectory.getDesignDefinitionFile(dir);
                    if (syncInfo != null || dir.getChildren().length == 0) {
                        designs.add(DESIGNNAMEPREFIX_ADDITIONALDIR + entry.getKey());
                    }
                }
            }
            
            return designs;
        }
        catch (FileSystemException e) {
            throw new WGADesignRetrievalException("Exception retrieving file system designs", e);
        }
        
    }

    private CSConfig loadConfig(FileObject syncInfo) {
        
        try {
            if (syncInfo == null || !syncInfo.exists()) {
                return new CSConfig();
            }
            
            FileObject csConfig = syncInfo.getParent().resolveFile(SystemContainerManager.CSCONFIG_PATH);
            if (csConfig.exists()) {
                return CSConfig.load(csConfig);
            }
            else {
                return null;
            }
        }
        catch (Exception e) {
            _core.getLog().error("Exception loading design config", e);
            return null;
        }
    }
    
    private OverlayData loadOverlayData(FileObject syncInfo) {
        
        try {
            if (syncInfo == null || !syncInfo.exists()) {
                return null;
            }
            
            FileObject overlayData = syncInfo.getParent().resolveFile(SystemContainerManager.OVERLAY_DATA_PATH);
            if (overlayData.exists()) {
                return OverlayData.load(overlayData);
            }
            else {
                return null;
            }
        }
        catch (Exception e) {
            _core.getLog().error("Exception loading overlay data", e);
            return null;
        }
    }

    public String getName() {
        return _name;
    }

    public void init(WGACore core, String name, LocalizedInformation locInfo, Map<String, String> options) throws WGADesignConfigurationException {

        try {
            _core = core;
            _name = name;
            _locInfo = locInfo;
            String dirPath = determinePath(core, options);
            if (dirPath == null) {
                throw new WGADesignConfigurationException("No option " + DesignSource.OPTION_PATH + " provided");
            }
            
            _dir = VFS.getManager().resolveFile(_core.getConfigFile().getParentFile(), dirPath);
            if (!_dir.exists()) {
                _dir.createFolder();
            }
            
            if (!_dir.getType().equals(FileType.FOLDER)) {
                throw new WGADesignConfigurationException("Design source directory '" + _dir.getURL().toString() + "' does not exist or is no directory");
            }
            
            _additionalDirs = WGUtils.extractMapByPrefix(options, OPTIONPREFIX_ADDITIONALDIR);
            
            
            
            
        }
        catch (FileSystemException e) {
            throw new WGADesignConfigurationException("Exception building file system design source", e);
        }

    }

    protected String determinePath(WGACore core, Map<String, String> options) {
        return options.get(DesignSource.OPTION_PATH);
    }

    public void applyDesign(WGADesign design, WGDatabase db, Map<String,String> options) throws WGADesignConfigurationException {

        try {
            	
            // Look if we must sync
            boolean sync = WGUtils.getBooleanMapValue(options, FileSystemDesignProvider.OPTION_SYNC, false);
            if (sync) {
                String location = getDesignLocation(design);
                _core.getLog().info("Web application " + db.getDbReference() + " synchronizes design with design directory " + location);
                db.setAttribute(WGACore.DBATTRIB_DESIGNSYNC, new DesignSyncManager(design.createDesignReference(), _core, db, location, options));
            }
            else {
                FileSystemDesignProvider designProvider = createDesignProvider(design, db, options);
                _core.getLog().info("Web application " + db.getDbReference() + " uses design from design directory " + designProvider.getDesignPath());
                db.setDesignProvider(designProvider);
                db.setAllowDesignModification(false);
            }
            
        }
        catch (Exception e) {
            throw new WGADesignConfigurationException("Exception applying design to web application " + db.getDbReference(), e);
        }
        
    }

    public FileSystemDesignProvider createDesignProvider(WGADesign design, WGDatabase db, Map<String, String> options) throws WGADesignConfigurationException {
        try {
            String location = getDesignLocation(design);
            FileSystemDesignProvider designProvider = new FileSystemDesignProvider(design.createDesignReference(), _core, db, location, options);
            return designProvider;
        }
        catch (Exception e) {
            throw new WGADesignConfigurationException("Exception creating design provider", e);
        }
    }

    public String getDesignLocation(WGADesign design) throws WGADesignConfigurationException, FileSystemException {
        String location;
        if (design.getName().startsWith(DESIGNNAMEPREFIX_ADDITIONALDIR)) {
            String addName = design.getName().substring(DESIGNNAMEPREFIX_ADDITIONALDIR.length());
            location = _additionalDirs.get(addName);
            if (location == null) {
                throw new WGADesignConfigurationException("Additional directory of name " + addName + " + is not defined");
            }
        } else if (design.getName().startsWith(DESIGNNAMEPREFIX_ARCHIVE)) {            
        	FileObject designDir = WGUtils.resolveDirLink(_dir.getChild(design.getName().substring(DESIGNNAMEPREFIX_ARCHIVE.length()) + "." + ARCHIVED_DESIGN_EXTENSION));
            if (designDir == null) {
                throw new WGADesignConfigurationException("Archived design '" + _dir.resolveFile(design.getName()).getURL().toString() + " not found");
            }
            location = "zip:" + designDir.getURL().toString();
        } else {
            FileObject designDir = WGUtils.resolveDirLink(_dir.getChild(design.getName()));
            if (designDir == null) {
                throw new WGADesignConfigurationException("Design directory '" + _dir.resolveFile(design.getName()).getURL().toString() + " not found");
            }                
            location = designDir.getURL().toString();                
        }
        return location;
    }
    
    public String getDescription(Locale locale) {
        return _locInfo.getDescription(locale);
    }
    
    public String getTitle(Locale locale) {
        return _locInfo.getTitle(locale);
    }

    public void createDesign(String designName) throws WGNotSupportedException, WGADesignCreationException {
        
        try {
            // Create the folder
            FileObject designDir = _dir.resolveFile(designName);
            if (designDir.exists()) {
                throw new WGADesignCreationException("A directory of name '" + designName + "' already exists");
            }
            designDir.createFolder();
            
            // Nuffin else to do .... initial deploy will do the rest
        }
        catch (Exception e) {
            throw new WGADesignCreationException("Exception creating file system design '" + designName + "'", e);
        }
        
    }

    public boolean isDesignCreatable() {
        return true;
    }

    public Class getDesignProviderClass() {
        return FileSystemDesignProvider.class;
    }

	public FileObject getDir() {
		return _dir;
	}

    public WGADesign getDesign(final String name) throws WGADesignRetrievalException {
        try {
            
            // Design archive (obsolete)
            if (name.startsWith(DESIGNNAMEPREFIX_ARCHIVE)) {
                String designName = name.substring(DESIGNNAMEPREFIX_ARCHIVE.length());
                FileObject designFile = _dir.resolveFile(designName + ARCHIVED_DESIGN_EXTENSION);
                if (!designFile.exists()) {
                    return null;
                }
                
                WGADesign design = new WGADesign();
                design.setSource(this);
                design.setName(name);
                design.setTitle(designFile.getName().getBaseName());
                design.setDescription("Design at location " + designFile.getURL().toString());
                return design;
            }
            
            // Additional directory
            else if (name.startsWith(DESIGNNAMEPREFIX_ADDITIONALDIR)) {
                String designName = name.substring(DESIGNNAMEPREFIX_ADDITIONALDIR.length());
                String dirPath = _additionalDirs.get(designName);
                if (dirPath == null) {
                    return null;
                }
                
                FileObject dir = VFS.getManager().resolveFile((String) dirPath);
                if (!dir.exists() || !dir.getType().equals(FileType.FOLDER)) {
                    return null;
                }
                    
                FileObject syncInfo = DesignDirectory.getDesignDefinitionFile(dir);
                
                // Non-empty directories without syncinfo may not be used as design directories
                if (syncInfo == null && dir.getChildren().length != 0) {
                    return null;
                }

                WGADesign design = new WGADesign();
                design.setSource(this);
                design.setName(DESIGNNAMEPREFIX_ADDITIONALDIR + name);
                design.setTitle(dir.getName().getBaseName());
                design.setDescription("Design at location " + dir.getURL().toString());
                design.setConfig(loadConfig(syncInfo));
                design.setOverlayData(loadOverlayData(syncInfo));                
                
                return design;
                
            }
            
            // Regular design in configured directory
            else {
                _dir.refresh();
                FileObject child = _dir.resolveFile(name);
                if (!child.exists()) {
                    return null;
                }
                
                FileObject resolvedChild = WGUtils.resolveDirLink(child);
                if (!resolvedChild.getType().equals(FileType.FOLDER)) {
                    return null;
                }
                 
                FileObject syncInfo = DesignDirectory.getDesignDefinitionFile(resolvedChild);
                
                // Non-empty directories without syncinfo may not be used as design directories
                if (syncInfo == null && child.getChildren().length != 0) {
                    return null;
                }
                
                
                WGADesign design = new WGADesign();
                design.setSource(this);
                design.setName(child.getName().getBaseName());
                design.setTitle(child.getName().getBaseName());
                design.setDescription("Design at location " + child.getURL().toString());
                design.setConfig(loadConfig(syncInfo));    
                design.setOverlayData(loadOverlayData(syncInfo));
                return design;
            }
        }
        catch (FileSystemException e) {
            throw new WGADesignRetrievalException("Exception retrieving file system designs", e);
        }
    }



}
