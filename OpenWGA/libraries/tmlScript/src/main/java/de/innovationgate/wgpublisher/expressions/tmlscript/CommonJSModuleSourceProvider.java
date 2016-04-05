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

package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.provider.ModuleSource;
import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.provider.ModuleSourceProvider;
import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.provider.UrlModuleSourceProvider;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal.RedirectingWGAContext;

public class CommonJSModuleSourceProvider extends UrlModuleSourceProvider {

    public static final String OPENWGA_COMMONJS_SCHEME = "openwga-commonjs";
    public static final String SYSTEM_COMMONJS_FOLDER = "system:commonjs";
    private WGA _wga = WGA.get(new RedirectingWGAContext());

    public CommonJSModuleSourceProvider(List<URI> privileged) throws URISyntaxException {
        super(privileged, Collections.<URI>emptyList());
    }
    
    @Override
    protected ModuleSource loadFromPrivilegedLocations(String moduleId, Object validator) throws IOException, URISyntaxException {
        
        try {
            
            // Super implementation loads Narwhal default modules
            ModuleSource moduleSource = super.loadFromPrivilegedLocations(moduleId, validator);
            if (moduleSource != null) {
                return moduleSource;
            }
            
            // Load modules from the current openwga design
            Design myDesign = _wga.design();
            URI myURI = new URI(OPENWGA_COMMONJS_SCHEME + "://" + myDesign.getBaseReference().getDesignApp() + "/$tmlscriptCode");
            URI moduleURI = myURI.resolve(moduleId);
            
            if (moduleURI.getScheme().equals(OPENWGA_COMMONJS_SCHEME)) {
                moduleSource = loadOpenwgaModule(moduleURI, myURI, validator);
                if (moduleSource != null) {
                    return moduleSource;
                }
            }
            
            return null;
            
        }
        catch (WGException e) {
            throw new IOException(e);
        }
        
        
        
    }

    private ModuleSource loadOpenwgaModule(URI moduleURI, URI baseURI, Object validator) throws WGException, URISyntaxException, WGAPIException, IOException {
        ModuleSource moduleSource = loadOpenwgaModule(moduleURI, baseURI, validator, "overlay:" + SYSTEM_COMMONJS_FOLDER);
        if (moduleSource != null) {
            return moduleSource;
        }
        return loadOpenwgaModule(moduleURI, baseURI, validator, SYSTEM_COMMONJS_FOLDER);
    }
    
    @Override
    protected ModuleSource loadFromUri(URI moduleURI, URI myURI, Object validator) throws IOException, URISyntaxException {
        if (OPENWGA_COMMONJS_SCHEME.equals(moduleURI.getScheme())) {
            try {
                return loadOpenwgaModule(moduleURI, myURI, validator);
            }
            catch (WGException e) {
                throw new IOException(e);
            }    
        }
        else {
            return super.loadFromUri(moduleURI, myURI, validator);
        }
        
    }

    private ModuleSource loadOpenwgaModule(URI moduleURI, URI myURI, Object validator, String commonJsFolder) throws WGException, URISyntaxException, WGAPIException, IOException {
    
        // Fetching the design context for the addressed apps CommonJS folder
        Design design = _wga.design(moduleURI.getHost()).resolve(commonJsFolder);
        
        // Extract module folder and file from the URI path
        String moduleFolder = null;
        String moduleFile = moduleURI.getPath() + ".js";
        int lastSlash = moduleFile.lastIndexOf("/");
        if (lastSlash != -1) {
            moduleFolder = moduleFile.substring(0, lastSlash);        
            moduleFile =  moduleFile.substring(lastSlash + 1);
        }
        if (!WGUtils.isEmpty(moduleFolder)) {
            design = design.resolve(".::" + moduleFolder.replace("/", ":"));
        }

        // Load doc and file
        WGFileContainer fileContainer = design.getFileContainer();
        if (fileContainer == null || !fileContainer.hasFile(moduleFile)) {
            return null;
        }
        
        if (validator instanceof Date && !fileContainer.getLastModified().after((Date) validator)) {
            return ModuleSourceProvider.NOT_MODIFIED;
        }
        
        ModuleSource moduleSource = new ModuleSource(fileContainer.getFileText(moduleFile, "UTF-8"), null, moduleURI, myURI, fileContainer.getLastModified());
        return moduleSource;

    }

    

}
