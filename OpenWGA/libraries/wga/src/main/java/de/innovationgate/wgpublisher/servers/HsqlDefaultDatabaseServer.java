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

package de.innovationgate.wgpublisher.servers;

import java.io.File;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.hsql.HsqlDatabaseServer;
import de.innovationgate.webgate.api.hsql.WGDatabaseImpl;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.RegistryAwareModule;
import de.innovationgate.wgpublisher.WGACore;

public class HsqlDefaultDatabaseServer extends HsqlDatabaseServer implements RegistryAwareModule {
    
    private ModuleRegistry _registry;

    @Override
    public File getDatabaseDirectory(String prefix) throws WGBackendException {
        
        WGACore wgaCore = (WGACore) _registry.getContextObjects().get(WGACore.class);
        File dir = new File(wgaCore.getWgaDataDir(), "#dbs" + prefix);
        return dir;

        
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }
    

}
