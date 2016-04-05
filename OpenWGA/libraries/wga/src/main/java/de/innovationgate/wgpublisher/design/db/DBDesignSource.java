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

package de.innovationgate.wgpublisher.design.db;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.LocalizedInformation;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wgpublisher.SystemContainerManager;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.WGADesign;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.WGADesignRetrievalException;
import de.innovationgate.wgpublisher.design.WGADesignSource;
import de.innovationgate.wgpublisher.design.WGADesignConfigurationException;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class DBDesignSource implements WGADesignSource {
    
    public static DesignReference createDefaultDesignReference(WGDocument doc) {
        return new DesignReference(Constants.DESIGNCOL_DB, doc.getDatabase().getDbReference(), doc.getDocumentKey());
    }

    private WGACore _core;
    private String _name;
    private LocalizedInformation _locInfo;

    public List<String> getDesignNames() {
        
        
        List<String> designs = new ArrayList<String>();
        Iterator<WGDatabase> dbs = _core.getContentdbs().values().iterator();
        while (dbs.hasNext()) {
            WGDatabase database = (WGDatabase) dbs.next();
            if (database.getRoles().contains(WGDatabase.ROLE_DESIGN) && !(database.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) && database.getDesignProvider() == null) {
                designs.add(database.getDbReference());
            }
        }
        
        return designs;
        
    }

    public void init(WGACore core, String name, LocalizedInformation locInfo, Map<String, String> options) {
        _core = core;
        _name = name;
        _locInfo = locInfo;
        
        _core.addEventListener(new DesignConsumerDisconnectListener());
    }

    public String getName() {
        return _name;
    }

    public void applyDesign(WGADesign design, WGDatabase db, Map<String, String> options) throws WGADesignConfigurationException {

        try {
            
            DBDesignProvider designProvider = createDesignProvider(design, db, options);
            db.setDesignProvider(designProvider);
            db.setAllowDesignModification(false);
        }
        catch (Exception e) {
            throw new WGADesignConfigurationException("Exception applying design to web application " + db.getDbReference(), e);
        }
        
        
    }

    public DBDesignProvider createDesignProvider(WGADesign design, WGDatabase db, Map<String, String> options) throws WGADesignConfigurationException {
        WGDatabase sourcedb = _core.getContentdbs().get(design.getName());
        
        // Can only validate database if already connected - If null the db may get connected later
        if (sourcedb != null) {
            if (sourcedb.getDesignProvider() != null) {
                throw new WGADesignConfigurationException("Database " + design.getName() + " itself uses a design provider and cannot be used as design source");
            }
            if (!sourcedb.getRoles().contains(WGDatabase.ROLE_DESIGN)) {
                throw new WGADesignConfigurationException("Database " + design.getName() + " contains no design and cannot be used as design source");
            }
        }
        
        _core.getLog().info("Web application " + db.getDbReference() + " uses design of database " + design.getName());
        DBDesignProvider designProvider = new DBDesignProvider(design.createDesignReference(), _core, db, design.getName(), options);
        return designProvider;
    }

    public String getDescription(Locale locale) {
        return _locInfo.getDescription(locale);
    }
    
    public String getTitle(Locale locale) {
        return _locInfo.getTitle(locale);
    }

    public void createDesign(String designName) throws WGNotSupportedException {
        throw new WGNotSupportedException("Creating designs is not supported for database design sources");
    }

    public boolean isDesignCreatable() {
        return false;
    }

    public Class getDesignProviderClass() {
        return DBDesignProvider.class;
    }

    public WGADesign getDesign(String name) throws WGADesignRetrievalException {
        WGDatabase database = _core.getContentdbs().get(name);
        if (database == null || !database.getRoles().contains(WGDatabase.ROLE_DESIGN) || database.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX) || database.getDesignProvider() != null) {
            return null;
        }
        
        WGADesign design = new WGADesign();
        design.setSource(this);
        design.setName(database.getDbReference());
        design.setTitle(database.getTitle());
        design.setDescription("Design from database \"" + database.getTitle() + "\"");
        
        if (!database.isSessionOpen()) {
            try {
                database.openSession();
                WGFileContainer con = database.getFileContainer("system");
                if (con != null && con.getFileNames().contains(SystemContainerManager.CSCONFIG_FILE)) {
                    InputStream in = con.getFileData(SystemContainerManager.CSCONFIG_FILE);
                    design.setConfig(CSConfig.load(in, true));
                }
            }
            catch (Exception e) {
                _core.getLog().error("Exception reading design config for database " + database.getDbReference(), e);
            }
        }
        
        return design;
 
    }
}
