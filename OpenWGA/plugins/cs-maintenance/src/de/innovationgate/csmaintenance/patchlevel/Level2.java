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

package de.innovationgate.csmaintenance.patchlevel;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import de.innovationgate.csmaintenance.CS5Patch;
import de.innovationgate.csmaintenance.CS5PatchLevel;
import de.innovationgate.csmaintenance.SimpleCS5Patch;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;

public class Level2 implements CS5PatchLevel {
    
    Map<String,CS5Patch> _patchLevel = new HashMap<String,CS5Patch>();
    
    public Level2(WGDatabase patchDB) throws WGAPIException, IOException {

        WGFileContainer patchesFC = patchDB.getFileContainer("patchlevel2");
        _patchLevel.put("jdbc/wgacontentstore/hsql", new SimpleCS5Patch(WGUtils.readString(patchesFC.getFileText("patch_hsqldb.sql"))));
        _patchLevel.put("jdbc/wgacontentstore/mysql", new SimpleCS5Patch(WGUtils.readString(patchesFC.getFileText("patch_mysql.sql"))));
        _patchLevel.put("jdbc/wgacontentstore/db2", new SimpleCS5Patch(WGUtils.readString(patchesFC.getFileText("patch_db2.sql"))));
        _patchLevel.put("jdbc/wgacontentstore/mssql", new SimpleCS5Patch(WGUtils.readString(patchesFC.getFileText("patch_mssql.sql"))));
        _patchLevel.put("jdbc/wgacontentstore/oracle", new SimpleCS5Patch(WGUtils.readString(patchesFC.getFileText("patch_oracle.sql"))));
        _patchLevel.put("jdbc/wgacontentstore/postgresql", new SimpleCS5Patch(WGUtils.readString(patchesFC.getFileText("patch_postgresql.sql"))));
        
    }

    public CS5Patch getPatch(WGDatabase db) {
        return _patchLevel.get(db.getTypeName());
    }
    

    @Override
    public Version getWGAIntroductionVersion() {
        return new Version(5,3,0);
    }
    
    @Override
    public String getDescription() {
        return "Support for content relation groups (#00000645)";
    }
    
    @Override
    public int getLevel() {
        return 2;
    }

}
