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

package de.innovationgate.wgpublisher.scheduler;

import java.util.Date;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAbstractResultSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wga.config.ClusterConfiguration;
import de.innovationgate.wgpublisher.WGACore;

public class PendingReleaseTask extends Task {

    @Override
    public void configure(WGACore core) throws ConfigurationException {
    }

    @Override
    public void execute(JobContext jobContext) throws TaskException {
        
        if (!jobContext.getWgaCore().isRunSingleNodeFunctionalities()) {
            return;
        }

        for (WGDatabase db : jobContext.getWgaCore().getContentdbs().values()) {
            
            
            
            try {
                
                if (!db.isConnected() || !db.isReady()) {
                    continue;
                }
                
                if (db.getContentStoreVersion() == WGDatabase.CSVERSION_NO_CONTENTSTORE) {
                    continue;
                }

                db.openSession();
                try {
                    db.releasePendingContents();                    
                }
                finally {
                    db.closeSession();
                }
            }
            catch (Throwable e) {
                jobContext.getLog().error("Exception retrieving pending release documents from app '" + db.getDbReference(), e);
            }
            
        }
        

    }

}
