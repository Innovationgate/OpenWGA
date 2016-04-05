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

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;
import de.innovationgate.wgpublisher.design.OverlayDesignProvider;
import de.innovationgate.wgpublisher.design.WGADesignManager;

public class DesignConsumerDisconnectListener implements WGACoreEventListener {

    public void contentStoreConnected(WGACoreEvent event) {
    }

    public void contentStoreDisconnected(WGACoreEvent event) {
        
        WGDatabase db = event.getDatabase();
        WGADesignManager designManager = event.getCore().getDesignManager();

        for (WGDatabase consumerDb : event.getCore().getContentdbs().values()) {
            
            try {
                WGDesignProvider provider = consumerDb.getDesignProvider();
                if (designManager.isDesignConsumerOfDatabase(provider, db)) {
                    event.getCore().removeContentDB(consumerDb.getDbReference());
                }
            }
            catch (WGAPIException e) {
                event.getCore().getLog().error("Exception determining design consumers of disconnected app", e);
            }
            
        }

    }

    public void startupPreConnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }
    
    

}
