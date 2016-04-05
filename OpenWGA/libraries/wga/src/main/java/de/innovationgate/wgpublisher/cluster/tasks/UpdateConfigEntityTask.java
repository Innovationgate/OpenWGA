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

package de.innovationgate.wgpublisher.cluster.tasks;

import de.innovationgate.wga.config.Administrator;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.config.Domain;
import de.innovationgate.wga.config.IdentifiableConfigBean;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACore;

/**
 * adds or updates the given config entity
 */
public class UpdateConfigEntityTask extends ClusterTask<Boolean> {


    private static final long serialVersionUID = 1L;
    
    private IdentifiableConfigBean _entity = null;
    
    public UpdateConfigEntityTask(IdentifiableConfigBean entity) {
        _entity = entity;
    }
    
    @Override
    public Boolean execute() throws Exception {
        WGACore core = getContext().getWGACore();
        try {         
            core.getLog().info("Perfoming config update due to cluster request from '"  + getContext().getCaller() + "'");
            if (_entity instanceof ContentDatabase || _entity instanceof Domain || 
                    _entity instanceof DatabaseServer || _entity instanceof Administrator) {
                WGAConfiguration config = core.getWgaConfiguration().clone();
                IdentifiableConfigBean existingEntry = config.getByUid(_entity.getUid()); 
                if (existingEntry != null) {
                    core.getLog().info("removing existing config entity '" + _entity.getUid() + "' - " + _entity.getClass().getName());
                    config.removeEntity(_entity.getUid());
                }
                
                
                if (_entity instanceof ContentDatabase) {
                    config.add((ContentDatabase)_entity);
                } else if (_entity instanceof Domain) {
                    config.add((Domain)_entity);
                } else if (_entity instanceof DatabaseServer) {
                    config.add((DatabaseServer)_entity);
                } else if (_entity instanceof Administrator) {
                    config.add((Administrator)_entity);
                }
                core.getLog().info("new config entity '" + _entity.getUid() + "' - " + _entity.getClass().getName() + " added");
                
                core.saveWgaConfiguration(config);
                return true;
            } else {
                throw new IllegalArgumentException("Config updates of type '" + _entity.getClass().getName() + "' not supported.");
            }
        } catch (Exception e) {
            core.getLog().error("UpdateConfigEntityTask failed.", e);
            throw e;
        }
    }


}
