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

import java.net.URI;

import de.innovationgate.utils.WGUtils;

/**
 * creates an MD5 checksum for the given resource
 */
public class ResourceMD5ChecksumTask extends ClusterTask<String> {

    private static final long serialVersionUID = 1L;
    
    private URI _resource;
    
    public ResourceMD5ChecksumTask(URI resource) {
        _resource = resource;
    }
    
    @Override
    public String execute() throws Exception {
        return WGUtils.createMD5HEX(_resource.toURL().openStream());
    }
        
}
