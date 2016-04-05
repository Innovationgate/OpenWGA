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

package de.innovationgate.wgpublisher.cluster;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import de.innovationgate.wgpublisher.cluster.tasks.WriteResourceTask;


public class ClusterResourceOutputStream extends OutputStream {
    
    private static final int WRITE_TIMEOUT = 5000;
    
    private int _resourceOffset = 0;
    private int _chunkSize = 1024 * 8; // 8kb

    private ClusterService _clusterService;
    private ClusterMember _member;
    private URI _resource;

    private ByteArrayOutputStream _buffer =  new ByteArrayOutputStream();
    
    public ClusterResourceOutputStream(ClusterService clusterService, ClusterMember member, URI resource) {
       _clusterService = clusterService;
       _member = member;
       _resource = resource;
    }



    @Override
    public void write(int b) throws IOException { 
        _buffer.write(b);        
        if (_buffer.size() >= _chunkSize) {
            flush();
        }        
    }
    

    @Override
    public void flush() throws IOException {
        super.flush();
        WriteResourceTask task = new WriteResourceTask(_resource, _buffer.toByteArray(), _resourceOffset > 0);
        Future<Boolean> future = _clusterService.submitTo(_member, task);
        try {
            future.get(WRITE_TIMEOUT, TimeUnit.MILLISECONDS);
            _resourceOffset += _buffer.size();
            _buffer = new ByteArrayOutputStream();
        }
        catch (Exception e) {
           throw new IOException(e);
        }
    }



    @Override
    public void close() throws IOException {
        flush();
        super.close();
    }
    
    

}
