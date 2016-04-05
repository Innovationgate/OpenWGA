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

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import de.innovationgate.wgpublisher.cluster.tasks.ReadResourceTask;


public class ClusterResourceInputStream extends InputStream {
    
    private static final int READ_TIMEOUT = 5000;
    
    private long _resourceOffset = 0;
    private int _chunkSize = 1024 * 8; // 8kb
    
    private byte[] _chunk;
    private int _chunkIndex = 0;
    private ClusterService _clusterService;
    private ClusterMember _member;
    private URI _resource;
    
    public ClusterResourceInputStream(ClusterService clusterService, ClusterMember member, URI resource) {
       _clusterService = clusterService;
       _member = member;
       _resource = resource;
    }


    @Override
    public int read() throws IOException {
        if (_chunk == null || _chunkIndex >= _chunkSize) {
            try {
                fetchChunk();
            }
            catch (Exception e) {
                throw new IOException(e);
            }
            if (_chunk == null || _chunk.length <= 0) {
                return -1;
            }            
        }
        
        if (_chunkIndex < _chunk.length) {
            return _chunk[_chunkIndex++];
        }
        return -1;
    }
    
    private void fetchChunk() throws InterruptedException, ExecutionException, TimeoutException {
        _chunkIndex = 0;
        _chunk = null;
        ReadResourceTask task = new ReadResourceTask(_resource, _resourceOffset, _chunkSize);
        Future<byte[]> future = _clusterService.submitTo(_member, task);
        if (future != null) {
            _chunk = future.get(READ_TIMEOUT, TimeUnit.MILLISECONDS);
            if (_chunk != null) {
                _resourceOffset += _chunk.length;
            }            
        }
    }

}
