package de.innovationgate.wgpublisher.cluster.tasks;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.net.URI;

/**
 * internal task for cluster output stream implementation
 */
public class WriteResourceTask extends ClusterTask<Boolean> {

    private static final long serialVersionUID = 1L;

    private URI _resource;

    private boolean _append = false;

    private byte[] _chunk;
    
    public WriteResourceTask(URI resource, byte[] chunk, boolean append) {
        _resource = resource;
        _chunk = chunk;
        _append = append;
    }

    @Override
    public Boolean execute() throws Exception { 
        if (_resource != null && !_resource.toURL().getProtocol().equalsIgnoreCase("file")) {
            throw new IllegalArgumentException("Unsupported protocol '" + _resource.toURL().getProtocol() + "'.");
        }
        
        OutputStream out = null;
        try {
            File target = new File(_resource);
            out = new FileOutputStream(target, _append);
            out.write(_chunk);              
            out.flush();
        } finally {
            if (out != null) {
                out.close();
            }
        }
        return true;
        
    }

}
