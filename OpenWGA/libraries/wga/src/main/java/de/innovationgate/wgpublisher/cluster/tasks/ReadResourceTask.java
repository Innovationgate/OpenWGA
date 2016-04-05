package de.innovationgate.wgpublisher.cluster.tasks;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URI;

/**
 * internal task for cluster input stream implementation
 */
public class ReadResourceTask extends ClusterTask<byte[]> {

    private static final long serialVersionUID = 1L;

    private URI _resource;

    private long _offset = 0;

    private int _length = 0;
    
    public ReadResourceTask(URI resource, long offset, int chunkSize) {
        _resource = resource;
        _offset = offset;
        _length = chunkSize;
    }

    @Override
    public byte[] execute() throws Exception {        
        InputStream in = null;
        try {
            in = _resource.toURL().openStream();
            byte[] buffer = new byte[_length]; 
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            in.skip(_offset);
            int len = in.read(buffer);
            if (len != -1) {
                out.write(buffer, 0, len);
            }        
            out.flush();
            return out.toByteArray();
        } finally {
            if (in != null) {
                in.close();
            }
        }
    }

}
