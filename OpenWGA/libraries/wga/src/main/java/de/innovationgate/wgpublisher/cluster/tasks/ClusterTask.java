package de.innovationgate.wgpublisher.cluster.tasks;

import java.io.Serializable;
import java.util.concurrent.Callable;


public abstract class ClusterTask<V> implements Callable<V>, Serializable {

    private static final long serialVersionUID = 1L;
    
    private ClusterTaskContext _context = new ClusterTaskContext();

    @Override
    public final V call() throws Exception {
        String caller = "unknown";
        if (_context.getCaller() != null) {
            caller = _context.getCaller().getUID();
        }
        if (_context.getWGACore().getClusterService().isDebug()) {
            _context.getWGACore().getLog().info("Running cluster task '" + this.getClass().getName() + "' for '" + caller + "'");
        }
        return execute();
    }
    
    public abstract V execute() throws Exception;

    public ClusterTaskContext getContext() {
        return _context;
    }
}
