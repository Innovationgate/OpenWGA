package de.innovationgate.wgpublisher.cluster.tasks;

import java.io.Serializable;

import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cluster.ClusterMember;

public class ClusterTaskContext implements Serializable {
    
    private static final long serialVersionUID = 1L;
    
    private transient WGACore _core;
    
    private ClusterMember _caller;
    
    public WGACore getWGACore() {
        return _core;
    }
    public void setWGACore(WGACore core) {
        _core = core;
    }
    
    public ClusterMember getCaller() {
        return _caller;
    }
    public void setCaller(ClusterMember caller) {
        _caller = caller;
    }

}
