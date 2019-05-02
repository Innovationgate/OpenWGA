package de.innovationgate.wgpublisher.sessions;


import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.wga.config.HttpSessionManagerConfiguration;

public class InMemoryHttpSessionManager extends AbstractWGAHttpSessionManager {
    
    public static final String OPTION_JVMROUTE = "jvmRoute";
    
    private Map<String, WGAHttpSession> _sessions = new ConcurrentHashMap<String, WGAHttpSession>();
    
    private SessionCleanupTask _sessionCleanupTask;
    private Timer _sessionCleanupTimer;

    private class SessionCleanupTask extends TimerTask {

        @Override
        public void run() {
            try {
                if (isDebug()) {
                    getCore().getLog().info("running InMemoryHttpSessionManager.SessionCleanupTask");
                }
                Iterator<WGAHttpSession> sessions = _sessions.values().iterator();
                while (sessions.hasNext()) {
                    WGAHttpSession session = sessions.next();
                    if (!isValid(session)) {
                        if (isDebug()) {
                            getCore().getLog().info("InMemoryHttpSessionManager.SessionCleanupTask invalidating session '" + session.getId()  + "' - default cleanup");
                        }
                        invalidateSession(session);
                        sessions.remove();
                    } 
                    /*
                     * The following code doen't make sense to me.
                     * The consequence is that the first request in a new session must be finished in one minute. Otherwise we get errors.
                    else if (session.isNew() && (System.currentTimeMillis() - session.getCreationTime()) >= 1000*10) {
                        // invalidate not joined sessions early
                        if (isDebug()) {
                            getCore().getLog().info("InMemoryHttpSessionManager.SessionCleanupTask invalidating session '" + session.getId()  + "' - early cleanup");
                        }
                        invalidateSession(session);
                        sessions.remove();
                    }
                    */
                }
            } catch (Throwable e) {
                getCore().getLog().error("InMemoryHttpSessionManager.SessionCleanupTask failed.", e);
            }
        }
        
    }

    public InMemoryHttpSessionManager() {
        _sessionCleanupTask = new SessionCleanupTask();
        _sessionCleanupTimer = new Timer("InMemoryHttpSessionManager.SessionCleanupTask");
        _sessionCleanupTimer.schedule(_sessionCleanupTask, 1000*30, 1000*30);
    }

    @Override
    public WGAHttpSession getSession(String id) {
        WGAHttpSession session = _sessions.get(id);
        if (session != null) {
            if (isValid(session)) {
                return session;
            } else {
                invalidateSession(session);
                unmapSession(session);
            }
        }
        return null;
    }

    @Override
    public WGAHttpSession createSession() {
        String id = UIDGenerator.generateUID();
//        if (_jvmRoute != null) {
//            id += "." + _jvmRoute;
//        }
        
        WGAHttpSession session = new WGAHttpSession(getContext(), id);
        mapSession(session);
        fireSessionCreatedEvent(session);
        return session;
    }
    
    private void mapSession(WGAHttpSession session) {
        if (session != null) {
            String id = session.getId();
            if (id != null) {
//                if (id.contains(".")) {
//                    // cut of jvmroute - we store sessions by id only in backend
//                    id = id.split("\\.")[0];
//                }
                _sessions.put(id, session);
            }
        }
    }

    private void unmapSession(WGAHttpSession session) {
        if (session != null) {
            String id = session.getId();
            if (id != null) {
//                if (id.contains(".")) {
//                    // cut of jvmroute - we store sessions by id only in backend
//                    id = id.split("\\.")[0];
//                }
                _sessions.remove(id);
            }
        }
    }
    

    @Override
    public void shutdown() {
        if (_sessionCleanupTimer != null) {
            _sessionCleanupTimer.cancel();
        }    
    }

    @Override
    public void requestFinished(HttpServletRequest request, HttpServletResponse response) {        
    }
    
    private void createSessionAttribInformation(String attrib, Object value, StringBuffer info) {
        if (value != null) {
            info.append(attrib + " (" + value.getClass().getName() + ")");
            info.append("\n");
            if (value instanceof Collection) {                
                Iterator it = ((Collection) value).iterator();
                while (it.hasNext()) {
                    createSessionAttribInformation("\tEntry:", it.next(), info);
                }                
            } else if (value instanceof Map) {
                Iterator it = ((Map) value).values().iterator();
                while (it.hasNext()) {
                    createSessionAttribInformation("\tEntry:", it.next(), info);                    
                }
            }
        } else {
            info.append(attrib + " (NULL)");
            info.append("\n");
        }
        
    }

    protected void updateSession(WGAHttpSession session) {
        // inject session context
        session.setContext(getContext());
        
//        // we might have to change jvmroute on session id
//        String id = session.getId();
//        if (id.contains(".")) {
//            id = id.split("\\.")[0];
//        }
//        if (_jvmRoute != null) {
//            id += "." + _jvmRoute;
//        }
//        session.setId(id);        
        
        mapSession(session);         
    }

    @Override
    public void startup(HttpSessionManagerConfiguration config) {
        long start = System.currentTimeMillis();
        getCore().logCategoryInfo("InMemoryHttpSessionManager.startup", 2);
        long end = System.currentTimeMillis();
        getCore().getLog().info("InMemoryHttpSessionManager.startup finished in " + (end - start) + " ms.");
    }
    
    protected Map<String, WGAHttpSession> getSessions() {
        return _sessions;
    }
}
