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

package de.innovationgate.wgpublisher.events;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.ApplicationEventBuilder;
import de.innovationgate.wga.server.api.DescriptificationConfig;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.events.EventManager.EventResultConsumer;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;

public class ApplicationEvent extends Event implements Serializable {
    
    public static class Builder implements ApplicationEventBuilder {
        
        private ApplicationEventPath _eventPath;
        private App _app;
        private Map<Object,Object> _params = new HashMap<Object, Object>();
        private WGA _wga;
        private List<String> _eventsToFireOnResult = new ArrayList<>();
        private String _windowId;
        
        public Builder(WGA wga, App app, ApplicationEventPath eventPath) {
            _wga = Unlocker.unlock(wga);
            _app = app;
            _eventPath = eventPath;

            TMLPageImpl page = (TMLPageImpl) _wga.tmlPage();
            PageConnection pageConn;
			try {
				pageConn = page.getPageConnection(false);
	            if(pageConn!=null)
	            	_windowId = pageConn.getWindowId();
			} catch (WGException e) {}
        }
        
        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.events.ApplicationEventBuilder#params(java.util.Map)
         */
        @Override
        public ApplicationEventBuilder params(Map<Object,Object> params) {
            _params.putAll(params);
            return this;
        }
        
        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.events.ApplicationEventBuilder#param(java.lang.Object, java.lang.Object)
         */
        @Override
        public Builder param(Object key, Object value) {
            _params.put(key, value);
            return this;
        }
        
        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.events.ApplicationEventBuilder#onResultFire(java.lang.String)
         */
        @Override
        public ApplicationEventBuilder onResultFire(String eventName) {
            _eventsToFireOnResult.add(eventName);
            return this;
        }
        
        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.events.ApplicationEventBuilder#fire()
         */
        @Override
        public void fire() throws WGException {
            doFire(Event.Scope.CLUSTER);
        }
        
        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.events.ApplicationEventBuilder#fireOnLocalServer()
         */
        @Override
        public void fireOnLocalServer() throws WGException {
            doFire(Event.Scope.SERVER);
        }
        
        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.events.ApplicationEventBuilder#fireOnSession()
         */
        @Override
        public void fireOnSession() throws WGException {
            doFire(Event.Scope.SESSION);
        }
        
        private void doFire(final Event.Scope eventScope) throws WGException {
            
            String sessionId = _wga.session().isAvailable() ? _wga.session().getJavaHttpSession().getId() : null;
            Map descriptifiedParams = _wga.tmlscript().descriptify(_params, Map.class, new DescriptificationConfig().convertObjectsToJSON());
            @SuppressWarnings("unchecked")
            final ApplicationEvent event = new ApplicationEvent(_eventPath.getApplicationEventHierarchy(), _app.getDbKey(), _app.db().getRevisionObject(), sessionId, descriptifiedParams, eventScope);
          	event.setSource(_windowId);
            
            Callable<Object> r = new Callable<Object>() {
                @Override
                public Object call() throws Exception {
                    
                    EventResultConsumer consumer = null;
                    if (_eventsToFireOnResult.size() > 0) {
                        consumer = new EventResultConsumer() {
                            @Override
                            public void consume(Event event, List<Object> results) throws WGException {
                                for (String eventName : _eventsToFireOnResult) {
                                    ((Builder) WGA.get().app(event.getDatabaseKey()).createEvent(eventName).param("results", results)).doFire(eventScope);
                                }
                            }
                        };
                        
                    }
                    
                    _wga.getCore().getEventManager().executeAsyncEvent(_eventPath, event, consumer);
                    return null;
                }
            };
            if (_app.db().isSessionOpen()) {
                _app.db().getSessionContext().addAfterTransactionTask(r);
            }
            else {
                try {
                    r.call();
                }
                catch (WGException e) {
                    throw e;
                }
                catch (Exception e) {
                    throw new WGException("Exception firing application event", e);
                }
            }
        }
        
    }
    
    private String _dbKey;
    private Map<Object,Object> _params;
    private EventPathEntry[] _path;
    private Scope _eventScope;
    
    public EventPathEntry[] getPath() {
        return _path;
    }

	@Override
    public Scope getScope() {
        return _eventScope;
    }

    public Map<Object, Object> getParams() {
        return _params;
    }

    public ApplicationEvent(EventPathEntry[] eventPath, String dbKey, WGDatabaseRevision revision, String sessionId, Map<Object,Object> params, Event.Scope eventScope) {
        super(revision, sessionId);
        _dbKey = dbKey;
        _params = params;
        _eventScope = eventScope;
        _path = eventPath;
    }

    public String getDbKey() {
        return _dbKey;
    }
    
    @Override
    public String getDatabaseKey() {
        return _dbKey;
    }
    
    @Override
    public ApplicationEvent createLocalDelegate() {
        
        @SuppressWarnings("unchecked")
        Map<Object,Object> clonedParams = (Map<Object, Object>) XStreamUtils.clone(_params);
        ApplicationEvent localEv = new ApplicationEvent(_path, _dbKey, getDatabaseRevision(), getSessionId(), clonedParams, Event.Scope.SERVER);
        return localEv;
        
        
    }


}
