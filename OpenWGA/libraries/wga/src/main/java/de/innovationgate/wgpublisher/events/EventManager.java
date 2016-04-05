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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentEvent;
import de.innovationgate.webgate.api.WGContentEventListener;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseConnectListener;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.workflow.WGWorkflowEventListener;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cluster.ClusterService;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class EventManager implements WGDatabaseEventListener, WGContentEventListener, WGWorkflowEventListener, WGDatabaseConnectListener {
	
    class EventReceiverNode extends ConcurrentHashMap<EventPathEntry,EventReceiverNode> {
        
        private Queue<EventReceiver> _eventReceivers = new ConcurrentLinkedQueue<EventReceiver>();

        public Queue<EventReceiver> getEventReceivers() {
            return _eventReceivers;
        }
        
    }
    
    public interface EventResultConsumer {
        
        public void consume(Event event, List<Object> results) throws WGException;
        
        
    }
    
    public static final Integer DEFAULT_THREADPOOLSIZE = 10;
    
	private static final String EVENTNAME_CONTENTHASBEENSAVED = "contenthasbeensaved";
	private static final String EVENTNAME_CONTENTHASBEENMOVED = "contenthasbeenmoved";
    private static final String EVENTNAME_CONTENTHASBEENDELETED = "contenthasbeendeleted";
    public static final String EVENTNAME_WORKFLOWMAIL = "workflowmail";
    public static final String EVENTNAME_STATUSCHANGE = "statuschange";
	public static final String EVENTNAME_SAVECONTENT = "savecontent";
	public static final String EVENTNAME_CREATECONTENT = "createcontent";
	
	private AtomicLong _thrownAsyncEventCount = new AtomicLong(Long.MIN_VALUE);
	private AtomicLong _processedAsyncEventIndex = new AtomicLong(Long.MIN_VALUE);
	
	private ThreadLocal<Event> _currentExecutedEvent = new ThreadLocal<Event>();
	private ThreadLocal<EventPath> _currentExecutedEventPath = new ThreadLocal<EventPath>();
	
	public static final Logger LOG = Logger.getLogger("wga.events");

    private static final int EVENT_MAX_CASCADING_LEVEL = 5;
    private WGACore _core;
    private ThreadPoolExecutor _executorService;
    
    private EventReceiverNode _eventReveicersRoot = new EventReceiverNode();
	
	public EventManager(WGACore core) {
		_core = core;
	}

    public void initExecutors() {
        Integer poolSize = (Integer) _core.getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_EVENTMANAGER_THREADPOOLSIZE);
        if (_executorService == null || _executorService.getMaximumPoolSize() != poolSize) {
            ThreadPoolExecutor oldExecutorService = _executorService;
            _executorService = (ThreadPoolExecutor) Executors.newFixedThreadPool(poolSize);
            if (oldExecutorService != null) {
                oldExecutorService.shutdown();
            }
        }
    }
    
    public void shutdown() {
        try {
            _executorService.shutdown();
            _executorService.awaitTermination(60, TimeUnit.SECONDS);
        }
        catch (InterruptedException e) {
            _core.getLog().error("Exception shutting down event manager", e);
        }
    }
	
    public void removeDatabaseEvents(String key) {
        _eventReveicersRoot.remove(new EventPathEntry(EventPathEntry.ENTRYKEY_DB, key));
    }
    
	public void removeContentTypeEvents(String key) {
		
		EventReceiverNode databaseNode = getOrCreateNode(_eventReveicersRoot, new EventPathEntry(EventPathEntry.ENTRYKEY_DB, key));
		EventReceiverNode ctEventNode = getOrCreateNode(databaseNode, new EventPathEntry(EventPathEntry.ENTRYKEY_TYPE, ContentTypeEventPath.EVENTTYPE));
		removeContentTypeEventsRecursion(ctEventNode);
		
	}
	
	private void removeContentTypeEventsRecursion(EventReceiverNode ctEventNode) {

	    Iterator<EventReceiver> receivers = ctEventNode.getEventReceivers().iterator();
	    while (receivers.hasNext()) {
	        EventReceiver receiver = receivers.next();
	        if (receiver instanceof ContentTypeEventScript) {
	            receivers.remove();
	        }
	    }
	    
	    for (EventReceiverNode subNodes : ctEventNode.values()) {
	        removeContentTypeEventsRecursion(subNodes);
	    }
	    
        
    }

    public EventPathEntry[] registerEventReceiver(EventPath eventPath, EventReceiver eventReceiver) {
		
		EventPathEntry[] hierarchy = eventPath.getEventHierarchy();
		mapEventReceiver(_eventReveicersRoot, hierarchy, eventReceiver, 0);
		return hierarchy;
		
	}

	/**
	 * @param eventPath
	 * @param eventScript
	 * @param i
	 */
	private void mapEventReceiver(EventReceiverNode refMap, EventPathEntry[] hierarchy, EventReceiver eventReceiver, int i) {
		
		EventPathEntry  newEntry = hierarchy[i];
		EventReceiverNode nextMap = getOrCreateNode(refMap, newEntry);
		
		if ((i+1) < hierarchy.length) {
			mapEventReceiver(nextMap, hierarchy, eventReceiver, i+1);
		}
		else {
			nextMap.getEventReceivers().add(eventReceiver);
		}
	}

    public EventReceiverNode getOrCreateNode(EventReceiverNode parentNode, EventPathEntry entry) {
        EventReceiverNode nextMap = (EventReceiverNode) parentNode.get(entry);
		if (nextMap == null) {
			nextMap = new EventReceiverNode();
			parentNode.put(entry, nextMap);
		}
        return nextMap;
    }
	
	public List<Object> executeEvent(EventPath eventPath, Event event, boolean failOnException) throws WGException {
		List<Object> results = new ArrayList<Object>();
		findEventReceivers(_eventReveicersRoot, eventPath.getEventHierarchy(), 0, event, results, failOnException);
		return results;
	}
	
	public void executeAsyncEvent(final EventPath eventPath, final Event event) throws WGException {
	    executeAsyncEvent(eventPath, event, null);
	}
	
	public void executeAsyncEvent(final EventPath eventPath, final Event event, final EventResultConsumer resultConsumer) throws WGException {
	    
	    if (LOG.isDebugEnabled()) {
	        String msg = "Executing event " + event.getClass().getName() + " for path " + eventPath.toString();
	        if (_currentExecutedEvent.get() != null) {
	            msg += ", caused by event " + _currentExecutedEvent.get().getClass().getName() + " for path " + _currentExecutedEventPath.get().toString() + " at cascading level " + event.getCascadingLevel(); 
	        }
	        LOG.debug(msg);
	    }
	    
	    final List<Future<List<Object>>> eventFutures = new ArrayList<Future<List<Object>>>();
        if (event.getScope() == Event.Scope.CLUSTER) {
            ClusterService service = _core.getClusterService();
            if (service != null && service.isInitialized()) {
                Collection<? extends Future<List<Object>>> remoteFutures = (Collection<? extends Future<List<Object>>>) service.submitToOthers(new ExecuteRemoteEventTask(eventPath, event.createLocalDelegate(), resultConsumer != null)).values();
                eventFutures.addAll(remoteFutures);
            }
        }
        
        eventFutures.add(executeLocalAsyncEvent(eventPath, event));
        if (resultConsumer != null) {
            _executorService.submit(new Runnable() {

                @Override
                public void run() {

                    
                    _currentExecutedEvent.set(event);
                    _currentExecutedEventPath.set(eventPath);
                    try {
                        
                        // Collect results, wait eventually
                        List<Object> allResults = new ArrayList<Object>();
                        for (Future<List<Object>> f : eventFutures) {
                            allResults.addAll(f.get());
                        }
                        
                        // Call consumer
                        WGA wga = WGA.get(WGACore.INSTANCE);
                        resultConsumer.consume(event, allResults);
                        
                    }
                    catch (Throwable t) {
                        LOG.error("Exception waiting for async event completion", t);
                    }

                    finally {
                        _currentExecutedEvent.remove();
                        _currentExecutedEventPath.remove();
                        WGFactory.getInstance().closeSessions();
                    }
                    
                }
                
            });
            
        }
        
	}

    protected Future<List<Object>> executeLocalAsyncEvent(final EventPath eventPath, final Event event) throws MaxEventCascadingLevelException {
        final Event parentEvent = _currentExecutedEvent.get();
        if (parentEvent != null) {
            event.addCascadingEventPath(parentEvent.getCascadingEventsPath(), eventPath);
            if (event.getCascadingLevel() > EVENT_MAX_CASCADING_LEVEL) {
                throw new MaxEventCascadingLevelException("Cannot throw event because the max cascading level of " + EVENT_MAX_CASCADING_LEVEL + " has been reached: " + WGUtils.serializeCollection(event.getCascadingEventsPath(), " -> "));
            }
        }
	    
        final long eventIndex = _thrownAsyncEventCount.incrementAndGet();
        final long throwingTime = System.currentTimeMillis();
	    return _executorService.submit(new Callable<List<Object>>() {

            @Override
            public List<Object> call() {
                try {
                    _currentExecutedEvent.set(event);
                    _currentExecutedEventPath.set(eventPath);
                    return executeEvent(eventPath, event, false);
                }
                catch (Throwable t) {
                    LOG.error("Exception executing async event", t);
                    return Collections.emptyList();
                }
                finally {
                    _currentExecutedEvent.remove();
                    _currentExecutedEventPath.remove();
                    long executionTime = System.currentTimeMillis() - throwingTime; 
                    if (executionTime > 5000) {
                        LOG.debug("Delay of event " + event.toString() + " between throwing and finishing of execution: " + WGUtils.DECIMALFORMAT_STANDARD.format(executionTime));
                    }
                    
                    WGFactory.getInstance().closeSessions();
                    while (true) {
                        long oldValue = _processedAsyncEventIndex.get();
                        if (oldValue >= eventIndex) {
                            break;
                        }
                        if (_processedAsyncEventIndex.compareAndSet(oldValue, eventIndex)) {
                            break;
                        }
                        Thread.yield();
                    }
                }
            }
            
        });
    }
    
    public void waitForEvents() {
        
        long thrownEventIndex = _thrownAsyncEventCount.get();
        while (true) {
            long currentEventIndex = _processedAsyncEventIndex.get();
            if (currentEventIndex >= thrownEventIndex) {
                return;
            }
            try {
                Thread.sleep(100);
            }
            catch (InterruptedException e) {
            }
        }
        
    }

	/**
	 * @param _eventReveicersRoot
	 * @param hierarchy
	 * @param hierarchyIndex
	 * @param event
	 * @param failOnException 
	 */
	private void findEventReceivers(EventReceiverNode refMap, EventPathEntry[] hierarchy, int hierarchyIndex, Event event, List<Object> results, boolean failOnException) throws WGException {
	
		for (EventReceiver eventReceiver : refMap.getEventReceivers()) {
		    try {
		        LOG.debug("Calling event receiver " + eventReceiver.getDescription());
                List<Object> result = eventReceiver.execute(this, event);
                if (result != null) {
                    results.addAll(result);
                }
            }
            catch (WGException e) {
                if (failOnException) {
                    throw e;
                }
                else {
                    LOG.error("Exception executing event receiver " + eventReceiver.getClass().getName(), e);
                }
            }
		}
	
		if (hierarchyIndex < hierarchy.length) {
			EventPathEntry pathEntry = hierarchy[hierarchyIndex];
			
			EventReceiverNode nextMap = (EventReceiverNode) refMap.get(pathEntry);
			if (nextMap != null) {
				findEventReceivers(nextMap, hierarchy, hierarchyIndex+1, event, results, failOnException);
			}
	
			nextMap = (EventReceiverNode) refMap.get(pathEntry.toWildcardVersion());
			if (nextMap != null) {
				findEventReceivers(nextMap, hierarchy, hierarchyIndex+1, event, results, failOnException);
			}
		}
	
	}

	
	public void databaseUpdate(WGDatabaseEvent event) {
	
		// Verify database is open(able)
		WGDatabase db = event.getDatabase();
		WGDocumentKey docKey = event.getEditedDocumentKey();
		if (event.getType() == WGDatabaseEvent.TYPE_UPDATE &&  (docKey == null || docKey.getDocType() == WGDocument.TYPE_CONTENTTYPE)) {
			updateDatabaseEvents(db);
		}
		
			
	}

	public void updateDatabaseEvents(WGDatabase db) {
	    
	    if (!db.getRoles().contains(WGDatabase.ROLE_CONTENT) || !db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) { 
	        return;
	    }
	    
		String dbKey = (String) db.getAttribute(WGACore.DBATTRIB_DBKEY);
		if (!db.isSessionOpen()) {
			try {
				if (db.openSession() < WGDatabase.ACCESSLEVEL_READER) {
					LOG.error("Could not update events for database '" + dbKey + "' because of access denial");
					return;
				}
                db.getSessionContext().setTask("WGA Event Manager");
			}
			catch (WGAPIException e) {
				LOG.error("Could not update events for database '" + dbKey + "' because of database unavailability");
				return;
			}
		}
		
		// Re-register db events
        try {
		removeContentTypeEvents(dbKey);
		List<WGContentType> contentTypesList = db.getContentTypes();
		if (contentTypesList == null) {
		    return;
		}
        Iterator<WGContentType> contentTypes = contentTypesList.iterator();
		WGContentType contentType;
		while (contentTypes.hasNext()) {
			contentType = contentTypes.next();
			updateContentTypeEvents(dbKey, contentType);
		}
        } catch (WGAPIException e) {
            LOG.error("Could not update events for database '" + dbKey + "'.", e);
        }
	}

	private void updateContentTypeEvents(String dbKey, WGContentType contentType) throws WGAPIException {
		
		// Create content
		String ctEvent = (String) contentType.getMetaData(WGContentType.META_EVENT_CREATECONTENT);
		if (ctEvent != null) {
			String desc = "Eventscript createContent of Contenttype '" + contentType.getName() + "' (" + contentType.getDatabase().getDbReference() + ")";
			ContentTypeEventScript eventScript = new ContentTypeEventScript(ctEvent, desc);
			EventPath eventPath = new ContentTypeEventPath(EVENTNAME_CREATECONTENT, dbKey, contentType.getName(), null);
			registerEventReceiver(eventPath, eventScript);
		} 
		
		// Save content
		ctEvent = (String) contentType.getMetaData(WGContentType.META_EVENT_SAVECONTENT);
		if (ctEvent != null) {
			String desc = "Eventscript saveContent of Contenttype '" + contentType.getName() + "' (" + contentType.getDatabase().getDbReference() + ")";
			ContentTypeEventScript eventScript = new ContentTypeEventScript(ctEvent, desc);
			EventPath eventPath = new ContentTypeEventPath(EVENTNAME_SAVECONTENT, dbKey, contentType.getName(), null);
			registerEventReceiver(eventPath, eventScript);
		}
		
		// Workflow mail
		ctEvent = (String) contentType.getMetaData(WGContentType.META_EVENT_WORKFLOWMAIL);
		if (ctEvent != null) {
			String desc = "Eventscript workflowMail of Contenttype '" + contentType.getName() + "' (" + contentType.getDatabase().getDbReference() + ")";
			ContentTypeEventScript eventScript = new ContentTypeEventScript(ctEvent, desc);
			EventPath eventPath = new ContentTypeEventPath(EVENTNAME_WORKFLOWMAIL, dbKey, contentType.getName(), null);
			registerEventReceiver(eventPath, eventScript);
		}
		
		// Status change
	    ctEvent = (String) contentType.getMetaData(WGContentType.META_EVENT_STATUSCHANGE);
        if (ctEvent != null) {
            String desc = "Eventscript statusChange of Contenttype '" + contentType.getName() + "' (" + contentType.getDatabase().getDbReference() + ")";
            ContentTypeEventScript eventScript = new ContentTypeEventScript(ctEvent, desc);
            EventPath eventPath = new ContentTypeEventPath(EVENTNAME_STATUSCHANGE, dbKey, contentType.getName(), null);
            registerEventReceiver(eventPath, eventScript);
        }

	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseEventListener#isTemporary()
	 */
	public boolean isTemporary() {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGContentEventListener#contentCreated(de.innovationgate.webgate.api.WGContentEvent)
	 */
	public void contentCreated(WGContentEvent contentEvent) throws WGAPIException {
	    
	    try {
            WGContent newContent = contentEvent.getContent();
    		WGDatabase db = contentEvent.getDatabase();
    		if (!db.getSessionContext().isContentTypeEventsEnabled()) {
                return;
            }
    		
    		HttpServletRequest request = (HttpServletRequest) db.getSessionContext().getAttribute(WGACore.DBSESSIONCONTEXT_REQUEST);
    		TMLUserProfile userProfile = null;
    		if (request != null) {
    		    userProfile = _core.getPersManager().getProfileFromRequestCache(request, db);
    		}
    		
    		String contentType = contentEvent.getContentType();
            WGDocumentKey docKey = new WGDocumentKey(contentEvent.getDocumentKey());
    		EventPath eventPath = new ContentTypeEventPath(EVENTNAME_CREATECONTENT, db.getDbReference(), contentType, WGContentKey.parse(docKey.getName(), db));
    		executeEvent(eventPath, new ContentTypeEvent(newContent, userProfile, null), true);
	    }
        catch (WGAPIException e) {
            throw e;
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGContentEventListener#contentSaved(de.innovationgate.webgate.api.WGContentEvent)
	 */
	public boolean contentSaved(WGContentEvent contentEvent) throws WGAPIException {
		
       try {
        WGContent newContent = contentEvent.getContent();
        
        	WGDatabase db = contentEvent.getDatabase();
        	if (!db.getSessionContext().isContentTypeEventsEnabled()) {
        	    return true;
        	}
        	
        	HttpServletRequest request = (HttpServletRequest) db.getSessionContext().getAttribute(WGACore.DBSESSIONCONTEXT_REQUEST);
        	TMLUserProfile userProfile = null;
        	if (request != null) {
        	    userProfile = _core.getPersManager().getProfileFromRequestCache(request, db);
        	}
        	
        	String contentType = contentEvent.getContentType();
            WGDocumentKey docKey = new WGDocumentKey(contentEvent.getDocumentKey());
        	EventPath eventPath = new ContentTypeEventPath(EVENTNAME_SAVECONTENT, db.getDbReference(), contentType, WGContentKey.parse(docKey.getName(), db));
        	Iterator<Object> results = executeEvent(eventPath, new ContentTypeEvent(newContent, userProfile, null), true).iterator();
        	
        	while (results.hasNext()) {
        		Object result = results.next();
        		
        		if (result instanceof List<?>) {
        			result = ((List<?>) result).get(0);
        		}
        		
        		if (result instanceof Boolean && ((Boolean) result).booleanValue() == false) {
        			return false;
        		}
        		if (result instanceof Number && ((Number) result).intValue() == 0) {
        			return false;
        		}
        	}
        	
        	return true;
    }
    catch (WGAPIException e) {
        throw e;
    }
    catch (WGException e) {
        throw new WGAPIException(e);
    }
		
        
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGWorkflowEventListener#workflowMail(de.innovationgate.webgate.api.WGWorkflowEvent)
	 */
	public void workflowMail(de.innovationgate.webgate.api.workflow.WGWorkflowEvent workflowEvent) throws WGAPIException {
		
		WGContent newContent = workflowEvent.getContent();
		WGDatabase db = newContent.getDatabase();
		if (!db.getSessionContext().isContentTypeEventsEnabled()) {
            return;
        }
		
		String contentType = "*";
		try {
            if (newContent.getStructEntry() != null && newContent.getStructEntry().getContentType() != null) {
            	contentType = newContent.getStructEntry().getContentType().getName();
            }
            EventPath eventPath = new ContentTypeEventPath(EVENTNAME_WORKFLOWMAIL, db.getDbReference(), contentType, newContent.getContentKey());
            Map<String,Object> contextObjects = new HashMap<String,Object>();
            contextObjects.put("mailbody", workflowEvent.getMailBody());
            executeEvent(eventPath, new ContentTypeEvent(newContent, null, contextObjects), true);

        }
        catch (WGAPIException e) {
            throw e;
        }			
		catch (WGException e) {
		    throw new WGAPIException(e);
		}
	}
	

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGContentEventListener#contentHasBeenDeleted(de.innovationgate.webgate.api.WGContentEvent)
     */
    public void contentHasBeenDeleted(WGContentEvent contentEvent) throws WGAPIException {
        
		WGDatabase db = contentEvent.getDatabase();
		if (!db.getSessionContext().isContentTypeEventsEnabled()) {
            return;
        }
        
        WGContent content = null;
        try {
            content = contentEvent.getContent();
            if (content == null) {
                try {
                    content = db.getDummyContent(db.getDefaultLanguage());
                }
                catch (WGAPIException e) {
                    LOG.error("Cannot process deletion event for content '" + contentEvent.getDocumentKey() + "' (DB " + contentEvent.getDatabase().getDbReference() + ") because no context is available.", e);
                    return;
                }
            }
            
    		HttpServletRequest request = (HttpServletRequest) db.getSessionContext().getAttribute(WGACore.DBSESSIONCONTEXT_REQUEST);
    		TMLUserProfile userProfile = null;
    		if (request != null) {
    		    userProfile = _core.getPersManager().getProfileFromRequestCache(request, db);
    		}
    		
    		String contentType = "*";
    		if (contentEvent.getContentType() != null) {
    			contentType = contentEvent.getContentType();
    		}
    		
            WGDocumentKey docKey = new WGDocumentKey(contentEvent.getDocumentKey());
    		EventPath eventPath = new ContentTypeEventPath(EVENTNAME_CONTENTHASBEENDELETED, db.getDbReference(), contentType, WGContentKey.parse(docKey.getName(), db));
            Map<String,Object> contextObjects = new HashMap<String,Object>();
            contextObjects.put("contentkey", docKey.getName());
            contextObjects.put("contenttype", contentType);
            
    		executeEvent(eventPath, new ContentTypeEvent(content, userProfile, null), true);
        }
        catch (WGAPIException e) {
            throw e;
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }

    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGContentEventListener#contentHasBeenSaved(de.innovationgate.webgate.api.WGContentEvent)
     */
    public void contentHasBeenSaved(WGContentEvent contentEvent) throws WGAPIException {
        
		WGContent newContent = null;
        try {
            newContent = contentEvent.getContent();
            if (newContent == null) {
                //LOG.warn("Cannot process update event for content '" + contentEvent.getDocumentKey() + "' (DB " + contentEvent.getDatabase().getDbReference() + ") because the content is not retrievable. Might have been deleted in the meantime.");
                return;
            }
        }
        catch (WGAPIException e) {
            LOG.error("Cannot process update event for content '" + contentEvent.getDocumentKey() + "' (DB " + contentEvent.getDatabase().getDbReference() + ").", e);
            return;
        }
        
        
		WGDatabase db = contentEvent.getDatabase();
		if (!db.getSessionContext().isContentTypeEventsEnabled()) {
            return;
        }
		
		HttpServletRequest request = (HttpServletRequest) db.getSessionContext().getAttribute(WGACore.DBSESSIONCONTEXT_REQUEST);
		TMLUserProfile userProfile = null;
		if (request != null) {
		    userProfile = _core.getPersManager().getProfileFromRequestCache(request, db);
		}
		
		String contentType = "*";
		try {
            if (newContent != null && newContent.hasCompleteRelationships()) {
            	contentType = newContent.getStructEntry().getContentType().getName();
            }                		
    		EventPath eventPath = new ContentTypeEventPath(EVENTNAME_CONTENTHASBEENSAVED, db.getDbReference(), contentType, newContent.getContentKey());
    		executeEvent(eventPath, new ContentTypeEvent(newContent, userProfile, null), true);
		}
        catch (WGAPIException e) {
            throw e;
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }
    }
    


    public void databaseConnected(WGDatabaseEvent event) {
        updateDatabaseEvents(event.getDatabase());
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseConnectListener#databaseConnectionError(de.innovationgate.webgate.api.WGDatabaseEvent)
     */
    public void databaseConnectionError(WGDatabaseEvent event) {
    }

    public void contentHasBeenMoved(WGContentEvent contentEvent) throws WGAPIException {
        
        WGContent newContent = null;
        try {
            newContent = contentEvent.getContent();
            if (newContent == null) {
                //LOG.warn("Cannot process move event for content '" + contentEvent.getDocumentKey() + "' (DB " + contentEvent.getDatabase().getDbReference() + ") because the content is not retrievable. Might have been deleted in the meantime.");
                return;
            }
        }
        catch (WGAPIException e) {
            LOG.error("Cannot process move event for content '" + contentEvent.getDocumentKey() + "' (DB " + contentEvent.getDatabase().getDbReference() + ").", e);
            return;
        }
        
        
        WGDatabase db = contentEvent.getDatabase();
        if (!db.getSessionContext().isContentTypeEventsEnabled()) {
            return;
        }
        
        HttpServletRequest request = (HttpServletRequest) db.getSessionContext().getAttribute(WGACore.DBSESSIONCONTEXT_REQUEST);
        TMLUserProfile userProfile = null;
        if (request != null) {
            userProfile = _core.getPersManager().getProfileFromRequestCache(request, db);
        }
        
        String contentType = "*";
        try {
            if (newContent != null && newContent.hasCompleteRelationships()) {
                contentType = newContent.getStructEntry().getContentType().getName();
            }                       
            EventPath eventPath = new ContentTypeEventPath(EVENTNAME_CONTENTHASBEENMOVED, db.getDbReference(), contentType, newContent.getContentKey());
            executeEvent(eventPath, new ContentTypeEvent(newContent, userProfile, null), true);
        }
        catch (WGAPIException e) {
            throw e;
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }
        
    }

    public void contentStatusChanged(WGContentEvent contentEvent) throws WGAPIException {

        WGContent newContent = null;
        try {
            newContent = contentEvent.getContent();
            
            WGDatabase db = contentEvent.getDatabase();
            if (!db.getSessionContext().isContentTypeEventsEnabled()) {
                return;
            }
            
            HttpServletRequest request = (HttpServletRequest) db.getSessionContext().getAttribute(WGACore.DBSESSIONCONTEXT_REQUEST);
            TMLUserProfile userProfile = null;
            if (request != null) {
                userProfile = _core.getPersManager().getProfileFromRequestCache(request, db);
            }
            
            String contentType = contentEvent.getContentType();
            WGDocumentKey docKey = new WGDocumentKey(contentEvent.getDocumentKey());
            EventPath eventPath = new ContentTypeEventPath(EVENTNAME_STATUSCHANGE, db.getDbReference(), contentType, WGContentKey.parse(docKey.getName(), db));
            executeEvent(eventPath, new ContentTypeEvent(newContent, userProfile, null), true).iterator();
        
        }
        catch (WGAPIException e) {
            throw e;
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }
    }

    public WGACore getCore() {
        return _core;
    }

    public void reloadConfig() {
        initExecutors();
    }
    
    public String dumpReceivers() throws WGException {
        
        StringBuilder b = new StringBuilder();
        dumpReceivers(0, b, _eventReveicersRoot);        
        return b.toString();
        
        
    }

    private void dumpReceivers(int level, StringBuilder b, EventReceiverNode node) throws WGException {
        
        for (EventReceiver receiver : node.getEventReceivers()) {
            levelPrefix(b, level);
            b.append("Receiver: " + receiver.getClass().getName()).append(": ").append(receiver.getDescription()).append("\n");;
        }

        for (Map.Entry<EventPathEntry,EventReceiverNode> subnode : node.entrySet()) {
            levelPrefix(b, level);
            b.append("Node: " + subnode.getKey().toString()).append("\n");
            dumpReceivers(level+1, b, subnode.getValue());
        }
        
        
    }

    private void levelPrefix(StringBuilder b, int level) {
        for (int idx=0; idx < level; idx++) {
            b.append("-");
        }
    }
}
