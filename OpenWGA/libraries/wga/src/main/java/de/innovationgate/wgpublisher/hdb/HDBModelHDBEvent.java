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

package de.innovationgate.wgpublisher.hdb;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEvent;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventException;
import de.innovationgate.webgate.api.WGIllegalDataException;

/**
 * Object "event" in HDBModel event scripts that are based on HDB events
 */
public class HDBModelHDBEvent extends HDBModelEvent {
    
    private WGHierarchicalDatabaseEvent _event;

    private String _modelEventType;

    private boolean _cancelable;

    private WGContent _content;

    private WGContent _eventReceiver;

    private String _contentClass;

    private WGContentKey _refDocumentKey;

    public WGContent getEventReceiver() {
        return _eventReceiver;
    }

    public HDBModelHDBEvent(WGHierarchicalDatabaseEvent event, String modelEventType, WGContent eventReceiver, WGContent content, WGContentKey refDocumentKey, boolean cancelable) throws WGAPIException {
        _event = event;
        _modelEventType = modelEventType;
        _cancelable = cancelable;
        _eventReceiver = eventReceiver;
        _content = content;
        _refDocumentKey = refDocumentKey;
        
        if (content != null) {
            _contentClass = content.getContentClass();
        }
        else {
            HDBModelParams params = getParameter();
            if (params != null) {
                _contentClass = params.getContentClass();
            }
            else {
                throw new WGIllegalDataException("This is no HDBModel event as HDBParams are not available");
            }
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#cancel(java.lang.String, java.lang.Throwable)
     */
    public void cancel(String message, Throwable throwable) throws WGHierarchicalDatabaseEventCanceledException {
        try {
            if (!_cancelable) {
                HDBModel.LOG.warn("Event script of content class '" + _event.getContent().getContentClass() + "' attempted to cancel event '" + _modelEventType + "' which cannot be canceled");
            }
            else {
                _event.cancel(message, throwable);
            }
        }
        catch (WGHierarchicalDatabaseEventCanceledException e) {
            throw e;
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception cancelling HDB model event", e);
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#cancel(java.lang.String)
     */
    public void cancel(String message) throws WGHierarchicalDatabaseEventCanceledException {
        try {
            if (!_cancelable) {
                HDBModel.LOG.warn("Event script of content class '" + _event.getContent().getContentClass() + "' attempted to cancel event '" + _modelEventType + "' which cannot be canceled");
            }
            else {
                _event.cancel(message);
            }
        }
        catch (WGHierarchicalDatabaseEventCanceledException e) {
            throw e;
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception cancelling HDB model event", e);
        }
    }

    public boolean equals(Object obj) {
        return _event.equals(obj);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#getContent()
     */
    public WGContent getContent() {
        return _content;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#getDb()
     */
    public WGHierarchicalDatabase getDb() {
        return _event.getDb();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#getParameter()
     */
    public HDBModelParams getParameter() {
        if (_event.getParameter() instanceof HDBModelParams) {
            return (HDBModelParams) _event.getParameter();
        }
        else {
            return null;
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#getTargetParentContent()
     */
    public WGContent getMoveTargetContent() {
        return _event.getTargetParentContent();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.hdb.HDBModelEvent#getType()
     */
    public String getType() {
        return _modelEventType;
    }

    public int hashCode() {
        return _event.hashCode();
    }

    public String toString() {
        return _event.toString();
    }
    
    public String getContentClass() throws WGAPIException {
        return _contentClass;
    }

    @Override
    public WGContentKey getDeletedContentKey() throws WGAPIException {
        
        if (getType().startsWith(HDBModel.EVENT_PRE_DELETE) || getType().startsWith(HDBModel.EVENT_POST_DELETE)) {
            return _refDocumentKey;
        }
        else {
            return null;
        }
        
    }
    

}
