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
import de.innovationgate.webgate.api.WGCancelledException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentEvent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEvent;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;

/**
 * Object "event" in HDBModel event scripts that are based on content events
 */
public class HDBModelContentEvent extends HDBModelEvent {

    private String _modelEventType;
    private boolean _cancelable;
    private WGContent _content;
    private HDBModelParams _parameter;

    public HDBModelContentEvent(WGContent content, String modelEventType, HDBModelParams parameter, boolean cancelable) {
        _content = content;
        _modelEventType = modelEventType;
        _cancelable = cancelable;
        _parameter = parameter;
    }

    public WGContent getEventReceiver() throws WGAPIException {
        return _content;
    }
    
    public void cancel(String message, Throwable throwable) throws WGCancelledException {
        
        try {
            if (!_cancelable) {
                HDBModel.LOG.warn("Event script of content class '" + _content.getContentClass() + "' attempted to cancel event '" + _modelEventType + "' which cannot be canceled");
            }
            else {
                throw new WGCancelledException("Content Event cancelled because of exception", throwable);
            }
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception cancelling HDB model event", e);
        }
    }

    public void cancel(String message) throws WGCancelledException {
        try {
            if (!_cancelable) {
                HDBModel.LOG.warn("Event script of content class '" + _content.getContentClass() + "' attempted to cancel event '" + _modelEventType + "' which cannot be canceled");
            }
            else {
                throw new WGCancelledException(message);
            }
        }
        catch (WGCancelledException e) {
            throw e;
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception cancelling HDB model event", e);
        }
    }

    public WGHierarchicalDatabase getDb() throws WGAPIException {
        return WGHierarchicalDatabase.getOrCreateInstance(_content.getDatabase()); 
    }

    public WGContent getContent() throws WGAPIException {
        return _content;
    }

    public HDBModelParams getParameter() throws WGAPIException {
        return _parameter;
    }

    public WGContent getMoveTargetContent() {
        return null;
    }

    public String getType() {
        return _modelEventType;
    }
    
    public String getContentClass() throws WGAPIException {
        return _content.getContentClass();
    }

    @Override
    public WGContentKey getDeletedContentKey() throws WGAPIException {
        return null;
    }

}
