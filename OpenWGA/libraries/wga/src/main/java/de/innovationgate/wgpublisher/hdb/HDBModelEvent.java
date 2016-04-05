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
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Form;

/**
 * Generic interface for object "event" in HDBModel event scripts
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public abstract class HDBModelEvent {

    /**
     * Cancels the event, giving an exception as reason
     * @param message The cancelling message
     * @param throwable A causing exception
     * @throws Exception
     */
    public abstract void cancel(String message, Throwable throwable) throws Exception;

    /**
     * Cancels the event
     * @param message The cancelling message
     * @throws Exception
     */
    public abstract void cancel(String message) throws Exception;
    
    /**
     * The content that owns the event currently executed.
     * On hierarchical events this may NOT be the content on which the event happened but a parent of it that wants to react on the event.
     * @throws WGAPIException
     */
    public abstract WGContent getEventReceiver() throws WGAPIException;

    /**
     * The content that triggered the event execution, i.e. on which the event happened.
     * On hierarchical events this may NOT be the content whose event is executed but some content in its sub hierarchy.
     * This may be null if the event happens while the triggering content does not exist (preCreate, postDelete)
     * @throws WGAPIException
     */
    public abstract WGContent getContent() throws WGAPIException;
    
    /**
     * The content class of the content that triggered the event execution, i.e. on which the event happened.
     * On hierarchical events this may NOT be the content whose event is executed but some content in its sub hierarchy.
     * This is filled even when the triggering content does not exist while the event happens (preCreate, postDelete)
     * @return The content class
     * @throws WGAPIException
     */
    public abstract String getContentClass() throws WGAPIException;
    
    /**
     * The HDB database
     * @throws WGAPIException
     */
    public abstract WGHierarchicalDatabase getDb() throws WGAPIException;

    /**
     * The HDBModel param passed to this HDBModel operation
     * Public for backward compatibility of pre-release event scripts.
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract HDBModelParams getParameter() throws WGAPIException;

    /**
     * On move events: The target content under which the document will get moved
     * @throws WGAPIException
     */
    public abstract WGContent getMoveTargetContent() throws WGAPIException;;

    /**
     * The HDBModel event type. Equals constants HDBModel.EVENT_*
     */
    public abstract String getType();
    
    /**
     * Returns a WebTML form that was passed to this HDBModel operation
     * @throws WGAPIException 
     */
    public Form getForm() throws WGAPIException {
        return getParameter().getForm();
    }
    
    /**
     * Returns a custom parameter object that may be passed to the operation.
     * The object can also be used to transport custom data accross the events of the operation.
     * @throws WGAPIException
     */
    public Object getCustomParam() throws WGAPIException {
        return getParameter().getCustomParam();
    }
    
    /**
     * Sets a custom parameter object to the event.
     * The parameter object will be retrievable on all following events of this operation via {@link #getCustomParam()}.
     * @throws WGAPIException
     */
    public void setCustomParam(Object customParam) throws WGAPIException {
        getParameter().setCustomParam(customParam);
    }
    
    /**
     * In case of an deletion event servers the content key of the deleted content
     * @throws WGAPIException
     */
    public abstract WGContentKey getDeletedContentKey() throws WGAPIException;

}