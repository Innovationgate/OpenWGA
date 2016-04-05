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
package de.innovationgate.webgate.api;

/**
 * <p>
 * event which is fired by modify methods of WGHierarchicalDatabase
 * </p>
 * <p>
 * "pre..." - events are fired directly after the method call before any processing is done
 * "post..." - events are fired directly before the return statement of the correponding method after all processing is done
 * </p>
 */
public class WGHierarchicalDatabaseEvent {
    
        public static final String TYPE_PRE_CREATE_CONTENT = "preCreateContent";
        public static final String TYPE_POST_CREATE_CONTENT = "postCreateContent";
        
        public static final String TYPE_PRE_UPDATE_CONTENT = "preUpdateContent";
        public static final String TYPE_POST_UPDATE_CONTENT = "postUpdateContent";
        
        public static final String TYPE_PRE_DELETE_CONTENT = "preDeleteContent";
        public static final String TYPE_POST_DELETE_CONTENT = "postDeleteContent";
        
        public static final String TYPE_PRE_MOVE_CONTENT_TO = "preMoveContentTo";
        public static final String TYPE_POST_MOVE_CONTENT_TO = "postMoveContentTo";
        
        public static final String TYPE_PRE_MOVE_CONTENT_FROM = "preMoveContentFrom";
        public static final String TYPE_POST_MOVE_CONTENT_FROM = "postMoveContentFrom";
        
        
        private String _type;
        private WGHierarchicalDatabase _db;
        private WGContent _parentContent;
        private WGContent _content;
        private Object _parameter;
        private boolean _directChild;
        private WGContent _listenerContent;
        // on move operations this prop. contains the target parent to move the content to
        private WGContent _targetParentContent;
        
        /**
         * returns the storage or content the listener was registered on 
         * @return WGContent
         */
        public WGContent getListenerContent() {
            return _listenerContent;
        }
        
        /**
         * returns true if the created, updated or deleted content is a direct child of the
         * storage or content the listener was registered on
         * @return true/ false
         */
        public boolean isDirectChild() {
            return _directChild;
        }

        protected void setDirectChild(boolean directChild) {
            _directChild = directChild;
        }

        public WGHierarchicalDatabaseEvent(String type, WGHierarchicalDatabase db) {
            _type = type;
            _db = db;
        }        
        
        /**
         * returns the created, updated or deleted content
         * will be 'null' on the following events
         * - preCreateContent
         * - postDeleteContent 
         * @return WGContent
         */
        public WGContent getContent() {
            return _content;
        }

        protected void setContent(WGContent content) {
            _content = content;
        }
        
        /**
         * returns the custom defined parameter object given in the hdb method call
         * @return Object
         */
        public Object getParameter() {
            return _parameter;
        }
        
        protected void setParameter(Object parameter) {
            _parameter = parameter;
        }
        
        /**
         * returns the parent content or storage of the created, updated or deleted content
         * @return WGContent
         */
        public WGContent getParentContent() {
            return _parentContent;
        }
        
        protected void setParentContent(WGContent parent) {
            _parentContent = parent;
        }
        
        /**
         * returns the hdb
         * @return WGHierarchicalDatabase
         */
        public WGHierarchicalDatabase getDb() {
            return _db;
        }

        /**
         * returns the event type
         * @return String
         */
        public String getType() {
            return _type;
        }

        protected void setType(String type) {
            _type = type;
        }
        
        /**
         * cancels the current process with the given message
         * the corresponding hdb() method which fired the event will 
         * throw an WGHierarchicalDatabaseEventException with the given message
         * @param message
         * @throws WGHierarchicalDatabaseEventException
         */
        public void cancel(String message) throws WGHierarchicalDatabaseEventException {
            throw new WGHierarchicalDatabaseEventCanceledException(this, message);
        }
        
        /**
         * cancels the current process with the given message
         * the corresponding hdb() method which fired the event will 
         * throw an WGHierarchicalDatabaseEventException with the given message and throwable
         * @param message
         * @param throwable
         * @throws WGHierarchicalDatabaseEventException
         */
        public void cancel(String message, Throwable throwable) throws WGHierarchicalDatabaseEventException {
            throw new WGHierarchicalDatabaseEventCanceledException(this, message, throwable);
        }


        protected void setListenerContent(WGContent content) {
            _listenerContent = content;            
        }

        /**
         * Returns the target content in move operations
         */
		public WGContent getTargetParentContent() {
			return _targetParentContent;
		}

		protected void setTargetParentContent(WGContent targetParentContent) {
			_targetParentContent = targetParentContent;
		}        
}
