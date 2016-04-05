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
package de.innovationgate.webgate.api.locking;

import de.innovationgate.webgate.api.WGAPIException;

/**
 * interface for all lockable wgapi-objects
 *
 */
public interface Lockable {
        
    /**
     * locks the object for the given owner
     * @throws WGAPIException
     *          - instance of LockWaitTimeoutException when the lock request timed out 
     *            @see LockManager#DEFAULT_LOCK_WAIT_TIMEOUT
     *          - instance of ParentIsLockedException when the lock cannot be obtained
     *            because the parent object is already locked 
     *          - other API Exceptions on API errors   
     */
    public void lock(LockOwner owner) throws WGAPIException;

    
    /**
     * unlock the object if it is locked by the given owner
     * this method only delegates to LockOwner.releaseLock()
     * @param owner - the LockOwner who has obtained the lock
     */
    public void unlock(LockOwner owner);
    
    /** 
     * returns the current lockstatus of the lockable - if parent is set and locked the lockstatus is inherited
     * @param caller - LockOwner who wants to retrieve his lockstatus for this document
     * @return NOT_LOCKED, LOCKED_BY_FOREIGN_OBJECT or LOCKED_BY_THIS_OBJECT
     * @throws WGAPIException 
     */
    public int getLockStatus(LockOwner caller) throws WGAPIException;
    
    /**
     * returns the parent lockable of this lockable for e.g. WGStructEntry in case of WGContent
     * @throws WGAPIException 
     */
    public Lockable getParentLockable() throws WGAPIException;

}
