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

import java.util.Date;

/**
 * represents a lock which can be obtained on all objects of a WGDatabase
 * by using LockManager
 *
 */
public class Lock {
    
    public static final int NOT_LOCKED = 0;
    public static final int LOCKED_BY_FOREIGN_OBJECT = 1;
    public static final int LOCKED_BY_THIS_OBJECT = 2;    
    
    private Date _lockCreated;
    private LockOwner _owner;
    private Lockable _lockable;

    public Lock(Lockable lockable, LockOwner owner) {
        _lockable = lockable;
        _owner = owner;
        _lockCreated = new Date(System.currentTimeMillis());
    }

    /**
     * 
     * @return the date the lock was obtained
     */
    public Date getLockCreated() {
        return _lockCreated;
    }
    
    /**
     * checks if this lock is owned by the given owner
     * @param owner
     * @return true/ false
     */
    public boolean isOwnedBy(LockOwner owner) {
        if (_owner != null) {
            return _owner.equals(owner);
        } else {
            return false;
        }
    }
    
    /**
     * @return the lockable this lock was obtained on
     */
    public Lockable getLockable() {
        return _lockable;
    }   
    
    /**
     * returns the current lockStatus
     * @param owner
     * @return LOCKED_BY_FOREIGN_OBJECT, LOCKED_BY_THIS_OBJECT
     */
    public int getLockStatus(LockOwner owner) {
        if (isOwnedBy(owner)) {
            return LOCKED_BY_THIS_OBJECT;
        } else {
            return LOCKED_BY_FOREIGN_OBJECT;
        }
    } 
}
