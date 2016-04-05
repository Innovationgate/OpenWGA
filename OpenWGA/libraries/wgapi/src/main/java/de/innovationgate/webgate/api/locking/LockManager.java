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

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGStructEntry;

/**
 * used by WGDatabase to manage locks
 * each WGDatabase has its own LockManager 
 *
 */
public class LockManager {
    
    /**
     * Default time an owner waits for obtaining an lock
     */
    public static final long DEFAULT_LOCK_WAIT_TIMEOUT = 60000;    

    /**
     * Map containing currentLocks mapped by lockable
     */
    private Map _currentLocks = Collections.synchronizedMap(new HashMap());    
        
    /**
     * returns the lockstatus of the given lockable for the given owner
     * if the given lockable is not locked, the lockstatus is retrieved
     * hierarchically by the parent-lockable
     * @param lockable
     * @param owner
     * @return Lock.NOT_LOCKED, Lock.LOCKED_BY_FOREIGN_OBJECT, Lock.LOCKED_BY_THIS_OBJECT
     * @throws WGAPIException 
     */
    public int getLockStatus(Lockable lockable, LockOwner owner) throws WGAPIException {
        
        // check if any locks exists. In none, return fast
        if (_currentLocks.size() == 0) {
            return Lock.NOT_LOCKED;
        }
        
        // check if lockable is locked
        Lock lock = (Lock) _currentLocks.get(lockable);
        if (lock != null) { 
            return lock.getLockStatus(owner);
        }
        
        // Ask parent for lock
        else {
            Lockable parentLockable = lockable.getParentLockable();
            if (parentLockable != null) {
                return parentLockable.getLockStatus(owner);
            }
        }
        return Lock.NOT_LOCKED;
    }
     
    /**
     * obtains a lock on the given lockable for the given owner
     * @param lockable
     * @param owner
     * @throws WGAPIException 
     */
    public void obtainLock(Lockable lockable, LockOwner owner) throws WGAPIException {
        long lockStartTime = System.currentTimeMillis();
        while ((System.currentTimeMillis() - lockStartTime) < DEFAULT_LOCK_WAIT_TIMEOUT) {             
            synchronized (this) {
                // check lockstatus
                if (lockable.getLockStatus(owner) != Lock.LOCKED_BY_FOREIGN_OBJECT) {
                    // if not locked by foreign owner - check if childLocksExists
                    if (!foreignChildLocksExists(lockable, owner)) {
                        // obtain lock
                        Lock lock = new Lock(lockable, owner);
                        _currentLocks.put(lockable, lock);
                        return;
                    }                    
                }
            }                        
            try {
                Thread.sleep(10);
            }
            catch (InterruptedException e) {
                // nothing to do
            }
        }      
        throw new LockWaitTimeoutException("waiting for lock timed out after " + DEFAULT_LOCK_WAIT_TIMEOUT + " ms.");        
    }
    
    /**
     * releases the lock obtained on the given lockable by the given owner
     * @param lockable
     * @param owner
     */
    public void releaseLock(Lockable lockable, LockOwner owner) {
        // check if lock exist
    	Lock lock = (Lock) _currentLocks.get(lockable);
        if (lock != null) {            
            if (lock.isOwnedBy(owner)) {
                // release lock
                _currentLocks.remove(lockable);
            } else {
                // some one else tries to remove the lock
            }
        }            
    }
    
    /**
     * releases all locks obtained from the given owner
     * @param owner
     */
    public void releaseAllLocks(LockOwner owner) {
    	synchronized (_currentLocks) {
    		Iterator it = _currentLocks.values().iterator();        
	        while (it.hasNext()) {
	            Lock lock = (Lock) it.next();
	            if (lock.isOwnedBy(owner)) {
	                // release lock
	                it.remove();                
	            }
	        }
        }
    }     
    

    /**
     * checks if foreign locks exists on decendant objects of the given lockable
     * @param lockable
     * @param owner
     * @return true/ false
     * @throws WGAPIException 
     */
    public boolean foreignChildLocksExists(Lockable lockable, LockOwner owner) throws WGAPIException {
        if (lockable instanceof WGDatabase) {
            return foreignChildLocksExist((WGDatabase)lockable, owner);
        } else if (lockable instanceof WGArea) {
            return foreignChildLocksExists((WGArea)lockable, owner);
        } else if (lockable instanceof WGStructEntry) {
            return foreignChildLocksExists((WGStructEntry)lockable, owner);
        } else if (lockable instanceof WGContent) {
            // content has no childs
            return false;
        } else {
            return false;
        }
    }    
    
    /**
     * 
     * checks if foreign locks exists on descendant objects (structentries, content) of given structentry
     * @param entry
     * @param owner
     * @return true/ false
     * @throws WGAPIException 
     */
    private boolean foreignChildLocksExists(WGStructEntry entry, LockOwner owner) throws WGAPIException {
    	// iterate over clone - to prevent concurrent modification exception
    	HashMap locks = null;
    	synchronized (_currentLocks) {
    		locks = new HashMap(_currentLocks);	
		}    	
        Iterator it = locks.values().iterator();
        while (it.hasNext()) {
            Lock lock = (Lock) it.next();
            if (!lock.isOwnedBy(owner)) {
                if (lock.getLockable() instanceof WGStructEntry) {                    
                    WGStructEntry lockedStructEntry = (WGStructEntry) lock.getLockable();
                    // check if locked structentry is a descendant of given entry 
                    if (lockedStructEntry.isDescendantOf(entry)) {
                        return true;
                    }
                } else if (lock.getLockable() instanceof WGContent) {
                    WGContent lockedContent = (WGContent) lock.getLockable();
                    // check if lockedContent is a content of a descendant structentry or equals given entry
                    if (lockedContent.getStructEntry().isDescendantOf(entry) || lockedContent.getStructEntry().equals(entry)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }    
    
    
    /**
     * checks if foreign locks exists on descendant objects (structentries, content) of given area
     * @param area
     * @param owner
     * @return true/ false
     * @throws WGAPIException 
     */
    private boolean foreignChildLocksExists(WGArea area, LockOwner owner) throws WGAPIException {
    	// iterate over clone - to prevent concurrent modification exception
    	HashMap locks = null;
    	synchronized (_currentLocks) {
    		locks = new HashMap(_currentLocks);	
		} 
    	Iterator it = locks.values().iterator();
        while (it.hasNext()) {
            Lock lock = (Lock) it.next();
            if (!lock.isOwnedBy(owner)) {
                if (lock.getLockable() instanceof WGStructEntry) {                    
                    WGStructEntry lockedStructEntry = (WGStructEntry) lock.getLockable();
                    if (lockedStructEntry.getArea().equals(area)) {
                        return true;
                    }
                } else if (lock.getLockable() instanceof WGContent) {
                    WGContent lockedContent = (WGContent) lock.getLockable();
                    // check if structentry of lockedContent belongs to given area
                    if (lockedContent.getStructEntry().getArea().equals(area)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    
    /**
     * checks if foreign locks exists on descendant objects (area, structentries, content) of given database
     * @param database
     * @param owner
     * @return true/ false
     */
    private boolean foreignChildLocksExist(WGDatabase database, LockOwner owner) {
        // db has no parent, therefore simply iterate over all locks and check lockowner
    	// iterate over clone - to prevent concurrent modification exception
    	HashMap locks = null;
    	synchronized (_currentLocks) {
    		locks = new HashMap(_currentLocks);	
		}     	
    	Iterator it = locks.values().iterator();
        while (it.hasNext()) {
            Lock lock = (Lock) it.next();
            if (!lock.isOwnedBy(owner)) {
                return true;
            }
        }
        return false;
    }     
        
     

}
