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
 * A sequence service provided by the WGDatabase. Sequences provide unique, incremental numbers with each call of {@link #increment()}. 
 * Each sequence is identified by a name that may consist of alphanumeric characters plus the characters _.-:$.
 * The state of the sequence is stored inside the database and is therefor persistent and also restorable from database backups.
 * It is guaranteed that the sequence service is consistent across threads and WGA server cluster nodes, returning one unique value only one time on the whole system.
 * Instances of this object can be used across multiple database sessions, but are only usable while a database session is open.
 * Sequences may be initialized with a custom start value - the first value to be retrieved by {@link #increment()} - using {@link #initialize(long)}, but do not need to. The default start value is 1, which is implictly used when calling {@link #increment()} on an uninitialized sequence.
 */
public class WGSequence {
    
    private WGDatabase _db;
    private String _name;

    protected WGSequence(WGDatabase db, String name) {
        _db = db;
        _name = name;
    }
    
    /**
     * Initialize a sequence with the given start value if it is not already initialized.
     * This is only necessary if you want the sequence to start at a custom start value. Otherwise you could only call {@link #increment()} on an uninitialized sequence.
     * @param startValue The start value of the sequence, i.e. the first value returned by the first call of {@link #increment()}
     * @return true if the sequence was actually initialized, false if not.
     * @throws WGAPIException
     */
    public boolean initialize(long startValue) throws WGAPIException {
        return initialize(startValue, false);
    }

    /**
     * Initialize a sequence with the given start value if it is not already initialized or argument forceInit is true.
     * This is only necessary if you want the sequence to start at a custom start value or if you want to reset a sequence that is already initialized. Otherwise you could only call {@link #increment()} on an uninitialized sequence.
     * @param startValue The start value of the sequence, i.e. the first value returned by the first call of {@link #increment()}
     * @param forceInit Specify true if you want to reset the sequence to the given start value even if it was already initialized and may be in use.
     * @return true if the sequence was actually initialized, false if not.
     * @throws WGAPIException
     */

    public boolean initialize(long startValue, boolean forceInit) throws WGAPIException {
        
        if (!_db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (forceInit && _db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
            throw new WGAuthorisationException("Only managers may re-initialize a sequence");
        }
        
        if (startValue < 1) {
            throw new WGIllegalArgumentException("Sequences may not start at a lower value than 1");
        }
        
        return getSequenceProvider().initSequence(_name, startValue, forceInit);
    }

    private WGDatabaseCoreFeatureSequenceProvider getSequenceProvider() {
        WGDatabaseCoreFeatureSequenceProvider core = (WGDatabaseCoreFeatureSequenceProvider) _db.getCore();
        return core;
    }
    
    /**
     * Returns if the sequence is already initialized, either by calling {@link #initialize(long)} and/or by calling {@link #increment()}.
     * This method may return false if the sequence was initialized via {@link #initialize(long)} with default start value 1, as this creates the standard starting point for uninitialized sequences. 
     * @throws WGAPIException
     */
    public boolean isInitialized() throws WGAPIException {
        
        if (!_db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getSequenceProvider().isSequenceInitialized(_name);
        
    }
    
    /**
     * Increments the sequence and returns a new unique number
     * @throws WGAPIException
     */
    public long increment() throws WGAPIException {
        
        if (!_db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getSequenceProvider().incrementSequence(_name);
        
    }
    
    /**
     * Removes the sequence, deleting all initialisation and incrementation status
     * @throws WGAPIException
     */
    public void remove() throws WGAPIException {
        
        if (!_db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (_db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
            throw new WGAuthorisationException("Only managers may remove a sequence");
        }
        
        getSequenceProvider().deleteSequence(_name);

    }
    
    /**
     * Returns the last value that was returned from this sequence on incrementation.
     * The returned value is a "best effort" attempt, which may return slightly outdated values, and is not to be used as factual sequence number. 
     * @throws WGAPIException
     */
    public long getLastIncrementValue() throws WGAPIException {
        if (!_db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getSequenceProvider().getLastSequenceIncrementValue(_name);
    }
    

}
