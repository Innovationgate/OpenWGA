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

package de.innovationgate.webgate.api.utils;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;

/**
 * Tool class that provides a convenient way to execute a custom task on a database in a master session on a parallel thread.
 * Login data of the current thread is untouched.
 * Just subclass this one and implement method exec() that gets a reference to the database from the constructor.
 * When exec() gets executed the database is already open in master session. 
 * The session will be closed automatically once exec() is thru. 
*/
public abstract class MasterSessionTask {
    
    class TaskThread extends Thread {
        
        public void run() {
            runTask();
        }
        
    }

    private WGDatabase _db;
    private Throwable _throwable = null;
    private Object _result = null;
    private boolean _allowInline = false;

    /**
     * Construct a master session task for a specific database
     * @param db
     */
    public MasterSessionTask(WGDatabase db) {
        _db = db;
    }
    
    /**
     * Construct a master session task for a specific database
     * @param db The database
     * @param allowInline Specify true if the task may also be executed in the current thread if a master session is already open
     */
    public MasterSessionTask(WGDatabase db, boolean allowInline) {
        _db = db;
        _allowInline = allowInline;
    }

    /**
     * Implement the task to execute here
     * @param db The database that the task uses
     * @throws Throwable
     */
    protected abstract void exec(WGDatabase db) throws Throwable;
    
    /**
     * Run the task. 
     * If an exception occurs this method returns false. 
     * The exception that occured can be retrieved by {@link #getThrowable()}
     * @return true, if the task finished without exceptions, false otherwise
     */
    public boolean run() {
        
        runMasterThread();
        
        if (_throwable == null) {
            return true;
        }
        else {
            return false;
        }
        
    }
    
    /**
     * Run the task. Exceptions that are thrown in the master session task are thrown on by this method.
     * @throws Throwable
     */
    public void runWithExceptions() throws Throwable {
        
        runMasterThread();
        
        if (_throwable == null) {
            return;
        }
        else {
            throw _throwable;
        }
        
    }

    private void runMasterThread() {
        
        if (_allowInline && _db.isSessionOpen() && _db.getSessionContext().isMasterSession()) {
            runTask();
        }
        else {
            Thread taskThread = new TaskThread();
            taskThread.start();
            try {
                taskThread.join();
            }
            catch (InterruptedException e) {
            }
        }
        
    }



    /**
     * Returns an exception that was thrown in the task when started with {@link #run()}
     */
    public Throwable getThrowable() {
        return _throwable;
    }

    /**
     * Retrieves a result object that may be set by the MasterSessionTask
     */
    public Object getResult() {
        return _result;
    }



    /**
     * Sets the result of this task. May be used inside the task execution to define some kind of task result
     */
    public void setResult(Object result) {
        this._result = result;
    }

    private void runTask() {
        try {
            _db.openSession();
            exec(_db);
        }
        catch (Throwable e) {
            _throwable = e;
        }
        finally {
            WGFactory.getInstance().closeSessions();
        }
    }
}
