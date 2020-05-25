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
package de.innovationgate.wgpublisher.scheduler;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.quartz.JobExecutionContext;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

/**
 * This object is passed to WGA jobs to provide access to WGA resources.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE, beanMode=CodeCompletion.BEAN_MODE_ALL)
public class JobContext {

	private static final Integer CLEAR_CACHE_AFTER_UPDATES = 100;
	
	private JobExecutionContext _quartzContext = null;
	private WGACore _wgaCore = null;
	private Object result = null;
	private Object previousResult = null;
	private Logger _log = null;
	private Map<String,Object> _customOptions = new HashMap<String,Object>();
    private Job _currentJob;
    private Task _currentTask;
    private volatile boolean _cancelled = false;
    private volatile boolean _cancelable = false;
	
    private Map<WGDatabase,Long> _dbUpdates = new HashMap<WGDatabase,Long>();
    
	/**
	 * Constructor.
	 * @param quartzContext
	 * @param wgaCore
	 * @param log
	 */
	public JobContext(Job currentJob, JobExecutionContext quartzContext, WGACore wgaCore, Logger log) {
		_currentJob = currentJob;
        _quartzContext = quartzContext;
		_wgaCore = wgaCore;
		_log = log;
	}
	
	/**
	 *  Provides the options defined on the job
	 *  @deprecated Use {@link #getOption(String)} to read an option no matter where it is defined
	 */
	@CodeCompletion
	public Map<String,String> getOptions() {
	    return Collections.unmodifiableMap(_currentJob.getOptions());
	}
	
	/**
	 * Returns a task option.
	 * @param name Name of the option
	 * @return The option string value
	 */
	@CodeCompletion
	public String getOption(String name) {
	    return _currentTask.getOption(name);
	}
	
	/**
	 * Returns a task option
	 * @param name Name of the option
	 * @param defaultValue Value to return if the option does not exist
	 * @return The option string value
	 */
	@CodeCompletion
	public String getOption(String name, String defaultValue) {
        return _currentTask.getOption(name, defaultValue);
    }

	/**
	 * Returns the schedulers Quartz context. NOT IMPLEMENTED YET!
	 */
	public JobExecutionContext getQuartzContext() {
		return _quartzContext;
	}

	/**
	 * Returns the WGA core object
	 * @deprecated Use {@link WGA} object to access WGA global resources like these
	 */
	public WGACore getWgaCore() {
		return _wgaCore;
	}

	/**
	 * Returns the result of this task
	 */
	@CodeCompletion
	public Object getResult() {
		return result;
	}

	/**
	 * Sets the result of this task
	 * @param object The tasks result
	 */
	@CodeCompletion
	public void setResult(Object object) {
		result = object;
	}

	/**
	 * Returns a logger object to log output to the WGA application log
	 */
	@CodeCompletion
	public Logger getLog() {
		return _log;
	}

	/**
	 * Returns custom options that have been added to this runtime of the job
	 * Use this method especially to retrieve custom options that are not of String value
	 */
	@CodeCompletion
	public Map<String,Object> getCustomOptions() {
		return _customOptions;
	}

	/**
	 * Returns the result of the previous task in this Job, if any.
	 */
	@CodeCompletion
	public Object getPreviousResult() {
		return previousResult;
	}

	/**
	 * Sets the previous result.
	 */
    protected void setPreviousResult(Object object) {
		previousResult = object;
	}

    /**
     * Returns the object representing the current job
     */
    public Job getCurrentJob() {
        return _currentJob;
    }
    
    public void setEndMessage(String msg){
    	_currentJob.setEndMessage(msg);
    }
    public String getEndMessage(){
    	return _currentJob.getEndMessage();
    }

    /**
     * Returns the object representing the current task
     */
    public Task getCurrentTask() {
        return _currentTask;
    }

    protected void setCurrentTask(Task currentTask) {
        _currentTask = currentTask;
    }
    
    /**
     * Returns the {@link WGDatabase} object of the application / data source with the given database key
     * @param dbkey The database key
     * @return The database object or null if no database of this key is connected
     * @throws WGAPIException
     * @deprecated Use {@link WGA} object to access WGA global resources like these
     */
    public WGDatabase db(String dbkey) throws WGAPIException {
        
        WGDatabase db = _wgaCore.getContentdbs().get(dbkey);
        if (db == null) {
            return null;
        }
        
        if (!db.isSessionOpen()) {
            db.openSession();
        }
        
        return db;
        
    }
    
    /**
     * Returns the database keys of all web applications and data sources that are connected
     * @deprecated Use {@link WGA} object to access WGA global resources like these
     */
    public Set<String> getDatabaseKeys() {
        return Collections.unmodifiableSet(_wgaCore.getContentdbs().keySet());
    }

    /**
     * Returns if this Job was cancelled
     */
    @CodeCompletion(isProperty=true)
    public boolean isCancelled() {
        return _cancelled;
    }

    /**
     * Sets the Job to be cancelled
     */
    protected void setCancelled(boolean cancelled) {
        _cancelled = cancelled;
    }

    /**
     * Cancel the Job
     */
    public void cancelJob() {
    	setCancelled(true);
    }

    /**
     * Throws a {@link JobFailedException} if the Job has been cancelled.
     * Use this call inside a Jobs code at a place where the Job can be safely cancelled without leaving any data inconsistent.
     * @throws JobFailedException
     */
    @CodeCompletion
    public void breakIfCancelled() throws JobCancelledException {
        if (_cancelable == false) {
            _cancelable = true;
        }
        
        if (_cancelled) {
            throw new JobCancelledException("Job was cancelled");
        }
    }

    @CodeCompletion(isProperty=true)
    public boolean isCancelable() {
        return _cancelable;
    }

    public void setCancelable(boolean cancelable) {
        _cancelable = cancelable;
    }

    public void dbUpdated(WGDatabase db) throws WGAPIException{
    	Long updates = _dbUpdates.get(db);
    	if(updates==null)
    		updates=1L;	// first update
    	if(updates % CLEAR_CACHE_AFTER_UPDATES == 0){
    		db.getSessionContext().clearCache();
    		_log.info(db.getDbReference() + ": cleared document cache after " + CLEAR_CACHE_AFTER_UPDATES + " updates. Total updates: " + updates);
    	}
    	_dbUpdates.put(db, ++updates);
    }

}
