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

package de.innovationgate.wga.server.api;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.quartz.SchedulerException;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.scheduler.ConfigurationException;
import de.innovationgate.wgpublisher.scheduler.JavaTask;
import de.innovationgate.wgpublisher.scheduler.Job;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.scheduler.JobSchedule;
import de.innovationgate.wgpublisher.scheduler.ScriptTask;
import de.innovationgate.wgpublisher.scheduler.TaskImplementation;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextEnvironment.RootEnvironmentUserData;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

/**
 * Provides services regarding OpenWGA Jobs
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Jobs {
    
    private WGA _wga;

    protected Jobs(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Runs a job
     * This will run a job just like it would have been run from the OpenWGA admin client manually.
     * @param jobName Name of the job
     * @throws WGAServerException
     * @throws JobFailedException
     */
    public void run(String jobName) throws WGException {
        run(jobName, null);
    }

    /**
     * Runs a job
     * This will run a job just like it would have been run from the OpenWGA admin client manually.
     * @param jobName Name of the job
     * @param customOptions Optional additional options that will be available to the tasks of the job via {@link JobContext#getOption(String)}
     * @throws WGAServerException
     * @throws JobFailedException
     */
    public void run(String jobName, Map<String,Object> customOptions) throws WGException {
        
        Job job = _wga.getCore().getScheduler().getJob(jobName);
        if (job != null) {
            
            String executor = "WGA Server API";
            if (_wga.isTMLContextAvailable()) {
                App app = _wga.app();
                if (app.isOpen()) {
                    if (app.db().getSessionContext().isMasterSession()) {
                        RootEnvironmentUserData userData = ((TMLContext) _wga.tmlcontext()).getEnvironment().getRootEnvironmentUserData();
                        if (userData !=null && userData.getDbkey().equals(app.getDbKey())) {
                            executor = userData.getUserAccess().getPrimaryName();
                        }
                        else {
                            executor = "Master Session";
                        }
                    }
                    else {
                        executor = app.db().getSessionContext().getUser(); 
                    }
                }
            }
            
            _wga.getCore().getScheduler().run(jobName, executor, customOptions, null);
        }
        else {
            throw new WGAServerException("Unknown job: " + jobName);
        }
        
    }
    
    /**
     * Runs the code of a TMLScript module as a job
     * Running a TMLScript module as a job is equivalent to creating a job, adding a single task of type "TMLScript" to it, and specifying a TMLScript module to run. However the job created and run by this method will be transient, which means that it will be gone again after it has finished running. The job is given a generated job name which is returned by this method and can be used to monitor it.
     * @param design Design reference of the TMLScript module that is to be run as job
     * @return The job name of the running job
     * @throws WGAServerException
     */
    public String runScriptModule(String design) throws WGException {
        return runScriptModule(design, null);
    }

    /**
     * Runs the code of a TMLScript module as a job
     * Running a TMLScript module as a job is equivalent to creating a job, adding a single task of type "TMLScript" to it, and specifying a TMLScript module to run. However the job created and run by this method will be transient, which means that it will be gone again after it has finished running. The job is given a generated job name which is returned by this method and can be used to monitor it.
     * @param design Design reference of the TMLScript module that is to be run as job
     * @param options Optional additional options that will be available to the tasks of the job via {@link JobContext#getOption(String)}
     * @return The job name of the running job
     * @throws WGAServerException
     */
    public String runScriptModule(String design, Map<String,Object> options) throws WGException {
        
        try {
            DesignResourceReference ref;
            if (_wga.isTMLContextAvailable()) {
                ref = _wga.design().resolveReference(design).normalize();
            }
            else {
                ref = new DesignResourceReference(null, design).normalize();
            }
            
            WGScriptModule module = _wga.db(ref.getDesignApp()).getScriptModule(ref.getResourceName(), "tmlscript");
            if (module == null) {
                throw new WGAServerException("Unknown script module: " + ref.getResourceName() + " from app " + ref.getDesignApp());
            }
            
            String jobName = "TMLScriptModule_" + module.getName() + "_" + UIDGenerator.generateUID();
            
            Map<String,Object> jobOptions = new HashMap<String, Object>();
            if (options != null) {
                jobOptions.putAll(options);
            }
            jobOptions.put(ScriptTask.OPTION_DATABASE, module.getDatabase().getDbReference());
            jobOptions.put(ScriptTask.OPTION_MODULE, module.getName());
            
            _wga.getCore().runTransientTask(ScriptTask.class.getName(), jobName, jobOptions);
            return jobName;
        }
        catch (Exception e) {
            throw new WGAServerException("Exception running script module on scheduler", e);
        }
        
    }
    
    /**
     * Runs a custom Java task implementation as transient job
     * The job created and run by this method will be transient, which means that it will be gone again after it has finished running. The job is given a generated job name which is returned by this method and can be used to monitor it.
     * @param taskClass Class of the task implementation
     * @param options Options given to the job
     * @return The job name of the running job
     * @throws WGAServerException
     */
    public String runTask(Class<? extends TaskImplementation> taskClass, Map<String,Object> options) throws WGException {
        
        try {
            String jobName = "CustomTask_" + UIDGenerator.generateUID();
            
            Map<String,Object> jobOptions = new HashMap<String, Object>();
            if (options != null) {
                jobOptions.putAll(options);
            }
           
            jobOptions.put(JavaTask.OPTION_TASKCLASS, taskClass.getName());
            _wga.getCore().runTransientTask(JavaTask.class.getName(), jobName, jobOptions);
            return jobName;
        }
        catch (Exception e) {
            throw new WGAServerException("Exception running task on scheduler", e);
        }
        
        
    }
    
    /**
     * Tests if the given job is currently running
     * This method will also return false if a job of the given name is not defined.
     * @param jobName Name of the job
     */
    public boolean isRunning(String jobName) throws WGException {
        
        Job job = _wga.getCore().getScheduler().getJob(jobName);
        if (job != null) {
            return job.isRunning();
        }
        else {
            return false;
        }
        
    }
    
    /**
     * Pauses execution until the job is no longer running or the timeout is reached
     * Use this method if you want to continue execution of the current thread not until a certain Job has finished execution.
     * If no job of that name is running while calling the method then the method returns directly.
     * @param jobName Name of the job
     * @param timeout Timeout in seconds
     */
    public void waitWhileRunning(String jobName, int timeout) throws WGException {
        
        
        try {
            long waitTill = System.currentTimeMillis() + (timeout * 1000);
            do  {
                
                if (!isRunning(jobName)) {
                    break;
                }
                
                Thread.sleep(1000);
                
            }
            while (waitTill > System.currentTimeMillis());
        
        }
        catch (InterruptedException e) {
        }
        
    }
    
    /**
     * Returns an URL under which OpenWGA publishes the log of a running job
     * OpenWGA publishes the log of running jobs under URLs that can be retrieved by this method. However one must be logged in as OpenWGA administrator to view the job log. 
     * @param jobName Name of the job
     * @return URL of the job log
     * @throws WGAServerException
     */
    public String getLogURL(String jobName) throws WGException {
        
        try {
            URLBuilder url = _wga.urlBuilder(_wga.server().getBaseURL());
            url.setPath(_wga.server().getBaseURL() + "/joblog");
            url.setParameter("name", jobName);
            return url.build(false);
        }
        catch (Exception e) {
            throw new WGAServerException("Exception generating job log URL", e);
        }
        
    }
    
    /**
     * Returns the names of currently registered jobs
     * @throws WGException
     */
    public List<String> getNames() throws WGException {
        return new ArrayList<String>(_wga.getCore().getScheduler().getJobNames());
    }
    
    /**
     * Adds a schedule to an existing job
     * This should only be used on jobs that are registered via some Designs configuration.
     * It should not be used for those jobs not created and configured in OpenWGA admin client as a reload of the OpenWGA configuration will clear schedules added by this method on those jobs. 
     * @param jobName The name of the job
     * @param cronSchedule The CRON schedule string, having the known format from CRON schedules when configuring in admin client
     * @param startingDate The date from which the job starts running or null for no starting date
     * @param endingDate The date on which the job ends running or null for no ending date
     * @throws WGException
     */
    public void schedule(String jobName, String cronSchedule, Date startingDate, Date endingDate) throws WGException {
        
        JobSchedule schedule = new JobSchedule();
        schedule.setEnabled(true);
        schedule.setType(JobSchedule.TYPE_CRON);
        schedule.setScheduleData(cronSchedule);
        schedule.setStartingDate(startingDate);
        schedule.setEndingDate(endingDate);
        
        Job job = _wga.getCore().getScheduler().getJob(jobName);
        if (job == null) {
            throw new WGAServerException("Unknown job name: " + jobName);
        }
        
        try {
            job.addSchedules(Collections.singletonList(schedule), _wga.getCore().getQuartzScheduler());
        }
        catch (ConfigurationException e) {
            throw new WGAServerException("Exception scheduling job '" + jobName + "'", e);
        }
        
    }
    
    /**
     * Adds a schedule to an existing job
     * @param jobName The name of the job
     * @param scheduleStr The CRON schedule string, having the known format from CRON schedules when configuring in admin client
     * @throws WGException
     */
    public void schedule(String jobName, String scheduleStr) throws WGException {
        schedule(jobName, scheduleStr, null, null);
    }
    
    /**
     * Removes all schedules from an existing job
     * @param jobName The name of the job
     * @throws WGException
     */
    public void unschedule(String jobName) throws WGException {
        Job job = _wga.getCore().getScheduler().getJob(jobName);
        if (job == null) {
            throw new WGAServerException("Unknown job name: " + jobName);
        }
        
        try {
            job.clearSchedules(_wga.getCore().getQuartzScheduler());
        }
        catch (SchedulerException e) {
            throw new WGAServerException("Exception unscheduling job '" + jobName + "'", e);
        }
    }

}
