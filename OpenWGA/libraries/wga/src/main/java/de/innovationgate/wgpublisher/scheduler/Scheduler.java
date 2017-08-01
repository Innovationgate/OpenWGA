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

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.quartz.JobExecutionContext;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.config.Schedule;
import de.innovationgate.wga.server.api.ApplicationEventBuilder;
import de.innovationgate.wgpublisher.WGACore;

public class Scheduler {
    
    class JobRunnerThread extends Thread {
		
		private JobExecutionContext _quartzContext;
		private Map<String,Object> _customOptions;
		private String _jobName; 
        private String _executor;
        private JobFailedException _exception;
        private boolean _finished = false;
		
        private ApplicationEventBuilder _event;
        
		public JobRunnerThread(String jobName, String executor, Map<String,Object> customOptions, JobExecutionContext quartzContext) {
			this(jobName, executor, customOptions, quartzContext, null);
		}

		public JobRunnerThread(String jobName, String executor, Map<String,Object> customOptions, JobExecutionContext quartzContext, ApplicationEventBuilder event) {
			_jobName = jobName;
			_customOptions = customOptions;
			_quartzContext = quartzContext;
			_executor = executor;
			_event = event;
		}
		
		public void run() {
			
			Thread.currentThread().setName(("WGA Scheduler running Job '" + _jobName + "'"));
            _exception = null;
            Job job = null;
			try {
				job = getJob(_jobName);
				if (job == null) {
					throw new JobFailedException("Job does not exist: " + _jobName);
				}
				job.run(_core, _customOptions, _quartzContext, _executor);
			}
			catch (JobFailedException e) {
                _exception = e;
			}
			finally {
			    _finished = true;
			    if(_event!=null){
			    	try {
						_event.fire();
					} catch (WGException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
			    }
			    WGFactory.getInstance().closeSessions();
                /*if (job != null && job.isTransient()) {
                    _jobs.remove(job.getName());
                }*/
			}
		}

        /**
         * @return Returns the exception.
         */
        public JobFailedException getException() {
            return _exception;
        }

        public boolean isFinished() {
            return _finished;
        }
		
	}

    private static final long TRANSIENT_JOB_LIFETIME = 1000 * 60 * 30;

    public static final String LOGGINGDIR_DEFAULT = "joblogs";

	private WGACore _core;
	private File _loggingDir = null;

	private Map<String,Job> _jobs = Collections.synchronizedMap(new HashMap<String,Job>());
	
	public Scheduler(WGACore core) {
		_core = core;
	}
	
	public void addJob(de.innovationgate.wga.config.Job jobConfig) throws ConfigurationException {
        getCore().getLog().info("Adding job '" + jobConfig.getName() + "' to WGA Scheduler");
		Job job = new Job(this, jobConfig);
        job.addToScheduler(parseSchedules(jobConfig), getCore().getQuartzScheduler());
		Job previousJob = (Job) _jobs.put(job.getName(), job);
		if (previousJob != null) {
			job.retrieveStats(previousJob);
		}
	}
    
    private List<JobSchedule> parseSchedules(de.innovationgate.wga.config.Job jobConfig) {
        
        List<JobSchedule> schedules = new ArrayList<JobSchedule>();
        Iterator<Schedule> it = jobConfig.getSchedules().iterator();
        while (it.hasNext()) {
            try {
                JobSchedule schedule = new JobSchedule(it.next());
                schedules.add(schedule);
            }
            catch (ConfigurationException e) {
                _core.getLog().error("Error parsing job schedule", e);
            }
        }
        
        return schedules;
        
    }

    public Job addCustomTaskJob(String jobName, Task task, boolean istransient, JobSchedule schedule) throws ConfigurationException {
        
        if (istransient && schedule != null) {
            throw new ConfigurationException("A job with a schedule cannot be transient");
        }
        
        
        Job job = new Job(this, null);
        job.setOrigin(Job.ORIGIN_CUSTOM);
       
        if (jobName == null) {
            jobName = "customJob_" + UIDGenerator.generateUID();
        }
        
        job.setName(jobName);
        job.setDescription("Temporary job running task " + task.getClass().getName());
        job.setTransient(istransient);
        
        job.addTask(task);
        task.init(job, null);

        getCore().getLog().info("Adding job '" + job.getName() + "' to WGA Scheduler");
        
        List<JobSchedule> scheduleList = new ArrayList<JobSchedule>();
        if (schedule != null) {
             scheduleList.add(schedule);
        }
        job.addToScheduler(scheduleList, getCore().getQuartzScheduler());
        
        Job previousJob = (Job) _jobs.put(job.getName(), job);
        if (previousJob != null) {
            job.retrieveStats(previousJob);
        }
                
        return job;
    
        
    }
	
	public Job getJob(String name) {
		return (Job) _jobs.get(name);
	}
	
	public void run(String name, String executor, Map<String,Object> customOptions, JobExecutionContext quartzContext) throws JobFailedException {
		run(name, executor, customOptions, quartzContext, null);
	}
		
	public void run(String name, String executor, Map<String,Object> customOptions, JobExecutionContext quartzContext, ApplicationEventBuilder event) throws JobFailedException {
    	Job job = (Job) _jobs.get(name);
    	if (job != null) {
    		JobRunnerThread runnerThread = new JobRunnerThread(name, executor, customOptions, quartzContext, event);
    		runnerThread.start();
    		
    		// Ensure we return not before either the job has started or is already finished (#00003499)
    		int count = 0;
    		while (!job.isRunning() && !runnerThread.isFinished()) {
    		    count++;
    		    if (count > 100) {
    		        throw new JobFailedException("Job failed to start in 10 seconds");
    		    }
    		    try {
                    Thread.sleep(100);
                }
                catch (InterruptedException e) {
                }
    		}
    		
    	}
    	else {
    		throw new JobFailedException("Job '" + name + "' does not exist");
    	}
    	
    }

    /**
	 * @return
	 */
	public WGACore getCore() {
		return _core;
	}

	/**
	 * @param core
	 */
	public void setCore(WGACore core) {
		_core = core;
	}

	/**
	 * @return
	 */
	public File getLoggingDir() {
		return _loggingDir;
	}

	/**
	 * @param file
	 */
	public void setLoggingDir(File file) throws IllegalArgumentException {
		
	    if (!file.exists()) {
	        if (!file.mkdirs()) {
	            throw new IllegalArgumentException("Scheduler logging directory '" + file.getPath() + " does not exist and could not be created. Permanent job log will be disabled.");
	        }
	    }

		if (!file.isDirectory()) {
		    throw new IllegalArgumentException("Scheduler logging directory '" + file.getPath() + " either does not exist or is a data file. Permanent job log will be disabled.");
		}
	
		_loggingDir = file;
	}
	
	public Set<String> getJobNames() {
		return Collections.unmodifiableSet(_jobs.keySet());
	}
	
	public void removeJob(String name) {
		Job job = (Job) _jobs.remove(name);
		if (job != null) {
		    job.shutdown();
		}
	}
    
    public void clearTransientJobs() {
        
        Date minLastRun = new Date(System.currentTimeMillis() - TRANSIENT_JOB_LIFETIME); 
        
        synchronized (_jobs) {
            Iterator<Job> jobsIt = _jobs.values().iterator();
            while (jobsIt.hasNext()) {
                Job job = (Job) jobsIt.next();
                if (job.isTransient() && !job.isRunning() && job.getLastRun().before(minLastRun)) {
                    jobsIt.remove();
                }
            }
        }
        
    }

}
