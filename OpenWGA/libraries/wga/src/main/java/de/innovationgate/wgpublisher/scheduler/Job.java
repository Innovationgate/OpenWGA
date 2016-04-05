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
import java.io.IOException;
import java.io.StringWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import net.sf.cglib.transform.impl.AddPropertyTransformer;

import org.apache.log4j.FileAppender;
import org.apache.log4j.Layout;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.WriterAppender;
import org.dom4j.Element;
import org.quartz.CronTrigger;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.core.QuartzScheduler;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.modules.joboptions.JobOptionsModuleDefinition;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.JobScope;
import de.innovationgate.wgpublisher.problems.MessageVariableProvider;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.webtml.utils.UniqueNamePartFormatter;

/**
 * Job runtime object
 */
public class Job {
    
    public static final String OPTION_PERMANENT_LOG = "PermanentLog";
    
    public static final String OPTION_PERMANENT_LOG_CLEANUP = "PermanentLogCleanup";
    
    public static final Integer LOG_CLEANUP_DEFAULT = 14;
    
    public class JobRunProblemOccasion implements ProblemOccasion, MessageVariableProvider {

        private String _jobName;
        private String _schedule;
        private String _executor;

        public JobRunProblemOccasion(Job job, JobExecutionContext quartzContext, String executor) {
            _jobName = job.getName();
            _schedule = WGUtils.DATEFORMAT_FULL.format(new Date());
            _executor = executor;
        }
        
        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
        
        @Override
        public ProblemScope getDefaultScope() {
            return new JobScope(getName());
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return Job.class;
        }

        @Override
        public Problem.Vars getDefaultMessageVariables() {

            return Problem
                .var("jobname", _jobName)
                .var("runtime", _schedule)
                .var("executor", (_executor != null ? _executor : "OpenWGA Scheduler"));
            
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_jobName == null) ? 0 : _jobName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            JobRunProblemOccasion other = (JobRunProblemOccasion) obj;
            if (_jobName == null) {
                if (other._jobName != null)
                    return false;
            }
            else if (!_jobName.equals(other._jobName))
                return false;
            return true;
        }
        
    }
    

	public static final String CUSTOM_TASK_UID_PREFIX = "custom_";
    public static final String QUARTZ_JOBGROUP_WGASCHEDULER = "WGAScheduler";
    private FileAppender _logAppender;
	private de.innovationgate.wgpublisher.scheduler.Scheduler _scheduler;
	private Object _lastResult;
	public static final Layout JOBLOG_LAYOUT = new PatternLayout("%d{dd.MM.yyyy HH:mm:ss} %p %m\n");
	public static final SimpleDateFormat DATE_FORMAT_LOG = new SimpleDateFormat("yyyyMMdd_HHmmss");
    public static final int ORIGIN_WGACONFIG = 1;
    public static final int ORIGIN_CUSTOM = 2;
    

	private String _name;
	private String _description;
	private List<Task> _tasks = new ArrayList<Task>();
	private volatile boolean _running = false;
	private boolean _failed = false;
	private Throwable _exception = null;
    private List _trigger = new ArrayList();
    private String _endMessage = null;
    private int _origin = ORIGIN_WGACONFIG;

	private Date _lastRun = null;
	private String _lastLog = "(Job has not been run yet)";
	private StringWriter _currentLog = null;
	private Map<String,String> _options = new HashMap<String,String>();
    private boolean _transient = false;
    private boolean _quiet = false;
    private String _uid;
    private JobContext _currentJobContext;
    public Job(de.innovationgate.wgpublisher.scheduler.Scheduler scheduler, de.innovationgate.wga.config.Job conf) throws ConfigurationException {

		// Base info
		_scheduler = scheduler;
        
        if (conf == null) {
            _name = "Custom Task";
            _description = "A custom task generated by the WGA runtime";
            _transient = true;
            _uid = CUSTOM_TASK_UID_PREFIX + UIDGenerator.generateUID();
            return;
        }
        
		_name = conf.getName();
		_uid = conf.getUid();

		_description = conf.getDescription();
		if (_description == null) {
			_description = "(no description)";
		}

		_options.putAll(conf.getOptions());

		// Tasks
		Iterator<de.innovationgate.wga.config.Task> tasks = conf.getTasks().iterator();
		while (tasks.hasNext()) {
			de.innovationgate.wga.config.Task taskConfig = tasks.next();
			String className = taskConfig.getImplClassName();
			try {
				@SuppressWarnings("unchecked")
                Class<? extends Task> taskClass = (Class<? extends Task>) WGACore.getLibraryLoader().loadClass(className);
				Task task = (Task) taskClass.newInstance();
				task.init(this, taskConfig);
				task.configure(_scheduler.getCore());
				addTask(task);
			}
			catch (ClassNotFoundException e) {
				throw new ConfigurationException("Task class '" + className + "' cannot be found", e);
			}
			catch (NoClassDefFoundError e) {
                throw new ConfigurationException("Task class '" + className + "' cannot be instantiated bc. a dependency class is missing", e);
            }
			catch (InstantiationException e) {
				throw new ConfigurationException("Task class '" + className + "' cannot be instantiated: " + e.getMessage());
			}
			catch (IllegalAccessException e) {
				throw new ConfigurationException("Task class '" + className + "' default contructor is not visible");
			}
		} 
	}

    public void addTask(Task task) {
        _tasks.add(task);
    }

    public void addToScheduler(List schedules, Scheduler quartzScheduler) throws ConfigurationException {
        JobDataMap jobDataMap = new JobDataMap();
        jobDataMap.put("Job", this);
        jobDataMap.put("WGACore", _scheduler.getCore());
        
        JobDetail jobDetail = new JobDetail();
        jobDetail.setName(getName());
        jobDetail.setGroup(QUARTZ_JOBGROUP_WGASCHEDULER);
        jobDetail.setDescription(_description);
        jobDetail.setJobClass(QuartzJobRunner.class);
        jobDetail.setJobDataMap(jobDataMap);
        jobDetail.setDurability(true);
        try {
            // To remove old job and also remove it's associated triggers
            quartzScheduler.deleteJob(getName(), QUARTZ_JOBGROUP_WGASCHEDULER);

            quartzScheduler.addJob(jobDetail, false);
        }
        catch (SchedulerException e1) {
            throw new ConfigurationException("Could not add job to scheduler: " + e1.getMessage());
        }
        
        // Add triggers
        addSchedules(schedules, quartzScheduler);
    }

    public void addSchedules(List schedulesList, Scheduler quartzScheduler) throws ConfigurationException {
        
        Iterator schedules = schedulesList.iterator();
        int triggerCount = 0;
        while (schedules.hasNext()) {
            triggerCount++;
            JobSchedule schedule = (JobSchedule) schedules.next();
            if (!schedule.isEnabled()) {
                continue;
            }
            
            String type = schedule.getType();
            String text = schedule.getScheduleData();
            Date startingDate = schedule.getStartingDate();
            Date endingDate = schedule.getEndingDate();
            
            Trigger trigger = null;
            if (type.equals(JobSchedule.TYPE_SIMPLE)) {
                trigger = createSimpletrigger(text, startingDate, endingDate);
            }
            else if (type.equals(JobSchedule.TYPE_CRON)) {
                trigger = createCronTrigger(text, startingDate, endingDate);
            }
                                   
            if (trigger != null) {
                trigger.setJobName(getName());
                trigger.setJobGroup(QUARTZ_JOBGROUP_WGASCHEDULER);
                try {
                    quartzScheduler.scheduleJob(trigger);
                }
                catch (SchedulerException e) {
                   throw new ConfigurationException("Could not schedule job: " + e.getMessage());
                }
                catch (UnsupportedOperationException e) {
                    throw new ConfigurationException("Could not schedule job: " + e.getMessage());
                }
                addTrigger(trigger);
            }
        }
    }





    private Trigger createCronTrigger(String text, Date startingDate, Date endingDate) throws ConfigurationException {
        Trigger trigger;
        CronTrigger cronTrigger = new CronTrigger(getName() + "_" + UIDGenerator.generateUID(), QUARTZ_JOBGROUP_WGASCHEDULER);
        cronTrigger.setMisfireInstruction(CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING);
        try {
            cronTrigger.setCronExpression(text);
        }
        catch (ParseException e) {
            throw new ConfigurationException("Error parsing cron pattern for job: " + e.getClass().getName() + " - " + e.getMessage());
        }
        
        if (startingDate != null) {
            cronTrigger.setStartTime(startingDate);
        }
        if (endingDate != null) {
            cronTrigger.setEndTime(endingDate);
        }
        trigger = cronTrigger;
        return trigger;
    }

    private Trigger createSimpletrigger(String text, Date startingDate, Date endingDate) {
        try {
            Trigger trigger;
            String repeatIntervalStr = text.substring(0, text.length() - 1);
            long repeatInterval = Long.parseLong(repeatIntervalStr);
            
            String repeatType = text.substring(text.length() - 1);
            if (repeatType.equals("h")) {
                repeatInterval = repeatInterval * 1000 * 60 * 60;
            }
            else if (repeatType.equals("m")) {
                repeatInterval = repeatInterval * 1000 * 60;
            }
            
            SimpleTrigger simpleTrigger = new SimpleTrigger(getName() + "_" + UIDGenerator.generateUID(), QUARTZ_JOBGROUP_WGASCHEDULER);
            simpleTrigger.setRepeatCount(SimpleTrigger.REPEAT_INDEFINITELY);
            simpleTrigger.setRepeatInterval(repeatInterval);
            simpleTrigger.setMisfireInstruction(SimpleTrigger.MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT);
            
            if (startingDate != null) {
                simpleTrigger.setStartTime(startingDate);
            }
            if (endingDate != null) {
                simpleTrigger.setEndTime(endingDate);
            }
            trigger = simpleTrigger;
            return trigger;
        }
        catch (NumberFormatException e) {
            _scheduler.getCore().getLog().error("Cannot create schedule bc. of number formatting error", e);
            return null;
        }
        catch (IllegalArgumentException e) {
            _scheduler.getCore().getLog().error("Cannot create schedule bc. of illegal argument", e);
            return null;
        }
    }
    
    public void shutdown() {
       Scheduler quartzScheduler = _scheduler.getCore().getQuartzScheduler();
       
       // Quartz scheduler already terminated
       if (quartzScheduler == null) {
           return;
       }
       
       
       try {
        quartzScheduler.deleteJob(getName(), QUARTZ_JOBGROUP_WGASCHEDULER);
    }
    catch (SchedulerException e) {
       _scheduler.getCore().getLog().error("Error on shutdown of job " + getName(), e);
    }
    }

	public String getLog() {

		if (isRunning() && _currentLog != null) {
			return _currentLog.toString();
		}
		else {
			return _lastLog;
		}

	}

	protected synchronized void run(WGACore core, Map<String,Object> customOptions, JobExecutionContext quartzContext, String executor) {

		Logger logger = Logger.getLogger("wga.job." + _name);
        logger.setLevel(Level.INFO);
		_currentLog = new StringWriter();
		WriterAppender tempAppender = new WriterAppender(JOBLOG_LAYOUT, _currentLog);
        logger.addAppender(tempAppender);
		_failed = false;
		_exception = null;
        _running = true;
        
        // Create a options reader for predefined global options. Can only use those custom options that are of string value.
        Map<String,String> options = new HashMap<String,String>();
        options.putAll(getOptions());
        if (customOptions != null) {
            for (Map.Entry<String,Object> option : customOptions.entrySet()) {
                if (option.getValue() instanceof String) {
                    options.put(option.getKey(), (String) option.getValue());
                }
            }
        }
        OptionReader globalOptions = OptionReader.create(options, new JobOptionsModuleDefinition()); 
        
        JobRunProblemOccasion occ = new JobRunProblemOccasion(this, quartzContext, executor);
        core.getProblemRegistry().clearProblemOccasion(occ);

		try {
		    _currentJobContext = new JobContext(this, quartzContext, core, logger);
		    
			// Prepare logging
			_lastRun = new Date();

			if (_scheduler.getLoggingDir() != null && globalOptions.readOptionValueOrDefault(OPTION_PERMANENT_LOG).equals(true)) {
				initPermanentLog(logger, (Integer) globalOptions.readOptionValueOrDefault(OPTION_PERMANENT_LOG_CLEANUP), occ, core);
			}

			// Prepare jobContext
			if (customOptions != null) {
			    _currentJobContext.getCustomOptions().putAll(customOptions);
			}

			// Run tasks
			if (!_quiet) {
                if (executor != null) {
                    logger.info("Starting job " + _name + " started by admin account '" + executor + "'");
                }
                else {
                    logger.info("Starting job " + _name + " triggered by WGA Scheduler");
                }
			}
			runTasks(logger, _currentJobContext);

			if (!_quiet) {
    			logger.info("Job execution finished");
    			if (_currentJobContext.getResult() != null) {
    				logger.info("End result: " + String.valueOf(_currentJobContext.getResult()));
    				_lastResult = _currentJobContext.getResult();
    			}
			}
		}
		catch (JobCancelledException e) {
		    logger.info("Job execution was cancelled");
		}
		catch (JobFailedException e) {
		    logger.error("Job " + _name + " failed", e);
		    Problem problem;
		    if (e.getFailingTask() != null) {
		        problem = Problem.create(occ, "jobFailed.failingTask", ProblemSeverity.HIGH, Problem.var("failingtask", e.getFailingTask()), e);
		        
		    }
		    else {
		        problem = Problem.create(occ, "jobFailed.failure", ProblemSeverity.HIGH, e);
		    }
		     
		    core.getProblemRegistry().addProblem(problem);
            _exception = e;
            _failed = true;
		}
		catch (Throwable e) {
			logger.error("Exception executing job " + _name, e);
			core.getProblemRegistry().addProblem(Problem.create(occ, "jobFailed.exception", ProblemSeverity.HIGH, e));
			_exception = e;
			_failed = true;
		}
		finally {
			// We switch off stats on the currently registered job of this name
			Job currentJob = _scheduler.getJob(_name);
			closeStats(logger, currentJob);
			if (currentJob != this) {
			    closeStats(logger, this);
			}
            if (tempAppender != null) {
                tempAppender.close();
                logger.removeAppender(tempAppender);
            }
		}

	}

    private void closeStats(Logger logger, Job currentJob) {
        currentJob._running = false;
        currentJob._lastLog = _currentLog.toString();
        currentJob._currentLog = null;
        if (currentJob._logAppender != null) {
            currentJob._logAppender.close();
        	logger.removeAppender(currentJob._logAppender);
        	currentJob._logAppender = null;				
        }
    }

    private void runTasks(Logger logger, JobContext jobContext) throws JobFailedException {
        Iterator tasksIt = _tasks.iterator();
        while (tasksIt.hasNext()) {
        	Task task = (Task) tasksIt.next();
        	
        	String taskTitle = task.getTitle(Locale.getDefault());
            if (!_quiet) {
        	    logger.info("--- Running task: " + taskTitle + "------------------------------------------------");
        	}
        	
        	try {
        	    jobContext.setCurrentTask(task);
        	    jobContext.setCancelable(false);
        	    task.setCustomOptions(jobContext.getCustomOptions());
        		task.execute(jobContext);
        		
        		if (jobContext.isCancelled()) {
                    throw new JobCancelledException("Job was cancelled");
                }
        		if (!_quiet) {
        		    logger.info("Task successfully executed");
        		}
        		
        		
        	}
        	catch (TaskException e) {
        		logger.error("Task canceled with error: " + taskTitle, e);
        		if (task.isCancelJobOnFail()) {
        			logger.fatal("Canceling job!");
        			throw new JobFailedException(
        				"Canceled because of failure of task: " + taskTitle, taskTitle, e);
        		}
        	}

        	jobContext.setPreviousResult(jobContext.getResult());
        	jobContext.setResult(null);

        }
    }

    private void initPermanentLog(Logger logger, int cleanupDays, JobRunProblemOccasion occ, WGACore core) throws JobFailedException {
        
        try {
            int counter = 0;
            File loggingFile;
            
            // Do cleanup
            String jobFilePrefix = "job_" + UniqueNamePartFormatter.INSTANCE.format(getName()) + "_";
            if (cleanupDays >= 0) {
                for (File file : _scheduler.getLoggingDir().listFiles()) {
                    
                    if (file.getName().startsWith(jobFilePrefix)) {
                        Date modDate = new Date(file.lastModified());
                        modDate = WGA.get(core).modifyDate(modDate, "d", cleanupDays);
                        if (modDate.before(new Date())) {
                            logger.info("Deleting old job log file: " + file.getAbsolutePath());
                            file.delete();
                        }
                    }
                    
                }
            }
            
            // Find the name of the current logging file
            do {
            	counter++;
                if (counter > 1) {
            	    loggingFile = new File(_scheduler.getLoggingDir(), jobFilePrefix + DATE_FORMAT_LOG.format(_lastRun) + "_" + counter + ".log");
            	}
            	else {
            	    loggingFile = new File(_scheduler.getLoggingDir(), jobFilePrefix + DATE_FORMAT_LOG.format(_lastRun) + ".log");
            	}
            }
            while (loggingFile.exists());
            logger.info("Storing job log in file: " + loggingFile.getAbsolutePath());
   
            // Create file and its appender
        	loggingFile.createNewFile();
        	_logAppender = new FileAppender(JOBLOG_LAYOUT, loggingFile.getPath());
        	logger.addAppender(_logAppender);
        }
        catch (Exception e) {
        	core.getProblemRegistry().addProblem(Problem.create(occ, "jobProblem.permanentLog", ProblemSeverity.LOW, e));
        }
    }

	/**
	 * @return
	 */
	public Map<String,String> getOptions() {
		return _options;
	}

	/**
	 * @return
	 */
	public Date getLastRun() {
		return _lastRun;
	}

	/**
	 * @return
	 */
	public String getName() {
		return _name;
	}

	/**
	 * @return
	 */
	public List<Task> getTasks() {
		return _tasks;
	}

	/**
	 * @param previousJob
	 */
	public void retrieveStats(Job previousJob) {
		_lastLog = previousJob._lastLog;
		_lastRun = previousJob._lastRun;
		_lastResult = previousJob._lastResult;
		_failed = previousJob._failed;
		_exception = previousJob._exception;
        _endMessage = previousJob._endMessage;
        _currentLog = previousJob._currentLog;
        _running = previousJob._running;
	}

	/**
	 * @return
	 */
	public String getDescription() {
		return _description;
	}

	/**
	 * @param string
	 */
	public void setDescription(String string) {
		_description = string;
	}

	/**
	 * @return
	 */
	public boolean isRunning() {
		return _running;
	}

	/**
	 * @return
	 */
	public boolean isFailed() {
		return _failed;
	}

	public void clearLog() {
		_lastLog = "";
	}
    
    public void addTrigger(Trigger trigger) {
        _trigger.add(trigger);
    }
    
    public Date nextScheduledRun() {
        
        Iterator trigger = _trigger.iterator();
        Date nextRun = null;
        while (trigger.hasNext()) {
            Trigger element = (Trigger) trigger.next();
            if (nextRun == null || element.getNextFireTime().before(nextRun)) {
                nextRun = element.getNextFireTime();
            }
        }
        
        return nextRun;
        
    }

    public boolean isTransient() {
        return _transient;
    }

    public void setName(String name) {
        _name = name;
    }

    public String getEndMessage() {
        return _endMessage;
    }

    public void setEndMessage(String endMessage) {
        _endMessage = endMessage;
    }

    public void setTransient(boolean transient1) {
        _transient = transient1;
    }

    public int getOrigin() {
        return _origin;
    }

    public void setOrigin(int origin) {
        _origin = origin;
    }
    
    protected Throwable getException() {
        return _exception;
    }

    public String getUid() {
        return _uid;
    }

    public de.innovationgate.wgpublisher.scheduler.Scheduler getScheduler() {
        return _scheduler;
    }

    public boolean isQuiet() {
        return _quiet;
    }

    public void setQuiet(boolean quiet) {
        _quiet = quiet;
    }
    
    public void clearSchedules(Scheduler quartzScheduler) throws SchedulerException {
        for (Trigger trigger : quartzScheduler.getTriggersOfJob(_name, QUARTZ_JOBGROUP_WGASCHEDULER)) {
            quartzScheduler.unscheduleJob(trigger.getName(), QUARTZ_JOBGROUP_WGASCHEDULER);
        }
        _trigger.clear();
    }

    protected JobContext getCurrentJobContext() {
        return _currentJobContext;
    }
    
    public boolean isCancelable() {
        if (_currentJobContext != null) {
            return _currentJobContext.isCancelable();
        }
        return false;
    }
    
    
    public boolean isCancelled() {
        if (_currentJobContext != null) {
            return _currentJobContext.isCancelled();
        }
        return false;
    }
    
    public void cancel() {
        if (_currentJobContext != null) {
            _currentJobContext.setCancelled(true);
        }
        else {
            throw new IllegalStateException("Job is not running and cannot be cancelled");
        }
    }

}
