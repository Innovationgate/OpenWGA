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

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.dom4j.Element;

import de.innovationgate.wgpublisher.WGACore;

public class JavaTask extends Task {
	
	public static final String OPTION_TASKCLASS = "taskclass";

    private Class _jobClass;

	private TaskImplementation _job = null;

    public void setClassName(String taskClassName) throws ConfigurationException {
        
		if (taskClassName == null) {
			throw new ConfigurationException("No implementation class configured");
		}
		
		try {
			_jobClass = WGACore.getLibraryLoader().loadClass(taskClassName);
		}
		catch (ClassNotFoundException e) {
			throw new ConfigurationException("Class not found: " + taskClassName);
		}
		catch (NoClassDefFoundError e) {
            throw new ConfigurationException("Task '" + taskClassName + "' cannot be instantiated bc. a dependency class is missing", e);
        }
		
		try {
			_job = (TaskImplementation) _jobClass.newInstance();
		}
		catch (InstantiationException e1) {
			throw new ConfigurationException("No default constructor: " + taskClassName);
		}
		catch (IllegalAccessException e1) {
			throw new ConfigurationException("Default constructor not visible: " + taskClassName);
		}

    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.scheduler.Task#execute(de.innovationgate.wgpublisher.scheduler.JobContext)
	 */
	public void execute(JobContext jobContext) throws TaskException {
	
		try {
			_job.execute(jobContext);
		}
		catch (JobFailedException e) {
			throw new TaskException("Job failed: " + e.getMessage(), e);
		}
	
	}

    public String getClassName() {
        return (_jobClass != null ? _jobClass.getName() : null);
    }



	@Override
	public void configure(WGACore core)
			throws ConfigurationException {
		String taskClassName = getOption(OPTION_TASKCLASS);
        setClassName(taskClassName);
	}
	
	@Override
	public String getTitle(Locale locale) {
	    if (_job instanceof TitledTaskImplementation) {
	        Map<String,String> options = new HashMap<String, String>();
	        Map<String,String> jobOptions = getJob().getOptions();
	        if (jobOptions != null) {
	            options.putAll(jobOptions);
	        }
	        if (getDefinition() != null) {
	            Map<String,String> defOptions = getDefinition().getOptions();
	            if (defOptions != null) {
	                options.putAll(getDefinition().getOptions());
	            }
	        }
	        Map<String,Object> customOptions = getCustomOptions();
	        if (customOptions != null) {
	            for (Map.Entry<String,Object> option : customOptions.entrySet()) {
	                if (option.getValue() instanceof String) {
	                    options.put(option.getKey(), (String) option.getValue());
	                }
	            }
	        }
	        
	        return ((TitledTaskImplementation) _job).getTitle(locale, options);
	    }
	    else {
	        return super.getTitle(locale);
	    }
	}

}
