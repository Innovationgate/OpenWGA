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

import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.dom4j.Element;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.types.SchedulerTaskModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.modules.schedulertasks.SchedulerTaskProperties;

public abstract class Task {

    private Job job;
	private String description;
	private boolean cancelJobOnFail = true;
	private de.innovationgate.wga.config.Task definition;
	private ThreadLocal<Map<String,Object>> customOptions = new ThreadLocal<Map<String,Object>>();
	
	
	protected Map<String,Object> getCustomOptions() {
        return customOptions.get();
    }

    protected void setCustomOptions(Map<String,Object> customOptions) {
        this.customOptions.set(customOptions);
    }

    public void init(Job job, de.innovationgate.wga.config.Task definition) {
	    this.job = job;
	    this.definition = definition;
	}
	
	public String getUid() {
	    return this.definition.getUid();
	}
	
	public abstract void configure(WGACore core) throws ConfigurationException;
	public abstract void execute(JobContext jobContext) throws TaskException;


	/**
	 * @return
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @param string
	 */
	public void setDescription(String string) {
		description = string;
	}

	/**
	 * @return
	 */
	public boolean isCancelJobOnFail() {
		return cancelJobOnFail;
	}

	/**
	 * @param b
	 */
	public void setCancelJobOnFail(boolean b) {
		cancelJobOnFail = b;
	}
	
	protected String getOption(String key, String defaultValue) {
	    
	    if (getCustomOptions() != null && getCustomOptions().containsKey(key)) {
	        Object value = getCustomOptions().get(key);
	        if (value != null) {
	            return value.toString();
	        }
	        else {
	            return null;
	        }
	    }
	    
	    if (definition != null && definition.getOptions().containsKey(key)) {
	        return definition.getOptions().get(key);
	    }
	    
	    if (job != null && job.getOptions().containsKey(key)) {
	        return job.getOptions().get(key);
	    }
	    
		return defaultValue;
	}
	

	
	protected String getOption(String key) {
	    return getOption(key, null);
	}
    public de.innovationgate.wga.config.Task getDefinition() {
        return definition;
    }
    public void setDefinition(de.innovationgate.wga.config.Task definition) {
        this.definition = definition;
    }
    
    public String getTitle(Locale locale) {
        
        ModuleRegistry reg = job.getScheduler().getCore().getModuleRegistry();
        ModuleDefinition def = reg.getModuleDefinition(SchedulerTaskModuleType.class, getClass().getName());
        if (def == null) {
            return "Task " + getClass().getName();
        }
        
        SchedulerTaskProperties props = (SchedulerTaskProperties) def.getProperties();
        if (props == null || definition == null) {
            return def.getTitle(locale);
        }
        
        return props.createTitle(definition, locale);
        
    }

    protected Job getJob() {
        return job;
    }

}
