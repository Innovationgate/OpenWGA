/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

/**
 * Configuration of an OpenWGA scheduler job
 */
@Root(strict=false)
public class Job extends IdentifiableConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute
	@NotNull
	private String name;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String title;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String description;
	
    @ElementMap(entry="option", key="name", attribute=true, required=false)
    @NotNull
    @Options
	private Map<String,String> options = new LinkedHashMap<String,String>();
	
	@ElementList(required=false)
	@NotNull
	private List<Schedule> schedules = new ArrayList<Schedule>();
	
	@ElementList(required=false)
	@NotNull
	private List<Task> tasks = new ArrayList<Task>();


	public Job() {
	    super();
	}
	
	public Job(String name) {
		super();
		setName(name);
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name.toLowerCase();
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Map<String, String> getOptions() {
		return options;
	}

	public List<Schedule> getSchedules() {
		return schedules;
	}

	public List<Task> getTasks() {
		return tasks;
	}


	public void setOptions(Map<String, String> options) {
		if (options == null) {
			this.options = new HashMap<String, String>();
		} else {
			this.options = options;
		}
	}

	public void setSchedules(List<Schedule> schedules) {
		if (schedules == null) {
			this.schedules = new ArrayList<Schedule>();
		} else {
			this.schedules = schedules;
		}
	}

	public void setTasks(List<Task> tasks) {
		if (tasks == null) {
			this.tasks = new ArrayList<Task>();
		} else {
			this.tasks = tasks;
		}
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

}
