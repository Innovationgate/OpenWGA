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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

import de.innovationgate.wga.model.ValidationError;

/**
 * Global configuration of the OpenWGA scheduler
 */
@Root(strict=false)
public class SchedulerConfiguration extends ConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute(required=false)
	@NormalizeEmptyValue
	private String loggingDir;
	
	@ElementList(required=false)
	@NotNull
	private List<Job> jobs = new ArrayList<Job>();

	public void setJobs(List<Job> jobs) {
		if (jobs == null) {
			this.jobs = new ArrayList<Job>();
		} else {
			this.jobs = jobs;
		}
	}

	public String getLoggingDir() {
		return loggingDir;
	}

	public void setLoggingDir(String loggingDir) {
		this.loggingDir = loggingDir;
	}

	public List<Job> getJobs() {
		return jobs;
	}
	
	public Job getJobByName(String name) {
	    
	    for (Job job : jobs) {
	        if (job.getName().equals(name)) {
	            return job;
	        }
	    }
	    
	    return null;
	    
	}

	@Override
	protected void validate(List<ValidationError> errors, boolean integrityCheckOnly) {
		super.validate(errors, integrityCheckOnly);
		
		// check for duplicate job uids & name
		Iterator<Job> jobs = getJobs().iterator();		
		Set<String> uids = new HashSet<String>();
		Set<String> names = new HashSet<String>();
		while (jobs.hasNext()) {
			Job job = jobs.next();
			if (uids.contains(job.getUid())) {
				errors.add(new ValidationError("Duplicate job uid '" + job.getUid()  + "'.", new String[]{"jobs"}, this));
			} else {
				uids.add(job.getUid());
			}
			if (names.contains(job.getName())) {
				errors.add(new ValidationError("Duplicate job name '" + job.getName()  + "'.", new String[]{"jobs"}, this));
			} else {
				names.add(job.getName());
			}
		}
	}
}
