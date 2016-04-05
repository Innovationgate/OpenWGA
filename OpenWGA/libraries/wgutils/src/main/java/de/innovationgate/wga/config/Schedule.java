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


import java.text.ParseException;
import java.util.Date;
import java.util.List;

import org.quartz.CronTrigger;
import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.Root;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.wga.model.ValidationError;

/**
 * The schedule definition of a Job
 */
@Root(strict=false)
public class Schedule extends IdentifiableConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute
	@NotNull
	private String type;
	
	@Attribute(required=false)
	private boolean enabled = false;
	
	@Element
	@NotNull
	private String data;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private Date startDate;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private Date endDate;
	
    public static final String TYPE_SIMPLE = "simple";
    public static final String TYPE_CRON = "cron";
	
	public Schedule() {
	    super();
	}
	
	protected Schedule(String type, String data) {
	    this();
		setType(type);
		setData(data);
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getData() {
		return data;
	}

	public void setData(String data) {
		this.data = data;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	@Override
	protected void validate(List<ValidationError> errors, boolean integrityCheckOnly) {
		int errorCount = errors.size();
		super.validate(errors, integrityCheckOnly);
		
		if (errors.size() - errorCount <= 0) {
			// no integrity errors so far		
			if (!integrityCheckOnly) {
				// validate cron expression
				if (getType().equals(TYPE_CRON)) {
					try {
						new CronTrigger().setCronExpression(getData());
					} catch (ParseException e) {
						errors.add(new ValidationError("Invalid cron expression '" + getData() + "' (" + e.getMessage() + ").", new String[] {"data"}, this));
					}
				}
			}
		}
	}

}
