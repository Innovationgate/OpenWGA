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

import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

import de.innovationgate.wga.model.ValidationError;

/**
 * Global configuration for OpenWGA designs
 */
@Root(strict=false)
public class DesignConfiguration extends ConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    @Element(required=false)
	@NormalizeEmptyValue
	private String defaultEncoding;
	@Element(required=false)
	private int pollingInterval = 1;
	@Element(required=false)
	private boolean throttlingEnabled = false;
	@Element(required=false)
	private int throttlingPeriodMinutes = 10;
	@Element(required=false)
	@Deprecated
	private int designFileCacheSize = 1000;
	
	@ElementList(required=false)
	@NotNull
	private List<String> fileExclusions = new ArrayList<String>();


	@ElementList
	@NotNull
    private List<DesignSource> designSources = new ArrayList<DesignSource>();
	
	public DesignConfiguration() {	
	}
	
	public String getDefaultEncoding() {
		return defaultEncoding;
	}
	
	public void setDefaultEncoding(String defaultEncoding) {
		this.defaultEncoding = defaultEncoding;
	}
	
	public int getPollingInterval() {
		return pollingInterval;
	}
	
	public void setPollingInterval(int pollingInterval) {
		this.pollingInterval = pollingInterval;
	}
	
	public boolean isThrottlingEnabled() {
		return throttlingEnabled;
	}
	
	public void setThrottlingEnabled(boolean throttlingEnabled) {
		this.throttlingEnabled = throttlingEnabled;
	}
	
	public int getThrottlingPeriodMinutes() {
		return throttlingPeriodMinutes;
	}
	
	public void setThrottlingPeriodMinutes(int throttlingPeriodMinutes) {
		this.throttlingPeriodMinutes = throttlingPeriodMinutes;
	}

	public List<String> getFileExclusions() {
		return fileExclusions;
	}

    public List<DesignSource> getDesignSources() {
        return designSources;
    }
    
    public DesignSource getDesignSource(String uid) {
    	Iterator<DesignSource> sources = designSources.iterator();
    	while (sources.hasNext()) {
    		DesignSource source = sources.next();
    		if (source.getUid().equals(uid)) {
    			return source;
    		}
    	}
    	return null;
    }

    /**
     * @deprecated Succeeded by server option {@link WGAConfiguration#SERVEROPTION_CACHE_DESIGN_SIZE} 
     */
    @Deprecated
    public int getDesignFileCacheSize() {
        return designFileCacheSize;
    }

    /**
     * @deprecated Succeeded by server option {@link WGAConfiguration#SERVEROPTION_CACHE_DESIGN_SIZE} 
     */
    @Deprecated
    public void setDesignFileCacheSize(int designFileCacheSize) {
        this.designFileCacheSize = designFileCacheSize;
    }
    
	
	public void setFileExclusions(List<String> fileExclusions) {
		if (fileExclusions == null) {
			this.fileExclusions = new ArrayList<String>();
		} else {
			this.fileExclusions = fileExclusions;
		}
	}

	public void setDesignSources(List<DesignSource> designSources) {
		if (designSources == null) {
			this.designSources = new ArrayList<DesignSource>();
		} else {
			this.designSources = designSources;
		}
	}

	@Override
	protected void validate(List<ValidationError> errors, boolean integrityCheckOnly) {
		super.validate(errors, integrityCheckOnly);
		
		// check for duplicate design source uids
		Iterator<DesignSource> designSources = getDesignSources().iterator();
		Set<String> uids = new HashSet<String>();
		while (designSources.hasNext()) {
			DesignSource designSource = designSources.next();
			if (uids.contains(designSource.getUid())) {
				errors.add(new ValidationError("Duplicate designsource uid '" + designSource.getUid()  + "'.", new String[]{"designSources"}, this));
			}
		}
	}

}
