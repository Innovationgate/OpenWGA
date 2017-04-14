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

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.Root;

/**
 * Global configuration of the Lucene fulltext index
 */
@Root(strict=false)
public class LuceneManagerConfiguration extends ConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute(required=false)
	private boolean enabled = true;
	
	@Element
	@NotNull
	private String path;
	
	@Element(required=false)
	private int maxBooleanClauseCount = 1024;
	
	@Element(required=false)
	private int maxDocsPerDBSession = 5;
	
	@Element(required=false)
    private int indexInterval = 5000;

    @Attribute(required=false)
    private boolean optimizeIndexAutomatically = true;

    @Attribute(required=false)
	private boolean useLanguageAnalyzers = false;	// to be backwords compatible

    @Attribute(required=false)
	private boolean indexReleasedContentsOnly=false;

	public LuceneManagerConfiguration() {	
	}
	
	public LuceneManagerConfiguration(String path) {
		setPath(path);
	}
	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public int getMaxBooleanClauseCount() {
		return maxBooleanClauseCount;
	}

	public void setMaxBooleanClauseCount(int maxBooleanClauseCount) {
		this.maxBooleanClauseCount = maxBooleanClauseCount;
	}

	public int getMaxDocsPerDBSession() {
		return maxDocsPerDBSession;
	}

	public void setMaxDocsPerDBSession(int maxDocsPerDBSession) {
		this.maxDocsPerDBSession = maxDocsPerDBSession;
	}

    public int getIndexInterval() {
        return indexInterval;
    }

    public void setIndexInterval(int indexInterval) {
        this.indexInterval = indexInterval;
    }

    public boolean isOptimizeIndexAutomatically() {
        return optimizeIndexAutomatically;
    }

    public void setOptimizeIndexAutomatically(boolean optimizeIndexAutomatically) {
        this.optimizeIndexAutomatically = optimizeIndexAutomatically;
    }

    public void setUseLanguageAnalyzers(boolean use) {
        this.useLanguageAnalyzers = use;
    }
    public boolean isUseLanguageAnalyzers() {
        return useLanguageAnalyzers;
    }

	public boolean isIndexReleasedContentsOnly() {
		return indexReleasedContentsOnly;
	}
	
	public void setIndexReleasedContentsOnly(boolean index) {
		this.indexReleasedContentsOnly=index;
	}

}
