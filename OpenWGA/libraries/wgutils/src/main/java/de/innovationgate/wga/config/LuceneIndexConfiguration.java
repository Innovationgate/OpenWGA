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
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

import de.innovationgate.wga.model.ValidationError;

/**
 * Configuration of the Lucene fulltext index for a single OpenWGA application
 */
@Root(strict=false)
public class LuceneIndexConfiguration extends ConfigBean {
    
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute(required=false)
    private boolean enabled = false;
	
	@ElementList(required=false)
	@NotNull
    private List<LuceneIndexItemRule> itemRules = new ArrayList<LuceneIndexItemRule>();

	@ElementList(required=false)
	@NotNull
    private List<LuceneIndexFileRule> fileRules = new ArrayList<LuceneIndexFileRule>();

	@ElementList(required=false)
	@NotNull
	private List<String> indexEnhancers = new ArrayList<String>();
	
    @Attribute(required=false)
    private boolean indexFileContentOnDocuments = false;

	public boolean isIndexFileContentOnDocuments() {
        return indexFileContentOnDocuments;
    }

    public void setIndexFileContentOnDocuments(boolean indexFileContentOnDocuments) {
        this.indexFileContentOnDocuments = indexFileContentOnDocuments;
    }
    
    public List<String> getIndexEnhancers() {
        return indexEnhancers;
    }

    public void setIndexEnhancers(List<String> indexEnhancers) {
        this.indexEnhancers = indexEnhancers;
    }

    public LuceneIndexConfiguration() {
	}
    
    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public List<LuceneIndexItemRule> getItemRules() {
        return itemRules;
    }

    public List<LuceneIndexFileRule> getFileRules() {
        return fileRules;
    }
    
	public void setItemRules(List<LuceneIndexItemRule> itemRules) {
		if (itemRules == null) {
			this.itemRules = new ArrayList<LuceneIndexItemRule>();
			this.itemRules.add(LuceneIndexItemRule.DEFAULT_RULE);
		} else {
			boolean hasDefaultRule = false;
			Iterator<LuceneIndexItemRule> itemRulesIt = itemRules.iterator();
			while (itemRulesIt.hasNext()) {
				LuceneIndexItemRule rule = itemRulesIt.next();
				if (rule.getItemExpression().equals(LuceneIndexItemRule.DEFAULT_RULE.getItemExpression())) {
					hasDefaultRule = true;
					break;
				}
			}
			if (!hasDefaultRule) {
				itemRules.add(LuceneIndexItemRule.DEFAULT_RULE);
			}
			this.itemRules = itemRules;
		}

		Collections.sort(itemRules, LuceneIndexItemRule.COMPARATOR);
	}

	public void setFileRules(List<LuceneIndexFileRule> fileRules) {
		if (fileRules == null) {
			this.fileRules = new ArrayList<LuceneIndexFileRule>();
			this.fileRules.add(LuceneIndexFileRule.DEFAULT_RULE);
		} else {
    		Iterator<LuceneIndexFileRule> fileRulesIt = fileRules.iterator();
    		boolean hasDefaultRule = false;
    		while (fileRulesIt.hasNext()) {
    			LuceneIndexFileRule rule = fileRulesIt.next();
    			if (rule.getFilePattern().equals(LuceneIndexFileRule.DEFAULT_RULE.getFilePattern())) {
    				hasDefaultRule = true;
    				break;
    			}
    		}
    		if (!hasDefaultRule) {
    			fileRules.add(LuceneIndexFileRule.DEFAULT_RULE);
    		}
			this.fileRules = fileRules;
		}
		Collections.sort(fileRules, LuceneIndexFileRule.COMPARATOR);
	}

	@Override
	protected void validate(List<ValidationError> errors, boolean integrityCheckOnly) {
		int errorCount = errors.size();
		super.validate(errors, integrityCheckOnly);
		
		if (errors.size() - errorCount <= 0) {
			// no integrity errors so far			
			// sort rules
			//Collections.sort(itemRules, LuceneIndexItemRule.COMPARATOR);
			//Collections.sort(fileRules, LuceneIndexFileRule.COMPARATOR);
		
			if (!integrityCheckOnly) {
				// check for duplicate item rules and default rule
				Iterator<LuceneIndexItemRule> itemRulesIt = itemRules.iterator();
				Set<String> expressions = new HashSet<String>();
				while (itemRulesIt.hasNext()) {
					LuceneIndexItemRule rule = itemRulesIt.next();
					if (expressions.contains(rule.getItemExpression())) {
						errors.add(new ValidationError("Duplicate item rule for expression '" + rule.getItemExpression() + "'.", new String[] {"itemRules"}));
					} else {
						expressions.add(rule.getItemExpression());
					}
				}
				if (!expressions.contains(LuceneIndexItemRule.DEFAULT_RULE.getItemExpression()) || itemRules.isEmpty()) {
					errors.add(new ValidationError("Default item rule '" + LuceneIndexItemRule.DEFAULT_RULE.getItemExpression() + "' is missing.", new String[] {"itemRules"}));
				}
				
				// check for duplicate file rules and default rule
				Iterator<LuceneIndexFileRule> fileRulesIt = fileRules.iterator();
				expressions.clear();
				while (fileRulesIt.hasNext()) {
					LuceneIndexFileRule rule = fileRulesIt.next();
					if (expressions.contains(rule.getFilePattern())) {
						errors.add(new ValidationError("Duplicate file rule for pattern '" + rule.getFilePattern() + "'.", new String[] {"fileRules"}));
					} else {
						expressions.add(rule.getFilePattern());
					}
				}
				if (!expressions.contains(LuceneIndexFileRule.DEFAULT_RULE.getFilePattern()) || fileRules.isEmpty()) {
					errors.add(new ValidationError("Default file rule '" + LuceneIndexFileRule.DEFAULT_RULE.getFilePattern() + "' is missing.", new String[] {"fileRules"}));
				}		
			}
		}
	}

}
