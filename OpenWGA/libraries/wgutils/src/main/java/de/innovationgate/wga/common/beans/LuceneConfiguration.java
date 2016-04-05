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
package de.innovationgate.wga.common.beans;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Represents the configuration for the lucene index on one database
 */
public class LuceneConfiguration {
    
    private boolean _enabled;
    private List _itemRules;
    private List _fileRules;
    
    /**
     * Creates an empty not enabled lucene configuration
     */
    public LuceneConfiguration() {
        _enabled = false;
        _itemRules = new ArrayList();
        _fileRules = new ArrayList();
        _itemRules.add(LuceneIndexItemRule.getDefaultRule());
        _fileRules.add(LuceneIndexFileRule.getDefaultRule());
    }    
    
    /**
     * Returns if the lucene index is enabled for this database
     */
    public boolean isEnabled() {
        return _enabled;
    }
    /**
     * Sets if the lucene index is enabled for this database
     * @param enabled
     */
    public void setEnabled(boolean enabled) {
        this._enabled = enabled;
    }
    /**
     * Returns the item rules of this configuration as list of {@link LuceneIndexItemRule} objects
     */
    public List getItemRules() {
        return _itemRules;
    }
    /**
     * Sets the item rules of this configuration.
     * @param rules A list of {@link LuceneIndexItemRule} objects
     */
    public void setItemRules(List rules) {
        this._itemRules = rules;
    }
    /**
     * Returns the file rules of this configuration as list of {@link LuceneIndexFileRule} objects
     */
    public List getFileRules() {
        return _fileRules;
    }
    /**
     * Sets the file rules of this configuration.
     * @param fileRules A list of {@link LuceneIndexFileRule} objects
     */
    public void setFileRules(List fileRules) {
        _fileRules = fileRules;
    }

    
    public boolean equals(Object obj) {
        if (obj instanceof LuceneConfiguration) {
            LuceneConfiguration config = (LuceneConfiguration) obj;
            if (!_enabled == config.isEnabled()) {
                return false;
            }
            
            // check if rules are equal
            if (rulesAreEqual(_itemRules, config.getItemRules()) &&
                rulesAreEqual(_fileRules, config.getFileRules()) ) {
                return true;                
            } else {
                return false;
            }
        } else {
            return super.equals(obj);
        }
    }
    
    /**
     * Compares two lists of rules for equality.
     * Equality is achieved when both lists contain equal rules in equal order.
     * @param ownRules First rule list
     * @param foreignRules Second rule list
     */
    private boolean rulesAreEqual(List ownRules, List foreignRules) {
        if ( (ownRules == null) && (foreignRules == null) ) {
            return true;
        }
        if ( (ownRules != null) && (foreignRules != null) ) {
            if (ownRules.size() != foreignRules.size()) {
                return false;
            }
            
            Iterator rulesItThis = ownRules.iterator();
            Iterator rulesItForeign = foreignRules.iterator();
            
            while (rulesItThis.hasNext()) {
                Object ruleThis = rulesItThis.next();
                Object ruleForeign = rulesItForeign.next();
                if (!ruleThis.equals(ruleForeign)) {
                    return false;
                }
            }                                
        } else {
            return false;
        }
        return true;
    }

    /**
     * Returns the matching item rule for a given item name.
     * To determine this the method will return the first rule in the list that matches
     * the item name.
     * @param itemName The item name
     * @return Item rule that should apply to this item
     */
    public LuceneIndexItemRule getMatchingItemRule(String itemName) {
        Iterator it = _itemRules.iterator();
        while (it.hasNext()) {
            LuceneIndexItemRule rule = (LuceneIndexItemRule) it.next();
            if (rule.isDefaultRule()) {
                return rule;
            }
            if (rule.hasWildcard()) {
                String compare = rule.getItemExpressionClearWildcard().toLowerCase();
                if (itemName.toLowerCase().startsWith(compare)) {
                    return rule;
                }
            }
            if (rule.getItemExpression().equalsIgnoreCase(itemName)) {
                return rule;
            }                
        }                       
        // if no rule found return defaultRule
        return LuceneIndexItemRule.getDefaultRule();        
    }

    /**
     * Returns the matching file rule for a given file name.
     * To determine this the method will return the first rule in the list that matches
     * the file name.
     * @param fileName The file name
     * @return File rule that should apply to this file attachment
     */
    public LuceneIndexFileRule getMatchingFileRule(String fileName) {
        Iterator it = _fileRules.iterator();
        while (it.hasNext()) {
            LuceneIndexFileRule rule = (LuceneIndexFileRule) it.next();
            if (rule.isDefaultRule()) {
                return rule;
            }
            if (rule.hasWildcard()) {
                String compare = rule.getFilePatternClearWildcard().toLowerCase();
                if (fileName.toLowerCase().endsWith(compare)) {
                    return rule;
                }
            }
            if (rule.getFilePattern().equalsIgnoreCase(fileName)) {
                return rule;
            }                
        }                       
        // if no rule found return defaultRule
        return LuceneIndexFileRule.getDefaultRule();        
    }
    



}
