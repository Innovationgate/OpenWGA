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

import org.dom4j.Element;

import de.innovationgate.utils.WGUtils;

/**
 * Represents a lucene rule to index file attachments
 */
public class LuceneIndexFileRule {
    
    private String _filePattern = "";
    private int _fileSizeLimit = 0;
    
    private boolean _includedInAllContent = false;
    
    private float _boost = 1.0F;
    
    /**
     * Wildcard for file name patterns
     */
    public static final String FILEPATTERN_WILDCARD = "*";       
    /**
     * File size limit representing that no files are indexed
     */
    public static final int FILESIZELIMIT_INDEX_NONE = 0;
    /**
     * File size limit representing that all files are indexed
     */
    public static final int FILESIZELIMIT_INDEX_ALL = -1;
   
    /**
     * get rules from configfile element lucene
     * @param lucene configfile element
     * @return list LuceneIndexFileRules
     */
    public static List getRules(Element lucene) {
        ArrayList list = new ArrayList();
        Element rules = lucene.element("filerules");
        if (rules == null) {
            return null;
        }
        Iterator rulesIt = rules.elementIterator("filerule");
        while (rulesIt.hasNext()) {
            Element ruleElement = (Element) rulesIt.next();
            LuceneIndexFileRule rule = new LuceneIndexFileRule();
            rule.setFilePattern(ruleElement.getText());            
            rule.setFileSizeLimit(Integer.parseInt(ruleElement.attributeValue("filesizelimit")));            
            if (ruleElement.attributeValue("includedinallcontent").equals("true")) {
                rule.setIncludedInAllContent(true);
            } else {
                rule.setIncludedInAllContent(false);
            }
            rule.setBoost(Float.parseFloat(ruleElement.attributeValue("boost", "1.0")));
            list.add(rule);
        }
        return list;
    }
    
    /**
     * get rules from configbean list
     * @return list LuceneIndexItemRules
     */
    public static List<LuceneIndexFileRule> getRules(List<de.innovationgate.wga.config.LuceneIndexFileRule> rules) {
        ArrayList<LuceneIndexFileRule> list = new ArrayList<LuceneIndexFileRule>();
        Iterator<de.innovationgate.wga.config.LuceneIndexFileRule> itemrulesIt = rules.iterator();
        while (itemrulesIt.hasNext()) {
            de.innovationgate.wga.config.LuceneIndexFileRule ruleConf = itemrulesIt.next();
            LuceneIndexFileRule rule = new LuceneIndexFileRule();
            rule.setFilePattern(ruleConf.getFilePattern());            
            rule.setFileSizeLimit(ruleConf.getFileSizeLimit());
            rule.setIncludedInAllContent(ruleConf.isIncludedInAllContent());
            rule.setBoost(ruleConf.getBoost());
            list.add(rule);
        }
        return list;
    }
    
    /**
     * Saves rules to lucene configuration in wga.xml
     * @param lucene The lucene configuration element
     * @param rules List of {@link LuceneIndexFileRule} objects to be saved
     */
    public static void saveRules(Element lucene, List rules) {
        Element filerules = WGUtils.getOrCreateElement(lucene, "filerules");
        //remove old rules
        filerules.clearContent();
        Iterator itemrulesIt = rules.iterator();
        while (itemrulesIt.hasNext()) {
            LuceneIndexFileRule rule = (LuceneIndexFileRule) itemrulesIt.next();
            Element ruleElement = filerules.addElement("filerule");
            ruleElement.addAttribute("filesizelimit", new Integer(rule.getFileSizeLimit()).toString());            
            if (rule.isIncludedInAllContent()) {
                ruleElement.addAttribute("includedinallcontent", "true");
            } else {
                ruleElement.addAttribute("includedinallcontent", "false");
            }
            ruleElement.addAttribute("boost", Float.toString(rule.getBoost()));
            ruleElement.addText(rule.getFilePattern());
        }
    }
    
    /**
     * Returns the default rule for newly created databases
     */
    public static LuceneIndexFileRule getDefaultRule() {
        LuceneIndexFileRule rule = new LuceneIndexFileRule();
        rule.setFilePattern(FILEPATTERN_WILDCARD);
        rule.setFileSizeLimit(FILESIZELIMIT_INDEX_NONE);
        rule.setIncludedInAllContent(false);
        rule.setBoost(1.0F);
        return rule;
    }    
    
    /**
     * adds the default rule to the lucene.itemrules element
     * @param lucene luceneElement
     */
    public static void addDefaultRule(Element lucene) {
        Element rules = lucene.element("filerules");
        LuceneIndexFileRule rule = getDefaultRule();
        Element ruleElement = rules.addElement("filerule");
        ruleElement.addAttribute("filesizelimit", new Integer(rule.getFileSizeLimit()).toString());            
        if (rule.isIncludedInAllContent()) {
            ruleElement.addAttribute("includedinallcontent", "true");
        } else {
            ruleElement.addAttribute("includedinallcontent", "false");
        }
        ruleElement.addAttribute("boost", Float.toString(rule.getBoost()));
        ruleElement.addText(rule.getFilePattern());    
    }  
    
    /**
     * Determines if the rule file pattern has a wildcard
     */
    public boolean hasWildcard() {
        if (this._filePattern == null) {
            return false;
        }
        if (this._filePattern.indexOf(FILEPATTERN_WILDCARD) != -1) {
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Determines if this rule is a default rule that will apply to all files not taken by other rules
     */
    public boolean isDefaultRule() {
        if (this._filePattern == null) {
            return false;
        }
        if (this._filePattern.trim().equals(FILEPATTERN_WILDCARD)) {
            return true;
        } else {
            return false;
        }
    }
    
    
    /**
     * Returns the file pattern name without eventually trailing text after the wildcard
     */
    public String getFilePatternClearWildcard() {
        if (this.isDefaultRule()) {
            return "";
        } else {
            if (_filePattern.length() > 1) {
                return _filePattern.substring(_filePattern.indexOf(FILEPATTERN_WILDCARD)+1);
            } else {
                return "";
            }
        }
    }    

    
    public boolean equals(Object obj) {
        if (obj instanceof LuceneIndexFileRule) {
            LuceneIndexFileRule rule = (LuceneIndexFileRule) obj;
            if ( (_filePattern.equals(rule.getFilePattern())) && 
                 (_fileSizeLimit == rule.getFileSizeLimit()) && 
                 (_includedInAllContent == rule.isIncludedInAllContent()) && 
                 (_boost == rule.getBoost()) ) {
                return true;
            } else {
                return false;
            }
        } else {
            return super.equals(obj);
        }
    }

    /**
     * Returns the file pattern
     */
    public String getFilePattern() {
        return _filePattern;
    }

    /**
     * Sets the file pattern. The pattern may end with a wildcard character {@link #FILEPATTERN_WILDCARD}.
     * @param filePattern
     */
    public void setFilePattern(String filePattern) {
        _filePattern = filePattern;
    }

    /**
     * Returns the size limit of bytes for indexing files via this rule.
     * This rule will only apply to files up to this limit.
     * Note special values {@link #FILESIZELIMIT_INDEX_ALL} and {@link #FILESIZELIMIT_INDEX_NONE} for this setting.
     */
    public int getFileSizeLimit() {
        return _fileSizeLimit;
    }

    /**     
     * Sets the size limit of bytes for indexing files via this rule.
     * This rule will only apply to files up to this limit.
     * Note special values {@link #FILESIZELIMIT_INDEX_ALL} and {@link #FILESIZELIMIT_INDEX_NONE} for this setting.

     * @param fileSizeLimit
     */
    public void setFileSizeLimit(int fileSizeLimit) {
        _fileSizeLimit = fileSizeLimit;
    }

    /**
     * Returns if the index information of this file should be used for unspecific searches where no item to search is defined.
     */
    public boolean isIncludedInAllContent() {
        return _includedInAllContent;
    }

    /**
     * Sets if the index information of this file should be used for unspecific searches where no item to search if defined.
     * If this is false the index information will only be used for searches against item "allattachments"
     * @param includedInAllContent
     */
    public void setIncludedInAllContent(boolean includedInAllContent) {
        _includedInAllContent = includedInAllContent;
    }
    
    /**
     * returns the boost value of this rule
     */
	public float getBoost() {
		return _boost;
	}
	
	/**
	 * sets the boost value for this rule
	 * @param boost
	 * @throws IllegalArgumentException if boost is <=0
	 */
	public void setBoost(float boost) {
		if (boost <= 0) {
			throw new IllegalArgumentException("Boost value must be >0.");
		}
		_boost = boost;
	}
    

}
