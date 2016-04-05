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

/**
 * Represents a lucene rule to index content items
 *
 */
public class LuceneIndexItemRule {
    
    private String _itemExpression = "";
    private String _indexType = "";
    private boolean _sortable = false;
    private String _contentType = "";
    private float _boost = 1.0F;
    
    /**
     * Wildcard for item name patterns
     */
    public static final String EXPRESSION_WILDCARD = "*";       
    
    /**
     * Indexing type "keyword"
     */
    public static final String INDEX_TYPE_KEYWORD = "keyword";
    /**
     * Indexing type "fulltext"
     */
    public static final String INDEX_TYPE_FULLTEXT = "fulltext";
    /**
     * Indexing type "noindex" - Meaning that items applying to this rule will not be indexed
     */
    public static final String INDEX_TYPE_NO_INDEX = "noindex";
    
    /**
     * Content type "plaintext" which will be indexed unparsed
     */
    public static final String CONTENT_TYPE_PLAINTEXT = "plaintext";
    /**
     * Content type "htmlxml" which will index only real content, no HTML/XML tag code
     */
    public static final String CONTENT_TYPE_HTML_XML = "htmlxml";    
   

    /**
     * get rules from configfile element lucene
     * @param lucene configfile element
     * @return list LuceneIndexItemRules
     */
    public static List getRules(Element lucene) {
        ArrayList list = new ArrayList();
        Element itemrules = lucene.element("itemrules");
        Iterator itemrulesIt = itemrules.elementIterator("itemrule");
        while (itemrulesIt.hasNext()) {
            Element itemruleElement = (Element) itemrulesIt.next();
            LuceneIndexItemRule rule = new LuceneIndexItemRule();
            rule.setItemExpression(itemruleElement.getText());
            rule.setIndexType(itemruleElement.attributeValue("indextype"));
            rule.setContentType(itemruleElement.attributeValue("contenttype"));
            if (itemruleElement.attributeValue("sortable").equals("true")) {
                rule.setSortable(true);
            } else {
                rule.setSortable(false);
            }
            rule.setBoost(Float.parseFloat(itemruleElement.attributeValue("boost", "1.0")));
            list.add(rule);
        }
        return list;
    }
    
    /**
     * get rules from configbean list
     * @return list LuceneIndexItemRules
     */
    public static List<LuceneIndexItemRule> getRules(List<de.innovationgate.wga.config.LuceneIndexItemRule> rules) {
        ArrayList<LuceneIndexItemRule> list = new ArrayList<LuceneIndexItemRule>();
        Iterator<de.innovationgate.wga.config.LuceneIndexItemRule> itemrulesIt = rules.iterator();
        while (itemrulesIt.hasNext()) {
            de.innovationgate.wga.config.LuceneIndexItemRule ruleConf = itemrulesIt.next();
            LuceneIndexItemRule rule = new LuceneIndexItemRule();
            rule.setItemExpression(ruleConf.getItemExpression());
            rule.setIndexType(ruleConf.getIndexType());
            rule.setContentType(ruleConf.getContentType());
            rule.setSortable(ruleConf.isSortable());
            rule.setBoost(ruleConf.getBoost());
            list.add(rule);
        }
        return list;
    }
    
    /**
     * Saves rules to lucene configuration in wga.xml
     * @param lucene The lucene configuration element
     * @param rules List of {@link LuceneIndexItemRule} objects to be saved
     */
    public static void saveRules(Element lucene, List rules) {        
        Element itemrules = lucene.element("itemrules");
        //remove old rules
        itemrules.clearContent();
        Iterator itemrulesIt = rules.iterator();
        while (itemrulesIt.hasNext()) {
            LuceneIndexItemRule rule = (LuceneIndexItemRule) itemrulesIt.next();
            Element itemrule = itemrules.addElement("itemrule");
            itemrule.addAttribute("indextype", rule.getIndexType());
            itemrule.addAttribute("contenttype", rule.getContentType());
            if (rule.isSortable()) {
                itemrule.addAttribute("sortable", "true");
            } else {
                itemrule.addAttribute("sortable", "false");
            }
            itemrule.addAttribute("boost", Float.toString(rule.getBoost()));
            itemrule.addText(rule.getItemExpression());
        }
    }
    
    /**
     * Returns the default rule for newly created databases
     */
    public static LuceneIndexItemRule getDefaultRule() {
        LuceneIndexItemRule rule = new LuceneIndexItemRule();
        rule.setItemExpression("*");
        rule.setIndexType(LuceneIndexItemRule.INDEX_TYPE_FULLTEXT);
        rule.setContentType(LuceneIndexItemRule.CONTENT_TYPE_PLAINTEXT);
        rule.setSortable(false);
        rule.setBoost(1.0F);
        return rule;
    }    
    
    /**
     * adds the default rule to the lucene.itemrules element
     * @param lucene luceneElement
     */
    public static void addDefaultRule(Element lucene) {
        Element itemrules = lucene.element("itemrules");
        LuceneIndexItemRule rule = getDefaultRule();
        Element itemrule = itemrules.addElement("itemrule");
        itemrule.addAttribute("indextype", rule.getIndexType());
        itemrule.addAttribute("contenttype", rule.getContentType());
        if (rule.isSortable()) {
            itemrule.addAttribute("sortable", "true");
        } else {
            itemrule.addAttribute("sortable", "false");
        }
        itemrule.addAttribute("boost", Float.toString(rule.getBoost()));
        itemrule.addText(rule.getItemExpression());
        
    }  
    
    /**
     * Determines if the rule item name pattern has a wildcard
     */
    public boolean hasWildcard() {
        if (this._itemExpression == null) {
            return false;
        }
        if (this._itemExpression.indexOf(EXPRESSION_WILDCARD) != -1) {
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Determines if this rule is a default rule that will apply to all items not taken by other rules
     */
    public boolean isDefaultRule() {
        if (this._itemExpression == null) {
            return false;
        }
        if (this._itemExpression.trim().equals(EXPRESSION_WILDCARD)) {
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Returns the content type of indexing as constant CONTENT_TYPE_...
     */
    public String getContentType() {
        return _contentType;
    }

    /**
     * Sets the content type for indexing. This will trigger eventual parsing to extract text from the item format.
     * Use constants CONTENT_TYPE...
     * @param contentType
     */
    public void setContentType(String contentType) {
        _contentType = contentType;
    }

    /**
     * Returns the indexing type as constant INDEX_TYPE...
     */
    public String getIndexType() {
        return _indexType;
    }

    /**
     * Sets the indexing type. Use constant INDEX_TYPE..
     * @param indexType
     */
    public void setIndexType(String indexType) {
        _indexType = indexType;
    }

    /**
     * Returns the item name pattern
     */
    public String getItemExpression() {
        return _itemExpression;
    }
    
    /**
     * Returns the item name pattern name without eventually trailing text after the wildcard
     */
    public String getItemExpressionClearWildcard() {
        if (this.isDefaultRule()) {
            return "";
        } else {
            return _itemExpression.substring(0, _itemExpression.length()-1);
        }
    }    

    /**
     * Sets the item name pattern. The pattern may end with a wildcard character {@link #EXPRESSION_WILDCARD}.
     * @param itemExpression
     */
    public void setItemExpression(String itemExpression) {
        _itemExpression = itemExpression;
    }

    /**
     * Returns if searches should be sortable by items applying to this rule
     */
    public boolean isSortable() {
        return _sortable;
    }

    /**
     * Sets if searches should be sortable by items applying to this rule
     * @param sortable
     */
    public void setSortable(boolean sortable) {
        _sortable = sortable;
    }
    
    public boolean equals(Object obj) {
        if (obj instanceof LuceneIndexItemRule) {
            LuceneIndexItemRule rule = (LuceneIndexItemRule) obj;
            if ( (_contentType.equals(rule.getContentType())) && 
                 (_indexType.equals(rule.getIndexType())) && 
                 (_itemExpression.equals(rule.getItemExpression())) &&
                 (_sortable == rule.isSortable()) && 
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
     * Parses an items value according to the content type of this rule
     * @param value The item value
     * @return The parsed item value
     */
    public String parseItemValue(String value) {        
        if (value != null) {                    
            // if content type html/xml replace tags with '$' which is removed by analyzer
            // so <table id="tab1"> is replaced with "$$$$$$$$$$$$$$$$$"
            // this change was necessary for feature 'F0000367A' to ensure
            // tokenStream by analyzer has the same length than the original text
            if (getContentType().equals(LuceneIndexItemRule.CONTENT_TYPE_HTML_XML)) {
                StringBuffer output = new StringBuffer();
                char[] chars = value.toCharArray();
                boolean tagOpen = false;
                for (int i=0; i< chars.length; i++) {
                    
                    if (chars[i] =='<') {
                        tagOpen = true;
                    }
                    
                    if (tagOpen) {
                        output.append("$");
                    } else {
                        output.append(chars[i]);
                    }
                    
                    if (chars[i] == '>') {
                        tagOpen = false;
                    }
                }
                
                value = output.toString();
            }
        }
        return value;
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
