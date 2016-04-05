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

import java.util.Comparator;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Root;

/**
 * A lucene rule to index content items
 */
@Root(strict=false)
public class LuceneIndexItemRule extends ConfigBean {
    
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    @Attribute
	@NotNull
    private String itemExpression;
	@Attribute
	@NotNull
    private String indexType;
	@Attribute(required=false)
    private boolean sortable = false;
	@Attribute
	@NotNull
    private String contentType;
	@Attribute(required=false)
    private float boost = 1.0F;
    
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
    
    public static final LuceneIndexItemRule DEFAULT_RULE = new LuceneIndexItemRule(EXPRESSION_WILDCARD, INDEX_TYPE_FULLTEXT, CONTENT_TYPE_PLAINTEXT);
    static {
    	DEFAULT_RULE.setSortable(false);
    	DEFAULT_RULE.setBoost(1.0F);
    }
    
	public LuceneIndexItemRule() {    	
    }
    
    public LuceneIndexItemRule(String itemExpression, String indexType, String contentType) {
    	setItemExpression(itemExpression);
    	setIndexType(indexType);
    	setContentType(contentType);
    }

    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public String getIndexType() {
        return indexType;
    }

    public void setIndexType(String indexType) {
        this.indexType = indexType;
    }

    public String getItemExpression() {
        return itemExpression;
    }

    public void setItemExpression(String itemExpression) {
        this.itemExpression = itemExpression;
    }

    public boolean isSortable() {
        return sortable;
    }

    public void setSortable(boolean sortable) {
        this.sortable = sortable;
    }


	public float getBoost() {
		return boost;
	}

	public void setBoost(float boost) {
		this.boost = boost;
	}
	
	public static final Comparator<LuceneIndexItemRule> COMPARATOR = new Comparator<LuceneIndexItemRule>() {
        public int compare(LuceneIndexItemRule rule1, LuceneIndexItemRule rule2) {
          String exp1 = rule1.getItemExpression().toLowerCase().trim();
          String exp2 = rule2.getItemExpression().toLowerCase().trim();
          
          //wildcard
          String wc = LuceneIndexItemRule.EXPRESSION_WILDCARD;
          
          //no wildcard compare strings
          String noAsterisk1 = exp1;
          if (exp1.endsWith(wc)) {
              noAsterisk1 = exp1.substring(0, exp1.length()-1);
          }
          String noAsterisk2 = exp2;
          if (exp2.endsWith(wc)) {
              noAsterisk2 = exp2.substring(0, exp2.length()-1);
          }
          
          
          //single wildcard at end of list
          if (exp1.equals(wc)) {
              return 1;
          }
          if (exp2.equals(wc)) {
              return -1;
          }
                            
          if ( (exp1.endsWith(wc)) && (!exp2.endsWith(wc)) ) {
              return 1;
          }
          if ( (!exp1.endsWith(wc)) && (exp2.endsWith(wc)) ) {
              return -1;
          }
          if ( (exp1.endsWith(wc)) && (exp2.endsWith(wc)) ) {
              
              if (noAsterisk1.startsWith(noAsterisk2)) {
                  return -1;
              }
              if (noAsterisk2.startsWith(noAsterisk1)) {
                  return 1;
              }
                                    
              return noAsterisk1.compareTo(noAsterisk2);
          } else {
              if (exp1.startsWith(exp2)) {
                  return -1;
              }
              if (exp2.startsWith(exp1)) {
                  return 1;
              }
                                    
              return exp1.compareTo(exp2);                      
          }
        }
        
    };

	@Override
	public boolean isDefaultResource() {
		return getItemExpression() != null && getItemExpression().equals(DEFAULT_RULE.getItemExpression());
	}

}
