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
 * A lucene rule to index file attachments
 */
@Root(strict=false)
public class LuceneIndexFileRule extends ConfigBean {
    
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    @Attribute
	@NotNull
    private String filePattern;
	@Attribute(required=false)
    private int fileSizeLimit = 0;
    @Attribute(required=false)
    private boolean includedInAllContent = false;
    @Attribute(required=false)
    private float boost = 1.0F;
    
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
    
    public static final LuceneIndexFileRule DEFAULT_RULE = new LuceneIndexFileRule(FILEPATTERN_WILDCARD);
    static {
    	DEFAULT_RULE.setFileSizeLimit(FILESIZELIMIT_INDEX_NONE);
    	DEFAULT_RULE.setIncludedInAllContent(false);
    	DEFAULT_RULE.setBoost(1.0F);
    }
    
    public LuceneIndexFileRule() {    	
    }

    public LuceneIndexFileRule(String filePattern) {
    	setFilePattern(filePattern);
    }

    public String getFilePattern() {
        return filePattern;
    }

    public void setFilePattern(String filePattern) {
        this.filePattern = filePattern;
    }

    public int getFileSizeLimit() {
        return fileSizeLimit;
    }

    public void setFileSizeLimit(int fileSizeLimit) {
        this.fileSizeLimit = fileSizeLimit;
    }

    public boolean isIncludedInAllContent() {
        return includedInAllContent;
    }

    public void setIncludedInAllContent(boolean includedInAllContent) {
        this.includedInAllContent = includedInAllContent;
    }

	public float getBoost() {
		return boost;
	}

	public void setBoost(float boost) {
		this.boost = boost;
	}
    
    public static final Comparator<LuceneIndexFileRule> COMPARATOR = new Comparator<LuceneIndexFileRule>() {
        public int compare(LuceneIndexFileRule rule1, LuceneIndexFileRule rule2) {
        	  String pattern1 = rule1.getFilePattern().toLowerCase().trim();
	          String pattern2 = rule2.getFilePattern().toLowerCase().trim();
	          
	          //wildcard
	          String wc = LuceneIndexFileRule.FILEPATTERN_WILDCARD;
	          
	          //no wildcard compare strings
	          String noWCPattern1 = pattern1;
	          if (pattern1.startsWith(wc)) {
	              noWCPattern1 = pattern1.substring(1);
	          }
	          String noWCPattern2 = pattern2;
	          if (pattern2.startsWith(wc)) {
	              noWCPattern2 = pattern2.substring(1);
	          }
	          
	          
	          //single wildcard at end of list
	          if (pattern1.equals(wc)) {
	              return 1;
	          }
	          if (pattern2.equals(wc)) {
	              return -1;
	          }
	                            
	          if ( (pattern1.startsWith(wc)) && (!pattern2.startsWith(wc)) ) {
	              return 1;
	          }
	          if ( (!pattern1.startsWith(wc)) && (pattern2.startsWith(wc)) ) {
	              return -1;
	          }
	          if ( (pattern1.startsWith(wc)) && (pattern2.startsWith(wc)) ) {
	              
	              if (noWCPattern1.startsWith(noWCPattern2)) {
	                  return -1;
	              }
	              if (noWCPattern2.startsWith(noWCPattern1)) {
	                  return 1;
	              }
	                                    
	              return noWCPattern1.compareTo(noWCPattern2);
	          } else {
	              if (pattern1.startsWith(pattern2)) {
	                  return -1;
	              }
	              if (pattern2.startsWith(pattern1)) {
	                  return 1;
	              }
	                                    
	              return pattern1.compareTo(pattern2);                      
	          }         
        }
        
    };

	@Override
	public boolean isDefaultResource() {
		return getFilePattern() != null && getFilePattern().equals(DEFAULT_RULE.getFilePattern());
	}
}
