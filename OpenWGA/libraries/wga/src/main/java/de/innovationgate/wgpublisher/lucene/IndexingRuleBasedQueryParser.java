/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.wgpublisher.lucene;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryParser.MultiFieldQueryParser;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.search.MultiTermQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.util.Version;

import de.innovationgate.wga.common.beans.LuceneConfiguration;
import de.innovationgate.wga.common.beans.LuceneIndexItemRule;

/**
 * This analyzer is used in LuceneManger to parse the query-phrase.
 * Based on the indexingRules it decides if a field have to be analyzed or not.
 * (issue: Keywordfield is analyzed by the standard QueryParser)
 * The standard wga search should be like Google. Defaultoparator is set to "AND".
 *
 */
public class IndexingRuleBasedQueryParser extends MultiFieldQueryParser {

    private Map _configs;
    private List _searchDBKeys;
    private Set _metaKeywordFields;

    /**
     * 
     * @param arg0 - the default field for query terms.
     * @param arg1 - analyzer used to find terms in the query text.
     * @param luceneConfigurations - map with luceneConfigurations for dbs (key=dbKey, value=LuceneConfiguration)
     * @param searchDBKeys - list of dbkeys (Strings) to search
     * @param metaKeywordFields - set of metaFields (String fieldname) indexed as keyword
     */
    public IndexingRuleBasedQueryParser(String[] fields, Analyzer analyzer, Map boosts, Map luceneConfigurations, List searchDBKeys, Set metaKeywordFields) {
        super(Version.LUCENE_35, fields, analyzer, boosts);
        // google like search
        _configs = luceneConfigurations;
        _searchDBKeys = searchDBKeys;
        _metaKeywordFields = metaKeywordFields;
        // highlighting for wildcard queries @see http://www.gossamer-threads.com/lists/lucene/java-user/92528
        setMultiTermRewriteMethod(MultiTermQuery.SCORING_BOOLEAN_QUERY_REWRITE);
    }
    
    @SuppressWarnings("deprecation")
	@Override
    protected Query getFieldQuery(String field, String queryText) throws ParseException {
        if (field!=null && (isMetaKeywordField(field) || isIndexedAsKeyword(field)) ) {
            return new TermQuery(new Term(field, queryText));
        } else {
            return super.getFieldQuery(field, queryText);
        }
    }
    
    @Override
    protected Query getFieldQuery(String field, String queryText, int slop) throws ParseException {
        if (field!=null && (isMetaKeywordField(field) || isIndexedAsKeyword(field)) ) {
            return new TermQuery(new Term(field, queryText));
        } else {
            return super.getFieldQuery(field, queryText, slop);
        }
    }
    

    @Override
    protected Query getFieldQuery(String field, String queryText, boolean quoted) throws ParseException {
        if (field!=null &&  (isMetaKeywordField(field) || isIndexedAsKeyword(field)) ) {
            return new TermQuery(new Term(field, queryText));
        } else {
            return super.getFieldQuery(field, queryText, quoted);
        }
    }
        
    @Override
    protected Query getFuzzyQuery(String field, String termStr, float minSimilarity) throws ParseException {
        if (field!=null && (isMetaKeywordField(field) || isIndexedAsKeyword(field)) ) {
            Term t = new Term(field, termStr);
            return newFuzzyQuery(t, minSimilarity, getFuzzyPrefixLength());
        } else {
            return super.getFuzzyQuery(field, termStr, minSimilarity);
        }        
    }    

    @Override
    protected Query getWildcardQuery(String field, String termStr) throws ParseException {
        if (field!=null && (isMetaKeywordField(field) || isIndexedAsKeyword(field)) ) {
          if (!getAllowLeadingWildcard() && (termStr.startsWith("*") || termStr.startsWith("?"))) {
              throw new ParseException("'*' or '?' not allowed as first character in WildcardQuery");
          }
          Term t = new Term(field, termStr);
          return newWildcardQuery(t);
        } else {
            return super.getWildcardQuery(field, termStr);
        }
    }

    @Override
    protected Query getPrefixQuery(String field, String termStr) throws ParseException {
        if (field!=null && (isMetaKeywordField(field) || isIndexedAsKeyword(field)) ){
            if (!getAllowLeadingWildcard() && termStr.startsWith("*")) {
                throw new ParseException("'*' not allowed as first character in PrefixQuery");
            }
            Term t = new Term(field, termStr);
            return newPrefixQuery(t);
        } else {
            return super.getPrefixQuery(field, termStr);
        }
    }

    /**
     * checks if field is indexed as metafield
     * @param field
     * @return
     */
    private boolean isMetaKeywordField(String field) {
        if (_metaKeywordFields.contains(field)) {
            return true;
        } else {
            return false;
        }
    }
    
    
    /**
     * checks if field is indexed as keyword in one db from searchDBKeys
     * @param field
     * @return
     */
    private boolean isIndexedAsKeyword(String field) {
        // relations are indexed as keyword
        if (field != null && (field.startsWith(LuceneManager.RELATION_PREFIX) || field.startsWith(LuceneManager.RELATIONGROUP_PREFIX))) {
            return true;
        }
        
        if (field != null && field.startsWith("$LUCENE_")) {
            return true;
        }
        
        // retrieve config for each searchDBKey
        // retrieve rules from dbConfig
        // retrieve matching rule for field and check if field was indexed as KEYWORD
        Iterator searchDBKeys = _searchDBKeys.iterator();
        while (searchDBKeys.hasNext()) {
            String dbKey = (String) searchDBKeys.next();
            if (dbKey != null) {
                LuceneConfiguration config = (LuceneConfiguration) _configs.get(dbKey);
                if (config != null) {
                    LuceneIndexItemRule rule = config.getMatchingItemRule(field);
                    if (rule.getIndexType().equals(LuceneIndexItemRule.INDEX_TYPE_KEYWORD)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
