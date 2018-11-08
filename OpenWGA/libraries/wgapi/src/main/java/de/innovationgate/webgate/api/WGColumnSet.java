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

package de.innovationgate.webgate.api;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;

/**
 * A definition of field columns of content documents for various purposes.
 */
public class WGColumnSet {
    
    public static final Pattern TERM_PATTERN = Pattern.compile("^([\\w\\-$ÄÖÜäöüß]+)\\s*(?:\\(([\\w\\-=,]*)\\))?+$");    
    
    public static final Pattern FLAG_PATTERN = Pattern.compile("^([\\w]+)(?:=([\\w]+))?+$");
    
    public static final String FLAG_DIVIDER = ",";
    public static final String TERM_DIVIDER = ";";
    
    public static final String FLAG_ASC = "asc";
    public static final String FLAG_DESC = "desc";
    
    /**
     * Definition of a metadata field column, valid for usage in column sets
     */
    public static class ColumnMeta {
        
        private List<Class<? extends WGDocument>> _locations = new ArrayList<Class<? extends WGDocument>>();
        private String _metaName;

        public ColumnMeta(String metaName, Class<?>... locations) {
            _metaName = metaName;
            for (Class<?> loc : locations) {
                @SuppressWarnings("unchecked")
                Class<? extends WGDocument> docClass = (Class<? extends WGDocument>) loc; 
                _locations.add(docClass);
            }
        }

        /**
         * Returns the WGAPI documents that actually feature this metadata field 
         */
        public List<Class<? extends WGDocument>> getLocations() {
            return _locations;
        }

        /**
         * Returns the name of the field
         */
        public String getMetaName() {
            return _metaName;
        }


        
    }
    
    public static final Map<String,ColumnMeta> COLUMN_METAS = new HashMap<String, WGColumnSet.ColumnMeta>();
    static {
        COLUMN_METAS.put("PUBLISHED", new ColumnMeta(WGContent.META_PUBLISHED, WGContent.class, WGStructEntry.class));
        COLUMN_METAS.put("PAGEPUBLISHED", new ColumnMeta(WGStructEntry.META_PUBLISHED, WGStructEntry.class));
        COLUMN_METAS.put("CREATED", new ColumnMeta(WGContent.META_CREATED, WGContent.class, WGStructEntry.class));
        COLUMN_METAS.put("PAGECREATED", new ColumnMeta(WGStructEntry.META_CREATED, WGStructEntry.class));
        COLUMN_METAS.put("LASTMODIFIED", new ColumnMeta(WGContent.META_LASTMODIFIED, WGContent.class, WGStructEntry.class));
        COLUMN_METAS.put("PAGELASTMODIFIED", new ColumnMeta(WGStructEntry.META_LASTMODIFIED, WGStructEntry.class));
        COLUMN_METAS.put("CONTENTCLASS", new ColumnMeta(WGContent.META_CONTENTCLASS, WGContent.class));
        COLUMN_METAS.put("CONTENTTYPE", new ColumnMeta(WGContentType.META_NAME, WGContentType.class));
        COLUMN_METAS.put("POSITION", new ColumnMeta(WGStructEntry.META_POSITION, WGStructEntry.class));
        COLUMN_METAS.put("TITLE", new ColumnMeta(WGContent.META_TITLE, WGContent.class, WGStructEntry.class));
        COLUMN_METAS.put("PAGETITLE", new ColumnMeta(WGStructEntry.META_TITLE, WGStructEntry.class));
        COLUMN_METAS.put("AUTHOR", new ColumnMeta(WGContent.META_AUTHOR, WGContent.class));
        COLUMN_METAS.put("OWNER", new ColumnMeta(WGContent.META_OWNER, WGContent.class));
        COLUMN_METAS.put("STATUS", new ColumnMeta(WGContent.META_STATUS, WGContent.class));
        COLUMN_METAS.put("UNIQUENAME", new ColumnMeta(WGContent.META_UNIQUE_NAME, WGContent.class, WGStructEntry.class));
        COLUMN_METAS.put("NAME", new ColumnMeta(WGContent.META_UNIQUE_NAME, WGContent.class, WGStructEntry.class));
        COLUMN_METAS.put("PAGENAME", new ColumnMeta(WGStructEntry.META_UNIQUENAME, WGStructEntry.class));
        COLUMN_METAS.put("PAGEUNIQUENAME", new ColumnMeta(WGStructEntry.META_UNIQUENAME, WGStructEntry.class));
        COLUMN_METAS.put("VERSION", new ColumnMeta(WGContent.META_VERSION, WGContent.class));
        COLUMN_METAS.put("VISIBLE", new ColumnMeta(WGContent.META_VISIBLE, WGContent.class));
        COLUMN_METAS.put("LANGUAGE", new ColumnMeta(WGLanguage.META_NAME, WGLanguage.class));
    }
    
    
    /**
     * A single term defining a column in the set
     */
    public static class Term {
        
        private String _name;
        private Map<String,String> _flags = Collections.emptyMap();
        
        /**
         * Constructor
         * @param term The term including all flags
         * @throws WGAPIException
         */
        public Term(String term) throws WGAPIException {
            Matcher termMatcher = TERM_PATTERN.matcher(term);
            if (!termMatcher.matches()) {
                throw new WGIllegalArgumentException("Invalid column set term: " + term);
            }
            
            _name = termMatcher.group(1);
            Map<String,String> flags = new HashMap<String, String>();
            if (termMatcher.groupCount() >= 2) {
                String flagsStr = termMatcher.group(2);
                if (flagsStr != null) {
                    for (String flagStr : Collections.unmodifiableList(WGUtils.deserializeCollection(flagsStr, FLAG_DIVIDER, true))) {
                        Matcher flagMatcher = FLAG_PATTERN.matcher(flagStr);
                        if (!flagMatcher.matches()) {
                            throw new WGIllegalArgumentException("Invalid column set flag: " + flagStr);
                        }
                        String flagName = flagMatcher.group(1);
                        String flagValue = flagMatcher.group(2);
                        if (flagValue == null) {
                            flagValue = "true";
                        }
                        
                        flags.put(flagName, flagValue);
                    }
                    _flags = Collections.unmodifiableMap(flags);
                }
            }
        }

        /**
         * Returns the name of the field addressed by the term
         */
        public String getName() {
            return _name;
        }

        /**
         * Returns the flags on this term
         */
        public Map<String,String> getFlags() {
            return _flags;
        }
        
        @Override
        public String toString() {
            StringBuffer out = new StringBuffer();
            out.append(_name);
            if (_flags.size() > 0) {
                out.append("(");
                out.append(WGUtils.serializeCollection(_flags.entrySet(), FLAG_DIVIDER, new ObjectFormatter() {
                    @Override
                    public String format(Object obj) throws FormattingException {
                        @SuppressWarnings("unchecked")
                        Map.Entry<String,String> entry = (Map.Entry<String,String>) obj;
                        return entry.getKey() + "=" + entry.getValue();
                    }
                }));
                out.append(")");
            }
            return out.toString();
        }
        
    }
    
    private List<Term> _terms;
    
    /**
     * Constructor
     * @param columnExpression The complete column set expression
     * @throws WGAPIException
     */
    public WGColumnSet(String columnExpression) throws WGAPIException {
        
        List<Term> terms = new ArrayList<Term>();
        for (String term : WGUtils.deserializeCollection(columnExpression, TERM_DIVIDER, true)) {
            terms.add(new Term(term));
        }
        _terms = Collections.unmodifiableList(terms);
        
        
    }

    /**
     * Returns the terms
     */
    public List<Term> getTerms() {
        return _terms;
    }
    
    @Override
    public String toString() {
        return WGUtils.serializeCollection(_terms, TERM_DIVIDER);
    }

}
