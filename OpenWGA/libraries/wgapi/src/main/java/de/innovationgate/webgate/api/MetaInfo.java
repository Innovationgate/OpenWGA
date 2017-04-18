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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Bean containing information about a document metadata field.
 *
 */

public class MetaInfo {
    
    private String _name;
    private Class<?> _dataType;
    private boolean _multiple = false;
    private boolean _lowerCase = false;
    private boolean _cacheable = true;
    private Object _defaultValue;
    private Class<?> _definingClass = null;
    
    // converter used during setMetaData
    private MetaConverter _inputConverter = null;
    
    // converter used during getMetaData
    private MetaConverter _outputConverter = null;
    
    private Set<String> _synonyms = new HashSet<String>();
    
    // if != null - setMetaData() should check if value is allowed
    private Set<Object> _allowedValues = null;
    
    
    /**
     * Simple lucene fulltext indexing
     */
    public static final String LUCENE_INDEXTYPE_FULLTEXT = "fulltext";
    /**
     * Lucene keyword indexing that indexes the exact content
     */
    public static final String LUCENE_INDEXTYPE_KEYWORD = "keyword";
    /**
     * Field should not be added to lucene index
     */
    public static final String LUCENE_INDEXTYPE_NOINDEX = "noindex";
    
    private String _luceneIndexType = LUCENE_INDEXTYPE_KEYWORD;
    private boolean _luceneAddToAllContent = false;
    private float _luceneBoost = (float)1.0;

    // is a special treatment during indexing in lucene neccessary?
    private boolean _luceneSpecialTreatment = false;
    
    /**
     * Defines that this meta is stored as an extension data field since the given CS Version
     */
    private Double _extdataSinceCSVersion = null;
    
    /**
     * Minimal version of content store that features this metadata field
     */
    private double _minCsVersion = 0;
    
    
    /**
     * Constructor
     * @param name
     * @param dataType
     * @param defaultValue
     */
    public MetaInfo(String name, Class<?> dataType, Object defaultValue) {
        _name = name;
        _dataType = dataType;
        _defaultValue = defaultValue;
    }
    
    /**
     * Returns the expected data type
     */
    public Class<?> getDataType() {
        return _dataType;
    }

    /**
     * Returns the default value for that metadata, if defined
     */
    public Object getDefaultValue() {
        return _defaultValue;
    }

    /**
     * @deprecated
     * Returns if the contents of this metadata field is added to the global lucene fulltext index field for field-unspecific searches
     */
    public boolean getLuceneAddToAllContent() {
        return _luceneAddToAllContent;
    }

    /**
     * @deprecated
     */
    void setLuceneAddToAllContent(boolean luceneAddToAllContent) {
        _luceneAddToAllContent = luceneAddToAllContent;
    }

    /**
     * Returns the boost value that this metadata should have in the lucene index
     */
    public float getLuceneBoost() {
        return _luceneBoost;
    }

    void setLuceneBoost(float luceneBoost) {
        _luceneBoost = luceneBoost;
    }

    /**
     * Returns the type of lucene indexing that should be used for this metadata field.
     * Constants LUCENE_INDEXTYPE_... are returned.
     */
    public String getLuceneIndexType() {
        return _luceneIndexType;
    }

    void setLuceneIndexType(String luceneIndexType) {
        _luceneIndexType = luceneIndexType;
    }

    /**
     * Returns if the metadata field hosts a list of values
     */
    public boolean isMultiple() {
        return _multiple;
    }

    /**
     * Returns the metadata name
     */
    public String getName() {
        return _name;
    }

    /**
     * Returns if the contents of this metadata field should always be stored lowercase
     */
    public boolean isLowerCase() {
        return _lowerCase;
    }

    void setInputConverter(MetaConverter converter) {
        _inputConverter = converter;
    }

    void setOutputConverter(MetaConverter converter) {
        _outputConverter = converter;
    }

    void setLowerCase(boolean lowerCase) {
        _lowerCase = lowerCase;
    }

    void setMultiple(boolean multiple) {
        _multiple = multiple;
    }
    
    void addSynonym(String synonym) {
        _synonyms.add(synonym);
    }

    /**
     * Returns metadata synonyms usable in WGAPI or WebTML
     */
    public Set<String> getSynonyms() {
        return _synonyms;
    }

    /**
     * Returns if te lucene indexer needs some special treatment when indexing this metadata field
     */
    public boolean isLuceneSpecialTreatment() {
        return _luceneSpecialTreatment;
    }

    void setLuceneSpecialTreatment(boolean luceneSpecialTreatment) {
        _luceneSpecialTreatment = luceneSpecialTreatment;
    }

    /**
     * Returns the allowed values for this metadata if they are restricted
     */
    public Set<Object> getAllowedValues() {
        return _allowedValues;
    }

    void addAllowedValue(Object value) {
        if (_allowedValues == null) {
            _allowedValues = new HashSet<Object>();
        }
        _allowedValues.add(value);
    }
    
    /**
     * reads of backend values are converted through this method
     * @param doc
     * @param value
     * @return Converted values
     * @throws WGAPIException
     */
    Object convertOutput(WGDocument doc, Object value) throws WGAPIException {
        // if null value given directly return default
        if (value == null) {                
           return getDefaultValue();
        } 
        
        // validate value
        // check multiple
        if (isMultiple()) {
            if (!(value instanceof List)) {
                throw new WGIllegalArgumentException("Error validating meta '" + getName() + "' - none list value found where list was expected.");
            }
        } else {
            if (value instanceof List) {
                throw new WGIllegalArgumentException("Error validating meta '" + getName() + "' - list value found where none list value was expected.");
            }
        }
        
        //liberal number convertion
        if (Number.class.isAssignableFrom(getDataType())) {
            
            if (!(value instanceof Number)) {
                throw new WGIllegalArgumentException("Error validating meta '" + getName() + "' - Should be number but is of type " + value.getClass().getName());
            }
            
            if (!isMultiple()) {                
                value = numberToConcreteType(value);
            }
            else {
                @SuppressWarnings("unchecked")
                List<Object> values = ((List<Object>)value);
                for (int i=0; i<values.size(); i++) {
                    values.set(i, numberToConcreteType(values.get(i)));
                }
            }
        }
        
        // we may have to lowercase
        value = assureProperCase(doc, value);
        
        // execute custom converter if present
        if (_outputConverter != null) {
            return _outputConverter.convert(doc, this, value);
        }
        return value;
    }

    private Object numberToConcreteType(Object value) {
        if (value == null) {
            return null;
        }
        Number numValue = (Number)value;
        if (getDataType().equals(Integer.class)) {                
            value = new Integer(numValue.intValue());
        } else if (getDataType().equals(Float.class)) {
            value = new Float(numValue.floatValue());
        } else if (getDataType().equals(Long.class)) {
            value = new Long(numValue.longValue());
        } else if (getDataType().equals(Double.class)) {
            value = new Double(numValue.doubleValue());
        } else if (getDataType().equals(Byte.class)) {
            value = new Byte(numValue.byteValue());
        } else if (getDataType().equals(Short.class)) {
            value = new Short(numValue.shortValue());
        }
        return value;
    }
    
    /**
     * writes to backend are converted through this method
     * @param doc
     * @param value
     * @return Converted values
     * @throws WGAPIException
     */
    Object convertInput(WGDocument doc, Object value) throws WGAPIException {
        // no conversion for null values
        if (value == null) {
            return value;
        }
        
        // Be sure that non-list metas are not put through as lists or vice versa
        if (value instanceof List && !isMultiple()) {
            List<?> valueList = (List<?>) value;
            if (valueList.size() >= 1) {
                value = valueList.get(0);
            }
            else {
                return null;
            }
        }
        else if (!(value instanceof List) && isMultiple()) {
            List<Object> valueList = new ArrayList<Object>();
            valueList.add(value);
            value = valueList;
        }
        
        // validate value        
        // check dataType            
        if (!isMultiple()) {
            if (!getDataType().isAssignableFrom(value.getClass())) {
                throw new WGIllegalArgumentException("Value of type '" + value.getClass().getName() + "' is unsupported for meta '" + getName() + "'. Expected: '" + getDataType().getName() + "'.");
            }
        } else {
            Iterator<?> listValues = ((List<?>)value).iterator();
            while (listValues.hasNext()) {
                Object singleValue = listValues.next();
                if (singleValue != null) {
                    if (!getDataType().isAssignableFrom(singleValue.getClass())) {
                        throw new WGIllegalArgumentException("Value of type '" + singleValue.getClass().getName() + "' is unsupported for listmeta '" + getName() + "'. Expected: '" + getDataType().getName() + "'.");                            
                    }
                } else {
                    throw new WGIllegalArgumentException("Nullvalue in list is unsupported for listmeta '" + getName() + "'.");
                }
            }
        }
        
        // check allowed values            
        if (getAllowedValues() != null) {
            if (!isMultiple()) {
                if (!getAllowedValues().contains(value)) {
                    throw new WGIllegalArgumentException("Value '" + value + "' is not allowed for meta '" + getName() + "'.");
                }
            } else {
                Iterator<?> listValues = ((List<?>)value).iterator();
                while (listValues.hasNext()) {
                    Object singleValue = listValues.next();
                    if (!getAllowedValues().contains(singleValue)) {
                        throw new WGIllegalArgumentException("Value '" + value + "' is not allowed for meta '" + getName() + "'.");                            
                    }
                }
            }
            
        }   
        
        // we may have to lowercase
        value = assureProperCase(doc, value);
        
        // execute custom converter if present
        if (_inputConverter != null) {
            return _inputConverter.convert(doc, this, value);
        }
        return value;
    }
    
    private Object assureProperCase(WGDocument doc, Object value) throws WGIllegalStateException, WGSystemException, WGIllegalArgumentException {
        if (value != null) {      
            if (isLowerCase() && !doc.getDatabase().hasFeature(WGDatabase.FEATURE_DISABLE_META_LOWERCASING)) {
                if (!isMultiple()) {
                    if (value instanceof String) {
                        value = ((String) value).toLowerCase();
                    } else {
                        throw new WGIllegalArgumentException("Cannot lowercase meta '" + getName() + "'. Value of type '" + value.getClass().getName() + "' found - String expected."); 
                    }
                } else {
                    @SuppressWarnings("unchecked")
                    List<String> valueList = (List<String>) value;
                    for (int idx = 0; idx < valueList.size(); idx++) {
                        
                        Object elementObj = valueList.get(idx);
                        if (elementObj instanceof String) {
                            String element = (String) elementObj;
                            valueList.set(idx, element.toLowerCase());
                        } else {
                            String message = "";
                            if (elementObj != null) {
                                message = "Cannot lowercase list meta '" + getName() + "'. Value of type '" + elementObj.getClass().getName() + "' found - String expected.";
                            } else {
                                message = "Cannot lowercase list meta '" + getName() + "'. Value 'null' found - String expected.";
                            }                        
                            throw new WGIllegalArgumentException(message);
                        }
                    }                
                }
            }
        }
        return value;
    }

    /**
     * Returns the minimum version of OpenWGA content stores that features this metadata field
     */
    public double getMinCsVersion() {
        return _minCsVersion;
    }

    void setMinCsVersion(double minCsVersion) {
        _minCsVersion = minCsVersion;
    }
    
    /**
     * Returns if the metadata field is moved to an extension data field since a given content store version
     * @return The content store version to test
     */
    public boolean isExtdataInCsVersion(double csVersion) {
        return (_extdataSinceCSVersion != null && _extdataSinceCSVersion.doubleValue() <= csVersion);
    }
    
    void setExtdata(boolean attribute) {
        if (attribute) {
            _extdataSinceCSVersion = new Double(0);
        }
        else {
            _extdataSinceCSVersion = null;
        }
    }
    
    void setExtdataSinceCsVersion(double version) {
        _extdataSinceCSVersion = new Double(version);
    }

    /**
     * Returns if the data if this metadata field may be cached by WGAPI cache
     */
    public boolean isCacheable() {
        return _cacheable;
    }

    void setCacheable(boolean cacheable) {
        _cacheable = cacheable;
    }

    /**
     * Returns the class of the document defining this metadata field
     */
    public Class<?> getDefiningClass() {
        return _definingClass;
    }

    void setDefiningClass(Class<?> definingClass) {
        _definingClass = definingClass;
    }
    
}
