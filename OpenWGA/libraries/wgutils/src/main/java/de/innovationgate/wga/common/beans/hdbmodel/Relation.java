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

package de.innovationgate.wga.common.beans.hdbmodel;

import java.util.ArrayList;
import java.util.List;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.util.Dictionary;

/**
 * Represents a relation definition
 */
@Root(name="relation",strict=false)
public class Relation implements CustomParamsCarrier {

    @Attribute(name="optional",required=false)
    private boolean _optional;
    
    @Attribute(name="name")
    private String _name;
    
    @Attribute(name="baseclass",required=false)
    private String _baseClass;
    
    @Attribute(name="targetclass")
    private String _targetClass;
    
    public boolean isGroup() {
        return _group;
    }

    public void setGroup(boolean group) {
        _group = group;
    }

    @Attribute(name="filter",required=false)
    private String _filter;
    
    @Attribute(name="group",required=false)
    private boolean _group;
    
    @ElementList(inline=true,required=false)
    private List<FilterParam> _extraFilterParams = new ArrayList<FilterParam>();
    
    @ElementList(inline=true,required=false)
    private List<WhereClause> _extraWhereClauses = new ArrayList<WhereClause>();
    
    @Element(required=false,name="order")
    private OrderClause _extraOrderClause;
    
    @ElementList(inline=true,required=false)
    private Dictionary<CustomParam> _customParams = new Dictionary<CustomParam>();


    public Dictionary<CustomParam> getCustomParams() {
        return _customParams;
    }

    public void setCustomParams(Dictionary<CustomParam> customParams) {
        _customParams = customParams;
    }

    public String getName() {
        return _name;
    }

    public void setName(String name) {
        _name = name;
    }

    public String getBaseClass() {
        return _baseClass;
    }

    public void setBaseClass(String baseClass) {
        _baseClass = baseClass;
    }

    public String getTargetClass() {
        return _targetClass;
    }

    public void setTargetClass(String targetClass) {
        _targetClass = targetClass;
    }

    public String getFilter() {
        return _filter;
    }

    public void setFilter(String filter) {
        _filter = filter;
    }

    public List<WhereClause> getExtraWhereClauses() {
        return _extraWhereClauses;
    }

    public void setExtraWhereClauses(List<WhereClause> extraWhereClauses) {
        _extraWhereClauses = extraWhereClauses;
    }

    public List<FilterParam> getExtraFilterParams() {
        return _extraFilterParams;
    }

    public void setExtraFilterParams(List<FilterParam> extraFilterParams) {
        _extraFilterParams = extraFilterParams;
    }

    public OrderClause getExtraOrderClause() {
        return _extraOrderClause;
    }

    public void setExtraOrderClause(OrderClause extraOrderClause) {
        _extraOrderClause = extraOrderClause;
    }

    public boolean isOptional() {
        return _optional;
    }

    public void setOptional(boolean optional) {
        _optional = optional;
    }
    
    
}
