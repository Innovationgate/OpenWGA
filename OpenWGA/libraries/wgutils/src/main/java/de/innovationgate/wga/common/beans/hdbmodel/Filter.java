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

import java.util.List;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.util.Dictionary;

/**
 * Represents a filter definition
 */
@Root(name="filter",strict=false)
public class Filter implements CustomParamsCarrier {

    @Attribute(name="name")
    private String _name;
    
    @ElementList(inline=true,required=false)
    private List<WhereClause> _whereClauses;
    
    @ElementList(inline=true,required=false)
    private List<FilterParam> _filterParams;
    
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


    public List<WhereClause> getWhereClauses() {
        return _whereClauses;
    }


    public void setWhereClauses(List<WhereClause> whereClauses) {
        _whereClauses = whereClauses;
    }


    public List<FilterParam> getFilterParams() {
        return _filterParams;
    }


    public void setFilterParams(List<FilterParam> filterParams) {
        _filterParams = filterParams;
    }


    public OrderClause getOrderClause() {
        return _orderClause;
    }


    public void setOrderClause(OrderClause orderClause) {
        _orderClause = orderClause;
    }


    @Element(required=false,name="order")
    private OrderClause _orderClause;
    
}
