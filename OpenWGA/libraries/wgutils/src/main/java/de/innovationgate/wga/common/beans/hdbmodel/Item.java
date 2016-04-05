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

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.util.Dictionary;

/**
 * Represents an item definition
 */
@Root(name="item",strict=false)
public class Item implements CustomParamsCarrier {
    
    @Attribute(name="name")
    private String _name;
    
    @Attribute(name="default",required=false)
    private String _defaultValueExpression;
    
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

    public String getDefaultValueExpression() {
        return _defaultValueExpression;
    }

    public void setDefaultValueExpression(String defaultValueExpression) {
        _defaultValueExpression = defaultValueExpression;
    }
    

}
