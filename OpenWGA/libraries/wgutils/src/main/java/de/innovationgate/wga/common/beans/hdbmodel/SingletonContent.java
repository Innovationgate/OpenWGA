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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.util.Dictionary;

/**
 * Represents a singleton content definition
 */
@Root(name="singleton-content",strict=false)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SingletonContent implements Document, CustomParamsCarrier {

    @Attribute(name="cid")
    @XmlAttribute(name="contentId")
    private String _contentId;
    
    @Override
    public String toString() {
        return "Singleton content of content id " + getContentId();
    }

    @ElementList(inline=true,required=false)
    @XmlElementWrapper(name="items")
    @XmlElement(name="item")
    private List<Item> _items = new ArrayList<Item>();
    
    @ElementList(inline=true,required=false)
    private Dictionary<CustomParam> _customParams = new Dictionary<CustomParam>();


    public Dictionary<CustomParam> getCustomParams() {
        return _customParams;
    }

    public void setCustomParams(Dictionary<CustomParam> customParams) {
        _customParams = customParams;
    }

    public String getContentId() {
        return _contentId;
    }

    public void setContentId(String uid) {
        _contentId = uid;
    }

    public List<Item> getItems() {
        return _items;
    }

    public void setItems(List<Item> items) {
        _items = items;
    }
    
    @Override
    public void accept(DocumentVisitor visitor) {
        visitor.visit(this);
    }
    
    @Override
    public String getId() {
        return getContentId();
    }

    
}
