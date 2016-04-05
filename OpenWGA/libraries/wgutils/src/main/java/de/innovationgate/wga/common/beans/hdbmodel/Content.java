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
import org.simpleframework.xml.core.Commit;
import org.simpleframework.xml.util.Dictionary;

/**
 * Represents a content definition
 */
@Root(name="content",strict=false)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Content implements Document, DocumentParent, CustomParamsCarrier {
    
    @Override
    public String toString() {
        return "Content of contentclass " + getContentClass();
    }

    @Attribute(name="contentclass")
    @XmlAttribute(name="contentClass")
    private String _contentClass;
    
    @ElementList(inline=true,required=false)
    @XmlElementWrapper(name="items")
    @XmlElement(name="item")
    private List<Item> _items = new ArrayList<Item>();
    
    @ElementList(inline=true,required=false)
    @XmlElementWrapper(name="relations")
    @XmlElement(name="relation")
    private List<Relation> _relations = new ArrayList<Relation>();
    
    @ElementList(inline=true,required=false)
    @XmlElementWrapper(name="filters")
    @XmlElement(name="filter")
    private List<Filter> _filters = new ArrayList<Filter>();
    
    @ElementList(inline=true,required=false)
    @XmlElementWrapper(name="customParams")
    @XmlElement(name="param")
    private Dictionary<CustomParam> _customParams = new Dictionary<CustomParam>();
    
    public List<Filter> getFilters() {
        return _filters;
    }

    public void setFilters(List<Filter> filters) {
        _filters = filters;
    }

    public List<Item> getItems() {
        return _items;
    }

    public void setItems(List<Item> items) {
        _items = items;
    }

    @ElementList(inline=true,required=false)
    private List<Storage> _childStorages = new ArrayList<Storage>();
    
    @ElementList(inline=true,required=false)
    private List<SingletonContent> _childSingletonContents = new ArrayList<SingletonContent>();

    public List<Storage> getChildStorages() {
        return _childStorages;
    }

    public void setChildStorages(List<Storage> childStorages) {
        _childStorages = childStorages;
    }

    public String getContentClass() {
        return _contentClass;
    }

    public void setContentClass(String contentClass) {
        _contentClass = contentClass;
    }

    public List<Relation> getRelations() {
        return _relations;
    }

    public void setRelations(List<Relation> relations) {
        _relations = relations;
    }
    
    public List<Document> getChildDocuments() {
        List<Document> docs = new ArrayList<Document>();
        docs.addAll(getChildStorages());
        docs.addAll(getChildSingletonContents());
        return docs;
    }
    
    @Commit
    public void commit(Map session) {
        for (Document doc : getChildDocuments()) {
           session.put(doc, this); 
        }
    }

    public Dictionary<CustomParam> getCustomParams() {
        return _customParams;
    }

    public void setCustomParams(Dictionary<CustomParam> customParams) {
        _customParams = customParams;
    }

    public List<SingletonContent> getChildSingletonContents() {
        return _childSingletonContents;
    }

    public void setChildSingletonContents(List<SingletonContent> childSingletonContents) {
        _childSingletonContents = childSingletonContents;
    }
    
    @Override
    public void accept(DocumentVisitor visitor) {
        visitor.visit(this);
        for (Document doc : getChildDocuments()) {
            doc.accept(visitor);
        }
    }
    
    @Override
    public String getId() {
        return getContentClass();
    }


}
