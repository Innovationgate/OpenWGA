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
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.core.Commit;
import org.simpleframework.xml.util.Dictionary;

/**
 * Represents a storage definition
 */
@Root(name="storage",strict=false)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Storage implements Document, DocumentParent, CustomParamsCarrier {
    
    @Attribute(name="sid")
    @XmlAttribute(name="storageId")
    private String _storageId;
    
    @Attribute(name="readers",required=false)
    @XmlAttribute(name="readers")
    private String _readers;
    
    @Attribute(name="editors",required=false)
    @XmlAttribute(name="editors")
    private String _editors;
    
    public String getEditors() {
        return _editors;
    }

    public void setEditors(String childEditors) {
        _editors = childEditors;
    }

    public String getReaders() {
        return _readers;
    }

    public void setReaders(String readers) {
        _readers = readers;
    }

    @ElementList(inline=true,required=false)
    private List<Content> _childContents = new ArrayList<Content>();

    @Override
    public String toString() {
        return "Storage of storage id " + getStorageId();
    }

    @ElementList(inline=true,required=false)
    private List<Storage> _childStorages = new ArrayList<Storage>();

    @ElementList(inline=true,required=false)
    private List<SingletonContent> _childSingletonContents = new ArrayList<SingletonContent>();
    
    @ElementList(inline=true,required=false)
    @XmlElementWrapper(name="customParams")
    @XmlElement(name="param")
    private Dictionary<CustomParam> _customParams = new Dictionary<CustomParam>();

    /* (non-Javadoc)
     * @see de.innovationgate.wga.common.beans.hdbmodel.CustomParamsCarrier#getCustomParams()
     */
    public Dictionary<CustomParam> getCustomParams() {
        return _customParams;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wga.common.beans.hdbmodel.CustomParamsCarrier#setCustomParams(org.simpleframework.xml.util.Dictionary)
     */
    public void setCustomParams(Dictionary<CustomParam> customParams) {
        _customParams = customParams;
    }

    public String getStorageId() {
        return _storageId;
    }

    public void setStorageId(String uid) {
        _storageId = uid;
    }

    public List<Content> getChildContents() {
        return _childContents;
    }

    public void setChildContents(List<Content> childContents) {
        _childContents = childContents;
    }

    public List<Storage> getChildStorages() {
        return _childStorages;
    }

    public void setChildStorages(List<Storage> childStorages) {
        _childStorages = childStorages;
    }

    public List<SingletonContent> getChildSingletonContents() {
        return _childSingletonContents;
    }

    public void setChildSingletonContents(List<SingletonContent> childSingletonContents) {
        _childSingletonContents = childSingletonContents;
    }
    
    public List<Document> getChildDocuments() {
        List<Document> docs = new ArrayList<Document>();
        docs.addAll(getChildSingletonContents());
        docs.addAll(getChildStorages());
        docs.addAll(getChildContents());
        return docs;
    }
    
    @Commit
    public void commit(Map session) {
        for (Document doc : getChildDocuments()) {
           session.put(doc, this); 
        }
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
        return getStorageId();
    }

}
