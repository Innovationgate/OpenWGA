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
package de.innovationgate.webgate.api.jdbc;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.vfs2.FileMonitor;
import org.hibernate.Hibernate;

import de.innovationgate.webgate.api.jdbc.filehandling.FileAttachmentEntity;

public class Content extends MainEntity implements EntityContainingFiles,EntityContainingItems {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** identifier field v3,4 */
    private String cuid;

    /** persistent field */
    private java.util.Date created;

    /** persistent field */
    private java.util.Date lastmodified;

    /** persistent field */
    private String title;

    /** persistent field */
    private String status;

    /** nullable persistent field */
    private String uniquename;

    /** persistent field */
    private Integer version;

    /** persistent field */
    private Boolean visible;

    /** nullable persistent field */
    private java.util.Date validfrom;

    /** nullable persistent field */
    private java.util.Date validto;

    /** nullable persistent field */
    private String contentclass;

    /** nullable persistent field */
    private String virtuallink;

    /** nullable persistent field */
    private String virtuallinktype;

    /** nullable persistent field */
    private String linktarget;

    /** nullable persistent field */
    private String author;
    
    /**  persistent field */
    private String owner;
    
    /** nullable persistent field */
    private String description;
    
    /** nullable persistent field */
    private java.util.Date published;
    
    /** Obsolete field remaining bc it is declared "not null" */
    private boolean usenavimages = false;

    /** nullable persistent field */
    private de.innovationgate.webgate.api.jdbc.StructEntry structentry;

    /** nullable persistent field */
    private de.innovationgate.webgate.api.jdbc.Language language;

    /** persistent field */
    private List ishiddenfrom;

    /** persistent field */
    private List readers;

    /** persistent field */
    private List keywords;
    
    /** persistent field */
    private List coauthors;

    /** persistent field */
    private List wfhistory;

    /** persistent field */
    private Map items;

    /** persistent field */
    private Map relations;
    
    /** persistent field */
    private Map relationgroups;
    
    /** persistent field */
    private Set incomingRelations;
    
    /** persistent field */
    private Map<String,FileAttachmentEntity> files;
    
    private String lastclient;
    
    /** full constructor */
    public Content(java.util.Date created, java.util.Date lastmodified, java.lang.String title, java.lang.String status, java.lang.String uniquename, java.lang.Integer version, java.lang.Boolean visible, java.util.Date validfrom, java.util.Date validto, java.lang.String virtuallink, java.lang.String virtuallinktype, java.lang.String linktarget, java.lang.String author, java.lang.String description, de.innovationgate.webgate.api.jdbc.StructEntry structentry, de.innovationgate.webgate.api.jdbc.Language language, List ishiddenfrom, List readers, List keywords, List wfhistory, Map items, Map relations, Map files) {
        this.created = created;
        this.lastmodified = lastmodified;
        this.title = title;
        this.status = status;
        this.uniquename = uniquename;
        this.version = version;
        this.visible = visible;
        this.validfrom = validfrom;
        this.validto = validto;
        this.virtuallink = virtuallink;
        this.virtuallinktype = virtuallinktype;
        this.linktarget = linktarget;
        this.author = author;
        this.description = description;
        this.structentry = structentry;
        this.language = language;
        this.ishiddenfrom = ishiddenfrom;
        this.readers = readers;
        this.keywords = keywords;
        this.wfhistory = wfhistory;
        this.items = items;
        this.relations = relations;
        this.files = files;
    }

    /** default constructor */
    public Content() {
    }

    /** minimal constructor */
    public Content(java.util.Date created, java.util.Date lastmodified, java.lang.String title, java.lang.String status, java.lang.Integer version, java.lang.Boolean visible, List ishiddenfrom, List readers, List keywords, List wfhistory, Map items, Map relations, Map files) {
        this.created = created;
        this.lastmodified = lastmodified;
        this.title = title;
        this.status = status;
        this.version = version;
        this.visible = visible;
        this.ishiddenfrom = ishiddenfrom;
        this.readers = readers;
        this.keywords = keywords;
        this.wfhistory = wfhistory;
        this.items = items;
        this.relations = relations;
        this.files = files;
    }

    public java.lang.String getCuid() {
        return this.cuid;
    }

    public void setCuid(java.lang.String cuid) {
        this.cuid = cuid;
    }

    public java.util.Date getCreated() {
        return this.created;
    }

    public void setCreated(java.util.Date created) {
        this.created = created;
    }

    public java.util.Date getLastmodified() {
        return this.lastmodified;
    }

    public void setLastmodified(java.util.Date lastmodified) {
        this.lastmodified = lastmodified;
    }

    public java.lang.String getTitle() {
        return this.title;
    }

    public void setTitle(java.lang.String title) {
        this.title = title;
    }

    public java.lang.String getStatus() {
        return this.status;
    }

    public void setStatus(java.lang.String status) {
        this.status = status;
    }

    public java.lang.String getUniquename() {
        return this.uniquename;
    }

    public void setUniquename(java.lang.String uniquename) {
        this.uniquename = uniquename;
    }

    public java.lang.Integer getVersion() {
        return this.version;
    }

    public void setVersion(java.lang.Integer version) {
        this.version = version;
    }

    public java.lang.Boolean isVisible() {
        return this.visible;
    }
    
    public Boolean getVisible() {
        return isVisible();
    }

    public void setVisible(java.lang.Boolean visible) {
        this.visible = visible;
    }

    public java.util.Date getValidfrom() {
        return this.validfrom;
    }

    public void setValidfrom(java.util.Date validfrom) {
        this.validfrom = validfrom;
    }

    public java.util.Date getValidto() {
        return this.validto;
    }

    public void setValidto(java.util.Date validto) {
        this.validto = validto;
    }

    public java.lang.String getContentclass() {
        return this.contentclass;
    }

    public void setContentclass(java.lang.String baseimage) {
        this.contentclass = baseimage;
    }

    public java.lang.String getVirtuallink() {
        return this.virtuallink;
    }

    public void setVirtuallink(java.lang.String virtuallink) {
        this.virtuallink = virtuallink;
    }

    public java.lang.String getVirtuallinktype() {
        return this.virtuallinktype;
    }

    public void setVirtuallinktype(java.lang.String virtuallinktype) {
        this.virtuallinktype = virtuallinktype;
    }

    public java.lang.String getLinktarget() {
        return this.linktarget;
    }

    public void setLinktarget(java.lang.String linktarget) {
        this.linktarget = linktarget;
    }

    public java.lang.String getAuthor() {
        return this.author;
    }

    public void setAuthor(java.lang.String author) {
        this.author = author;
    }

    public java.lang.String getDescription() {
        return this.description;
    }

    public void setDescription(java.lang.String description) {
        this.description = description;
    }

    public de.innovationgate.webgate.api.jdbc.StructEntry getStructentry() {
        return this.structentry;
    }

    public void setStructentry(de.innovationgate.webgate.api.jdbc.StructEntry structentry) {
        this.structentry = structentry;
    }

    public de.innovationgate.webgate.api.jdbc.Language getLanguage() {
        return this.language;
    }

    public void setLanguage(de.innovationgate.webgate.api.jdbc.Language language) {
        this.language = language;
    }

    public java.util.List getIshiddenfrom() {
        return this.ishiddenfrom;
    }

    public void setIshiddenfrom(java.util.List ishiddenfrom) {
        this.ishiddenfrom = ishiddenfrom;
    }

    public java.util.List getReaders() {
        return this.readers;
    }

    public void setReaders(java.util.List readers) {
        this.readers = readers;
    }

    public java.util.List getKeywords() {
        return this.keywords;
    }

    public void setKeywords(java.util.List keywords) {
        this.keywords = keywords;
    }

    public java.util.List getWfhistory() {
        return this.wfhistory;
    }

    public void setWfhistory(java.util.List wfhistory) {
        this.wfhistory = wfhistory;
    }

    public java.util.Map getItems() {
        return this.items;
    }

    public void setItems(java.util.Map items) {
        this.items = items;
    }
    
    public java.util.Map getRelations() {
        return this.relations;
    }

    public void setRelations(java.util.Map relations) {
        this.relations = relations;
    }

    public java.util.Map<String,FileAttachmentEntity> getFiles() {
        return this.files;
    }

    public void setFiles(java.util.Map<String,FileAttachmentEntity> files) {
        this.files = files;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("cuid", getCuid())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof Content) ) return false;
        Content castOther = (Content) other;
        return new EqualsBuilder()
            .append(this.getCuid(), castOther.getCuid())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getCuid())
            .toHashCode();
    }

	/**
	 * @return
	 */
	public String getLastclient() {
		return lastclient;
	}

	/**
	 * @param string
	 */
	public void setLastclient(String string) {
		lastclient = string;
	}
	
	public Set getIncomingRelations() {
        return incomingRelations;
    }

    public void setIncomingRelations(Set incomingRelations) {
        this.incomingRelations = incomingRelations;
    }

    public List getCoauthors() {
        return coauthors;
    }

    public void setCoauthors(List coauthors) {
        this.coauthors = coauthors;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public boolean isUsenavimages() {
        return usenavimages;
    }

    public void setUsenavimages(boolean usenavimages) {
        this.usenavimages = usenavimages;
    }

    public java.util.Date getPublished() {
        return published;
    }

    public void setPublished(java.util.Date published) {
        this.published = published;
    }

    public Map getRelationgroups() {
        return relationgroups;
    }

    public void setRelationgroups(Map relationgroups) {
        this.relationgroups = relationgroups;
    }
    
    public void loadFully() {
        Hibernate.initialize(this);
        
        for (FileAttachmentEntity fileMeta : getFileEntities().values()) {
            Hibernate.initialize(fileMeta);
            Hibernate.initialize(fileMeta.getExtensionData());
        }
        
        Hibernate.initialize(this.getFileEntities());
        Hibernate.initialize(this.getExtensionData());
        Hibernate.initialize(this.getWfhistory());
        Hibernate.initialize(this.getFiles());
        Hibernate.initialize(this.getKeywords());
    }

    public Item createNewItem(String name) {
        ContentItem item  = new ContentItem();
        item.setParentcontent(this);
        item.setName(name);
        getItems().put(item.getName(), item);
        return item;
        
    }
    
    @Override
    public Map<String, FileAttachmentEntity> getFileEntities() {
        return this.files;
    }
    

}
