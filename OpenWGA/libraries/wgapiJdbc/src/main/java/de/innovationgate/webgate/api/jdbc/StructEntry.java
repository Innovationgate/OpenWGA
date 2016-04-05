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
import org.hibernate.Hibernate;

public class StructEntry extends MainEntity {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String key;

    /** persistent field */
    private java.util.Date created;

    /** persistent field */
    private java.util.Date lastmodified;

    /** persistent field */
    private Integer position;

    /** nullable persistent field */
    private String title;

    /** nullable persistent field */
    private String workflow;

    
    /** nullable persistent field */
    private de.innovationgate.webgate.api.jdbc.StructEntry parententry;

    /** nullable persistent field */
    private de.innovationgate.webgate.api.jdbc.ContentType contenttype;

    /** nullable persistent field */
    private de.innovationgate.webgate.api.jdbc.Area area;

    /** persistent field */
    private List readers;
    
    /** persistent field */
    private List pageeditors;

    /** persistent field */
    private List childeditors;
    
    /** persistent field */
    private Map<String,Date> published;

    /** persistent field */
    private Map childentries;

    /** persistent field */
    private Set content;
    
    /** persistent field */
    private String uniquename;
    
    private Map<Language,Content> releasedcontent;
    
    private Map<Language,Content> draftcontent;
    
    public Map<Language, Content> getDraftcontent() {
        return draftcontent;
    }

    public void setDraftcontent(Map<Language, Content> draftcontent) {
        this.draftcontent = draftcontent;
    }

    public Map<Language, Content> getReleasedcontent() {
        return releasedcontent;
    }

    public void setReleasedcontent(Map<Language, Content> releasedcontent) {
        this.releasedcontent = releasedcontent;
    }

    /** full constructor */
    public StructEntry(java.lang.String key, java.util.Date created, java.util.Date lastmodified, java.lang.Integer position, java.lang.String title, de.innovationgate.webgate.api.jdbc.StructEntry parententry, de.innovationgate.webgate.api.jdbc.ContentType contenttype, de.innovationgate.webgate.api.jdbc.Area area, List contentcreators, List childentrycreators, Map childentries, Set content) {
        this.key = key;
        this.created = created;
        this.lastmodified = lastmodified;
        this.position = position;
        this.title = title;
        this.parententry = parententry;
        this.contenttype = contenttype;
        this.area = area;
        this.pageeditors = contentcreators;
        this.childeditors = childentrycreators;
        this.childentries = childentries;
        this.content = content;
    }

    /** default constructor */
    public StructEntry() {
    }

    /** minimal constructor */
    public StructEntry(java.lang.String key, java.util.Date created, java.util.Date lastmodified, java.lang.Integer position, List contentcreators, List childentrycreators, Map childentries, Set content) {
        this.key = key;
        this.created = created;
        this.lastmodified = lastmodified;
        this.position = position;
        this.pageeditors = contentcreators;
        this.childeditors = childentrycreators;
        this.childentries = childentries;
        this.content = content;
    }

    public java.lang.String getKey() {
        return this.key;
    }

    public void setKey(java.lang.String key) {
        this.key = key;
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

    public java.lang.Integer getPosition() {
        return this.position;
    }

    public void setPosition(java.lang.Integer position) {
        this.position = position;
    }

    public java.lang.String getTitle() {
        return this.title;
    }

    public void setTitle(java.lang.String title) {
        this.title = title;
    }

    public de.innovationgate.webgate.api.jdbc.StructEntry getParententry() {
        return this.parententry;
    }

    public void setParententry(de.innovationgate.webgate.api.jdbc.StructEntry parententry) {
        this.parententry = parententry;
    }

    public de.innovationgate.webgate.api.jdbc.ContentType getContenttype() {
        return this.contenttype;
    }

    public void setContenttype(de.innovationgate.webgate.api.jdbc.ContentType contenttype) {
        this.contenttype = contenttype;
    }

    public de.innovationgate.webgate.api.jdbc.Area getArea() {
        return this.area;
    }

    public void setArea(de.innovationgate.webgate.api.jdbc.Area area) {
        this.area = area;
    }

    public java.util.List getPageeditors() {
        return this.pageeditors;
    }

    public void setPageeditors(java.util.List contentcreators) {
        this.pageeditors = contentcreators;
    }

    public java.util.List getChildeditors() {
        return this.childeditors;
    }

    public void setChildeditors(java.util.List childentrycreators) {
        this.childeditors = childentrycreators;
    }

    public java.util.Map getChildentries() {
        return this.childentries;
    }

    public void setChildentries(java.util.Map childentries) {
        this.childentries = childentries;
    }

    public java.util.Set getContent() {
        return this.content;
    }

    public void setContent(java.util.Set content) {
        this.content = content;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("key", getKey())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof StructEntry) ) return false;
        StructEntry castOther = (StructEntry) other;
        return new EqualsBuilder()
            .append(this.getKey(), castOther.getKey())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getKey())
            .toHashCode();
    }

	public String getWorkflow() {
        return workflow;
    }

    public void setWorkflow(String workflow) {
        this.workflow = workflow;
    }

    public String getUniquename() {
        return uniquename;
    }

    public void setUniquename(String uniquename) {
        this.uniquename = uniquename;
    }

    public Map<String, Date> getPublished() {
        return published;
    }

    public void setPublished(Map<String, Date> published) {
        this.published = published;
    }

    public List getReaders() {
        return readers;
    }

    public void setReaders(List readers) {
        this.readers = readers;
    }
    
    public void loadFully() {
        Hibernate.initialize(this);
        Hibernate.initialize(this.getExtensionData());
        Hibernate.initialize(this.getPageeditors());
        Hibernate.initialize(this.getChildeditors());
        Hibernate.initialize(this.getPublished());
    }


}
