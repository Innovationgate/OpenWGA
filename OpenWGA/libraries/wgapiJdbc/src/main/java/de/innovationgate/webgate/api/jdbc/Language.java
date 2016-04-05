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
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.hibernate.Hibernate;

public class Language extends MainEntity {
    
    

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** persistent field */
    private String name;

    /** persistent field */
    private java.util.Date created;

    /** persistent field */
    private java.util.Date lastmodified;

    /** nullable persistent field */
    private String title;

    /** persistent field */
    private List editors;

    /** persistent field */
    private Map contents;
    
    private String description;

    /** full constructor */
    public Language(java.lang.String name, java.util.Date created, java.util.Date lastmodified, java.lang.String title, List contentcreators, Map contents) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.title = title;
        this.editors = contentcreators;
        this.contents = contents;
    }

    /** default constructor */
    public Language() {
    }

    /** minimal constructor */
    public Language(java.lang.String name, java.util.Date created, java.util.Date lastmodified, List contentcreators, Map contents) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.editors = contentcreators;
        this.contents = contents;
    }

    public java.lang.String getName() {
        return this.name;
    }

    public void setName(java.lang.String name) {
        this.name = name;
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

    public java.util.List getEditors() {
        return this.editors;
    }

    public void setEditors(java.util.List contentcreators) {
        this.editors = contentcreators;
    }

    public java.util.Map getContents() {
        return this.contents;
    }

    public void setContents(java.util.Map contents) {
        this.contents = contents;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof Language) ) return false;
        Language castOther = (Language) other;
        return new EqualsBuilder()
            .append(this.getName(), castOther.getName())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getName())
            .toHashCode();
    }

    public String getDescription() {
    	return description;
    }

    public void setDescription(String desc) {
    	this.description = desc;
    }
    
    public void loadFully() {
        Hibernate.initialize(this);
        Hibernate.initialize(this.getExtensionData());
        Hibernate.initialize(this.getEditors());
    }


}
