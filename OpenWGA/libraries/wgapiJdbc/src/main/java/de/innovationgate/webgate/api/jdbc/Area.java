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

public class Area extends MainEntity {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String description;

	private String name;

    /** persistent field */
    private java.util.Date created;

    /** persistent field */
    private java.util.Date lastmodified;

    /** persistent field */
    private List readers;
    
    /** persistent field */
    private List editors;

    /** persistent field */
    private Map rootentries;
    
    /** full constructor */
    public Area(java.lang.String name, java.util.Date created, java.util.Date lastmodified, List editors, Map rootentries) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.editors = editors;
        this.rootentries = rootentries;
    }

    /** default constructor */
    public Area() {
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

    public java.util.List getEditors() {
        return this.editors;
    }

    public void setEditors(java.util.List editors) {
        this.editors = editors;
    }

    public java.util.Map getRootentries() {
        return this.rootentries;
    }

    public void setRootentries(java.util.Map rootentries) {
        this.rootentries = rootentries;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof Area) ) return false;
        Area castOther = (Area) other;
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

    public List getReaders() {
        return readers;
    }

    public void setReaders(List readers) {
        this.readers = readers;
    }

    public void loadFully() {
        Hibernate.initialize(this);
        Hibernate.initialize(this.getExtensionData());
        Hibernate.initialize(this.getEditors());
    }

}
