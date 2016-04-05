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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.hibernate.Hibernate;

import de.innovationgate.webgate.api.jdbc.filehandling.FileAttachmentEntity;

public class FileContainer extends MainEntity implements EntityContainingFiles {

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

    /** persistent field */
    private Map files;
    
    private String description;

    /** full constructor */
    public FileContainer(java.lang.String name, java.util.Date created, java.util.Date lastmodified, Map files) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.files = files;
    }

    /** default constructor */
    public FileContainer() {
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

    public java.util.Map getFiles() {
        return this.files;
    }

    public void setFiles(java.util.Map files) {
        this.files = files;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof FileContainer) ) return false;
        FileContainer castOther = (FileContainer) other;
        return new EqualsBuilder()
            .append(this.getName(), castOther.getName())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getName())
            .toHashCode();
    }

    public void setDescription(String desc) {
    	this.description = desc;
    }

    public String getDescription() {
    	return description;
    }
    
    public void loadFully() {
        Hibernate.initialize(this);
        Hibernate.initialize(this.getExtensionData());
        Hibernate.initialize(this.getFiles());
    }
    
    @Override
    public Map<String, FileAttachmentEntity> getFileEntities() {
        return this.files;
    }


}
