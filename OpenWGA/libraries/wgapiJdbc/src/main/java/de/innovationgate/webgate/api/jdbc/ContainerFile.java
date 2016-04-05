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

import java.io.File;
import java.io.Serializable;
import java.sql.Blob;
import java.util.Date;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.jdbc.filehandling.CS3FileAttachmentEntity;

public class ContainerFile extends CS3FileAttachmentEntity implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private FileContainer parentcontainer;

    /** identifier field */
    private String name;

    /** nullable persistent field */
    private Blob data;



    /** default constructor */
    public ContainerFile() {
    }



    public java.lang.String getName() {
        return this.name;
    }

    public void setName(java.lang.String name) {
        this.name = name;
    }

    public Blob getData() {
        return this.data;
    }

    public void setData(Blob blob) {
        this.data = blob;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("containername", getParentcontainer().getName())
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof ContainerFile) ) return false;
        ContainerFile castOther = (ContainerFile) other;
        return new EqualsBuilder()
            .append(this.getParentcontainer().getName(), castOther.getParentcontainer().getName())
            .append(this.getName(), castOther.getName())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getParentcontainer().getName())
            .append(getName())
            .toHashCode();
    }
    
    public FileContainer getParentcontainer() {
    	return parentcontainer;
    }
    
    @Override
    public MainEntity getParentEntity() {
        return getParentcontainer();
    }
    
    public void setParentcontainer(FileContainer container) {
    	this.parentcontainer = container;
    }
    

}
