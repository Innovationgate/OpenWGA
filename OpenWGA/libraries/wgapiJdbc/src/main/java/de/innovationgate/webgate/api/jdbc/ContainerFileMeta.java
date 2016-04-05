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
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.jdbc.filehandling.CS41FileAttachmentEntity;
import de.innovationgate.webgate.api.jdbc.filehandling.FileAttachmentEntity;

public class ContainerFileMeta extends CS41FileAttachmentEntity implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private FileContainer parentcontainer;

    /** identifier field v4.1 */
    private String fuid;
    
    /** identifier field */
    private String name;

    /** persistent field */
    private long size;

    /** persistent field */
    private Date created;
    
    /** persistent field */
	private Date lastmodified;
    
    /** persistent field */
    private String checksum;
    
    /** persistent field list of ContentFilePart */
    private List parts;
    
    private transient File sourceFile;
    
    public File getSourceFile() {
		return sourceFile;
	}

	public void setSourceFile(File sourceFile) {
		this.sourceFile = sourceFile;
	}

	/** default constructor */
    public ContainerFileMeta() {
    }

    public java.lang.String getName() {
        return this.name;
    }

    public void setName(java.lang.String name) {
        this.name = name;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("containername", getParentcontainer().getName())
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof ContainerFileMeta) ) return false;
        ContainerFileMeta castOther = (ContainerFileMeta) other;
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

	public void setParentcontainer(FileContainer parentcontainer) {
		this.parentcontainer = parentcontainer;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public List getParts() {
		return parts;
	}

	public void setParts(List parts) {
		this.parts = parts;
	}

	public String getFuid() {
		return fuid;
	}

	public void setFuid(String fuid) {
		this.fuid = fuid;
	}

	public Date getCreated() {
		return created;
	}

	public void setCreated(Date created) {
		this.created = created;
	}
	
	public Date getLastmodified() {
		return lastmodified;
	}

	public void setLastmodified(Date lastmodified) {
		this.lastmodified = lastmodified;
	}	

	public String getChecksum() {
		return checksum;
	}

	public void setChecksum(String checksum) {
		this.checksum = checksum;
	}
	
	@Override
	public MainEntity getParentEntity() {
	    return getParentcontainer();
	}

}
