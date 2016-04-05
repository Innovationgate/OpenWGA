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
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

import de.innovationgate.webgate.api.jdbc.filehandling.CS5P4FileAttachmentEntity;

public class ContentFileMeta extends CS5P4FileAttachmentEntity implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private Content parentcontent;

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
    
    /** persistent field */
    private String checksumSha512;

    /** persistent field list of ContentFilePart */
    @SuppressWarnings("rawtypes")
    private List parts;
    
    private Set<ContentFileDerivate> derivates;
    
    private transient File sourceFile;
    
    private transient boolean dataFlushed = false;
    
    
    /** default constructor */
    public ContentFileMeta() {
    }

    public java.lang.String getName() {
        return this.name;
    }

    public void setName(java.lang.String name) {
        this.name = name;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("cuid", getParentcontent().getCuid())
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof ContentFileMeta) ) return false;
        ContentFileMeta castOther = (ContentFileMeta) other;
        return new EqualsBuilder()
            .append(this.getParentcontent().getCuid(), castOther.getParentcontent().getCuid())
            .append(this.getName(), castOther.getName())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getParentcontent().getCuid())
            .append(getName())
            .toHashCode();
    }
    
    public void setParentcontent(Content content) {
    	this.parentcontent = content;
    }
    
    public Content getParentcontent() {
    	return this.parentcontent;
    }

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	@SuppressWarnings("rawtypes")
    public List getParts() {
		return parts;
	}

	public void setParts(@SuppressWarnings("rawtypes") List parts) {
		this.parts = parts;
	}

	public String getFuid() {
		return fuid;
	}

	public void setFuid(String fuid) {
		this.fuid = fuid;
	}

	public File getSourceFile() {
		return sourceFile;
	}

	public void setSourceFile(File sourceFile) {
		this.sourceFile = sourceFile;
	}

	public boolean isDataFlushed() {
		return dataFlushed;
	}

	public void setDataFlushed(boolean dataFlushed) {
		this.dataFlushed = dataFlushed;
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

    public String getChecksumSha512() {
        return checksumSha512;
    }

    public void setChecksumSha512(String checksumSha256) {
        this.checksumSha512 = checksumSha256;
    }

    public Set<ContentFileDerivate> getDerivates() {
        return derivates;
    }

    public void setDerivates(Set<ContentFileDerivate> derivates) {
        this.derivates = derivates;
    }
    
    @Override
    public MainEntity getParentEntity() {
        return getParentcontent();
    }

}
