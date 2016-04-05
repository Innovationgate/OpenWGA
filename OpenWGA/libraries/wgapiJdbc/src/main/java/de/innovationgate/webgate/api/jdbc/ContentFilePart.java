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
import java.sql.Blob;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

import de.innovationgate.webgate.api.jdbc.filehandling.CS41FileAttachmentEntity;

public class ContentFilePart implements Serializable, AttachmentFilePart {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    private ContentFileMeta meta;	

    /** persistent field for partnr */
    private int partnr;
    
    /** persistent field for data */
    private Blob data;
    
    /** default constructor */
    public ContentFilePart() {
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("fuid", meta.getFuid())
            .append("partnr", partnr)
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof ContentFilePart) ) return false;
        ContentFilePart castOther = (ContentFilePart) other;
        return new EqualsBuilder()
            .append(meta, castOther.getMeta())
            .append(partnr, castOther.getPartnr())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(meta)
            .append(partnr)
            .toHashCode();
    }

	/* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.AttachmentFilePart#getPartnr()
     */
	public int getPartnr() {
		return partnr;
	}

	public void setPartnr(int partnr) {
		this.partnr = partnr;
	}

	/* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.AttachmentFilePart#getData()
     */
	public Blob getData() {
		return data;
	}

	public void setData(Blob data) {
		this.data = data;
	}

	public ContentFileMeta getMeta() {
		return meta;
	}

	public void setParentEntity(Entity meta) {
		setMeta((ContentFileMeta) meta);
	}
	
	public void setMeta(CS41FileAttachmentEntity meta) {
	    this.meta = (ContentFileMeta) meta;
	}

}
