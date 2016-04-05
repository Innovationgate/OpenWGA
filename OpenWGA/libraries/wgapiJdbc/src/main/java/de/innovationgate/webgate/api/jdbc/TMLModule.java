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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.hibernate.Hibernate;

public class TMLModule extends MainEntity {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** persistent field */
    private de.innovationgate.webgate.api.jdbc.TMLModuleKey modulekey;

    /** persistent field */
    private java.util.Date created;

    /** persistent field */
    private java.util.Date lastmodified;

    /** nullable persistent field */
    private String code;

    /** persistent field */
    private Boolean directaccess;

    /** persistent field */
    private Boolean cacheable;

    private String description;
    
    /** full constructor */
    public TMLModule(de.innovationgate.webgate.api.jdbc.TMLModuleKey modulekey, java.util.Date created, java.util.Date lastmodified, java.lang.String code, java.lang.Boolean directaccess, java.lang.Boolean cacheable) {
        this.modulekey = modulekey;
        this.created = created;
        this.lastmodified = lastmodified;
        this.code = code;
        this.directaccess = directaccess;
        this.cacheable = cacheable;
    }

    /** default constructor */
    public TMLModule() {
    }

    /** minimal constructor */
    public TMLModule(de.innovationgate.webgate.api.jdbc.TMLModuleKey modulekey, java.util.Date created, java.util.Date lastmodified, java.lang.Boolean directaccess, java.lang.Boolean cacheable) {
        this.modulekey = modulekey;
        this.created = created;
        this.lastmodified = lastmodified;
        this.directaccess = directaccess;
        this.cacheable = cacheable;
    }

    public de.innovationgate.webgate.api.jdbc.TMLModuleKey getModulekey() {
        if (this.modulekey == null) {
            this.modulekey = new TMLModuleKey();
        }
        return this.modulekey;
    }

    public void setModulekey(de.innovationgate.webgate.api.jdbc.TMLModuleKey modulekey) {
        this.modulekey = modulekey;
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

    public java.lang.String getCode() {
        return this.code;
    }

    public void setCode(java.lang.String code) {
        this.code = code;
    }

    public java.lang.Boolean isDirectaccess() {
        return this.directaccess;
    }

    public void setDirectaccess(java.lang.Boolean directaccess) {
        this.directaccess = directaccess;
    }

    public java.lang.Boolean isCacheable() {
        return this.cacheable;
    }
    
    

    public void setCacheable(java.lang.Boolean cacheable) {
        this.cacheable = cacheable;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("modulekey", getModulekey())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof TMLModule) ) return false;
        TMLModule castOther = (TMLModule) other;
        return new EqualsBuilder()
            .append(this.getModulekey(), castOther.getModulekey())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getModulekey())
            .toHashCode();
    }
    
	public String getName() {
		return getModulekey().getName();
	}
    
	public String getMediakey() {
		return getModulekey().getMediakey();
	}
	
	public void setName(String name) {
		getModulekey().setName(name);
	}
	
	
	public void setMediakey(String key) {
		getModulekey().setMediakey(key);
	}
	
	/**
	 * @return
	 */
	public Boolean getCacheable() {
		return cacheable;
	}

	/**
	 * @return
	 */
	public Boolean getDirectaccess() {
		return directaccess;
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
    }


}
