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

public class ContentType extends MainEntity {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String description;

	/** persistent field */
    private String name;

    /** persistent field */
    private java.util.Date created;

    /** persistent field */
    private java.util.Date lastmodified;

    /** nullable persistent field */
    private String workflow;

    /** nullable persistent field */
    private String outerlayout;

    /** nullable persistent field */
    private String innerlayout;

    /** nullable persistent field */
    private String positioning;

    /** persistent field */
    private List editors;

    /** persistent field */
    private List allowedpositions;

    /** persistent field */
    private List namealiases;
	private List descriptionaliases;
    
	/** persistent field */
    private String eventcreatecontent;
    
	/** persistent field */
	private String eventsavecontent;
	
	/** persistent field */
	private String eventworkflowmail;    
	
	private String preferredparent = null;
	
    /** full constructor */
    public ContentType(java.lang.String name, java.util.Date created, java.util.Date lastmodified, java.lang.String workflow, java.lang.String outerlayout, java.lang.String innerlayout, java.lang.String positioning,  List contenteditors, List allowedpositions) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.workflow = workflow;
        this.outerlayout = outerlayout;
        this.innerlayout = innerlayout;
        this.positioning = positioning;
        this.editors = contenteditors;
        this.allowedpositions = allowedpositions;
    }

    /** default constructor */
    public ContentType() {
    }

    /** minimal constructor */
    public ContentType(java.lang.String name, java.util.Date created, java.util.Date lastmodified, List contenteditors, List allowedpositions) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.editors = contenteditors;
        this.allowedpositions = allowedpositions;
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

    public java.lang.String getWorkflow() {
        return this.workflow;
    }

    public void setWorkflow(java.lang.String workflow) {
        this.workflow = workflow;
    }

    public java.lang.String getOuterlayout() {
        return this.outerlayout;
    }

    public void setOuterlayout(java.lang.String outerlayout) {
        this.outerlayout = outerlayout;
    }

    public java.lang.String getInnerlayout() {
        return this.innerlayout;
    }

    public void setInnerlayout(java.lang.String innerlayout) {
        this.innerlayout = innerlayout;
    }

    public java.lang.String getPositioning() {
        return this.positioning;
    }

    public void setPositioning(java.lang.String positioning) {
        this.positioning = positioning;
    }

    public java.util.List getEditors() {
        return this.editors;
    }

    public void setEditors(java.util.List contenteditors) {
        this.editors = contenteditors;
    }

    public java.util.List getAllowedpositions() {
        return this.allowedpositions;
    }

    public void setAllowedpositions(java.util.List allowedpositions) {
        this.allowedpositions = allowedpositions;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof ContentType) ) return false;
        ContentType castOther = (ContentType) other;
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
	/**
	 * @return
	 */
	public List getNamealiases() {
		return namealiases;
	}

	/**
	 * @return
	 */
	public String getEventcreatecontent() {
		return eventcreatecontent;
	}

	/**
	 * @return
	 */
	public String getEventsavecontent() {
		return eventsavecontent;
	}

	/**
	 * @return
	 */
	public String getEventworkflowmail() {
		return eventworkflowmail;
	}

	/**
	 * @param list
	 */
	public void setNamealiases(List list) {
		namealiases = list;
	}

	/**
	 * @param string
	 */
	public void setEventcreatecontent(String string) {
		eventcreatecontent = string;
	}

	/**
	 * @param string
	 */
	public void setEventsavecontent(String string) {
		eventsavecontent = string;
	}

	/**
	 * @param string
	 */
	public void setEventworkflowmail(String string) {
		eventworkflowmail = string;
	}

	/**
	 * @return
	 */
	public List getDescriptionaliases() {
		return descriptionaliases;
	}

	/**
	 * @param list
	 */
	public void setDescriptionaliases(List list) {
		descriptionaliases = list;
	}

	/**
	 * @return
	 */
	public String getPreferredparent() {
		return preferredparent;
	}

	/**
	 * @param string
	 */
	public void setPreferredparent(String string) {
		preferredparent = string;
	}
	
	public void loadFully() {
	    Hibernate.initialize(this);
	    Hibernate.initialize(this.getExtensionData());
	    Hibernate.initialize(this.getEditors());
	    Hibernate.initialize(this.getAllowedpositions());
	}

}
