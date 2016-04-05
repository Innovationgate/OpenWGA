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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

import de.innovationgate.webgate.api.WGPortletDefaultBean;

public class UserProfileItem extends Entity implements Serializable, Item {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** persistent field */
    private String name;

    /** nullable persistent field */
    private int type;

    /** nullable persistent field */
    private de.innovationgate.webgate.api.jdbc.UserProfile parentprofile;

    /** persistent field */
    private String text;

    /** persistent field */
    private Double number;

    /** persistent field */
    private Date date;
    
    private UserProfilePortlet portlet;
    
    private transient DeserialisationCache deserialisationCache;
    
    public DeserialisationCache getDeserialisationCache() {
        return deserialisationCache;
    }
    public void setDeserialisationCache(DeserialisationCache deserialisationCache) {
        this.deserialisationCache = deserialisationCache;
    }
    
    /** default constructor */
    public UserProfileItem() {
    }

    public java.lang.String getName() {
        return this.name;
    }

    public void setName(java.lang.String name) {
        this.name = name;
    }

    public int getType() {
        return this.type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public de.innovationgate.webgate.api.jdbc.UserProfile getParentprofile() {
        return this.parentprofile;
    }

    public void setParentprofile(de.innovationgate.webgate.api.jdbc.UserProfile parentprofile) {
        this.parentprofile = parentprofile;
    }

    public String getText() {
        return this.text;
    }

    public void setText(String string) {
        this.text = string;
    }

    public Double getNumber() {
        return this.number;
    }

    public void setNumber(Double number) {
        this.number = number;
    }

    public Date getDate() {
        return this.date;
    }

    public void setDate(java.util.Date date) {
        this.date = date;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("profilename", getParentprofile().getName())
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof UserProfileItem) ) return false;
        UserProfileItem castOther = (UserProfileItem) other;
        return new EqualsBuilder()
            .append(this.getName(), castOther.getName())
            .append(this.getName(), castOther.getName())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getParentprofile().getName())
            .append(getName())
            .toHashCode();
    }

    public UserProfilePortlet getPortlet() {
        return portlet;
    }

    public void setPortlet(UserProfilePortlet portlet) {
        this.portlet = portlet;
    }

}
