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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

public class TMLModuleKey implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** identifier field */
    private String name;

    /** identifier field */
    private String mediakey;

    /** full constructor */
    public TMLModuleKey(java.lang.String name, java.lang.String mediakey) {
        this.name = name;
        this.mediakey = mediakey;
    }

    /** default constructor */
    public TMLModuleKey() {
    }

    public java.lang.String getName() {
        return this.name;
    }

    public void setName(java.lang.String name) {
        this.name = name;
    }

    public java.lang.String getMediakey() {
        return this.mediakey;
    }

    public void setMediakey(java.lang.String mediakey) {
        this.mediakey = mediakey;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("name", getName())
            .append("mediakey", getMediakey())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof TMLModuleKey) ) return false;
        TMLModuleKey castOther = (TMLModuleKey) other;
        return new EqualsBuilder()
            .append(this.getName(), castOther.getName())
            .append(this.getMediakey(), castOther.getMediakey())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getName())
            .append(getMediakey())
            .toHashCode();
    }

}
