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
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.hibernate.Hibernate;

import de.innovationgate.utils.WGUtils;

public class UserProfile extends MainEntity implements EntityContainingItems {

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

    /** nullable persistent field */
    private Integer type;

    /** nullable persistent field */
    private java.util.Date lastaccess;

    /** nullable persistent field */
    private Integer hits;

    /** nullable persistent field */
    private Integer sessions;

    /** nullable persistent field */
    private String client;

    /** nullable persistent field */
    private String login;

    /** nullable persistent field */
    private String password;

    /** persistent field */
    private List languages;

    /** persistent field */
    private List portletkeys;
    
    /** persistent field */
    private Map<String,UserProfilePortlet> portlets;

    /** persistent field */
    private Map<String,UserProfileItem> items;
    
    /** full constructor */
    public UserProfile(java.lang.String name, java.util.Date created, java.util.Date lastmodified, java.lang.Integer type, java.util.Date lastaccess, java.lang.Integer hits, java.lang.Integer sessions, java.lang.String client, java.lang.String login, java.lang.String password, List languages, List portletkeys, Map items) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.type = type;
        this.lastaccess = lastaccess;
        this.hits = hits;
        this.sessions = sessions;
        this.client = client;
        this.login = login;
        this.password = password;
        this.languages = languages;
        this.portletkeys = portletkeys;
        this.items = items;
    }

    /** default constructor */
    public UserProfile() {
    }

    /** minimal constructor */
    public UserProfile(java.lang.String name, java.util.Date created, java.util.Date lastmodified, List languages, List portletkeys, Map items) {
        this.name = name;
        this.created = created;
        this.lastmodified = lastmodified;
        this.languages = languages;
        this.portletkeys = portletkeys;
        this.items = items;
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

    public java.lang.Integer getType() {
        return this.type;
    }

    public void setType(java.lang.Integer type) {
        this.type = type;
    }

    public java.util.Date getLastaccess() {
        return this.lastaccess;
    }

    public void setLastaccess(java.util.Date lastaccess) {
        this.lastaccess = lastaccess;
    }

    public java.lang.Integer getHits() {
        return this.hits;
    }

    public void setHits(java.lang.Integer hits) {
        this.hits = hits;
    }

    public java.lang.Integer getSessions() {
        return this.sessions;
    }

    public void setSessions(java.lang.Integer sessions) {
        this.sessions = sessions;
    }

    public java.lang.String getClient() {
        return this.client;
    }

    public void setClient(java.lang.String client) {
        this.client = client;
    }

    public java.lang.String getLogin() {
        return this.login;
    }

    public void setLogin(java.lang.String login) {
        this.login = login;
    }

    public java.lang.String getPassword() {
        return this.password;
    }

    public void setPassword(java.lang.String password) {
        this.password = password;
    }

    public java.util.List getLanguages() {
        return this.languages;
    }

    public void setLanguages(java.util.List languages) {
        this.languages = languages;
    }

    public java.util.List getPortletkeys() {
        return this.portletkeys;
    }

    public void setPortletkeys(java.util.List portletkeys) {
        if (this.portletkeys == null) {
            this.portletkeys = portletkeys;
        }
        else {
            WGUtils.updateList(this.portletkeys, portletkeys);
        }
    }

    public java.util.Map<String,UserProfileItem> getItems() {
        return this.items;
    }

    public void setItems(java.util.Map items) {
        this.items = items;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("name", getName())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof UserProfile) ) return false;
        UserProfile castOther = (UserProfile) other;
        return new EqualsBuilder()
            .append(this.getName(), castOther.getName())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getName())
            .toHashCode();
    }

    public Map<String,UserProfilePortlet> getPortlets() {
        return portlets;
    }

    public void setPortlets(Map<String,UserProfilePortlet> portlets) {
        this.portlets = portlets;
    }
    
    public void loadFully() {
        Hibernate.initialize(this);
        Hibernate.initialize(this.getExtensionData());
        Hibernate.initialize(this.getLanguages());
    }
    
    public Item createNewItem(String name) {
        UserProfileItem item = new UserProfileItem();
        item.setParentprofile(this);
        item.setName(name);
        getItems().put(item.getName(), item);
        return item;
    }


}

