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

import de.innovationgate.utils.WGUtils;

public class LogEntry implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** identifier field v5-n */
    private Long log_id;
    
    /** identifier field v3-4 */
    private String id;

    /** nullable persistent field */
    private java.util.Date logtime;

    /** nullable persistent field */
    private int type;

    /** nullable persistent field - targed determination v3-4 containing documentkey */
    private String target;
    
    /** nullable persistent field - targed determination v5-n containing entity id */
    private String target_id;

    /** nullable persistent field */
    private String loguser;
    
    /** nullable persistent field */
    private String operation;

    /** full constructor */
    public LogEntry(java.util.Date logtime, int type, java.lang.String target, java.lang.String loguser) {
        this.logtime = logtime;
        this.type = type;
        this.target = target;
        this.loguser = loguser;
    }

    /** default constructor */
    public LogEntry() {
    }

    public java.lang.String getId() {
        return this.id;
    }

    public void setId(java.lang.String id) {
        this.id = id;
    }

    public java.util.Date getLogtime() {
        return this.logtime;
    }

    public void setLogtime(java.util.Date logtime) {
        this.logtime = logtime;
    }

    public int getType() {
        return this.type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public java.lang.String getTarget() {
        return this.target;
    }

    public void setTarget(java.lang.String target) {
        this.target = target;
    }

    public java.lang.String getLoguser() {
        return this.loguser;
    }

    public void setLoguser(java.lang.String loguser) {
        this.loguser = loguser;
    }

    public String toString() {
        return new ToStringBuilder(this)
            .append("id", getId())
            .toString();
    }

    public boolean equals(Object other) {
        if ( !(other instanceof LogEntry) ) return false;
        LogEntry castOther = (LogEntry) other;
        return new EqualsBuilder()
            .append(this.getId(), castOther.getId())
            .isEquals();
    }

    public int hashCode() {
        return new HashCodeBuilder()
            .append(getId())
            .toHashCode();
    }

    public Long getLog_id() {
        return log_id;
    }

    public void setLog_id(Long log_id) {
        this.log_id = log_id;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = WGUtils.reduce(operation, 255);
    }

    public String getTarget_id() {
        return target_id;
    }

    public void setTarget_id(String target_id) {
        this.target_id = target_id;
    }

}
