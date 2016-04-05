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

import java.util.Date;

public class ContentFileDerivate extends Entity {
    
    private String _creator;
    
    private String _name;
    
    public String getCreator() {
        return _creator;
    }

    public void setCreator(String creator) {
        _creator = creator;
    }

    public String getName() {
        return _name;
    }

    public void setName(String name) {
        _name = name;
    }
    
    private Date _created;
    
    private Date _lastmodified;
    
    private ContentFileMeta _parentMeta;

    private String _parentSha512;
    
    private String _derivateSha512;
    
    private long _size;

    public long getSize() {
        return _size;
    }

    public void setSize(long size) {
        _size = size;
    }

    public String getParentSha512() {
        return _parentSha512;
    }

    public void setParentSha512(String parentSha512) {
        _parentSha512 = parentSha512;
    }

    public String getDerivateSha512() {
        return _derivateSha512;
    }

    public void setDerivateSha512(String derivateSha512) {
        _derivateSha512 = derivateSha512;
    }

    public Date getCreated() {
        return _created;
    }

    public void setCreated(Date created) {
        _created = created;
    }

    public Date getLastmodified() {
        return _lastmodified;
    }

    public void setLastmodified(Date lastModified) {
        _lastmodified = lastModified;
    }

    public ContentFileMeta getParentMeta() {
        return _parentMeta;
    }

    public void setParentMeta(ContentFileMeta parentMeta) {
        _parentMeta = parentMeta;
    }

}
