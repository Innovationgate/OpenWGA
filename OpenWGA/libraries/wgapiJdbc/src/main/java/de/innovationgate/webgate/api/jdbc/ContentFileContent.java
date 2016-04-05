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

import java.util.List;

import de.innovationgate.webgate.api.WGFileDerivateMetaData;

public class ContentFileContent extends Entity {
    
    private String _id;
    private String _checksumSha512;
    private long _size;
    private long _ordinalnr;
    public long getOrdinalnr() {
        return _ordinalnr;
    }
    public void setOrdinalnr(long ordinalNr) {
        _ordinalnr = ordinalNr;
    }
    private List<ContentFilePart> _parts;
    
    public String getId() {
        return _id;
    }
    public void setId(String id) {
        _id = id;
    }
    public String getChecksumSha512() {
        return _checksumSha512;
    }
    public void setChecksumSha512(String checksumSha512) {
        _checksumSha512 = checksumSha512;
    }
    public long getSize() {
        return _size;
    }
    public void setSize(long size) {
        _size = size;
    }
    public List<ContentFilePart> getParts() {
        return _parts;
    }
    public void setParts(List<ContentFilePart> parts) {
        _parts = parts;
    }


}
