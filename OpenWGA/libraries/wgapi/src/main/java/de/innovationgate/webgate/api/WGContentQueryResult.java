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

package de.innovationgate.webgate.api;

/**
 * An internal representation of query results on content queries
 */
public class WGContentQueryResult {
    
    private WGContentKey _contentKey;
    private Object _parentStructKey = null;
    private String _areaName = null;

    public WGContentQueryResult(Object structKey, String language, int version, Object parentStructKey, String areaName) {
        _contentKey = new WGContentKey(structKey, language, version);
        _parentStructKey = parentStructKey;
        _areaName = areaName;
    }
    
    public WGContentQueryResult(WGContentKey contentKey) {
        _contentKey = contentKey;
    }

    public WGContentQueryResult(WGContentKey contentKey, Object parentStructKey) {
        _contentKey = contentKey;
        _parentStructKey = parentStructKey;
    }
    
    public WGContentQueryResult(WGContentKey contentKey, Object parentStructKey, String areaName) {
        _contentKey = contentKey;
        _parentStructKey = parentStructKey;
        _areaName = areaName;
    }

    
    /**
     * Key of the result content
     */
    public WGContentKey getContentKey() {
        return _contentKey;
    }

    /**
     * Key of the parent struct entry of this contents struct entry, null if it is a root content 
     */
    public Object getParentStructKey() {
        return _parentStructKey;
    }

    /**
     * Name of the area containing the content
     */
    public String getAreaName() {
        return _areaName;
    }

}
