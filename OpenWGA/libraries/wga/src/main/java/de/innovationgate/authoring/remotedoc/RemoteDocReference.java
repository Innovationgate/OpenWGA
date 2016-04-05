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

package de.innovationgate.authoring.remotedoc;

import java.util.List;

import de.innovationgate.utils.WGUtils;

public class RemoteDocReference {
    
    private String _dbKey;
    private String _contentKey;
    
    public RemoteDocReference(String dbKey, String contentKey) {
        _dbKey = dbKey;
        _contentKey = contentKey;
    }
    
    public RemoteDocReference(String contextExpr) throws IllegalArgumentException {
        
        List elements = WGUtils.deserializeCollection(contextExpr, "/");
        if (elements.size() != 2) {
            throw new IllegalArgumentException("Unknown remote doc reference syntax: " + contextExpr);
        }
        
        String dbPath = (String) elements.get(0);
        if (!dbPath.startsWith("db:")) {
            throw new IllegalArgumentException("Unknown remote doc reference syntax: " + contextExpr);
        }
        _dbKey = dbPath.substring(3);
        
        String contentPath = (String) elements.get(1);
        if (!contentPath.startsWith("docid:")) {
            throw new IllegalArgumentException("Unknown remote doc reference syntax: " + contextExpr);
        }
        _contentKey = contentPath.substring(6);
        

    }

    public String getDbKey() {
        return _dbKey;
    }

    public String getContentKey() {
        return _contentKey;
    }

    public String toString() {
        return "db:" + _dbKey + "/docid:" + _contentKey;
    }

}
