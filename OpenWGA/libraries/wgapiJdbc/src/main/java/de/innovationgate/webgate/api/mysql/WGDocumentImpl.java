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

package de.innovationgate.webgate.api.mysql;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.jdbc.MainEntity;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;

public class WGDocumentImpl extends de.innovationgate.webgate.api.jdbc.WGDocumentImpl {

    protected WGDocumentImpl(WGDatabaseImpl parent, MainEntity entity, int type) {
        super(parent, entity, type);
    }
    
    @Override
    public boolean setItemValue(String strName, Object value) throws WGAPIException {

        // Catch some special value cases that are not supported by MySQL 
        if (value instanceof Double) {
            
            // Double.POSITIVE_INFINITY/MAX_VALUE ((#00001173)
            if (value.equals(Double.POSITIVE_INFINITY) || value.equals(Double.NEGATIVE_INFINITY) || value.equals(Double.MAX_VALUE)) {
                throw new WGIllegalDataException("Items in WGA Content Store for MySQL cannot store numerical values Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY and Double.MAX_VALUE");
            }
            
        }
        
        return super.setItemValue(strName, value);
    }

}
