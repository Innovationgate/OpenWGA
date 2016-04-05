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
package de.innovationgate.webgate.api.jdbc.custom;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.List;
import java.util.StringTokenizer;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.templates.TemporaryKey;

public class TemporaryKeyMap extends KeyMap implements TemporaryKey {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public TemporaryKeyMap(ResultSet resultSet) throws SQLException {
        put("row", new Integer(resultSet.getRow()));
        put("resultSet", new Integer(resultSet.hashCode()));
    }
    
    public TemporaryKeyMap(String key) throws ParseException {
        super();
        StringTokenizer tokens = new StringTokenizer(key, "--");
        int idx=0;
        while (tokens.hasMoreTokens()) {
            String token = tokens.nextToken();
            String valueStr = token.substring(1);
            Number value = NUMBER_FORMATTER.parse(WGUtils.strReplace(valueStr, "D", ".", true));
            String name = (idx == 0 ? "row" : "resultSet");
            put(name, value);
            idx++;
        }
    
    }
    
}
