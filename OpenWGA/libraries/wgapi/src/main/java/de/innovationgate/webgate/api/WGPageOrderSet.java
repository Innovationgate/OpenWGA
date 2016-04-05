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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A column set used to define the order of document collections
 * The order is determined by the fields defined on the set in descending order
 */
public class WGPageOrderSet extends WGColumnSet {

    private String _contentLanguage;
    
    public static final Pattern PAGE_ORDER_EXPRESSION_PATTERN = Pattern.compile("^\\[([^\\]]+)\\](.*)$");
    

    /**
     * Parse an order expression into a set
     * @param pageOrderExpression The expression
     * @throws WGAPIException
     */
    public static WGPageOrderSet parse(String pageOrderExpression) throws WGAPIException {
        Matcher matcher = PAGE_ORDER_EXPRESSION_PATTERN.matcher(pageOrderExpression);
        if (matcher.matches()) {
            return new WGPageOrderSet(matcher.group(2), matcher.group(1));
        }
        else {
            return new WGPageOrderSet(pageOrderExpression, null);
        }
        
        
    }

    private WGPageOrderSet(String columnExpression, String contentLanguage) throws WGAPIException {
        super(columnExpression);
        _contentLanguage = contentLanguage;
    }

    /**
     * Returns the language that is used to resolve language-dependent fields 
     */
    public String getContentLanguage() {
        return _contentLanguage;
    }

}
