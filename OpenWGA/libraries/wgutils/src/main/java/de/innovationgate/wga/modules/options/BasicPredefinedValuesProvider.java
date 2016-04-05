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

package de.innovationgate.wga.modules.options;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * A {@link PredefinedValuesValueProvider} implemnetation that just received values and titles for them without multilanguage support.
 */
public class BasicPredefinedValuesProvider implements PredefinedValuesValueProvider {
    
    private Map<String,String> _values = new HashMap<String,String>();

    @Override
    public List<String> getProvidedValues() {
        return new ArrayList<String>(_values.keySet());
    }

    @Override
    public String getValueTitle(String value, Locale locale) {
        return _values.get(value);
    }

    @Override
    public String getEmptyListMessage(Locale locale) {
        return "--- no options ---";
    }

    @Override
    public void addValue(String value, String title) {
        
        if (title == null) {
            title = value;
        }
        
        _values.put(value, title);
    }


}
