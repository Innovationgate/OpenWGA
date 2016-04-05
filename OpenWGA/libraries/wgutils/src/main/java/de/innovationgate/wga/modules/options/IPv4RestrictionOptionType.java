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

/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.wga.modules.options;

import java.util.Locale;

import de.innovationgate.utils.net.IPRestriction;
import de.innovationgate.utils.net.IPv4Restriction;

/**
 * Option for input of an IP4 restriction
 */
public class IPv4RestrictionOptionType extends StringOptionType {
    
    public static final IPv4RestrictionOptionType INSTANCE = new IPv4RestrictionOptionType();
    
    private IPv4RestrictionOptionType() {
    }

    @Override
    public boolean isEmptyAllowed() {
        return false;
    }

    @Override
    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        
        try {
            IPRestriction restriction = IPv4Restriction.parseRestrictionString(value);
        }
        catch (Exception e) {
            throw new OptionValueValidationException("No valid restriction string: " + value + ". Use single IP address to specify host, \"IP-IP\" to specify range and \"IP/Netmask\" to specify network");
        }
        
    }

}
