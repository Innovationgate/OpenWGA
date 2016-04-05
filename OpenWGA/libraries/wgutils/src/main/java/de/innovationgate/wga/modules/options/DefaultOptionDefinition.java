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

import java.util.List;
import java.util.Locale;

import de.innovationgate.wga.modules.ReferenceType;

/**
 * A very basic option definition without extra behaviour, to use internally when no explicit definition is available
 */
public class DefaultOptionDefinition implements OptionDefinition {
    
    private String _name;
    
    public DefaultOptionDefinition(String name) {
        _name = name;
    }

    public String getName() {
        return _name;
    }

    public boolean isOptional() {
        return false;
    }

    public boolean isExpert() {
        return false;
    }

    public String getDescription(Locale locale) {
        return "Default option definition";
    }

    public OptionType getOptionType() {
        return StringOptionType.INSTANCE;
    }

    public String getDefaultValue() {
        return null;
    }

    public String getTitle(Locale locale) {
        return "Default option definition";
    }

    public String getCategory() {
        return null;
    }

    public List<DependentOption> getDependentOptions() {
        return null;
    }

    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
    }

    public boolean isEmptyAllowed() {
        return true;
    }

    public boolean hasFlag(String flagName) {
        return false;
    }

    public Class<? extends ReferenceType> getReferenceType() {
        return null;
    }

}
