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

import java.util.Locale;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;

/**
 * Generic option type to select module definitions
 */
public class ModuleDefinitionsOptionType extends StringOptionType {

    private ModuleRegistry _registry;
    private Class<? extends ModuleType> _moduleType;
    private String _emptyListMessage = "(No entries)";
    private boolean _emptyAllowed = false;
    private boolean _multiValue = false;

    public ModuleDefinitionsOptionType(ModuleRegistry registry, Class<? extends ModuleType> moduleType) {
        _registry = registry;
        _moduleType = moduleType;
    }

    public String getEmptyListMessage() {
        return _emptyListMessage;
    }

    @Override
    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return new ModuleDefinitionsValueProvider(_registry, _moduleType.getName()) {
            @Override
            public String getEmptyListMessage(Locale locale) {
                return _emptyListMessage;
            }
        };
    }
    @Override
    public boolean isEmptyAllowed() {
        return _emptyAllowed;
    }
    
    @Override
    public boolean isMultiValue() {
        return _multiValue ;
    }

    @Override
    public boolean isRestricted() {
        return true;
    }

    public void setEmptyAllowed(boolean emptyAllowed) {
        _emptyAllowed = emptyAllowed;
    }

    public void setEmptyListMessage(String emptyListMessage) {
        _emptyListMessage = emptyListMessage;
    }

    public void setMultiValue(boolean multiValue) {
        _multiValue = multiValue;
    }

    @Override
    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(cx.getConfigCopy());
        if (!provider.getProvidedValues().contains(value)) {
            throw new OptionValueValidationException("The  type '" + value + "' is unknown");
        }
    }

}
