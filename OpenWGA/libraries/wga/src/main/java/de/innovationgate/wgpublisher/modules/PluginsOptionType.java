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

package de.innovationgate.wgpublisher.modules;

import java.util.Locale;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.options.OptionType;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wga.modules.options.OptionValueValidationException;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wgpublisher.WGACore;

public class PluginsOptionType extends StringOptionType {
    
    public static final int USAGE_ANY = 0;
    public static final int USAGE_CONTENTSTORE = 1;
    public static final int USAGE_AUTHSOURCE = 2;
    public static final int USAGE_DESIGNSOURCE = 3;

    private ModuleRegistry _registry;
    private int _usage;

    public PluginsOptionType(ModuleRegistry registry, int usage) {
        _registry = registry;
        _usage = usage;
    }


    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        WGACore core = (WGACore) _registry.getContextObjects().get(WGACore.class);
        return new PluginsOptionValueProvider(core, _usage);
    }

    public boolean isRestricted() {
        return true;
    }

    public void validate(String value, Locale locale, WGAConfiguration config) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(null);
        if (!provider.getProvidedValues().contains(value)) {
            throw new OptionValueValidationException("The plugin '" + value + "' in either unknown, not enabled or no content store");
        }
    }

}
