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
import de.innovationgate.wga.modules.ReferenceType;
import de.innovationgate.wga.modules.options.OptionType;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wga.modules.options.OptionValueValidationException;
import de.innovationgate.wga.modules.options.ReferenceOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.modules.reftypes.DatabaseKeyReferenceType;

public class RuntimeDatabasesOptionType extends StringOptionType implements ReferenceOptionType {
    
    
    public static final int SELECTION_CONTENT_STORES = 1;
    public static final int SELECTION_CONTENT_DATABASES = 2;
    public static final int SELECTION_PLUGIN_DATABASES = 4;
    

    private ModuleRegistry _registry;
    private int _selection;

    public RuntimeDatabasesOptionType(ModuleRegistry registry, int selection) {
        _registry = registry;
        _selection  = selection;
    }

    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return new RuntimeDatabaseOptionValueProvider(configCopy, (WGACore) _registry.getContextObjects().get(WGACore.class), _selection);
    }

    public boolean isRestricted() {
        return true;
    }

    public void validate(String value, Locale locale, WGAConfiguration config) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(config);
        if (!provider.getProvidedValues().contains(value)) {
            throw new OptionValueValidationException("The database '" + value + "' is unknown or not of the neccessary type");
        }
    }

    public Class<? extends ReferenceType> getReferenceType() {
        return DatabaseKeyReferenceType.class;
    }

}
