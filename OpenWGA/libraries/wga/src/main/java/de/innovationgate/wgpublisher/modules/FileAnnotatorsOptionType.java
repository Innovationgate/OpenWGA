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

import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.options.OptionType;
import de.innovationgate.wga.modules.options.CommaSeparatedListOptionType;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wga.modules.options.OptionValueValidationException;
import de.innovationgate.wga.modules.options.ValidationContext;

public class FileAnnotatorsOptionType extends CommaSeparatedListOptionType implements OptionType {    
    private ModuleRegistry _reg;

    public FileAnnotatorsOptionType(ModuleRegistry reg) {
        _reg = reg;
    }

    public Class<? extends Object> getDataTypeHint() {
        return String.class;
    }

    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return new FileAnnotatorsValueProvider(_reg);
    }

    public boolean isEmptyAllowed() {
        return true;
    }

    public boolean isRestricted() {
        return true;
    }

    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(cx.getConfigCopy());
        List<String> providedValues = provider.getProvidedValues();
        Iterator<String> values = WGUtils.deserializeCollection(value, ",", true).iterator();
        while (values.hasNext()) {
            String singleValue = (String) values.next();
            if (!providedValues.contains(singleValue)) {
                throw new OptionValueValidationException("The file annotator type '" + singleValue + "' is unknown");
            }
        }
    }

}
