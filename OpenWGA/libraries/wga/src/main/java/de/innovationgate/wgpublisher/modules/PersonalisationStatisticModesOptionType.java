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

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.options.OptionType;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wga.modules.options.OptionValueValidationException;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.TextEncodingOptionType;
import de.innovationgate.wgpublisher.WGACore;

public class PersonalisationStatisticModesOptionType extends PredefinedValuesOptionType {
    
    private static LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(PersonalisationStatisticModesOptionType.class) + "/optiontypes", TextEncodingOptionType.class.getClassLoader());
    public static final PersonalisationStatisticModesOptionType INSTANCE = new PersonalisationStatisticModesOptionType();
    
    private PersonalisationStatisticModesOptionType() {
        super(_bundleLoader, "persstatmodes");
        addValue(String.valueOf(Constants.PERSSTATMODE_OFF));
        addValue(String.valueOf(Constants.PERSSTATMODE_SESSION));
        addValue(String.valueOf(Constants.PERSSTATMODE_HIT));
    }

    public boolean isRestricted() {
        return true;
    }

}
