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

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wgpublisher.WGACore;

public class MediaKeyValueProvider implements OptionValueProvider {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/emptylistmessages", getClass().getClassLoader());
    private ModuleRegistry _reg;

    public MediaKeyValueProvider(ModuleRegistry reg) {
        _reg = reg;
    }

    public List<String> getProvidedValues() {
        WGACore core = (WGACore) _reg.getContextObjects().get(WGACore.class);
        if (core != null) {
            return new ArrayList<String>(core.getMediaKeys());
        }
        else {
            return new ArrayList<String>();
        }
    }

    public String getValueTitle(String value, Locale locale) {
        WGACore core = (WGACore) _reg.getContextObjects().get(WGACore.class);
        if (core != null) {
            MediaKey key = core.getMediaKey(value);
            if (key != null) {
                return value + " (" + key.getMimeType() + ")";
            }
        }

        return value;

    }

    public String getEmptyListMessage(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("option.mediakey.emptylist.message");
    }

}
