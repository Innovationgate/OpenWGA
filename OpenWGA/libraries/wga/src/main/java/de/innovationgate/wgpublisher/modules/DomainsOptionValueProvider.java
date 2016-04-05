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
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.Domain;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.options.OptionValueProvider;

public class DomainsOptionValueProvider implements OptionValueProvider {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/emptylistmessages", getClass().getClassLoader());
    
    private WGAConfiguration _config;

    private boolean _onlyPersonalized;

    public DomainsOptionValueProvider(WGAConfiguration configCopy, int flags) {
        _config = configCopy;
        _onlyPersonalized = (flags & DomainsOptionType.FLAG_ONLY_PERSONALIZED) == DomainsOptionType.FLAG_ONLY_PERSONALIZED;
    }

    public List<String> getProvidedValues() {
        List<String> dbKeys = new ArrayList<String>();
        Iterator<Domain> domains = _config.getDomains().iterator();
        while (domains.hasNext()) {
            Domain domain = domains.next();
            
            if (_onlyPersonalized && domain.getPersonalisation() == null) {
                continue;
            }
            
            dbKeys.add(domain.getName());
        }
        return dbKeys;
    }

    public String getValueTitle(String value, Locale locale) {
        return value;
    }

    public String getEmptyListMessage(Locale arg0) {
        if (_onlyPersonalized) {
            return _bundleLoader.getBundle(arg0).getString("option.domains.emptylist.message.persdomains");
        }
        else {
            return _bundleLoader.getBundle(arg0).getString("option.domains.emptylist.message.alldomains");
        }
            
    }
}
