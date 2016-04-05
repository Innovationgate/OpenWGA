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
package de.innovationgate.webgate.api.modules.servers;

import java.util.Locale;

import de.innovationgate.wga.modules.LocalisationBundleLoader;

public class DatabaseServerProperties {
    
    private boolean _singleton;
    private boolean _availableInInitWizard = false;
    private String _singletonTitle = null;
    private String _singletonUID = null;
    private LocalisationBundleLoader _singletonTitleBundleLoader = null;

    public String getSingletonTitle(Locale locale) {
        if (_singletonTitleBundleLoader != null) {
            return _singletonTitleBundleLoader.getBundle(locale).getString(_singletonTitle);
        }
        else {
            return _singletonTitle;
        }
    }

    public void setSingletonTitle(String singletonTitle) {
        _singletonTitle = singletonTitle;
    }

    public boolean isSingleton() {
        return _singleton;
    }

    public void setSingleton(boolean singleton) {
        _singleton = singleton;
    }

    public LocalisationBundleLoader getSingletonTitleBundleLoader() {
        return _singletonTitleBundleLoader;
    }

    public void setSingletonTitleBundleLoader(LocalisationBundleLoader singletonTitleBundleLoader) {
        _singletonTitleBundleLoader = singletonTitleBundleLoader;
    }

    public String getSingletonUID() {
        return _singletonUID;
    }

    public void setSingletonUID(String singletonUID) {
        _singletonUID = singletonUID;
    }

    public boolean isAvailableInInitWizard() {
        return _availableInInitWizard;
    }

    public void setAvailableInInitWizard(boolean availableInInitWizard) {
        _availableInInitWizard = availableInInitWizard;
    }

}
