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

package de.innovationgate.wgpublisher.modules.serviceapis;

import java.util.Locale;

import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.imgscalr.ImgScalrScaler;
import de.innovationgate.utils.security.BCrypt10HashingScheme;
import de.innovationgate.utils.security.HashingService;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.HashingSchemeType;
import de.innovationgate.wga.modules.types.ImageScalerModuleType;
import de.innovationgate.wga.modules.types.ServiceApiProperties;
import de.innovationgate.wga.modules.types.ServiceApiType;

public class ImageScalerApiModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {


    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/apis", getClass().getClassLoader());

    @Override
    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("scaler.title");
    }

    @Override
    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("scaler.description");
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return ServiceApiType.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return ImageScalerModuleType.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        ServiceApiProperties props = new ServiceApiProperties();
        props.setDefaultImplementation(ImgScalrScaler.class);
        return props;
    }

    @Override
    public String getRegistrationKey() {
        return ImageScaler.class.getName();
    }

}
