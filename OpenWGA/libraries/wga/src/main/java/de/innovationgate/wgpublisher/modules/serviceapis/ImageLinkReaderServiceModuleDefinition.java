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

import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.ServiceApiProperties;
import de.innovationgate.wga.modules.types.ServiceApiType;
import de.innovationgate.wgpublisher.webtml.utils.ImageLinkReader;

public class ImageLinkReaderServiceModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {

    @Override
    public String getTitle(Locale locale) {
        return "Image Link Reader Service";
    }

    @Override
    public String getDescription(Locale locale) {
        return "A service for reading, writing and creating image links which store information about an image to display, like stored into items by by <tml:image item=\"itemname\">";
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
        return ImageLinkReader.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        ServiceApiProperties props = new ServiceApiProperties();
        props.setImplementable(false);
        props.setDefaultImplementation(ImageLinkReader.class);
        return props;
    }

    @Override
    public String getRegistrationKey() {
        return ImageLinkReader.class.getName();
    }

}
