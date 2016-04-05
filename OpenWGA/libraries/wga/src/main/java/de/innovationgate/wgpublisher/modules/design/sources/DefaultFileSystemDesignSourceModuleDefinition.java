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

package de.innovationgate.wgpublisher.modules.design.sources;

import java.util.Locale;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.properties.DesignSourceProperties;
import de.innovationgate.wgpublisher.design.fs.DefaultFileSystemDesignSource;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignSource;

public class DefaultFileSystemDesignSourceModuleDefinition extends FileSystemDesignSourceModuleDefinition {
    
    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        return options;
    }
    
    public Object getProperties() {
        DesignSourceProperties props = new DesignSourceProperties();
        props.setDesignProviderClass(FileSystemDesignProvider.class);
        props.setSingleton(true);
        props.setSingletonUID(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
        props.setSingletonTitle("defaultds.title");
        props.setSingletonTitleBundleLoader(_bundleLoader);
        return props;
    }

    public Class<? extends Object> getImplementationClass() {
        return DefaultFileSystemDesignSource.class;
    }

}
