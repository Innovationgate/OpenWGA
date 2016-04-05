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

package de.innovationgate.wgpublisher.webtml.utils;

import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.MimeTypeSpecificImageScaler;
import de.innovationgate.utils.MimeTypeSpecificImageScalerFactory;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.types.ImageScalerModuleType;
import de.innovationgate.wga.modules.types.MimeTypeSpecificImageScalerFactoryModuleType;
import de.innovationgate.wga.modules.types.MimeTypeSpecificImageScalerModuleType;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

/**
 * A factory for image scalers
 */
public class ImageScalerFactory {
    
    /**
     * Creates a format-neutral image scaler. Same as retrieving the Service API for the {@link ImageScaler} interface.
     * @param core The WGA Core
     * @return The image scaler 
     * @throws WGException
     */
    public static ImageScaler createImageScaler(WGACore core) throws WGException {
        return WGA.get(core).service(ImageScaler.class);
    }
    
    /**
     * Creates an image scaler specifically for the given mime type.
     * This iterates over all available {@link MimeTypeSpecificImageScalerFactory} (while preferring the one that is configured as default on server configuration), trying to find a factory able to serve a scaler for the given type.
     * Returns null if none if found.
     * @param core The WGA core
     * @param mimeType The mime type
     * @return A scaler for the given mime type or null if none was found
     * @throws WGException
     * @throws ModuleInstantiationException
     */
    public static MimeTypeSpecificImageScaler createMimeTypeSpecificImageScaler(WGACore core, String mimeType) throws WGException, ModuleInstantiationException {
        
        // First try the preferred factory
        WGA wga = WGA.get(core);
        MimeTypeSpecificImageScalerFactory factory = wga.service(MimeTypeSpecificImageScalerFactory.class);
        MimeTypeSpecificImageScaler scaler = factory.getForMimeType(mimeType);
        if (scaler != null) {
            return scaler;
        }
        
        // Legacy: Look if some scaler registered itself specifically for this mime type (which is a deprecated form of scaler registration)
        ModuleDefinition scalerDef = core.getModuleRegistry().getModuleDefinitionByKey(MimeTypeSpecificImageScalerModuleType.class, mimeType);
        if (scalerDef != null) {
            scaler = (MimeTypeSpecificImageScaler) core.getModuleRegistry().instantiate(scalerDef);
            return scaler;
        }
        
        // Iterate through all registered factory to see if somebody supports the mimetype
        for (ModuleDefinition factoryModule : core.getModuleRegistry().getModulesForType(MimeTypeSpecificImageScalerFactoryModuleType.class).values()) {
            factory = wga.instantiateService(factoryModule); 
            scaler = factory.getForMimeType(mimeType);
            if (scaler != null) {
                return scaler;
            }
        }
        
        return null;
        
    }

}
