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

package de.innovationgate.webgate.api;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import com.google.common.collect.HashBiMap;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.mail.WGMail;
import de.innovationgate.webgate.api.mail.WGMailException;
import de.innovationgate.webgate.api.mail.WGMailService;
import eu.medsea.mimeutil.MimeUtil;

/**
 * A default implementation that always returns octet stream
 */
public class DefaultMimetypeDeterminationService implements MimetypeDeterminationService {
    
    private static Properties _mimeToSuffix = new Properties();
    
    public DefaultMimetypeDeterminationService() {
        
        try {
            _mimeToSuffix.load(WGFactory.class.getClassLoader().getResourceAsStream(WGUtils.getPackagePath(DefaultMimetypeDeterminationService.class) + "/mime-to-suffix.properties"));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
       
    }

    public String determineByFilename(String fileName) {
       Collection mimes = MimeUtil.getMimeTypes(fileName);
       if (mimes.size() > 0) {
           return String.valueOf(mimes.iterator().next());
       }
       else {
           return null;
       }
       
    }

    @Override
    public String determineSuffixByMimeType(String mimeType) {
        return _mimeToSuffix.getProperty(mimeType);
    }

    

}
