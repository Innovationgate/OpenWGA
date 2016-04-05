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

package de.innovationgate.wgpublisher.mail;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.mail.WGMail;
import de.innovationgate.webgate.api.mail.WGMailException;
import de.innovationgate.webgate.api.mail.WGMailService;
import de.innovationgate.wgpublisher.WGACore;

public class WGAMailService implements WGMailService {
    
    private static List<String> _mimeTypes = new ArrayList<String>();
    static {
        _mimeTypes.add("text/plain");
        _mimeTypes.add("text/html");
    }

    private WGACore _core;
    
    public WGAMailService(WGACore core) {
        _core = core;
    }

    public WGMail createMail(String mimeType) throws WGMailException {
        
        if (_core.getMailConfig() == null || WGUtils.isEmpty(_core.getMailConfig().getMailProperties().get("mail.host"))) {
            if ("true".equals(System.getProperty(WGACore.SYSPROPERTY_DEVELOPMENT_MODE))) {
                MailBase mail = new DevModeMail(_core);
                mail.setMimeType(mimeType);
                return mail;
            }
            else {
                throw new WGMailException("No mail configuration available");
            }
        }
        
        try {
            SmtpMail mail = new SmtpMail(_core.getMailConfig());
            mail.setMimeType(mimeType);
            return mail;
        }
        catch (UnsupportedEncodingException e) {
            throw new WGMailException("Exception creating mail object", e);
        }
        
    }

    public List<String> getSupportedMimeTypes() {
        return _mimeTypes;
    }

    public boolean isAvailable() {
        if (_core.getMailConfig() != null && !WGUtils.isEmpty(_core.getMailConfig().getMailProperties().get("mail.host"))) {
            return true;
        }
        
        if ("true".equals(System.getProperty(WGACore.SYSPROPERTY_DEVELOPMENT_MODE))) {
            return true;
        }
        
        return false;
    }

}
