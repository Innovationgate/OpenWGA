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

package de.innovationgate.wgpublisher;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WGAAwareService;
import de.innovationgate.wgpublisher.mail.WGAMailNotification;

/**
 * Service API for sending OpenWGA admin notifications mail
 */
public class WGAAdminNotificationService implements WGAAwareService {

    private WGA _wga;

    @Override
    public void injectWGA(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Create a notification mail
     * @return The mail object
     * @throws WGException
     */
    public WGAMailNotification createNotification() throws WGException {
        return new WGAMailNotification(WGAMailNotification.TYPE_CUSTOM);
    }
    
    /**
     * Send a notification mail to the administrators of the OpenWGA runtime
     * @param mail The mail object
     * @throws WGException
     */
    public void send(WGAMailNotification mail) throws WGException {
        _wga.getCore().send(mail);
    }

}
