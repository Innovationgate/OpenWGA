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

import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;

import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.WGACore;

public class DevModeMail extends MailBase {
    
    private WGACore _core;

    public DevModeMail(WGACore core) {
        _core = core;
    }

    @Override
    protected Session createMailSession() {
        return Session.getInstance(System.getProperties());
    }
    
    @Override
    public void send(Message msg) throws MessagingException {

        try {
            synchronized(_core) {
                File mailFolder = new File(_core.getConfigFile().getParentFile(), "mails");
                mailFolder.mkdirs();
                String dateString = new SimpleDateFormat("yyyy.MM.dd_HH.mm").format(new Date());
                
                int idx = 0;
                File outFile;
                do {
                    idx++;
                    outFile = new File(mailFolder, "mail_" + dateString + "_" + idx + ".eml");
                }
                while (outFile.exists());
                WGFactory.getLogger().info("No mail service available. Writing mail contents to file: " + outFile.getAbsolutePath());
                
                FileOutputStream out = new FileOutputStream(outFile);
                msg.writeTo(out);
                out.flush();
                out.close();
            }
        }
        catch (Exception e) {
            throw new MessagingException("Exception storing dev mode mail", e);
        }

    }

}
