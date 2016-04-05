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

package de.innovationgate.wgpublisher.webtml.portlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.security.GeneralSecurityException;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

public class JavaSerialisationPortletStateSerialisationService implements PortletStateSerialisationService {

    public static final String NAME = "java";
    
    @Override
    public TMLPortletState deserialize(String str) throws ClassNotFoundException, IOException {
        byte[] decrypted;
        try {
            byte[] bytes = Base64.decodeWeb(str);
            byte[] uncompressed = WGUtils.unzip(bytes);
            decrypted = WGACore.INSTANCE.getSymmetricEncryptionEngine().decrypt(uncompressed);
        }
        catch (GeneralSecurityException e) {
            // We ignore decryption errors here silently because they most likely originate from obsolete DES encryption of OpenWGA <= 6.2
            return null;
        }
        catch (Exception e) {
            WGACore.INSTANCE.getLog().error("Unable to deserialize portlet state.", e);
            return null;
        }
        
        TMLPortletState state = (TMLPortletState) new ObjectInputStream(new ByteArrayInputStream(decrypted)).readObject();
        return state;
    }

    @Override
    public String serialize(TMLPortletState state) throws IOException, GeneralSecurityException {

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        new ObjectOutputStream(out).writeObject(state);
        out.flush();
        
        byte[] encrypted = WGACore.INSTANCE.getSymmetricEncryptionEngine().encrypt(out.toByteArray());
        byte[] zipped = WGUtils.zip(encrypted);
        return Base64.encodeWeb(zipped);
        
    }
    
    @Override
    public String getId() {
        return NAME;
    }

}
