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

import java.security.GeneralSecurityException;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage.TMLPortletStateTypeAdapterFactory;

public class GsonPortletStateSerialisationService implements PortletStateSerialisationService {
    
    public static final String NAME = "gson";
    
    public static final Gson STATE_SERIALIZER = new GsonBuilder()
    .setVersion(TMLPortletState.STATE_VERSION)
    .registerTypeAdapter(Version.class, new Version.GsonTypeAdapter())
    .registerTypeAdapter(DesignResourceReference.class, new DesignResourceReference.GsonTypeAdapter())
    .registerTypeAdapterFactory(new TMLPortletStateTypeAdapterFactory())
    .create();

    @Override
    public TMLPortletState deserialize(String str) throws Exception {

      String json = readPortletStateJson(str);
      if (json == null) {
          return null;
      }
      
      return STATE_SERIALIZER.fromJson(json, TMLPortletState.class);
        
    }

    @Override
    public String serialize(TMLPortletState state) throws Exception {
      String json = STATE_SERIALIZER.toJson(state);
      byte[] zipped = WGUtils.zipString(json);
      byte[] encrypted = WGACore.INSTANCE.getSymmetricEncryptionEngine().encrypt(zipped);
      return Base64.encodeWeb(encrypted);
    }

    public String readPortletStateJson(String str) {
         
        byte[] decrypted;
        try {
            byte[] bytes = Base64.decodeWeb(str);
            decrypted = WGACore.INSTANCE.getSymmetricEncryptionEngine().decrypt(bytes);
        }
        catch (GeneralSecurityException e) {
            // We ignore decryption errors here silently because they most likely originate from obsolete DES encryption of OpenWGA <= 6.2
            return null;
        }
        catch (Exception e) {
            WGACore.INSTANCE.getLog().error("Unable to deserialize portlet state.", e);
            return null;
        }
        
        return WGUtils.unzipString(decrypted);
        
        
    }
    
    @Override
    public String getId() {
        return NAME;
    }

}
