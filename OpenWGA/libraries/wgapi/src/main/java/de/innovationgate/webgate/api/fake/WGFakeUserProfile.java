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
package de.innovationgate.webgate.api.fake;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Locale;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGUserProfile;

public class WGFakeUserProfile extends WGFakeDocument {


    private String name;

    public WGFakeUserProfile(WGDatabase db, String name) {
        super(db, WGDocument.TYPE_USERPROFILE);
        this.name = name;
    }
    
    public Object getMetaData(String type) throws WGSystemException, WGIllegalArgumentException, WGBackendException  {
        
        if (type.equals(WGUserProfile.META_NAME)) {
            return name; 
        }
        else if (type.equals(WGUserProfile.META_CLIENT)) {
            return ("(unknown)");
        }
        else if  (type.equals(WGUserProfile.META_CREATED)) {
            return new Date();
        }
        else if (type.equals(WGUserProfile.META_DBLOGIN)) {
            return WGDatabase.ANONYMOUS_USER;
        }
        else if (type.equals(WGUserProfile.META_HITS)) {
            return new Double(1);
        }
        else if (type.equals(WGUserProfile.META_LANGUAGES)) {
            return Collections.singletonList(Locale.getDefault().getLanguage());
        }
        else if (type.equals(WGUserProfile.META_LASTACCESS)) {
            return new Date();
        }
        else if (type.equals(WGUserProfile.META_LASTMODIFIED)) {
            return new Date();
        }
        else if (type.equals(WGUserProfile.META_LASTSESSIONDATE)) {
            return new Date();
        }
        else if (type.equals(WGUserProfile.META_LASTSESSIONHITS)) {
            return new Double(1);
        }
        else if (type.equals(WGUserProfile.META_LASTSESSIONID)) {
            return "dummy";
        }
        else if (type.equals(WGUserProfile.META_PASSWORD)) {
            return "";
        }
        else if (type.equals(WGUserProfile.META_PASTAUTHORS)) {
            return Collections.singletonList(WGDatabase.ANONYMOUS_USER);
        }
        else if (type.equals(WGUserProfile.META_PASTEDITDATES)) {
            return Collections.singletonList(new Date());
        }
        else if (type.equals(WGUserProfile.META_PORTLETREGISTRY)) {
            return null;
        }
        else if (type.equals(WGUserProfile.META_SESSIONS)) {
            return new Double(1);
        }
        else if (type.equals(WGUserProfile.META_TYPE)) {
            return new Double(0);
        }
        else {
            return super.getMetaData(type); 
        }

    }

    public WGDatabaseRevision remove() throws WGBackendException {
        // Fake user profile removing should fail silently, unlike other fake documents (B00004532)
        return null;
    }

    public WGDatabaseRevision save(Date lastModified) throws WGBackendException {
        // Fake user profile saving should fail silently, unlike other fake documents (B00004532)
        return null;
    }

}
