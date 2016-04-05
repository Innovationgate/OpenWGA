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

package de.innovationgate.webgate.api.auth;

/**
 * Subinterface for {@link AuthenticationSession} which provides an additional user cache qualifier for the WGAPI.
 * This allows it to return dynamic user rights depending on some custom condition, unknown to the WGAPI (for example: The DNS hostname of the request).
 * The qualifier returned by {@link #getCacheQualifier()} should be unique for that custom condition (for example: That DNS hostname). 
 * The WGAPI will use the qualifier to build a separate user cache for it to cache user rights.
 */
public interface AuthSessionWithUserCacheQualifier extends AuthenticationSession {
    
    /**
     * Return the additional cache qualifier for WGAPI
     */
    public String getCacheQualifier();

}
