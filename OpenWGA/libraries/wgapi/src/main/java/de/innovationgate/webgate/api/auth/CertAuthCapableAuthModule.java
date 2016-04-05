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

import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;

/**
 * Mixin Interface for {@link AuthenticationModule} implementations that are able to be used with certificate authentication
 */
public interface CertAuthCapableAuthModule {
    
    /**
     * Returns if certificate authentication is enabled and should be used by the database, if not otherwise configured there
     */
    public boolean isCertAuthEnabled();
    
    /**
     * Called to verify a login that uses a certificate as credential. Returns a session object if it succeeds.
     * @param cert The certificate 
     * @return An authentication session object if the login succeeds. null otherwise.
     * @throws AuthenticationException If the login cannot be verified
     */
    public AuthenticationSession login(X509Certificate cert) throws AuthenticationException;

    /**
     * Returns the certificate revoke list to use for certificate authentication
     * certificates listed here will not be allowed to login, even when they are signed against the correct CA
     * @throws AuthenticationException
     */
    public X509CRL getCRL() throws AuthenticationException;
    
    /**
     * Returns the certificate authority to use for certificate authentication
     * Only certificates will be allowed to login that are signed against this CA
     * @throws AuthenticationException
     */
    public X509Certificate getCA() throws AuthenticationException;
    


}
