/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils;

import java.security.cert.X509CRL;
import java.security.cert.X509CRLEntry;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.Set;

/**
 * Utility to verify validity of X.509 certificates and their certificate chain
 */
public class CertificateValidationUtils {


    
    /**
     * Verifies if the given certificate was signed with the given ca
     * @param cert The certificate to test
     * @param ca The certificate authority that the certificate should be signed against
     * @return true, if the certificate was signed by this ca, false otherwise
     */
    public static boolean verify(X509Certificate cert, X509Certificate ca) {
        try {
            cert.verify(ca.getPublicKey());
        }
        catch (Exception e) {
            return false;
        }        
        return true;
    }
    
    /**
     * Tests if a given certificate is contained in a certificate revocation list
     * @param crl The certificate revocation list
     * @param clientCert The certificate to test
     * @return true if the certificate was revoked in the list, false otherwise
     */
    public static boolean isCertRevoked(X509CRL crl, X509Certificate clientCert) {
        Set setEntries = crl.getRevokedCertificates();
        if (setEntries == null) {
            return false;
        } else {
            Iterator it = setEntries.iterator();
            while (it.hasNext()) {
                X509CRLEntry crlEntry = (X509CRLEntry) it.next();
                if (crlEntry.getSerialNumber().equals(clientCert.getSerialNumber())) {                    
                    return true;
                }
            }
        }
        return false;        
    }        

}
