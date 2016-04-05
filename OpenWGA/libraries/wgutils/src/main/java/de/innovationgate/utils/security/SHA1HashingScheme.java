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

package de.innovationgate.utils.security;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import de.innovationgate.utils.Base64;

/**
 * Hashing scheme using SHA-1
 */
public class SHA1HashingScheme implements HashingService {
    
    public static final String NAME = "sha-1";

    @Override
    public String createHash(byte[] secret, Object salt) throws HashingException {
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("SHA-1");
            byte[] digestedPwdBytes = messageDigest.digest(secret);
            return Base64.encode(digestedPwdBytes);
        }
        catch (NoSuchAlgorithmException e) {
            throw new HashingException("Missing algorithm", e);
        }
    }

    @Override
    public boolean checkPassword(byte[] data, String hash) throws HashingException {
        return createHash(data, null).equals(hash);
    }

    @Override
    public Object generateSalt() throws HashingException {
        return null;
    }
    
    @Override
    public String getName() {
        return NAME;
    }

}
