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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.types.HashingSchemeType;

/**
 * Tool class to created hashed passwords based on an determinable hashing scheme
 */
public class HashedPassword {
    
    public static final String FALLBACK_SCHEME = SHA1HashingScheme.NAME;
    public static final Pattern HASH_PLUS_SCHEME_PATTERN = Pattern.compile("^\\[([A-Z-a-z0-9\\-]+)\\] (.*)$");
    
    public static HashingService fetchScheme(ModuleRegistry reg, String schemeName) throws HashingException, ModuleInstantiationException {
        ModuleDefinition schemeDef = reg.getModuleDefinitionByKey(HashingSchemeType.class, schemeName);
        if (schemeDef == null) {
            return null;
        }
        
        HashingService scheme = (HashingService) reg.instantiate(schemeDef);
        return scheme;
    }
    
    
    /**
     * Creates a password hash from the given password, using the given scheme
     * @param pwd The password in plaintext
     * @param scheme The name of the hashing scheme
     * @throws HashingException 
     * @return The module registry used to fetch the hashing scheme
     */
    public static HashedPassword create(String pwd, HashingService scheme, Object salt) throws HashingException {
        
        try {
            String hash = scheme.createHash(pwd.getBytes("UTF-8"), salt);
            return new HashedPassword(hash, scheme.getName());
            
        }
        catch (Exception e) {
            throw new HashingException("Error checking password hash", e);
        }
        
    }
    

    
    private String _schemeName;
    private String _hash;
    
    /**
     * Reads a stored password hash which may also contain a scheme prefix and divides the hash from the scheme name
     * @param pwd The stored hash
     */
    public HashedPassword(String pwd) {
        
        Matcher pwdMatcher = HASH_PLUS_SCHEME_PATTERN.matcher(pwd);
        if (pwdMatcher.matches()) {
            _schemeName = pwdMatcher.group(1);
            _hash = pwdMatcher.group(2);
        }
        else {
            _schemeName = FALLBACK_SCHEME;
            _hash = pwd;
        }
    }
    
    /**
     * Default constructor
     * @param hash
     * @param scheme
     */
    public HashedPassword(String hash, String scheme) {
        _hash = hash;
        _schemeName = scheme;
        
    }

    public String getSchemeName() {
        return _schemeName;
    }

    public void setSchemeName(String schemeName) {
        _schemeName = schemeName;
    }

    public String getHash() {
        return _hash;
    }

    public void setHash(String hash) {
        _hash = hash;
    }
    
    /**
     * Checks a given plaintext password for equality to the hash.
     * The registry is given for retrieval of the correct hashing scheem
     * @param pwd The password in plaintext
     * @param reg The module registry
     * @return true if the given password equals the password which was used to build the hash
     */
    public boolean check(String pwd, ModuleRegistry reg) throws HashingException {
        
        try {
            HashingService scheme = fetchScheme(reg, _schemeName);
            if (scheme == null) {
                throw new HashingException("The hashing scheme '" + _schemeName + "', used to hash this password, is unknown.");
            }
            
            return scheme.checkPassword(pwd.getBytes("UTF-8"), _hash);
            
        }
        catch (Exception e) {
            throw new HashingException("Error checking password hash", e);
        }
    }
    
    @Override
    public String toString() {
        
        if (FALLBACK_SCHEME.equals(_schemeName)) {
            return _hash;
        }
        else {
            return "[" + _schemeName + "] " + _hash;
        }
    }

}
