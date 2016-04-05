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

/**
 * A service API for for unidirectional hashing
 */
public interface HashingService {
    
    /**
     * Returns the name of this hashing scheme
     */
    public String getName();
    
    /**
     * Create a hash of the given data
     * @param secret The data
     * @param salt A salt to use for hashing
     * @throws Exception 
     */
    public String createHash(byte[] secret, Object salt) throws HashingException;
    
    /**
     * Compares data to a previously created hash
     * @param data The data
     * @param hash The hash
     * @return true if the data equals the data for which the hash was created
     */
    public boolean checkPassword(byte[] data, String hash) throws HashingException;
    
    /**
     * Generates a salt to be used with the {@link #createHash(byte[], Object)} method.
     * This is callable independently, so a functionality may use the same salt for multiple password hashings and then may test hash equality
     * @throws HashingException
     */
    public Object generateSalt() throws HashingException;

}
