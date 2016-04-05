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

package de.innovationgate.wga.modules.options;

/**
 * Interface for password encoding modules, registered for type {@link PasswordOptionType}
 *
 */
public interface PasswordOptionEncoder {
    
    /**
     * Encode a password with the implemented encoding scheme
     * @param password
     * @return The encoded password
     * @throws PasswordEncodingException
     */
    public String encodePassword(String password) throws PasswordEncodingException;

    /**
     * Decodes a password with the implemented encoding scheme 
     * @param password
     * @return The decoded password
     * @throws PasswordEncodingException
     */
    public String decodePassword(String password) throws PasswordEncodingException;

    /**
     * Returns the encoding key by which the used password encoding on stored options should be determined
     */
    public String getEncodingKey();

}
