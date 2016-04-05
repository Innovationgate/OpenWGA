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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.apache.log4j.Logger;

import de.innovationgate.utils.Base64;

/**
 * engine to perform symmetric encryption
 * 
 * by default a transformation of "AES/CBC/PKCS5Padding" with a 128bit key length is used
 */
public class SymmetricEncryptionEngine {
    
    private static final String ALGORITHM = "AES";
    private static final String TRANSFORMATION = "AES/CBC/PKCS5Padding";
    private static final String SALT = "OpenWGA";
    private static final int KEYSIZE = 128; // maximum for restricted JVMs
    
    private SecretKeySpec _keySpec;
    private IvParameterSpec _iv;
    
    
    public SecretKey generateKey() throws NoSuchAlgorithmException {
        final KeyGenerator keyGen = KeyGenerator.getInstance(ALGORITHM);
        keyGen.init(KEYSIZE);
        return keyGen.generateKey();
    }

    public void init(byte[] key) throws GeneralSecurityException, UnsupportedEncodingException {
        MessageDigest md = MessageDigest.getInstance("MD5");
        _iv = new IvParameterSpec(md.digest(SALT.getBytes("UTF-8")));
        _keySpec = new SecretKeySpec(key, ALGORITHM);
    }
    
    private Cipher getCipher(int mode) throws GeneralSecurityException {
        Cipher cipher = Cipher.getInstance(TRANSFORMATION);
        cipher.init(mode, _keySpec, _iv);
        return cipher;
    }
    
    public byte[] encrypt(byte[] input) throws GeneralSecurityException {
        return getEncryptionCipher().doFinal(input);
    }
    
    public byte[] decrypt(byte[] input) throws GeneralSecurityException {
        return getDecryptionCipher().doFinal(input);
    }
    
    public String encryptBase64Web(byte[] input) throws GeneralSecurityException {
        return Base64.encodeWeb(encrypt(input));
    }
    
    public byte[] decryptBase64Web(String input) throws GeneralSecurityException, IOException {
        return decrypt(Base64.decodeWeb(input));
    }
    
    public Cipher getEncryptionCipher() throws GeneralSecurityException {
        return getCipher(Cipher.ENCRYPT_MODE);
    }

    public Cipher getDecryptionCipher() throws GeneralSecurityException {
        return getCipher(Cipher.DECRYPT_MODE);
    }

    public SecretKeySpec getKeySpec() {
        return _keySpec;
    }
    
    public String getTransformation() {
        return TRANSFORMATION;
    }
    
}
