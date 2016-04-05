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
import java.security.KeyPair;
import java.security.KeyPairGenerator;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.DESEncrypter.PersistentKeyException;


/**
 * Utils to work with RSA key pairs
 */
public class RSAKeyPairUtils {
	
	
	 // Salt
    private static byte[] _salt = {
        (byte)0xc7, (byte)0x73, (byte)0x21, (byte)0x8c,
        (byte)0x7e, (byte)0xc8, (byte)0xee, (byte)0x99
    };

    // Iteration count
    private static int _count = 20;

    // Create PBE parameter set
    private static final PBEParameterSpec _pbeParamSpec = new PBEParameterSpec(_salt, _count);
    
	/**
	 * generates a new RSA keypair
	 * and sets the passphrase on the private key if passphrase != null
	 * @param passphrase
	 * @return RSAKeyPair
	 * @throws IOException 
	 * @throws GeneralSecurityException 
	 * @throws PersistentKeyException 
	 */
	public static RSAKeyPair generateRSAKeyPair(String passphrase) throws PersistentKeyException, GeneralSecurityException, IOException{
		 KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
		 generator.initialize(1024);		 
		 KeyPair keyPair = generator.generateKeyPair();
		 
		 byte[] privateKey = keyPair.getPrivate().getEncoded();
		 byte[] publicKey = keyPair.getPublic().getEncoded();
		 
		 
		 String privateKeyBase64;
		 boolean encrypted = false;
		 if (passphrase != null) {
			 privateKeyBase64 = setPassphrase(passphrase, privateKey);
			 encrypted = true;
		 } else {
			 privateKeyBase64 = Base64.encode(privateKey);
		 }
		 
		 String publicKeyBase64 = Base64.encode(publicKey);
		 
		 		 
		 return new RSAKeyPair(privateKeyBase64, publicKeyBase64, encrypted);
	}


	private static String setPassphrase(String passphrase, byte[] privateKey) throws PersistentKeyException, GeneralSecurityException, IOException, UnsupportedEncodingException {
	    PBEKeySpec pbeKeySpec = new PBEKeySpec(passphrase.toCharArray());
	    SecretKeyFactory keyFac = SecretKeyFactory.getInstance("PBEWithMD5AndDES");
	    SecretKey pbeKey = keyFac.generateSecret(pbeKeySpec);

	    // Create PBE Cipher
	    Cipher pbeCipher = Cipher.getInstance("PBEWithMD5AndDES");

	    // Initialize PBE Cipher with key and parameters
	    pbeCipher.init(Cipher.ENCRYPT_MODE, pbeKey, _pbeParamSpec);

	    byte[] ciphertext = pbeCipher.doFinal(privateKey);
		return Base64.encode(ciphertext);		
	}
	
	
	public static void changePassphrase(RSAKeyPair keyPair, String oldPassphrase, String newPassphrase) throws PersistentKeyException, GeneralSecurityException, IOException, IncorrectPassphraseException {	
		byte[] privateKey = getPrivateKey(keyPair, oldPassphrase);		
		keyPair.setPrivateKey(setPassphrase(newPassphrase, privateKey));
	}

	public static boolean isPassphraseCorrect(RSAKeyPair keyPair, String passphrase) throws PersistentKeyException, GeneralSecurityException, IOException {
	    try {
            getPrivateKey(keyPair, passphrase);
        }
        catch (IncorrectPassphraseException e) {
            return false;
        }
        return true;
	}

	public static byte[] getPrivateKey(RSAKeyPair keyPair, String passphrase) throws PersistentKeyException, GeneralSecurityException, IOException, IncorrectPassphraseException {
		byte[] privateKey;
		if (keyPair.isEncrypted()) {
			PBEKeySpec pbeKeySpec = new PBEKeySpec(passphrase.toCharArray());
		    SecretKeyFactory keyFac = SecretKeyFactory.getInstance("PBEWithMD5AndDES");
		    SecretKey pbeKey = keyFac.generateSecret(pbeKeySpec);

		    // Create PBE Cipher
		    Cipher pbeCipher = Cipher.getInstance("PBEWithMD5AndDES");

		    // Initialize PBE Cipher with key and parameters
		    pbeCipher.init(Cipher.DECRYPT_MODE, pbeKey, _pbeParamSpec);
		    
		    byte[] encrypted = Base64.decode(keyPair.getPrivateKey());
		    
		    try {
                privateKey = pbeCipher.doFinal(encrypted);
            }
            catch (BadPaddingException e) {
                throw new IncorrectPassphraseException(e);
            }
		} else {
			privateKey = Base64.decode(keyPair.getPrivateKey());
		}
		return privateKey;
	}
	
	public static byte[] getPublicKey(RSAKeyPair keyPair) throws IOException {
		return Base64.decode(keyPair.getPublicKey());
	}
	
	public static String serialize(RSAKeyPair keyPair) {
	    return keyPair.getPrivateKey() + "#" + keyPair.getPublicKey() + "#" + Boolean.toString(keyPair.isEncrypted());
	}
	
	public static RSAKeyPair deserialize(String encodedKeyPair) throws IOException {
	    String[] tokens = encodedKeyPair.split("#");
	    if (tokens.length != 3) {
	        throw new IOException("Illegal token count != 3");
	    }
	    RSAKeyPair keyPair = new RSAKeyPair(tokens[0], tokens[1], Boolean.parseBoolean(tokens[2]));
	    return keyPair;
	}
}
