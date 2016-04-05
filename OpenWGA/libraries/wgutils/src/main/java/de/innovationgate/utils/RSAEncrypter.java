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

import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.KeyFactory;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;

/**
 * Util class to encrypt and decrypt data based on RSA keys. As RSA encryption is too slow to encrypt large data we use the following widely accepted method:
 * <ul>
 * <li> A random DES symmetric key is generated
 * <li> data is encrypted using DESEncrypter with the random DES key
 * <li> the random DES key ist encrypted with the given RSA Public Key
 * </ul>
 * <p> 
 * Note: 
 * To be able to decrypt the data again the receiver of the encrypted data needs both the DES-ENCRYPTED DATA and the RSA-ENCRYPTED DES KEY!
 * </p>
 * @deprecated use {@link de.innovationgate.utils.security.RSAEncrypter} instead
 */
public class RSAEncrypter {

   
    private byte[] _desKeyEncryption;
    private String _data;

    /**
     * Encrypt data by the given rsa key. The DES-encrypted data can after that be retrieved by {@link #getData()}. The RSA-encrypted DES key
     * used to encrypt the data can then be retrieved from {@link #getDesKeyEncryption()}.
     * @param data The data to encrypt
     * @param rsaPublicKey The rsa public key used for encryption
     * @throws GeneralSecurityException
     * @throws UnsupportedEncodingException
     */
    public void encrypt(byte[] data, byte[] rsaPublicKey) throws GeneralSecurityException, UnsupportedEncodingException {
        // create a tempoary desEncrypter with random desKEY
        DESEncrypter desEncrypter = new DESEncrypter();
        desEncrypter.init();        
       
        X509EncodedKeySpec pubKeySpec = new X509EncodedKeySpec(rsaPublicKey);        
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        Cipher ecipher = Cipher.getInstance("RSA");        
        ecipher.init(Cipher.ENCRYPT_MODE, keyFactory.generatePublic(pubKeySpec));
        
        // encrypt desKey with rsaPubKey
        _desKeyEncryption = ecipher.doFinal(desEncrypter.getKey().getEncoded());        
        
        // encrypt data with des key
        _data = desEncrypter.encrypt(data);
    }
    
    /**
     * Decrypts the encrypted data string using the given RSA key (to decrypt the DES key) and the DES key (to decrypt the data itself).
     * @param data The data to decrypt
     * @param rsaPrivateKey The RSA key to decrypt the DES key
     * @param desKeyEncryption The RSA-encrypted DES key to decrypt the data
     * @return The decrypted data
     * @throws GeneralSecurityException
     */
    public byte[] decrypt(String data, byte[] rsaPrivateKey, byte[] desKeyEncryption) throws GeneralSecurityException {
        // decrypt desKey with rsaPrivateKey
        PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(rsaPrivateKey);           
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        Key key = keyFactory.generatePrivate(keySpec);        
        Cipher dcipher = Cipher.getInstance("RSA");        
        dcipher.init(Cipher.DECRYPT_MODE, key);        
        byte[] desKeyData = dcipher.doFinal(desKeyEncryption);
        DESKeySpec desKeySpec = new DESKeySpec(desKeyData);
        SecretKeyFactory desKeyFactory = SecretKeyFactory.getInstance("DES");
        SecretKey desKey = desKeyFactory.generateSecret(desKeySpec);

        // create desEncrypter with decrypted desKey
        DESEncrypter desEncrypter = new DESEncrypter();
        desEncrypter.init(desKey);
        
        // decrypt data
        return desEncrypter.decrypt(data);
    }

    /**
     * Returns the DES-encrypted data after calling {@link #encrypt(byte[], byte[])}
     */
    public String getData() {
        return _data;
    }

    /**
     * Returns the RSA-encrypted DES key after calling {@link #encrypt(byte[], byte[])}
     */
    public byte[] getDesKeyEncryption() {
        return _desKeyEncryption;
    }

}
