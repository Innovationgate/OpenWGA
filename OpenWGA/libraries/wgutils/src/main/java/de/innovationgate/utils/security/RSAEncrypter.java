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
package de.innovationgate.utils.security;

import java.io.IOException;
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

import org.omg.CORBA._PolicyStub;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.DESEncrypter;
import de.innovationgate.utils.DESEncrypter.PersistentKeyException;
import de.innovationgate.utils.security.IncorrectPassphraseException;
import de.innovationgate.utils.security.RSAKeyPair;
import de.innovationgate.utils.security.RSAKeyPairUtils;

/**
 * Util class to encrypt and decrypt data based on RSA keys. As RSA encryption is too slow and possible data length depends on the RSA key length
 * we use the following widely accepted method:
 * <ul>
 * <li> A random DES symmetric key is generated
 * <li> data is encrypted using DESEncrypter with the random DES key
 * <li> the random DES key is encrypted with the given RSA Public Key
 * </ul>
 * <p> 
 * Note: 
 * To be able to decrypt the data again the receiver of the encrypted data needs both the DES-ENCRYPTED DATA and the RSA-ENCRYPTED DES KEY!
 * </p>
 *
 */
public class RSAEncrypter {

    /**
     * Encrypted data created by RSAEncrypter
     */
    public static class RSAEncryptedData {
        private static final String PREFIX = "$RSA_ENCRYPTED$";
        
        // base64 encoded, rsa encrypted desKey
        private String _desKey;
        
        // base64 encoded, des encrypted data
        private String _data;
        
        public RSAEncryptedData(String desKey, String data) {
            _desKey = desKey;
            _data = data;
        }

        /**
         * Returns the DES encrypted data
         */
        public String getData() {
            return _data;
        }

        /**
         * Sets the DES encrypted data
         */
        public void setData(String data) {
            _data = data;
        }

        /**
         * Sets the RSA encrypted DES key
         * @param desKey
         */
        public void setDESKey(String desKey) {
            _desKey = desKey;
        }

        /**
         * Returns the RSA encrypted DES key
         */
        public String getDESKey() {
            return _desKey;
        }        
        
        /**
         * Serializes the encrypted data into string form, including the given RSA encrypted DES key
         */
        public String serialize() {
            return PREFIX + "#" +getDESKey() + "#" + getData();
        }
        
        /**
         * Deserializes data serialized by {@link #serialize()}
         * @param input
         */
        public static RSAEncryptedData deserialize(String input) {
            if (input == null) {
                return null;
            }
            String[] tokens = ((String)input).split("#");
            if (tokens.length != 3) {
                throw new IllegalArgumentException("Incorrect token count != 3 for RSA data.");
            }
            
            return new RSAEncryptedData(tokens[1], tokens[2]);
        }
        
        /**
         * Determines if the given data is encrypted data by RSAEncrypter, so it can be deserialzed
         * @param input
         */
        public static boolean isEncryptedValue(Object input) {
            if (input != null && input instanceof String) {
                return ((String)input).startsWith(PREFIX);
            }
            return false;
        }
        
    }
    
    private RSAKeyPair _keyPair;
    private String _passphrase;
    
    public RSAEncrypter(RSAKeyPair keyPair, String passphrase) {
        _keyPair = keyPair;
        _passphrase = passphrase;
    }

    /**
     * Encrypt data by the given rsaKeyPair.
     * @param data The data to encrypt
     * @return RSAEncryptedData
     * @throws GeneralSecurityException
     * @throws IOException 
     */
    public RSAEncryptedData encrypt(byte[] data) throws GeneralSecurityException, IOException {
        // create a tempoary desEncrypter with random desKEY
        DESEncrypter desEncrypter = new DESEncrypter();
        desEncrypter.init();        
       
        X509EncodedKeySpec pubKeySpec = new X509EncodedKeySpec(RSAKeyPairUtils.getPublicKey(_keyPair));        
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        Cipher ecipher = Cipher.getInstance("RSA");        
        ecipher.init(Cipher.ENCRYPT_MODE, keyFactory.generatePublic(pubKeySpec));
        
        // encrypt desKey with rsaPubKey
        byte[] desKey = ecipher.doFinal(desEncrypter.getKey().getEncoded());        
        
        // encrypt data with des key
        String desData = desEncrypter.encrypt(data);
        
        return new RSAEncryptedData(Base64.encode(desKey), desData);
    }
    
    /**
     * Decrypts the given RSAEncryptedData 
     * @param input
     * @return decrypted data
     * @throws GeneralSecurityException
     * @throws IOException
     * @throws IncorrectPassphraseException 
     * @throws PersistentKeyException 
     */
    public byte[] decrypt(RSAEncryptedData input) throws GeneralSecurityException, IOException, PersistentKeyException, IncorrectPassphraseException {
     // decrypt desKey with rsaPrivateKey
        PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(RSAKeyPairUtils.getPrivateKey(_keyPair, _passphrase));           
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        Key key = keyFactory.generatePrivate(keySpec);        
        Cipher dcipher = Cipher.getInstance("RSA");        
        dcipher.init(Cipher.DECRYPT_MODE, key);        
        byte[] desKeyData = dcipher.doFinal(Base64.decode(input.getDESKey()));
        DESKeySpec desKeySpec = new DESKeySpec(desKeyData);
        SecretKeyFactory desKeyFactory = SecretKeyFactory.getInstance("DES");
        SecretKey desKey = desKeyFactory.generateSecret(desKeySpec);

        // create desEncrypter with decrypted desKey
        DESEncrypter desEncrypter = new DESEncrypter();
        desEncrypter.init(desKey);
        
        // decrypt data
        return desEncrypter.decrypt(input.getData());
    }
    
    
    
    /**
     * Decrypts the given item value if it is RSA encryped. Otherwise it is given back in unmodified form
     * @param value The item value
     * @return Decrypted item value
     * @throws IOException
     * @throws GeneralSecurityException
     * @throws PersistentKeyException
     * @throws IncorrectPassphraseException
     */
    public Object decryptItemValue(Object value) throws IOException, GeneralSecurityException, PersistentKeyException, IncorrectPassphraseException {
        if (RSAEncryptedData.isEncryptedValue(value)) {
            RSAEncryptedData data = RSAEncryptedData.deserialize((String)value);

            byte[] decryptedData = decrypt(data);            
            String serialized = new String(decryptedData, "UTF-8");   
            return new XStream(new Dom4JDriver()).fromXML(serialized);
        }
        return value;
    }

    /**
     * Encrypts the given item value to a string, which again can be stored as item value
     * @param value The item value to encrypt
     * @return The encrypted item value
     * @throws IOException
     * @throws GeneralSecurityException
     */
    public String encryptItemValue(Object value) throws IOException, GeneralSecurityException {
        if (value != null) {
            XStream xstream = new XStream(new Dom4JDriver());
            String serialized = xstream.toXML(value);            
            RSAEncryptedData rsaEncryptedData = encrypt(serialized.getBytes("UTF-8"));
            return rsaEncryptedData.serialize();
        } else {
            return null;
        }
    }
}
