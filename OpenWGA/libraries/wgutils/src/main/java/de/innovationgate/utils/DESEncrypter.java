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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;


/**
 * simple DES encrypter used to encrypt tml:form hidden fields
 * <p>
 * Usage:<br>
 * <ul>
 * <li>Create DESEncrypter
 * <li>Initialize DESEncrypter with one init...() method
 * <li>Use it to encrypt/decrypt
 * </ul> 
 * 
 * @deprecated Use {@link de.innovationgate.utils.security.SymmetricEncryptionEngine} instead
 */
public class DESEncrypter {
    
    private static final String OBFUSCATION_KEY = "JQSoj3B19xU=";
    
    private static final String ALGORITHM_OBFUSCATION = "DES";
    private static final String DEFAULT_ALGORITHM = "DES";
    
    private String _algorithm = null;
    
    public static final String SYSPROP_DESENCRYPTER_ALGORITHM = "de.innovationgate.wga.desencrypter.algorithm";
    
    /**
     * thrown when a key cannot be written or restored from filesystem
     *
     */
    public static class PersistentKeyException extends Exception {
        private static final long serialVersionUID = -3728142578426301492L;
        
        public PersistentKeyException(String msg) {
            super(msg);
        }
        
        public PersistentKeyException(String msg, Throwable cause) {
            super(msg, cause);
        }
    }
    
    Cipher ecipher;
    Cipher dcipher;
    
    boolean _initialized = false;
	private Key _key;
    
    /**
     * constructs a DESEncrypter 
     * init or init(File) must be called before use
     */
    public DESEncrypter() {
    	_algorithm = System.getProperty(SYSPROP_DESENCRYPTER_ALGORITHM, DEFAULT_ALGORITHM);
    	
    }
    
    public String getAlgorithm() {
        return _algorithm;
    }

    /**
     * Initializes the DESEncrypter with a temporarily generated key
     * @throws GeneralSecurityException
     */
    public void init() throws GeneralSecurityException {
        // generate temp key
        SecretKey key = KeyGenerator.getInstance(_algorithm).generateKey(); 
        init(key);       
    }
    
    /**
     * Initializes the DESEncrypter reading a encryption key from an input stream.
     * This variant reads keys stored in a platform-independentm manner 
     * (like with {@link #writeCodeFile(File)}) 
     * @param in The input stream to read from
     * @throws PersistentKeyException
     * @throws GeneralSecurityException
     * @throws IOException
     */
    public void init(InputStream in) throws PersistentKeyException, GeneralSecurityException, IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        WGUtils.inToOut(in, out, 2048);
        SecretKeySpec key = new SecretKeySpec(out.toByteArray(), _algorithm);
        init(key);
    }
    
    /**
     * Initializes the DESEncrypter using the MD5 hash of a custom string key as encryption key. Uses AES 128 bit encryption.
     * @param keyStr The key string
     * @throws UnsupportedEncodingException 
     * @throws GeneralSecurityException 
     */
    public void init(String keyStr) throws UnsupportedEncodingException, GeneralSecurityException {
        
        _algorithm = "AES";
        MessageDigest digest = MessageDigest.getInstance("MD5");
        digest.update(keyStr.getBytes("UTF-8"));
        byte[] md5sum = digest.digest();
        SecretKeySpec key = new SecretKeySpec(md5sum, _algorithm);
        init(key);
        
    }
    

    
    /**
     * Initializes the DESEncrypter using an obfuscation key
     * @throws PersistentKeyException
     * @throws GeneralSecurityException
     * @throws IOException
     */
    public void initObfuscation() throws PersistentKeyException, GeneralSecurityException, IOException {
        _algorithm = ALGORITHM_OBFUSCATION;
        InputStream in = new ByteArrayInputStream(Base64.decode(OBFUSCATION_KEY));
        init(in);
    }
    
    /**
     * Initializes the DESEncrypter reading a encryption key from a file.
     * If the keyfile does not exist a new key is generated and written to the file.
     * Key files written with this method may not be usable across different VM encryption implementations
     * since they rely on object serialisation.
     * @param keyFile The key file
     * @throws GeneralSecurityException
     * @throws PersistentKeyException
     */
    public void init(File keyFile) throws GeneralSecurityException, PersistentKeyException {
        SecretKey key = null;
        if (keyFile.exists()) {
            ObjectInputStream desKeyIn = null;
            try {
                FileInputStream fileInputStream = new FileInputStream(keyFile);
                // read key from file                   
                desKeyIn = new ObjectInputStream(fileInputStream);
                key = (SecretKey) desKeyIn.readObject();                    
            } catch (Exception e) {
                throw new PersistentKeyException("Unable to restore key from file.", e);
            }
            finally {
                if (desKeyIn != null) {
                    try {
                        desKeyIn.close();
                    } catch (IOException e) {                            
                    }
                }
            }
        }        
        if (key == null) {
            // create new key and write to file
            key = KeyGenerator.getInstance(_algorithm).generateKey();                
            ObjectOutputStream desKeyOut = null;
            try {
                desKeyOut = new ObjectOutputStream(new FileOutputStream(keyFile));
                desKeyOut.writeObject(key);
            } catch (Exception e) {
                throw new PersistentKeyException("Unable to write key to file.", e);
            } finally {
                if (desKeyOut != null) {
                    try {
                        desKeyOut.close();        
                    } catch (IOException e) {                            
                    }
                }
            }               
        }    
        init(key);
    }
    

    
    /**
     * Initializes the DESEncrypter using the given key instance
     * @param key
     * @throws GeneralSecurityException
     */
    public void init(Key key) throws GeneralSecurityException {
            ecipher = Cipher.getInstance(_algorithm);
            dcipher = Cipher.getInstance(_algorithm);
            ecipher.init(Cipher.ENCRYPT_MODE, key);
            dcipher.init(Cipher.DECRYPT_MODE, key);  
            _initialized = true;
            _key = key;
    }

    
    /**
     * Encrypts the given bytes to a string
     * @param input The bytes to encrypt
     * @return The encrypted string
     * @throws UnsupportedEncodingException
     */
    public String encrypt(byte input[]) throws UnsupportedEncodingException {
        if (!_initialized) {
            throw new IllegalStateException("This instance of DESEncrypter is uninitilized. Please call init() or init(<file>)) first.");
        }
        try {
            // Encrypt
            byte[] enc = ecipher.doFinal(input);

            // Encode bytes to base64 to get a string
            return Base64.encodeWeb(enc);
        } catch (javax.crypto.BadPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        }
        return null;
        
    }     

    /**
     * Decrypts the given string to original bytes
     * @param str The encrypted string
     * @return The decrypted bytes or null if it could not be decrypted
     */
    public byte[] decrypt(String str) {
        if (!_initialized) {
            throw new IllegalStateException("This instance of DESEncrypter is uninitialized. Please call init() or init(<file>)) first.");
        }
        try {
            // Decode base64 to get bytes
            byte[] dec = Base64.decodeWeb(str);

            // Decrypt
            return dcipher.doFinal(dec);            
        } catch (javax.crypto.BadPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
        return null;
    }
    
    /**
     * Decrypts a string encrypted with {@link #encryptString(String)}
     * @param str
     * @return Decrypted string
     * @throws UnsupportedEncodingException
     */
    public String decryptString(String str) throws UnsupportedEncodingException {
        byte[] decrypted = decrypt(str);
        if (decrypted != null) {
            return new String(decrypted, "UTF-8");
        }
        else {
            return null;
        }
    }
    
    /**
     * Encrypts a string based on its UTF-8 bytes values
     * @param str
     * @return The encrypted string
     * @throws UnsupportedEncodingException
     */
    public String encryptString(String str) throws UnsupportedEncodingException {
        return encrypt(str.getBytes("UTF-8"));
    }

	/**
     * Returns the key instance used for encryption/decryption
	 */
	public Key getKey() {
		return _key;
	}

    /**
     * Returns the decryption cipher
     */
    public Cipher getDcipher() {
        return dcipher;
    }

    /**
     * Returns the encryption cipher
     */
    public Cipher getEcipher() {
        return ecipher;
    }
    
    /**
     * Stores the encryption key to a file in a platform-independent manner.
     * @param codeFile The key file to write
     * @throws IOException
     */
    public void writeCodeFile(File codeFile) throws IOException {
        if (!codeFile.exists()) {
            codeFile.createNewFile();
        }
        
        FileOutputStream out = new FileOutputStream(codeFile);
        out.write(_key.getEncoded());
        out.close();
        
    }
    
  
}
