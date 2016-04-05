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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.apache.commons.codec.binary.Hex;

import de.innovationgate.utils.DirZipper.InputStreamProvider;

/**
 * A wrapper input stream that returns the data of the wrapped stream unmodified, but builds an MD5 hash from its data while reading.
 */
public class MD5HashingInputStream extends InputStream {
    
    /**
     * Reads the given input stream and returns the data hash base64 encoded. The input data is not conserved.
     * Use this method if you really just want to know what hash the given data produces, but do not want to use the data actually.
     * The input stream is closed implicitly.
     * @param in The input stream
     * @return The md5 hash of the input stream
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public static String getStreamHash(InputStream in) throws NoSuchAlgorithmException, IOException {
        
        MD5HashingInputStream hashIn = new MD5HashingInputStream(new BufferedInputStream(in));
        byte[] buf = new byte[2048];
        int len;
        do {
            len = hashIn.read(buf);
        }
        while (len != -1);
        
        hashIn.close();
        return hashIn.getHash();
        
        
        
    }

    /**
     * Reads the given input stream and returns the data hash bytes. The input data is not conserved.
     * Use this method if you really just want to know what hash the given data produces, but do not want to use the data actually.
     * The input stream is closed implicitly.
     * @param in The input stream
     * @return The md5 hash bytes
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public static byte[] getStreamHashBytes(InputStream in) throws NoSuchAlgorithmException, IOException {
        
        MD5HashingInputStream hashIn = new MD5HashingInputStream(new BufferedInputStream(in));
        byte[] buf = new byte[2048];
        int len;
        do {
            len = hashIn.read(buf);
        }
        while (len != -1);
        
        hashIn.close();
        return hashIn.getHashBytes();
        
        
        
    }
    
    /**
     * Reads the given input stream and returns the data hash bytes in hexadecimal representation. The input data is not conserved.
     * Use this method if you really just want to know what hash the given data produces, but do not want to use the data actually.
     * The input stream is closed implicitly.
     * @param in The input stream
     * @return The md5 hash bytes hex string
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public static String getStreamHashHex(InputStream in) throws NoSuchAlgorithmException, IOException {
        return new String(Hex.encodeHex(getStreamHashBytes(in)));
    }

    private InputStream _stream;

    private boolean _closed = false;

    private MessageDigest _digest;
    
    private byte[] _checksum = null;

    public int available() throws IOException {
        return _stream.available();
    }

    public void close() throws IOException {
        _stream.close();
        _closed = true;
    }

    public void mark(int readlimit) {
        throw new IllegalStateException("Marking not supported");
    }

    public boolean markSupported() {
        return false;
    }

    public int read() throws IOException {
        int theByte = _stream.read();
        if (theByte != -1) {
            _digest.update((byte) (theByte - 128));
        }
        return theByte;
    }

    public int read(byte[] b, int off, int len) throws IOException {
        int length = _stream.read(b, off, len);
        if (length != -1) {
            _digest.update(b, off, length);
        }
        return length;
    } 

    public int read(byte[] b) throws IOException {
        int length = _stream.read(b);
        if (length != -1) {
            _digest.update(b, 0, length);
        }
        return length;
    }

    public void reset() throws IOException {
        throw new IllegalStateException("Resetting not supported");
    }

    public long skip(long n) throws IOException {
        throw new IllegalStateException("Skipping not supported");
    }

    public String toString() {
        return _stream.toString();
    }

    /**
     * Constructor
     * @param stream The stream to wrap
     * @throws NoSuchAlgorithmException
     */
    public MD5HashingInputStream(InputStream stream) throws NoSuchAlgorithmException {
        _stream = stream;
        _digest = MessageDigest.getInstance("MD5");
    }

    /**
     * Returns the built MD5 hash (Base64 encoded). The stream must be closed prior to calling this.
     */
    public String getHash() {

        if (!_closed) {
            throw new IllegalStateException("Stream must be closed to retrieve the hash");
        }

        readChecksum();
        return Base64.encodeWeb(_checksum);

    }
    
    /**
     * Returns the built MD5 hash bytes. The stream must be closed prior to calling this.
     */
    public byte[] getHashBytes() {

        if (!_closed) {
            throw new IllegalStateException("Stream must be closed to retrieve the hash");
        }

        readChecksum();
        return _checksum;

    }

    private void readChecksum() {
        if (_checksum == null) {
            _checksum = _digest.digest();
        }
    }

}
