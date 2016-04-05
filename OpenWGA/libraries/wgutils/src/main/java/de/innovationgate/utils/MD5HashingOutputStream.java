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

import java.io.IOException;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * A wrapper output stream that gives the written data unmodified to the wrapped stream, but builds an MD5 hash from its data while writing.
 */
public class MD5HashingOutputStream extends OutputStream {
    
    private OutputStream _stream;
    private boolean _closed = false;
    private MessageDigest _digest;

    public MD5HashingOutputStream(OutputStream stream) throws NoSuchAlgorithmException {
        _stream = stream;
        _digest = MessageDigest.getInstance("MD5");
    }

    public void close() throws IOException {
        _stream.close();
        _closed = true;
    }

    public boolean equals(Object obj) {
        return _stream.equals(obj);
    }

    public void flush() throws IOException {
        _stream.flush();
    }

    public String toString() {
        return _stream.toString();
    }

    public void write(byte[] b, int off, int len) throws IOException, NullPointerException, IndexOutOfBoundsException {
        _stream.write(b, off, len);
        _digest.update(b, off, len);
    }

    public void write(byte[] b) throws IOException, NullPointerException {
        _stream.write(b);
        _digest.update(b, 0, b.length);
    }

    public void write(int arg0) throws IOException {
        _stream.write(arg0);
        _digest.update((byte) (arg0 - 128));
    }
    
    /**
     * Returns the built MD5 hash. The stream must be closed prior to calling this.
     */
    public String getHash() {

        if (!_closed) {
            throw new IllegalStateException("Stream must be closed to retrieve the hash");
        }

        byte[] md5sum = _digest.digest();
        return Base64.encodeWeb(md5sum);

    }

}
