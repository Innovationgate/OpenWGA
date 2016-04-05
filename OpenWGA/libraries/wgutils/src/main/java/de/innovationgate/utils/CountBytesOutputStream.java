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

/**
 * An output stream that throws away the data written but only counts the bytes.
 *
 */
public class CountBytesOutputStream extends OutputStream {

    private long _size = 0;
    
    /* (non-Javadoc)
     * @see java.io.OutputStream#write(int)
     */
    public void write(int arg0) throws IOException {
        _size++;
    }

    /* (non-Javadoc)
     * @see java.io.OutputStream#write(byte[], int, int)
     */
    public void write(byte[] arg0, int arg1, int len) throws IOException {
        _size+=len;
    }

    /* (non-Javadoc)
     * @see java.io.OutputStream#write(byte[])
     */
    public void write(byte[] arg0) throws IOException {
        _size+=arg0.length;
    }

    /**
     * Returns the size of bytes written
     */
    public long getSize() {
        return _size;
    }

}
