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

package de.innovationgate.webgate.api;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.activation.DataSource;

import org.apache.commons.io.IOUtils;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.wga.common.ImmutableObject;

/**
 * Represents binary data on a field. Is returned by {@link WGDocument#getExtensionData(String)} when the data on the extension data field is binary.
 */
public class BinaryFieldData implements ImmutableObject {
    
    public class SizeReportingInputStream extends InputStream {

        private long _bytes = 0;
        private InputStream _in;
        
        @Override
        public int hashCode() {
            return _in.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return _in.equals(obj);
        }

        @Override
        public long skip(long n) throws IOException {
            return _in.skip(n);
        }

        @Override
        public String toString() {
            return _in.toString();
        }

        @Override
        public int available() throws IOException {
            return _in.available();
        }

        @Override
        public void close() throws IOException {
            _in.close();
        }

        @Override
        public void mark(int readlimit) {
            _in.mark(readlimit);
        }

        @Override
        public void reset() throws IOException {
            _in.reset();
        }

        @Override
        public boolean markSupported() {
            return _in.markSupported();
        }

        public SizeReportingInputStream(InputStream in) {
            _in = in;
        }

        @Override
        public int read() throws IOException {
            int value = _in.read();
            if (value == -1) {
                BinaryFieldData.this._size = _bytes;
            }
            else {
                _bytes++;
            }
            return value;
        }

    }

    /**
     * Returns if a field value is "binary", i.e. can be used as input for this object
     */
    public static boolean isBinaryValue(Object value) {
        return (value instanceof byte[] || value instanceof InputStream || value instanceof DataSource || value instanceof File || value instanceof BinaryFieldData);
    }
    
    /**
     * An interface for the functionality that actually retrieves the input stream of binary field data
     */
    public interface Retriever {
        public InputStream retrieveInputStream() throws IOException, WGAPIException; 
    }
    
    private long _size;
    private Retriever _retriever;

    /**
     * Constructor for binary field datas from database. Only for internal WGAPI use
     * @param size
     * @param retriever
     */
    public BinaryFieldData(long size, Retriever retriever) {
        _size = size;
        _retriever = retriever;
    }
    
    /**
     * Constructor to create an new binary field.
     * @param binaryValue The binary value. Either an {@link InputStream}, byte array, {@link File}, {@link DataSource} or another {@link BinaryFieldData} object
     * @throws IOException 
     */
    public BinaryFieldData(Object binaryValue) throws IOException {
        
        _size = -1;
        
        InputStream in = getBinaryDataInputStream(binaryValue);
        final TemporaryFile tempFile = new TemporaryFile("binextdata.bin", in, WGFactory.getTempDir());
        tempFile.deleteOnEviction(this);
        _retriever = new Retriever() {
            @Override
            public InputStream retrieveInputStream() throws IOException {
                return new BufferedInputStream(new FileInputStream(tempFile.getFile()), 4092);
            }
        };
    }

    /**
     * Returns the size of the binary data in bytes. May be -1 when not yet saved.
     */
    public long getSize() {
        return _size;
    }
    
    /**
     * Returns the binary data as input stream
     * @throws IOException
     */
    public InputStream getInputStream() throws IOException {
        
        try {
            if (_size == -1) {
                return new SizeReportingInputStream(_retriever.retrieveInputStream());
            }
            else {
                return _retriever.retrieveInputStream();
            }
        }
        catch (WGAPIException e) {
            throw new IOException(e);
        }
        
    }
    
    /**
     * Returns the binary data as String
     * @param encoding (optional)
     * @return The text representation as a string.
     * @throws IOException
     */
    public String asString(String encoding) throws IOException {
    	return IOUtils.toString(getInputStream(), encoding);
    }
    public String asString() throws IOException {
    	return IOUtils.toString(getInputStream());
    }
    
    /**
     * Returns an input stream for all binary data object types that this object accepts as input
     * @param value The binary value. Accepts the same types as {@link BinaryFieldData#BinaryFieldData(Object)} constructor.
     * @throws IOException
     */
    public static InputStream getBinaryDataInputStream(Object value) throws IOException {

        if (value instanceof InputStream) {
            return (InputStream) value;
        }
        else if (value instanceof BinaryFieldData) {
            return ((BinaryFieldData) value).getInputStream();
        }
        else if (value instanceof byte[]) {
            return new ByteArrayInputStream((byte[]) value);
        }
        else if (value instanceof DataSource) {
            return ((DataSource) value).getInputStream();
        }
        else if (value instanceof File) {
            return new BufferedInputStream(new FileInputStream((File) value), 4092);
        }
        else if (value == null) {
            throw new IOException("null binary data value");
        }
        else {
            throw new IOException("Invalid type of binary data: " + value.getClass().getName());
        }

    }

}
