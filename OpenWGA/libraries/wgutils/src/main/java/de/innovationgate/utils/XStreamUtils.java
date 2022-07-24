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
/**
 * Provides some utility functions for frequently used operations with XStream
 *
 */
package de.innovationgate.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.zip.ZipInputStream;

import org.apache.commons.vfs2.FileObject;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;
import com.thoughtworks.xstream.security.AnyTypePermission;

/**
 * Some utilities for using the XStream serialization engine, mainly to read/write 
 * from/to UTF-8 encoded sources in a single statement
 */
public abstract class XStreamUtils {
    
    public static final XStream XSTREAM_CLONING = createXStream(); 

    /**
     * Serializes an object to a UTF-8 encoded file
     * @param obj The object to write
     * @param xstream The XStream instance to use
     * @param file The file to write
     * @throws IOException
     */
    public static void writeUtf8ToFile(Object obj, XStream xstream, File file) throws IOException {
        FileOutputStream out = new FileOutputStream(file);
        writeUtf8ToOutputStream(obj, xstream, out);
    }

    /**
     * Serializes an object to a UTF-8 encoded VFS file object
     * @param obj The object to write
     * @param xstream The XStream instance to use
     * @param file The file to write
     * @throws IOException
     */
    public static void writeUtf8ToFileObject(Object obj, XStream xstream, FileObject file) throws IOException {
        OutputStream out = file.getContent().getOutputStream();
        writeUtf8ToOutputStream(obj, xstream, out);
    }

    /**
     * Serializes an object to a UTF-8 encoded output stream
     * @param obj The object to write
     * @param xstream The XStream instance to use
     * @param out The output stream to write to
     * @throws IOException
     */
    public static void writeUtf8ToOutputStream(Object obj, XStream xstream, OutputStream out) throws IOException {
    	xstream.addPermission(AnyTypePermission.ANY);
        BufferedOutputStream bufOut = new BufferedOutputStream(out);
        OutputStreamWriter writer;
        try {
            writer = new OutputStreamWriter(bufOut, "UTF-8");
            xstream.toXML(obj, writer);
            writer.flush();
            writer.close();
        }
        catch (UnsupportedEncodingException e) {
           // Cannot happen since Java always supports UTF-8
        }
    }
    
    /**
     * Loads an object from a UTF-8 encoded input stream
     * @param xstream The XStream instance to use
     * @param in The input stream to read from
     * @return The loaded object
     * @throws IOException
     */  
    public static Object loadUtf8FromInputStream(XStream xstream, InputStream in) throws IOException {
    	return loadUtf8FromInputStream(xstream, in, false);
    }
    
    /**
     * Loads an object from a UTF-8 encoded input stream
     * @param xstream The XStream instance to use
     * @param in The input stream to read from
     * @param forceClose force close on the input stream
     * @return The loaded object
     * @throws IOException
     */    
    public static Object loadUtf8FromInputStream(XStream xstream, InputStream in, boolean forceClose) throws IOException {
    	xstream.addPermission(AnyTypePermission.ANY);
        BufferedInputStream bufIn = new BufferedInputStream(in);
        try {
            InputStreamReader reader = new InputStreamReader(bufIn, "UTF-8");
            Object obj = xstream.fromXML(reader);
            if (!(in instanceof ZipInputStream) || forceClose) {
                reader.close();
            }
            return obj;
        }
        catch (UnsupportedEncodingException e) {
            // Cannot happen since Java always supports UTF-8
            return null;
        }
    }
    
    /**
     * Loads an object from a UTF-8 encoded file
     * @param xstream The XStream instance to use
     * @param file The file to read from
     * @return The loaded object
     * @throws IOException
     */
    public static Object loadUtf8FromFile(XStream xstream, File file) throws IOException {
        FileInputStream in = new FileInputStream(file);
        return loadUtf8FromInputStream(xstream, in);
    }
    
    /**
     * Loads an object from a UTF-8 encoded VFS file object
     * @param xstream The XStream instance to use
     * @param file The file to read from
     * @return The loaded object
     * @throws IOException
     */
    public static Object loadUtf8FromFileObject(XStream xstream, FileObject file) throws IOException {
        InputStream in = file.getContent().getInputStream();
        return loadUtf8FromInputStream(xstream, in, true);
    }
    
    /**
     * Tries to clone an object by serializing and de-serializing it via XStream
     * In case of any error returns null to indicate an unclonable object.
     * @param obj The object to clone
     * @return The clone or null if cloning failed.
     */
    public static Object clone(Object obj) {
        
        try {
        	XSTREAM_CLONING.addPermission(AnyTypePermission.ANY);
            String xml = XSTREAM_CLONING.toXML(obj);
            return XSTREAM_CLONING.fromXML(xml);
        }
        catch (RuntimeException e) {
            return null;
        }
        
    }
    
    public static XStream createXStream(){
    	XStream xstream = new XStream(new Dom4JDriver());
    	xstream.addPermission(AnyTypePermission.ANY);
    	return xstream;
    }
    
}
