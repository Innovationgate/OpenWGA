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
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPOutputStream;

/**
 * Tool class to zip a string to zip-compressed bytes
 * @deprecated Because it uses the string bytes in platform encoding, which may not work with non-ASCII characters 
 */
public class Zipper {


    private static class PatchedGZIPInputStream extends java.util.zip.GZIPInputStream {

        public PatchedGZIPInputStream (InputStream in, int size)
            throws IOException {
          super (in, size);
        }

        public PatchedGZIPInputStream (InputStream in)
            throws IOException {
          super (in);
        }

        public int read (byte buf[], int off, int len) throws IOException {
          try {
            return super.read (buf, off, len);
          }
          catch (IOException e) {
            if (e.getMessage().indexOf("Corrupt GZIP trailer") != -1) {
              return -1;
            }
            else {
              throw e;
            }
          }
        }
      }
       
    
    public static byte[] zip(String input) {       
        try {                   
            ByteArrayOutputStream baos  = new ByteArrayOutputStream();            
            GZIPOutputStream zos        = new GZIPOutputStream(baos);
     
            byte [] uncompressedBytes   = input.getBytes();
     
            zos.write(uncompressedBytes, 0, uncompressedBytes.length);
            zos.close();
     
            return baos.toByteArray();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }      
    }
    
    public static String unzip(byte[] input) {        
        try {
            String unzipped = null;
            ByteArrayInputStream byteIn = new ByteArrayInputStream(input);
            PatchedGZIPInputStream zipIn = new PatchedGZIPInputStream(byteIn);

            ByteArrayOutputStream out = new ByteArrayOutputStream();
                        
            int numBytesRead = 0;
            byte[] tempBytes = new byte[1024];
            while ((numBytesRead = zipIn.read(tempBytes, 0, tempBytes.length)) != -1) {
                out.write(tempBytes, 0, numBytesRead);
            }

            unzipped = new String(out.toByteArray());
            out.close();
            zipIn.close();
            byteIn.close();
                                    
            return unzipped;
        }
        catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }           
}
