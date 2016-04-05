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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * Tool to zip the contents of a directory to a ZIP file, using the java ZIP functionalities
 */
public class DirZipper {
    
    /**
     * A provider of input streams for individual files in the directory to zip up.
     * May be used to add functionality to the stream beyond plain file input streams.
     */
    public interface InputStreamProvider {
        /**
         * Provides an input stream for the data of an individual file
         * @param file The file
         * @return The input stream returning the data of the file
         * @throws FileNotFoundException
         */
        public InputStream provideInputStream(File file) throws FileNotFoundException;
    }
    
    /**
     * Default implementation of InputStreamProvider that just returns
     * plain FileInputStream-Objects for files
     */
    public class DefaultInputStreamProvider implements InputStreamProvider {

        public InputStream provideInputStream(File file) throws FileNotFoundException {
            return new FileInputStream(file);
        }
        
    }
    
    private InputStreamProvider _inputStreamProvider = new DefaultInputStreamProvider();
    private PatternListVerifier _filenamesToIgnore = new PatternListVerifier();

    /**
     * Zips a directory to a zip output stream.
     * @param zipDir The directory whose contents will be in the zip file
     * @param zos The zip output stream adressing the correct target for zipped data
     * @throws IOException
     */
    public void zipDirectory(File zipDir, ZipOutputStream zos) throws IOException {
        zipDirectory(zipDir, zos, "");
    }
    
    /**
     * Zips a file or directory to the output stream
     * @param file The file to zip. In case of a directory all the contents will be zipped
     * @param zos The ZIP output stream
     * @throws IOException
     */
    public void zipFile(File file, ZipOutputStream zos) throws IOException {
        if (file.isDirectory()) {
            zipDirectory(file, zos);
        }
        else {
            zipNormalFile(file, zos, "");
        }
    }
    
    /**
     * Unzips the content of the given input stream to a directory
     * @param targetDir The directory where to unzip the data to
     * @param zis The input stream
     * @throws IOException if something goes horribly wrong
     */
    public void unzipDirectory(File targetDir, ZipInputStream zis) throws IOException {
        
        ZipEntry entry;
        while ((entry = zis.getNextEntry()) != null) {
            File targetFile = new File(targetDir, entry.getName());
            if (_filenamesToIgnore.verify(targetFile.getName()) != null) {
                continue;
            }
            targetFile.getParentFile().mkdirs();
            if (!entry.isDirectory()) {
                targetFile.createNewFile();
                FileOutputStream out = new FileOutputStream(targetFile);
                WGUtils.inToOut(zis, out, 2048);
                out.close();
            } else {
                targetFile.mkdir();
            }
        }
        
    }

    /**
     * Zips the contents of a local directory to a ZIP archive
     * 
     * @param zipDir
     *            The directory to zip, including all contained files
     * @param zos
     *            The zip output stream
     * @param qualifier Some qualifier identifying the directory inside the zip to which the file should be put.  Specify an empty string to make it a root file. Specify some dir path ending with "/" to put it to a directory.
     * @throws IOException 
     */
    public void zipDirectory(File zipDir, ZipOutputStream zos, String qualifier) throws IOException {

        if (_filenamesToIgnore.verify(zipDir.getName()) != null) {
            return;
        }
        
        String[] dirList = zipDir.list();
    
        for (int i = 0; i < dirList.length; i++) {
            File f = new File(zipDir, dirList[i]);
            if (f.isDirectory()) {
                zipDirectory(f, zos, qualifier + f.getName() + "/");
            }
            else {
                zipNormalFile(f, zos, qualifier);
            }
        }
    }

    /**
     * Zips a single normal file (no directory!) to the zip output stream.
     * @param f The file
     * @param zos The zip output stream
     * @param qualifier Some qualifier identifying the directory inside the zip to which the file should be put. Specify an empty string to make it a root file. Specify some dir path ending with "/" to put it to a directory.
     * @throws FileNotFoundException
     * @throws IOException
     */
    public void zipNormalFile(File f, ZipOutputStream zos, String qualifier) throws FileNotFoundException, IOException {
        
        if (_filenamesToIgnore.verify(f.getName()) != null) {
            return;
        }
        
        byte[] readBuffer = new byte[2156];
        int bytesIn = 0;
        InputStream fis = _inputStreamProvider.provideInputStream(f);
        ZipEntry anEntry = new ZipEntry(qualifier + f.getName());
        zos.putNextEntry(anEntry);
   
        while ((bytesIn = fis.read(readBuffer)) != -1) {
            zos.write(readBuffer, 0, bytesIn);
        }
   
        fis.close();
    }

    /**
     * Returns the input stream provider that the DirZipper will use to fetch
     * input streams for indidivual files
     */
    public InputStreamProvider getInputStreamProvider() {
        return _inputStreamProvider;
    }

    /**
     * Sets the input stream provider that the DirZipper will use to fetch
     * input streams for indidivual files
     * @param inputStreamProvider The provider to use
     */
    public void setInputStreamProvider(InputStreamProvider inputStreamProvider) {
        _inputStreamProvider = inputStreamProvider;
    }

    /**
     * Adds a file name pattern as regular expression that should be ignored when zipping the directory.
     * Files/Directories of that name will be omitted
     * @param name The pattern to ignore
     */
    public void addFilePatternToIgnore(String name) {
        _filenamesToIgnore.addPattern(name);
    }
    
    
}
