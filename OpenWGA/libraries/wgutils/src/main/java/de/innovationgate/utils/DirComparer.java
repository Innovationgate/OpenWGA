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

package de.innovationgate.utils;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.vfs2.FileFilter;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSelector;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.Selectors;

/**
 * A utility to compare the contents of two directories or files, or to create a hashCode representing the state of a directory/file.
 * Files are compared to have the same name and contents.
 * Directories are compared by doing the file comparison described above on any contained file.
 * Things not considered in comparison:
 * <ul>
 * <li>Metadata of files other than the file name (like created/modified dates, files system permissions)
 * </ul>
 */
public class DirComparer {
    
    /**
     * A difference between compared files
     */
    public static class Diff implements Comparable<Diff> {
        
        private String _path;
        public Diff(String path, String file1Hash, String file2Hash) {
            super();
            _path = path;
            _file1Hash = file1Hash;
            _file2Hash = file2Hash;
        }
        private String _file1Hash = null;
        private String _file2Hash = null;
        
        /**
         * Hash of file 1
         */
        public String getFile1Hash() {
            return _file1Hash;
        }
        /**
         * Hash of file 2
         */
        public String getFile2Hash() {
            return _file2Hash;
        }
        
        @Override
        public String toString() {

            return _path + ": " + displayHash(_file1Hash) + (WGUtils.nullSafeEquals(_file1Hash, _file2Hash) ? " == " : " != ") + displayHash(_file2Hash);
            
        }
        private String displayHash(String hash) {
            
            if (hash == null) {
                return "nonexistent";
            }
            else if (hash.equals("")) {
                return "directory";
            }
            else {
               return hash;
            }
            
        }
        /**
         * Path of the file
         */
        public String getPath() {
            return _path;
        }
        @Override
        public int compareTo(Diff o) {
            return _path.compareTo(o._path);
        }
                
        
    }

    private FileSelector _fileSelector;

    /**
     * Creates a comparer without filtering
     */
    public DirComparer() {
        _fileSelector = Selectors.SELECT_ALL;
    }
    
    /**
     * Creates a comparer which uses only files accepted by the parameter selector
     * @param fileSelector The selector to use
     */
    public DirComparer(FileSelector fileSelector) {
        _fileSelector = fileSelector;
    }
    
    
    /**
     * Creates a map of hashes of all files contained in the given directory (or a map of a single hash for a given regular file).
     * The map uses relative paths (relative to the given file) as keys and MD5 hashes of the files contents as values.
     * Subdirectories are represented as entries with the relative path as key and an empty string as value.
     * That way the map and its hashCode()/equals() implementation is suitable to represent/compare the complete file contents and file names of a given directory (or only the file contents of a given regular file).
     * Note that this method reads the complete contents of this directory for this operation, which may be very slow and exhaustive on large directories and files.
     * @param file
     * @return Map of hashes
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public Map<String,String> readFileHashes(FileObject file) throws NoSuchAlgorithmException, IOException {
        Map<String,String> hashes = new HashMap<String, String>();
        addFileHashes(hashes, file, file);        
        return hashes;

    }

    private void addFileHashes(Map<String, String> hashes, FileObject file, FileObject root) throws NoSuchAlgorithmException, IOException {
        if (file == null || !file.exists()) {
            return;
        }
        for (FileObject child : file.findFiles(_fileSelector)) {
            addFileHash(hashes, child, root);
        }
    }

    private void addFileHash(Map<String, String> hashes, FileObject file, FileObject root) throws NoSuchAlgorithmException, IOException, FileSystemException {
        
        String path = root.getName().getRelativeName(file.getName());
        if (file.getType().equals(FileType.FOLDER)) {
            hashes.put(path, "");
        }
        else {
            String hash = MD5HashingInputStream.getStreamHash(file.getContent().getInputStream());
            hashes.put(path, hash);
        }
    }
    
    
    /**
     * Compares to files/folders for equality
     * @param file1
     * @param file2
     * @return True if contained files and their contents are equal.  False if not
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public boolean areEqual(FileObject file1, FileObject file2) throws NoSuchAlgorithmException, IOException {
        Map<String,String> map1 = readFileHashes(file1);
        Map<String,String> map2 = readFileHashes(file2);
        return map1.equals(map2);
        
    }
    
    /**
     * Returns the differences between two files/folders
     * @param file1 File 1
     * @param file2 File 2
     * @return A map of {@link Diff} objects, representing files that differ, keyed by their path
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public Map<String,Diff> determineDifferences(FileObject file1, FileObject file2) throws NoSuchAlgorithmException, IOException {

        Map<String,String> map1 = readFileHashes(file1);
        Map<String,String> map2 = readFileHashes(file2);
        
        Map<String,Diff> diff = new HashMap<String, DirComparer.Diff>();
        
        Set<Map.Entry<String,String>> entrySet1 = map1.entrySet();
        for (Map.Entry<String,String> entry1 : entrySet1) {
            if (!map2.containsKey(entry1.getKey())) {
                diff.put(entry1.getKey(), new Diff(entry1.getKey(), entry1.getValue(), null));
                continue;
            }
            
            String map2hash = map2.get(entry1.getKey());
            if (!map2hash.equals(entry1.getValue())) {
                diff.put(entry1.getKey(), new Diff(entry1.getKey(), entry1.getValue(), map2hash));
            }
        }
        
        Set<Map.Entry<String,String>> entrySet2 = map2.entrySet();
        for (Map.Entry<String,String> entry2 : entrySet2) {
            if (!map1.containsKey(entry2.getKey())) {
                diff.put(entry2.getKey(), new Diff(entry2.getKey(), null, entry2.getValue()));
                continue;
            }
            
            String map1hash = map1.get(entry2.getKey());
            if (!map1hash.equals(entry2.getValue())) {
                diff.put(entry2.getKey(), new Diff(entry2.getKey(), map1hash, entry2.getValue()));
            }
        }
        
        return diff;
        
    }
    
    

}
