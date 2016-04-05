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
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.lang.ref.WeakReference;
import java.text.DecimalFormat;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.zip.ZipInputStream;

import org.apache.log4j.Logger;

/**
 * A helper object to use temporary files that must have a special file name in file system.
 * The object does not directly drop the temporary file in the temp folder but
 * creates a parent directory with temporary name to drop it in there. Because of this
 * the file may have any file name desired.
 * <p>
 * The file and directory will NOT be automatically deleted on VM termination anymore (see B000048C2). They should be manually
 * deleted by calling {@link #delete()} when you are finished with them, or be bound to the eviction of a context object by
 * using {@link #deleteOnEviction(Object)}.
 * </p>
 */
public class TemporaryFile {
    
    private static final long SCHEDULE_PERIOD = 1000 * 60;
    private static final DecimalFormat FOLDER_FORMAT = new DecimalFormat("00");
    private static final String TEMPFILES_FOLDER = "tempfiles";
    
    private static File _preparedDir = null;
    
    public static void prepareDefaultTempDirectory() {
        prepareDirectory(getDefaultTempDirectory());
    }
    
    public static void prepareDirectory(File dir) {
        
        File tempFiles = new File(dir, TEMPFILES_FOLDER);
        tempFiles.mkdirs();
        
        
        for (int idx=0; idx < 100; idx++) {
            File subFolder = new File(tempFiles, FOLDER_FORMAT.format(idx));
            subFolder.mkdirs();
        }
        
        _preparedDir = dir;
        
    }

    private static Map _waitForEviction = WGUtils.createSynchronizedMap();
    static class EvictionTask extends TimerTask {

        public void run() {
            Thread.currentThread().setName("WGA TemporaryFile Eviction Task");
            try {
                deleteNow();
            }
            catch (Throwable t) {
                Logger.getLogger("wga.tempfiles").error("Exception deleting files to evict", t);
            }
        }
    }
    private static Timer _evictionTimer = new Timer(true);
    static {
        _evictionTimer.scheduleAtFixedRate(new EvictionTask(), SCHEDULE_PERIOD, SCHEDULE_PERIOD);
    }
    
    /**
     * Stops the eviction thread
     */
    public static void stopEviction() {
        _evictionTimer.cancel();
    }
    
    
    /**
     * Deletes all files now whose deletion were connected to now evicted objects via {@link #deleteOnEviction(Object)}
     */
    public static void deleteNow() {
        synchronized (_waitForEviction) {
            
            Iterator tempFiles = _waitForEviction.keySet().iterator();
            while (tempFiles.hasNext()) {
                TemporaryFile file = (TemporaryFile) tempFiles.next();
                WeakReference refObject = (WeakReference) _waitForEviction.get(file);
                if (refObject.get() == null) {
                    file.delete();
                    tempFiles.remove();
                }
            }
        }
    }


    private File _dir;
    private File _file;
    private boolean _waitingForEviction = false;

    /**
     * Constructs a temporary file object and initializes it with the data from an input stream
     * <p>
     * If the input stream that is providing data is a java.util.zip.ZipInputStream it is NOT closed after
     * the file data is read (to allow further reading of other file entries in the zip data)! 
     * All other stream types are closed automatically after object creation.
     * </p>
     * @param name The name that file should have in file system
     * @param in The input stream providing data for the temporary file. Omit to leave the file empty
     * @param tempFolder The temp folder containing temporary files
     * @throws IOException
     */
    public TemporaryFile(String name, InputStream in, File tempFolder) throws IOException {
        
        
        if (tempFolder == null) {
            tempFolder = getDefaultTempDirectory();
        }
        
        File targetFolder = getTargetFolder(tempFolder);
        
        _dir = File.createTempFile("tfil", ".tmp", targetFolder);
        _dir.delete();
        _dir.mkdir();
        //B000048C2
        //_dir.deleteOnExit();
        
        _file = new File(_dir, name);
        if (!_file.createNewFile()) {
            throw new IOException("Unable to create temporary file");
        }
       
        if (in != null) {
            
            if (!(in instanceof ZipInputStream)) {
                in = new BufferedInputStream(in);
            }
            
            OutputStream out = new BufferedOutputStream(new FileOutputStream(_file));
            WGUtils.inToOut(in, out, 2048);
            
            if (!(in instanceof ZipInputStream)) {
                in.close();
            }
            
            out.close();
        }
        //B000048C2
        //_file.deleteOnExit();
        
    }

    private static File getDefaultTempDirectory() {
        
        if (_preparedDir != null) {
            return _preparedDir;
        }
        else {
            return new File(System.getProperty("java.io.tmpdir"));
        }
    }
    
    private File getTargetFolder(File tempFolder) {

        if (!tempFolder.equals(_preparedDir)) {
            return tempFolder;
        }
        
        int no = (int) Math.floor(Math.random() * 100);
        return new File(_preparedDir, TEMPFILES_FOLDER + "/" + FOLDER_FORMAT.format(no));
        
    }


    /**
     * Manually delete the temporary file and it's temporary directory
     */
    public void delete() {
        
        if (_waitingForEviction) {
            throw new IllegalStateException("The TemporaryFile is waiting for eviction of a context object and cannot be deleted manually");
        }
        
        _file.delete();
        _dir.delete();
    }

    /**
     * Will keep the file alive until the given object is garbage-collected. Then the file will be deleted
     * @param obj
     */
    public void deleteOnEviction(Object obj) {
        
        if (_waitingForEviction) {
            throw new IllegalStateException("The TemporaryFile is already waiting for eviction of a context object");
        }
        
        
        _waitForEviction.put(this, new WeakReference(obj));
    }

    /**
     * The temporary file as Java file object
     */
    public File getFile() {
        return _file;
    }
    
    /**
     * Creates a writer to write character data to the file. 
     * @param charset Target charset of the file
     * @throws FileNotFoundException 
     * @throws UnsupportedEncodingException 
     */
    public PrintWriter createWriter(String charset) throws UnsupportedEncodingException, FileNotFoundException {
        return new PrintWriter(new OutputStreamWriter(createOutputStream(), charset));
    }
    
    /**
     * Creates a writer to write UTF-8 character data to the file. 
     * @throws FileNotFoundException 
     * @throws UnsupportedEncodingException 
     */
    public PrintWriter createWriter() throws UnsupportedEncodingException, FileNotFoundException {
        return createWriter("UTF-8");
    }
    
    /**
     * Creates an output stream to write binary data to the file. 
     * @throws FileNotFoundException 
     */
    public OutputStream createOutputStream() throws FileNotFoundException {
            return new BufferedOutputStream(new FileOutputStream(_file), 2048);
    }
    
}
