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
import java.io.IOException;

/**
 * An input stream for a temporary file that gets unused after the stream is gc'd.
 * So this FileInputStream extension deletes the file when it is finalized.
 *
 */
public class TempFileInputStream extends FileInputStream {

    private File _file;

    public TempFileInputStream(File file) throws FileNotFoundException {
        super(file);
        _file = file;
    }

    protected void finalize() throws IOException {
        
       if (_file.exists()) {
           _file.delete();
       }
        
    }

}
