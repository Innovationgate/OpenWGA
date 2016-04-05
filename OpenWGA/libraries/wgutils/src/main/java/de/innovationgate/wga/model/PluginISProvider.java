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
package de.innovationgate.wga.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;

import javax.crypto.CipherInputStream;

import de.innovationgate.utils.DESEncrypter;
import de.innovationgate.utils.DESEncrypter.PersistentKeyException;
import de.innovationgate.wga.common.DesignDirectory;


public class PluginISProvider implements de.innovationgate.utils.DirZipper.InputStreamProvider {
	
	 private DESEncrypter _cipher;
     private File _tmlDir;
     private File _scriptDir;
     private boolean _obfuscate;

     public PluginISProvider(File designDirectory, boolean obfuscate) throws PersistentKeyException, GeneralSecurityException, IOException {
    	 if (obfuscate) {
    		 _cipher = new DESEncrypter();
			 _cipher.initObfuscation();
    	 }
         _tmlDir = new File(designDirectory, DesignDirectory.FOLDERNAME_TML);
         _scriptDir = new File(designDirectory, DesignDirectory.FOLDERNAME_SCRIPT);
         _obfuscate = obfuscate;
     }
     
     public InputStream provideInputStream(File file) throws FileNotFoundException {
         
         boolean useCipher = false;
         if (_obfuscate == true && _cipher != null) {
             
             if (file.getPath().startsWith(_tmlDir.getPath())) {
                 useCipher = true;
             }
             else if (file.getPath().startsWith(_scriptDir.getPath())) {
                 useCipher = true;
             }
         }
         
         if (useCipher) {
             return new CipherInputStream(new FileInputStream(file), _cipher.getEcipher());
         }
         else {
             return new FileInputStream(file);
         }
         
     }
}
