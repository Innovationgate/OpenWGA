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

package de.innovationgate.wgpublisher.scheduler;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Locale;
import java.util.Map;

import org.dom4j.Element;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wgpublisher.WGACore;

public class DumpContentStoreTask extends Task {

    public void execute(JobContext jobContext) throws TaskException {
        
        boolean fileCreated = false;
        File file = null;
        try {
            // Getting and validating options
            String dbkey = (String) getOption("dbkey");
            if (dbkey == null) {
                throw new TaskException("No dbkey provided");
            }
            
            WGDatabase db = (WGDatabase) jobContext.getWgaCore().getContentdbs().get(dbkey);
            if (db == null) {
                throw new TaskException("Database of key " + dbkey + " not connected");
            }
            db.openSession();
            
            String filter = (String) getOption("filter");
            String fileName = (String) getOption("filename");
            if (fileName == null) {
                throw new TaskException("Option 'filename' not specified");
            }
            
            file = new File(fileName);
            
            Boolean autoCorrect = Boolean.valueOf((String) getOption("autocorrect", "false"));
            
            String endMessage = (String) getOption("endmessage");
            
            // Creating dump
            jobContext.getLog().info("Starting Content Store Dump");
            InputStream dumpIn = jobContext.getWgaCore().dumpContentStore(db, filter, autoCorrect.booleanValue(), jobContext.getLog());
            
            
            // Copying dump to target
            jobContext.getLog().info("Copying dump file to target location");
            if (!file.exists()) {
                file.createNewFile();
                fileCreated = true;
            }
            OutputStream fileOut = new java.io.FileOutputStream(file);
            WGUtils.inToOut(dumpIn, fileOut, 2048);
            fileOut.close();
            dumpIn.close();
            if (endMessage != null) {
                jobContext.getCurrentJob().setEndMessage(endMessage);
            }
            else {
                jobContext.getCurrentJob().setEndMessage("Content store dump stored as file <b>" + file.getAbsolutePath() + "</b>");
            }
        }
        catch (Exception e) {
           jobContext.getLog().error("Error creating dump", e);
           if (fileCreated && file != null && file.exists()) {
               file.delete();
           }
           throw new TaskException("Task failed because of exception", e);
        }
    }

	@Override
	public void configure(WGACore core)
			throws ConfigurationException {
	}
}
