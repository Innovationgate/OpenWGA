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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.Locale;
import java.util.Map;
import java.util.zip.ZipInputStream;

import org.dom4j.Element;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wgpublisher.WGACore;

public class LoadDumpTask extends Task {

    public void execute(JobContext jobContext) throws TaskException {
        
        boolean fileCreated = false;
        File file = null;
        try {
            // Getting and validating options
            String dbkey = getOption("dbkey");
            if (dbkey == null) {
                throw new TaskException("No dbkey provided");
            }
            
            WGDatabase db = (WGDatabase) jobContext.getWgaCore().getContentdbs().get(dbkey);
            if (db == null) {
                throw new TaskException("Database of key " + dbkey + " not connected");
            }
            db.openSession();
            
            String fileName = getOption("filename");
            file = new File(fileName);
            if (!file.exists()) {
                throw new TaskException("Dump file does not exist :" + file.getPath());
            }
            
            boolean deleteFile = Boolean.valueOf((String) jobContext.getCustomOptions().get("deletefile")).booleanValue();
            
            // Loading dump
            jobContext.getLog().info("Starting dump load");
            ZipInputStream in = new ZipInputStream(new BufferedInputStream(new FileInputStream(file)));
            boolean result = jobContext.getWgaCore().importContentStoreDump(in, db, jobContext.getLog());
            if (!result) {
                throw new RuntimeException("Dump load finished with errors. Check log for details.");
            }
            
            if (deleteFile) {
                jobContext.getLog().info("Deleting dump file");
                file.delete();
            }
            
            jobContext.getLog().info("Content store dump was imported to database " + dbkey);

        }
        catch (Exception e) {
           jobContext.getLog().error("Error loading dump", e);
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
