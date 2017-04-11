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

package de.innovationgate.wgpublisher;

import java.io.File;
import java.io.FileFilter;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileContainer;

public class ExternalFileServingMaintenanceTask implements Runnable {

    private WGACore _core;
    
    private int sleepTime = 1000 * 10;
    private Thread thread;
    

    
    public ExternalFileServingMaintenanceTask(WGACore core) {
        _core = core;
    }
    
    public void start() {
    	this.thread = new Thread(this);
        this.thread.setDaemon(true);
        this.thread.setPriority(Thread.MIN_PRIORITY);
        this.thread.start();
    }
    
    public void stop() {
        this.thread = null;
    }

    private String getFilename(String deployed_filename){
        // handle URL parameter
    	String filename = deployed_filename;
        int q1_pos = filename.indexOf("["); 
        int q2_pos = filename.lastIndexOf("]");
        if(q1_pos>=0 && q2_pos>=0)
        	filename = filename.substring(q1_pos+1, q2_pos);
        return filename;
    }
    
    public void run() {
        try {
            Thread meThread = Thread.currentThread();
            meThread.setName("ExternalFileServingMaintenanceTask");
            
            while (this.thread == meThread) {
            
                ExternalFileServingConfig config = _core.getExternalFileServingConfig();
                
                if (!config.isEnabled()) {
                    // external file serving has been disabled ... remove all file entries
                    if (config.getDirectory() != null && config.getDirectory().exists()) {
                        _core.getLog().info("External file serving has been disabled. Removing all cached entries from '" + config.getDirectory().getAbsolutePath() + "'.");
                        WGUtils.delTree(config.getDirectory(), false);
                    }
                } else {
                    // perform check for deployed cache data
                    File[] dbDirectories = config.getDirectory().listFiles(new FileFilter() {                       
                        public boolean accept(File file) {
                            return file.isDirectory();
                        }
                    });                    
                    for (File dbDirectory : dbDirectories) {
                        String dbkey = dbDirectory.getName();
                        WGDatabase db = _core.getContentdbs().get(dbkey);
                        if (db == null) {
                            if (!_core.getWgaConfiguration().hasContentDatabase(dbkey) || !_core.getWgaConfiguration().getContentDatabase(dbkey).isEnabled()) { 
                                if (dbDirectory.exists()) {
                                    _core.getLog().info("Database '" + dbkey + "' is disabled or has been removed. Removing all cached entries from '" + dbDirectory.getAbsolutePath() + "'.");                
                                    WGUtils.delTree(dbDirectory);            
                                    if (dbDirectory.exists()) {
                                        _core.getLog().warn("External file cache maintenance failed. Unable to remove directory '" + dbDirectory.getAbsolutePath() + "'. Cache might serve protected file data.");            
                                    }
                                }
                            }
                        } else if (db.isConnected()) {
                            try {
                                db.openSession();
                                if (db.isSessionOpen()) {
                                    db.getSessionContext().setTask("ExternalFileServingMaintenanceTask");
                                    if (!db.isAnonymousAccessible()) {
                                        if (dbDirectory.exists()) {
                                            _core.getLog().info("Database '" + db.getDbReference() + "' is not accessible by anonymous users. Removing all cached entries from '" + dbDirectory.getAbsolutePath() + "'.");                
                                            WGUtils.delTree(dbDirectory);            
                                            if (dbDirectory.exists()) {
                                                _core.getLog().warn("External file cache maintenance failed. Unable to remove directory '" + dbDirectory.getAbsolutePath() + "'. Cache might serve protected file data.");            
                                            }
                                        }                                        
                                    } else if (!db.getBooleanAttribute(WGACore.DBATTRIB_EXTERNAL_FILE_SERVING_ENABLED, false)) {
                                        if (dbDirectory.exists()) {
                                            _core.getLog().info("External file serving has been disabled for database '" + db.getDbReference() + "'. Removing all cached entries from '" + dbDirectory.getAbsolutePath() + "'.");                
                                            WGUtils.delTree(dbDirectory);            
                                            if (dbDirectory.exists()) {
                                                _core.getLog().warn("External file cache maintenance failed. Unable to remove directory '" + dbDirectory.getAbsolutePath() + "'. Cache might serve stale file data.");            
                                            }
                                        }                                        
                                    } else {
                                        // perform file deletion checks
                                        if (dbDirectory.exists()) {
                                            File[] containers = dbDirectory.listFiles(new FileFilter() {                                                
                                                public boolean accept(File file) {
                                                    return file.isDirectory();
                                                }
                                            });
                                            for (File container : containers) {
                                                if (container.getName().startsWith("content:")) {
                                                    String contentKey = container.getName().substring("content:".length());
                                                    
                                                    WGContent content = db.getContentByKey(contentKey);
                                                    if (content == null || !content.getStatus().equals(WGContent.STATUS_RELEASE)) {
                                                        // content has been deleted or archived - remove file data
                                                        _core.getLog().info("Content '" + contentKey + "' has been deleted or archived. Clearing external file serving cache.");
                                                        WGUtils.delTree(container);
                                                        if (container.exists()) {
                                                            _core.getLog().warn("External file cache maintenance failed. Unable to remove directory '" + container.getAbsolutePath() + "'. Cache might serve stale file data.");            
                                                        }
                                                    } else {
                                                        // check general access to content                                                    
                                                        if (content.getReaders().size() > 0) {
                                                            // delete all deployed files of content
                                                            _core.getLog().info("Content '" + content.getContentKey() + "' is not accessible by anonymous. Clearing external file serving cache.");
                                                            WGUtils.delTree(container);  
                                                            if (container.exists()) {
                                                                _core.getLog().warn("External file cache maintenance failed. Unable to remove directory '" + container.getAbsolutePath() + "'. Cache might serve stale file data.");            
                                                            }
                                                        } else {
                                                            // check for existence and size of files 
                                                            File[] deployedFiles = container.listFiles();
                                                            for (File deployedFile : deployedFiles) {
                                                                if (deployedFile.isFile()) {
                                                                	
                                                                	String filename = getFilename(deployedFile.getName());
                                                                    if (!content.hasFile(filename)) {
                                                                        _core.getLog().info("File '" + filename + "' has been removed from content '" + content.getContentKey() + "' and will be removed from external file serving cache: " + deployedFile.getName());
                                                                        if (!deployedFile.delete()) {
                                                                            _core.getLog().warn("Unable to delete external file serving data '" + deployedFile.getAbsolutePath() + "'. File cache might serve stale data.");
                                                                        }
                                                                    } else if (deployedFile.length() < config.getThreshold()) {
                                                                        _core.getLog().info("Deployed file '" + deployedFile.getName() + "' of content '" + content.getContentKey() + "' deceeds current file size threashold and will be removed from external file serving cache.");
                                                                        if (!deployedFile.delete()) {
                                                                            _core.getLog().warn("Unable to delete external file serving data '" + deployedFile.getAbsolutePath() + "'. File cache might serve stale data.");
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                } else if (container.getName().startsWith("filecontainer:")) {
                                                    String fileContainerName = container.getName().substring("filecontainer:".length());
                                                    WGFileContainer fileContainer = db.getFileContainer(fileContainerName);
                                                    if (fileContainer == null) {
                                                        // content has been deleted remove file data
                                                        _core.getLog().info("File container '" + fileContainerName + "' has been deleted. Clearing external file serving cache.");
                                                        WGUtils.delTree(container);
                                                        if (container.exists()) {
                                                            _core.getLog().warn("External file cache maintenance failed. Unable to remove directory '" + container.getAbsolutePath() + "'. Cache might serve stale file data.");            
                                                        }
                                                    } else {
                                                        // check for existence and size of files 
                                                        File[] deployedFiles = container.listFiles();
                                                        for (File deployedFile : deployedFiles) {
                                                            if (deployedFile.isFile()) {
                                                            	String filename = getFilename(deployedFile.getName());
                                                                if (!fileContainer.hasFile(filename)) {
                                                                    _core.getLog().info("File '" + filename + "' has been removed from file container '" + fileContainer.getName() + "' and will be removed from external file serving cache: " + deployedFile.getName());
                                                                    if (!deployedFile.delete()) {
                                                                        _core.getLog().warn("Unable to delete external file serving data '" + deployedFile.getAbsolutePath() + "'. File cache might serve stale data.");
                                                                    }
                                                                } else if (deployedFile.length() < config.getThreshold()) {
                                                                    _core.getLog().info("Deployed file '" + deployedFile.getName() + "' of file container '" + fileContainer.getName() + "' deceeds current file size threashold and will be removed from external file serving cache.");
                                                                    if (!deployedFile.delete()) {
                                                                        _core.getLog().warn("Unable to delete external file serving data '" + deployedFile.getAbsolutePath() + "'. File cache might serve stale data.");
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        
                                    }
                                } else {
                                    _core.getLog().warn("External file serving maintenance failed. Unable to open session on database '" + db.getDbReference() + "'. Cache might serve protected file data.");
                                }                                
                            }                                                                                    
                            catch (Throwable e) {
                                _core.getLog().warn("External file serving maintenance failed. Cache might serve protected file data.", e);
                            } finally {
                                WGFactory.getInstance().closeSessions();
                            }
                        }                      
                    }
                }
                
                try {
                    Thread.sleep(sleepTime);
                }
                catch (InterruptedException exc) {}
            
            }
        } catch (Throwable e) {
            _core.getLog().error("Failure during external file serving maintenance.", e);
        }
    }

}
