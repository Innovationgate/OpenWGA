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

package de.innovationgate.wgpublisher.files;

import java.io.IOException;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentIterator;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.scheduler.TaskImplementation;
import de.innovationgate.wgpublisher.scheduler.TitledTaskImplementation;

public class UpdateAnnotationsTask implements TaskImplementation, TitledTaskImplementation {

    @Override
    public void execute(JobContext jobContext) throws JobFailedException {

        try {
            WGA wga = WGA.get(jobContext);
            String dbkey = jobContext.getOption("dbkey");
            if (dbkey == null) {
                throw new JobFailedException("No database key given");
            }
            
            App app = wga.app(dbkey);
            if (app == null) {
                throw new JobFailedException("Unknown database of key '" + dbkey + "'");
            }
            
            int count=0;
            Iterator<WGContent> allContent = app.db().getAllContent(true);
            while (allContent.hasNext()) {
                WGContent content = allContent.next();
                try {
                    updateAnnotations(jobContext, content);
                    count++;
                    if (count % 100 == 0) {
                        app.db().getSessionContext().clearCache();
                    }
                }
                catch (Throwable e) {
                    jobContext.getLog().error("Exception updating annotations on document " + content.getDocumentKey(), e);
                }
            }
            
        }
        catch (JobFailedException e) {
            throw e;
        }
        catch (WGException e) {
            throw new JobFailedException("Exception upgrading annotations", e);
        }
        
    }

    private void updateAnnotations(JobContext jobContext, WGContent content) throws WGAPIException, IOException {

        for (String fileName : content.getFileNames()) {
            WGFileMetaData md = content.getFileMetaData(fileName);
            TemporaryFile tempFile = new TemporaryFile(fileName, content.getFileData(fileName), WGFactory.getTempDir());
            try {
                content.getDatabase().annotateMetadata(tempFile.getFile(), md, null);
                if (content.isEdited()) {
                    content.saveQuiet();
                    jobContext.getLog().info("Updated metadata on file '" + fileName + "' from document " + content.getDocumentKey());
                }
            }
            finally {
                tempFile.delete();
            }
        }
        
    }

    @Override
    public String getTitle(Locale locale, Map<String, String> options) {
        return "Updating file annotations on app '" + options.get("dbkey") + "'";
    }

}
