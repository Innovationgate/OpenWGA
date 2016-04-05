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

package de.innovationgate.wgpublisher.logserver;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import de.innovationgate.utils.LineBufferWriter;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.LogHandler;
import de.innovationgate.wga.common.LogLevel;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.log.AppLog;

public class AppLogHandler implements LogHandler {
    
    private WGACore _core;
    private int _largestKnownLine = 1;
    private AppLog _appLog;

    public AppLogHandler(WGACore core) throws WGException {
        _core = core;
        _appLog = WGA.get(_core).service(AppLog.class);
    }

    public List getPage(int from, int size, LogLevel level) throws IOException {
        StringBuffer out = new StringBuffer();
        LineBufferWriter writer = new LineBufferWriter();
        AppLog.Page page = _appLog.writePage(from, size, writer, level, true);
        return new ArrayList(writer.getBuffer());

    }
    
    public List getLastPage(int size, LogLevel level) throws IOException {
        
        StringBuffer out = new StringBuffer();
        int offset = _largestKnownLine;
        
        // We try our largest known line, to see if we can start from there
        // If it is too large we start from line 1
        LineBufferWriter writer = new LineBufferWriter();
        AppLog.Page page = _appLog.writePage(_largestKnownLine, size, writer, level, true);
        if (!page.isPageExisting()) {
            offset = 1;
        }
        
        // We retrieve pages until we reach the end
        AppLog.Page endPage;
        LineBufferWriter lastWriterWithContent = null;
        while (true) {
            writer = new LineBufferWriter();
            endPage = fetchPage(offset, size, writer, level);
            if (endPage.isPageExisting()) {
                lastWriterWithContent = writer;
            }
            if (endPage.isEndReached()) {
                break;
            }
            
            offset = endPage.getEndIndex() + 1;
        }
        
        return new ArrayList(lastWriterWithContent.getBuffer());
        
    }

    private AppLog.Page fetchPage(int offset, int size, LineBufferWriter writer, LogLevel level) {
        AppLog.Page page = _appLog.writePage(offset, size, writer, level, true);
        offset = page.getEndIndex();
        if (page.isPageExisting() && offset > _largestKnownLine) {
            _largestKnownLine = offset;
        }
        
        return page;
    }

    public List search(int searchStartLine, int size, String searchString, int searchType, boolean forward) throws IOException {

        StringBuffer out = new StringBuffer();
        int offset = searchStartLine;
        
        // If we go backward we must start with the page that ENDs with the desired start line
        if (!forward) {
            offset = scrollOffsetBackward(offset + 1);
        }
        
        // We retrieve pages until we find what we look for
        LineBufferWriter writer = null;
        int line = -1;
        int lastUsedOffset = -1;
        
        while (offset != -1) {
            writer = new LineBufferWriter();
            lastUsedOffset = offset;
            AppLog.Page page = fetchPage(offset, 1000, writer, LogLevel.LEVEL_ALL);
            
            List lines = new ArrayList(writer.getBuffer());
            
            
            if (!forward) {
                // If the retrieved lines includes line numbers beyond the search start line, we must remove them to prevent them from being searched (can only happen in backward mode)
                if (offset + lines.size() > searchStartLine) {
                    lines = lines.subList(0, searchStartLine - offset + 1);
                }
                
                // If not searching forward we will reverse the buffer for searching. Resulting line numbers will be calculated back.
                Collections.reverse(lines);
            }
            
            switch (searchType) {
                case SEARCHTYPE_PLAINTEXT:
                    line = searchForTerm(lines, searchString);
                    break;
                    
                case SEARCHTYPE_REGEXP:
                    line = searchForRegExp(lines, searchString);
                    break;
                    
                default:
                    throw new IllegalArgumentException("Unknown search type " + searchType);
            }
            
            if (line != -1) {
                // Calculating back "real" line if we reversed the buffer previously
                if (!forward) {
                    line = lines.size() - line - 1;
                }
                break;
            }
            
            // Calculate next offset, if the end was not reached
            if (!page.isEndReached()) {
                if (forward) {
                    offset = page.getEndIndex() + 1;
                }
                else {
                    offset = scrollOffsetBackward(offset);
                }
            }
            else {
                offset = -1;
            }
            
            
        }
        
        // Get the exact page beginning with the found line
        if (line != -1) {
            writer = new LineBufferWriter();
            AppLog.Page page = fetchPage(lastUsedOffset + line, size, writer, LogLevel.LEVEL_ALL);
            return new ArrayList(writer.getBuffer());
        }
        else {
            return new ArrayList();
        }
       
        
        
    }

    private int searchForRegExp(List buffer, String searchString) {

        // We remove the obligatory line feed at the end
        Pattern pattern = Pattern.compile(searchString, Pattern.UNICODE_CASE | Pattern.CASE_INSENSITIVE);
        
        Iterator lines = buffer.iterator();
        int lineNr = -1;
        while (lines.hasNext()) {
            String line = (String) lines.next();
            lineNr++;
            if (pattern.matcher(line).find()) {
                return lineNr;
            }
        }
        return -1;
        
    }

    private int scrollOffsetBackward(int offset) {
        if (offset == 1) {
            offset = -1;
        }
        else if (offset <= 1000) {
            offset = 1;
        }
        else {
            offset = offset - 1000;
        }
        return offset;
    }

    private int searchForTerm(List buffer, String searchString) {
        
        Iterator lines = buffer.iterator();
        int lineNr = -1;
        while (lines.hasNext()) {
            String line = (String) lines.next();
            lineNr++;
            if (line.toLowerCase().indexOf(searchString.toLowerCase()) != -1) {
                return lineNr;
            }
        }
        return -1;
        
        
    }

    public List getPreviousPage(int start, int size, LogLevel level) throws IOException {
        List page;
        do {
            start = Math.max(1, start - size);
            page = getPage(start, size, level);
        }
        while (page.size() < size && start != 1);
        
        return page;
    }

}
