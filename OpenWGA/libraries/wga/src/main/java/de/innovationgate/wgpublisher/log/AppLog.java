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

package de.innovationgate.wgpublisher.log;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Writer;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.innovationgate.utils.LineBufferWriter;
import de.innovationgate.wga.common.LogLevel;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WGAAwareService;
import de.innovationgate.wgpublisher.log.AppLogAppender.AppenderFile;

/**
 * OpenWGA application log retrieval service
 */
public class AppLog implements WGAAwareService {

    /**
     * Search type which searches for the given string as plaintext
     */
    public static final int SEARCHTYPE_PLAINTEXT = 1;

    /**
     * Searchtype which searches for the given string as regular expression,
     * matching a single applog line.
     */
    public static final int SEARCHTYPE_REGEXP = 2;

    /**
     * A page of applog content
     */
    public static class Page {

        private boolean _pageExisting = false;

        private boolean _endReached = false;

        private int _endIndex = -1;

        private List<Message> _messages = new ArrayList<Message>();

        private Page(boolean pageReached, boolean endReached, int endIndex, List<Message> messages) {
            this._pageExisting = pageReached;
            this._endIndex = endIndex;
            this._endReached = endReached;
            if (messages != null) {
                _messages.addAll(messages);
            }
        }

        private Page(boolean pageReached, boolean endReached, int endIndex) {
            this(pageReached, endReached, endIndex, null);
        }

        /**
         * Returns if the requested page was reached
         */
        public boolean isPageExisting() {
            return _pageExisting;
        }

        /**
         * Returns the index of the last line of the page, -1 if it does not
         * apply
         */
        public int getEndIndex() {
            return _endIndex;
        }

        /**
         * Returns if all requested lines were reached
         */
        public boolean isEndReached() {
            return _endReached;
        }

        /**
         * Returns the applog messages
         */
        public List<Message> getMessages() {
            return _messages;
        }
    }

    /**
     * An applog message. A message may represent multiple lines in applog that
     * all originate from the same logging event
     */
    public static class Message {

        public static final DecimalFormat LINENUMBER_FORMAT = new DecimalFormat("000000");

        public static final Pattern MESSAGE_PATTERN = Pattern.compile("(\\d+.\\d+.\\d+ \\d+:\\d+:\\d+) (\\w+) (.+)");

        public static final DateFormat MESSAGE_DATEFORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

        private Date _time;

        private LogLevel _level;

        private String _mainMessage;

        private StringBuffer _details = null;

        private int _line;

        private static Message parseLine(String line, int lineNumber) {

            Matcher messageMatcher = MESSAGE_PATTERN.matcher(line);
            if (messageMatcher.matches()) {
                return new Message(messageMatcher, lineNumber);
            }
            else {
                return null;
            }

        }

        private Message(Matcher messageMatcher, int line) {

            String dateStr = messageMatcher.group(1);
            try {
                _time = MESSAGE_DATEFORMAT.parse(dateStr);
            }
            catch (ParseException e) {
                // Shouldn't happen. If it does we display bogus date
                _time = new Date(Long.MIN_VALUE);
            }

            String logLevelStr = messageMatcher.group(2);
            _level = LogLevel.getLevel(logLevelStr);

            _mainMessage = messageMatcher.group(3);
            _line = line;
        }

        private void addDetailsLine(String line) {
            if (_details == null) {
                _details = new StringBuffer();
            }

            _details.append(line).append("\n");
        }

        public boolean containsIgnoreCase(String term) {

            if (_mainMessage != null && _mainMessage.toLowerCase().contains(term.toLowerCase())) {
                return true;
            }

            if (_details != null && _details.toString().toLowerCase().contains(term.toLowerCase())) {
                return true;
            }

            return false;

        }

        /**
         * Returns the time of the logging event causing the message
         */
        public Date getTime() {
            return _time;
        }

        /**
         * Returns the log level of the message
         */
        public LogLevel getLevel() {
            return _level;
        }

        /**
         * Returns the main message text
         */
        public String getMainMessage() {
            return _mainMessage;
        }

        /**
         * Returns the message detail text, like a stack trace for example
         */
        public String getDetails() {
            if (_details != null) {
                return _details.toString();
            }
            else {
                return null;
            }
        }

        /**
         * Returns the line number in applog where this message starts
         */
        public int getLine() {
            return _line;
        }

        @Override
        public String toString() {

            StringBuffer str = new StringBuffer();
            str.append(LINENUMBER_FORMAT.format(_line));
            str.append(" ");
            str.append(MESSAGE_DATEFORMAT.format(_time));
            str.append(" ");
            str.append(_level.toString());
            str.append(" ");
            str.append(_mainMessage);

            String details = getDetails();
            if (details != null) {
                str.append("\n").append(details);
            }

            return str.toString();

        }

    }

    private WGA _wga;

    /**
     * Retrieves a page of applog content
     * 
     * @param offset
     *            The offset of the first line to retrieve, 1 being the first
     *            line in the applog
     * @param size
     *            The number of messages to retrieve
     * @param loglevel
     *            The minimum log level of messages to retrieve
     * @return The applog page
     */
    public Page getPage(int offset, int size, LogLevel loglevel) {

        DecimalFormat lineNumberFormat = new DecimalFormat("000000");
        Iterator<AppenderFile> logFiles = _wga.getCore().getTransientLogAppender().getActiveFiles().iterator();

        if (offset < 1) {
            return new Page(false, false, -1);
        }

        int totalLinesCount = 0;
        boolean firstLine = true;
        boolean pageReached = false;
        boolean endReached = true;

        List<Message> messages = new ArrayList<Message>();
        Message currentMessage = null;

        filesloop: while (logFiles.hasNext()) {

            endReached = false;
            AppLogAppender.AppenderFile appenderFile = (AppLogAppender.AppenderFile) logFiles.next();
            if (appenderFile.getStartingLine() == -1) {
                appenderFile.setStartingLine(totalLinesCount+1);
            }
            
            int fileLinesCount = 0;

            // If the line count of this logfile is known, and it is not the
            // last one (which still might grow)
            // it might be skipped when we need a line beyond it
            if (logFiles.hasNext() && appenderFile.getLinesCount() != -1) {
                if (totalLinesCount + appenderFile.getLinesCount() < offset) {
                    totalLinesCount += appenderFile.getLinesCount();
                    continue;
                }
            }

            synchronized (appenderFile.getFile()) {

                LineNumberReader reader = null;
                try {
                    reader = new LineNumberReader(new FileReader(appenderFile.getFile()));
                    String line;

                    // Skip lines that are below the offset
                    while (offset > (totalLinesCount + 1)) {
                        line = reader.readLine();
                        if (line == null) {
                            appenderFile.setLinesCount(fileLinesCount);
                            endReached = true;
                            continue filesloop;
                        }
                        if (Message.parseLine(line, totalLinesCount+1) != null) {
                            totalLinesCount++;
                            fileLinesCount++;
                        }
                    }

                    // Read lines that are in the targeted region
                    while (true) {

                        line = reader.readLine();
                        if (line == null) {
                            appenderFile.setLinesCount(fileLinesCount);
                            endReached = true;
                            continue filesloop;
                        }

                        Message newMessage = Message.parseLine(line, totalLinesCount+1);
                        if (newMessage != null) {

                            totalLinesCount++;
                            fileLinesCount++;

                            // Add previous message to list
                            if (currentMessage != null) {
                                if (currentMessage.getLevel().isHigherOrEqual(loglevel)) {
                                    pageReached = true;
                                    messages.add(currentMessage);
                                    currentMessage = null;

                                    // The message size is reached we go back
                                    // one line and exit the loop
                                    if (messages.size() == size) {
                                        totalLinesCount--;
                                        fileLinesCount--;
                                        break filesloop;
                                    }
                                }
                            }

                            currentMessage = newMessage;
                        }
                        else {
                            if (currentMessage != null) {
                                currentMessage.addDetailsLine(line);
                            }
                            // The offset does not start with a regular message.
                            // We ignore these lines
                            else {
                                continue;
                            }
                        }
                    }
                }
                catch (Exception e) {
                    _wga.getCore().getLog().error("Error reading application log", e);
                    return new Page(false, false, totalLinesCount);
                }
                finally {
                    try {
                        if (reader != null) {
                            reader.close();
                        }
                    }
                    catch (IOException e) {
                        _wga.getCore().getLog().error("Error closing application log reader", e);
                    }
                }
            }

        }

        if (currentMessage != null && currentMessage.getLevel().isHigherOrEqual(loglevel)) {
            messages.add(currentMessage);
        }

        return new Page(pageReached, endReached, totalLinesCount, messages);
    }
    
    /**
     * Retrieves a page of applog content, containing messages of all log levels
     * 
     * @param offset
     *            The offset of the first line to retrieve, 1 being the first
     *            line in the applog
     * @param size
     *            The number of messages to retrieve
     * @return The applog page
     */
    public Page getPage(int offset, int size) {
        return getPage(offset, size, LogLevel.LEVEL_ALL);
    }

    /**
     * Searches for the earliest message whose log time is equal or later than the given time
     * @param time The log time to search
     * @return The line number of the message of that time, or -1 if it was not found
     */
    public int searchTime(Date time) {
        
        // Ensure that all files know their line numbers and counts
        updateAppenderFilesData();
        
        Iterator<AppenderFile> logFiles = _wga.getCore().getTransientLogAppender().getActiveFiles().iterator();
        
        int lastLogFileIndex = -1;
        
        // Iterate through logfiles to see where the requested time is located
         while (logFiles.hasNext()) {

            AppLogAppender.AppenderFile appenderFile = (AppLogAppender.AppenderFile) logFiles.next();
            Page page = getPage(appenderFile.getStartingLine(), 1, LogLevel.LEVEL_ALL);
            if (page.getMessages().size() > 0) {
                Message msg = page.getMessages().get(0);
                if (msg.getTime().after(time)) {
                    break;
                }
            }
            lastLogFileIndex = appenderFile.getStartingLine();
         }
         
         // The time is before the earliest file
         if (lastLogFileIndex == -1) {
             return -1;
         }
         
         // Iterate through pages and messages to locate time
         int searchIndex = lastLogFileIndex;
         while (true) {
             
             Page page = getPage(searchIndex, 100, LogLevel.LEVEL_ALL);
             for (Message msg : page.getMessages()) {
                 if (!msg.getTime().before(time)) {
                     return msg.getLine();
                 }
             }
             
             if (page.isEndReached()) {
                 break;
             }
             
             searchIndex = page.getEndIndex() + 1;
             
         }
         
         return -1;
            
        
    }

    /**
     * Writes the content of an applog page to a writer. Returns the data of the
     * retrieved page as {@link Page} object. Note that this object does not
     * contain the page content.
     * 
     * @param offset
     *            The offset of the first line to retrieve, 0 being the first
     *            line in the applog
     * @param size
     *            The number of messages to retrieve
     * @param out
     *            The writer to write log content to
     * @param loglevel
     *            The minimum log level of messages to retrieve
     * @param lineNumbers
     *            If true all written log lines will be prefixed with line
     *            numbers
     * @return The applog page
     */
    public Page writePage(int offset, int size, Writer out, LogLevel loglevel, boolean lineNumbers) {

        DecimalFormat lineNumberFormat = new DecimalFormat("000000");
        Iterator<AppenderFile> logFiles = _wga.getCore().getTransientLogAppender().getActiveFiles().iterator();

        if (offset < 1) {
            return new Page(false, false, -1);
        }

        int totalLinesCount = 0;
        boolean firstLine = true;
        boolean pageReached = false;
        boolean endReached = true;

        filesloop: while (logFiles.hasNext()) {

            AppLogAppender.AppenderFile appenderFile = (AppLogAppender.AppenderFile) logFiles.next();

            // If the line count of this logfile is known, and it is not the
            // last one (which still might grow)
            // it might be skipped when we need a line beyond it
            if (logFiles.hasNext() && appenderFile.getLinesCount() != -1) {
                if (totalLinesCount + appenderFile.getLinesCount() < offset) {
                    totalLinesCount += appenderFile.getLinesCount();
                    continue;
                }
            }

            synchronized (appenderFile.getFile()) {

                LineNumberReader reader = null;
                try {
                    reader = new LineNumberReader(new FileReader(appenderFile.getFile()));
                    LogLevel level = LogLevel.LEVEL_INFO;
                    String line;

                    // Skip lines that are below the offset
                    while (offset > (totalLinesCount + 1)) {
                        line = reader.readLine();
                        if (line == null) {
                            appenderFile.setLinesCount(reader.getLineNumber());
                            continue filesloop;
                        }
                        totalLinesCount++;
                        level = LogLevel.isolateLogLevel(line, 20, level);
                    }

                    int toLine = offset + size - 1;

                    // Read lines that are in the targeted region
                    while (totalLinesCount < toLine) {
                        line = reader.readLine();
                        if (line == null) {
                            appenderFile.setLinesCount(reader.getLineNumber());
                            continue filesloop;
                        }

                        totalLinesCount++;
                        level = LogLevel.isolateLogLevel(line, 20, level);

                        // Filter level
                        if (level.isHigherOrEqual(loglevel)) {
                            pageReached = true;

                            // Add line break only if not on first line
                            if (firstLine) {
                                firstLine = false;
                            }
                            else {
                                out.write("\n");
                            }

                            if (lineNumbers) {
                                out.write(lineNumberFormat.format(totalLinesCount));
                                out.write(" ");
                            }

                            out.write(line);
                        }
                        else {
                            toLine++;
                        }
                    }
                    endReached = false;
                }
                catch (Exception e) {
                    _wga.getCore().getLog().error("Error reading application log", e);
                    return new Page(false, false, totalLinesCount);
                }
                finally {
                    try {
                        if (reader != null) {
                            reader.close();
                        }
                    }
                    catch (IOException e) {
                        _wga.getCore().getLog().error("Error closing application log reader", e);
                    }
                }
            }

        }

        return new Page(pageReached, endReached, totalLinesCount);

    }

    /**
     * Returns the current number of lines in the applog
     */
    public int getLinesCount() {
        return getPage(Integer.MAX_VALUE, 1, LogLevel.LEVEL_ALL).getEndIndex();
    }

    @Override
    public void injectWGA(WGA wga) {
        _wga = wga;
    }

    /**
     * Searches the applog for a term or via regular expression
     * 
     * @param searchStartLine
     *            The number of the line where to start search
     * @param searchString
     *            The term or regular expression to search for
     * @param searchType
     *            Type of the search, either {@link #SEARCHTYPE_PLAINTEXT} or
     *            {@link #SEARCHTYPE_REGEXP}
     * @param forward
     *            true to search forward from the starting line, false to search
     *            backward
     * @return The line number where the term was found/regexp matched first. -1
     *         if it was not found
     * @throws IOException
     */
    public int search(int searchStartLine, String searchString, int searchType, boolean forward) throws IOException {

        StringBuffer out = new StringBuffer();
        int offset = searchStartLine;

        // If we go backward we must start with the page that ENDs with the
        // desired start line
        if (!forward) {
            offset = scrollOffsetBackward(offset + 1);
        }

        // We retrieve pages until we find what we look for
        LineBufferWriter writer = null;
        int line = -1;
        int lastUsedOffset = -1;

        while (offset != -1) {
            lastUsedOffset = offset;
            AppLog.Page page = getPage(offset, 1000, LogLevel.LEVEL_ALL);
            List<Message> lines = page.getMessages();
            if (!forward) {
                // If the retrieved lines includes line numbers beyond the
                // search start line, we must remove them to prevent them from
                // being searched (can only happen in backward mode)
                if (offset + lines.size() > searchStartLine) {
                    lines = lines.subList(0, searchStartLine - offset + 1);
                }

                // If not searching forward we will reverse the buffer for
                // searching. Resulting line numbers will be calculated back.
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

        return line;

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

    private int searchForRegExp(List<Message> buffer, String searchString) {

        // We remove the obligatory line feed at the end
        Pattern pattern = Pattern.compile(searchString, Pattern.UNICODE_CASE | Pattern.CASE_INSENSITIVE);

        Iterator<Message> lines = buffer.iterator();
        while (lines.hasNext()) {
            Message msg = lines.next();
            if (msg.getMainMessage() != null) {
                if (pattern.matcher(msg.getMainMessage()).find()) {
                    return msg.getLine();
                }
            }
            if (msg.getDetails() != null) {
                if (pattern.matcher(msg.getDetails()).find()) {
                    return msg.getLine();
                }
            }
        }
        return -1;

    }

    private int searchForTerm(List<Message> buffer, String searchString) {

        Iterator<Message> lines = buffer.iterator();
        while (lines.hasNext()) {
            Message msg = lines.next();
            if (msg.containsIgnoreCase(searchString)) {
                return msg.getLine();
            }
        }
        return -1;

    }
    
    private void updateAppenderFilesData() {
        getPage(getLinesCount(), 1, LogLevel.LEVEL_ALL);
    }

}
