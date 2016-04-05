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

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.Set;

import org.apache.log4j.FileAppender;
import org.apache.log4j.Layout;
import org.apache.log4j.spi.ErrorHandler;
import org.apache.log4j.spi.LoggingEvent;
import org.apache.log4j.helpers.LogLog;

import de.innovationgate.utils.WGUtils;

public class AppLogAppender extends FileAppender {
    
    public class AppenderFile {
        
        private int _linesCount = -1;
        private int _startingLine = -1;
        private File _file;
        
        public AppenderFile(File file) {
            _file = file;
        }

        public int getLinesCount() {
            return _linesCount;
        }

        public void setLinesCount(int linesCount) {
            _linesCount = linesCount;
        }

        public File getFile() {
            return _file;
        }

        public int getStartingLine() {
            return _startingLine;
        }

        public void setStartingLine(int startingLine) {
            _startingLine = startingLine;
        }
        
    }

    private static final SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyy-DDD-HHmmss-SSS");
    
    private LinkedList<AppenderFile> activeFiles = new LinkedList<AppenderFile>();
    private volatile File currentFile;
    
    private long maxFileSize = 1024 * 1024 * 10;
    
    private int filesToKeep = 10;

    // ----------------------------------------------------- Instance Variables

    /**
     * The directory in which log files are created.
     * Wihtout a leading slash, this is relative to the Tomcat home directory.
     */
    private String m_directory = "logs";

    /**
     * The prefix that is added to log file filenames.
     */
    private String m_prefix = "tomcat.";

    /**
     * The suffix that is added to log file filenames.
     */
    private String m_suffix = ".log";

    /**
     * The File representation of the directory in which log files are created.
     */
    private File m_path = null;

    /**
     * A calendar object for manipulating dates and times.
     */
    private Calendar m_calendar = null;

    /**
     * The number of milliseconds since 1/1/1970 when tomorrow starts (local time).
     */
    private long m_tomorrow = 0L;

    // ----------------------------------------------------------- Constructors

    /** Creates a new <code>DatedFileAppender</code>
    with the specified characteristics.

    @param directory the directory in which log files are created.
    @param prefix the prefix that is added to log file filenames.
    @param suffix the suffix that is added to log file filenames.
    */
    public AppLogAppender(String directory, String prefix, String suffix) {
        m_directory = directory;
        m_prefix = prefix;
        m_suffix = suffix;
        activateOptions();
    }

    // ------------------------------------------------------------- Properties

    /**
     * Return the directory in which we create log files.
     */
    public String getDirectory() {
        return m_directory;
    }

    /**
     * Set the directory in which we create log files.
     *
     * @param directory The new log file directory
     */
    public void setDirectory(String directory) {
        m_directory = directory;
    }

    /**
     * Return the log file prefix.
     */
    public String getPrefix() {
        return m_prefix;
    }

    /**
     * Set the log file prefix.
     *
     * @param prefix The new log file prefix
     */
    public void setPrefix(String prefix) {
        m_prefix = prefix;
    }

    /**
     * Return the log file suffix.
     */
    public String getSuffix() {
        return m_suffix;
    }

    /**
     * Set the log file suffix.
     *
     * @param suffix The new log file suffix
     */
    public void setSuffix(String suffix) {
        m_suffix = suffix;
    }

    // --------------------------------------------------------- Public Methods


    /**
       Called once all options have been set on this Appender.
       Calls the underlying FileLogger's start() method.
    */
    public void activateOptions() {
    if (m_prefix == null) {
        m_prefix = "";
    }
    if (m_suffix == null) {
        m_suffix = "";
    }
    if ((m_directory == null) || (m_directory.length() == 0)) {
        m_directory = ".";
    }
        m_path = new File(m_directory);
        if (!m_path.isAbsolute()) {
            String base = System.getProperty("catalina.base");
        if (base != null) {
        m_path = new File(base, m_directory);
        }
    }
        m_path.mkdirs();
        if (m_path.canWrite()) {
        m_calendar = Calendar.getInstance();        // initialized
    }
    }

    /**
       Called by AppenderSkeleton.doAppend() to write a log message formatted
       according to the layout defined for this appender.
    */
    public void append(LoggingEvent event) {
    if(this.layout == null) {
        errorHandler.error("No layout set for the appender named ["+ name+"].");
        return;
    }
    if (this.m_calendar == null) {
        errorHandler.error("Improper initialization for the appender named ["+ name+"].");
        return;
    }
    
    synchronized(this) {
        if (currentFile == null || currentFile.length() > maxFileSize) {
            currentFile = new File(m_path, m_prefix + timestamp() + m_suffix);
            addActiveFile(currentFile);
            this.fileName = currentFile.getAbsolutePath();
            super.activateOptions();            // close current file and open new file
        }
    }
    
    if(this.qw == null) {               // should never happen
        errorHandler.error("No output stream or file set for the appender named ["+
                   name+"].");
        return;
    }
    subAppend(event);
    }

    private String timestamp() {
        return TIMESTAMP_FORMAT.format(new Date());
    }

    private void addActiveFile(File newFile) {
        activeFiles.add(new AppenderFile(newFile));
        if (activeFiles.size() > filesToKeep) {
            AppenderFile file = (AppenderFile) activeFiles.removeFirst();
            if (file.getFile().delete() == false) {
                System.err.println("WGA unable to delete applog file " + file.getFile().getPath());
            }
        }
    }

    /**
     * Sets a calendar to the start of tomorrow,
     * with all time values reset to zero.
     *
     * <p>Takes advantage of the fact that the Java Calendar implementations
     * are mercifully accommodating in handling non-existent dates. For example,
     * June 31 is understood to mean July 1. This allows you to simply add one
     * to today's day of the month to generate tomorrow's date. It also works
     * for years, so that December 32, 2004 is converted into January 1, 2005.</p>
     */
    public static void tomorrow(Calendar calendar) {
    int year = calendar.get(Calendar.YEAR);
    int month = calendar.get(Calendar.MONTH);
    int day = calendar.get(Calendar.DAY_OF_MONTH) + 1;
    calendar.clear();           // clear all fields
    calendar.set(year, month, day);     // set tomorrow's date
    }

    public LinkedList<AppenderFile> getActiveFiles() {
        return new LinkedList(activeFiles);
    }

    public int getFilesToKeep() {
        return filesToKeep;
    }

    public void setFilesToKeep(int filesToKeep) {
        this.filesToKeep = filesToKeep;
    }

    public long getMaxFileSize() {
        return maxFileSize;
    }

    public void setMaxFileSize(long maxFileSize) {
        this.maxFileSize = maxFileSize;
    }
    
}
