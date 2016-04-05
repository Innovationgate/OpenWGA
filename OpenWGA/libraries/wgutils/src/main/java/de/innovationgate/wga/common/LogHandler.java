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

package de.innovationgate.wga.common;

import java.io.IOException;
import java.util.List;


/**
 * Interface for functionalities retrieving data from the application log
 */
public interface LogHandler {
    
    /**
     * Search type which searches for the given string as plaintext
     */
    public static final int SEARCHTYPE_PLAINTEXT = 1;
    /**
     * Searchtype which searches for the given string as regular expression, matching a single applog line.
     */
    public static final int SEARCHTYPE_REGEXP = 2;

    /**
     * Gets an applog page. Each entry in returned list represents an applog line.
     * @param from Starting line of the page
     * @param size Number of lines
     * @param level Lowest loglevel to include in the page. Lower levels will be filtered.
     * @return The applog page as List of Strings
     * @throws IOException
     */
    public List getPage(int from, int size, LogLevel level) throws IOException;
    
    /**
     * Returns the end of the applog
     * @param size Number of lines to return
     * @param level Lowest loglevel to include in the page. Lower levels will be filtered.
     * @return The applog page as List of Strings
     * @throws IOException
     */
    public List getLastPage(int size, LogLevel level) throws IOException;
    
    /**
     * Returns the previous page from the given start position.
     * Takes care that the result always ends with the line before start that would be shown on the given loglevel filter.
     * @param start The position to whom the previous page should be returned
     * @param size The number of lines to retun
     * @param level Lowest loglevel to include in the page. Lower levels will be filtered.
     * @return The applog page sas List of Strings
     * @throws IOException
     */
    public List getPreviousPage(int start, int size, LogLevel level) throws IOException;

    /**
     * Performs a search on the applog.
     * @param searchStartLine The starting line in applog from which to search
     * @param size The number of result lines to return
     * @param searchString The string to search for.
     * @param searchType The type of search. Use constants SEARCHTYPE_....
     * @param forward When true search direction is forward, when false it is backward in the log
     * @return List of matching line strings
     * @throws IOException
     */
    public List search(int searchStartLine, int size, String searchString, int searchType, boolean forward) throws IOException;

}
