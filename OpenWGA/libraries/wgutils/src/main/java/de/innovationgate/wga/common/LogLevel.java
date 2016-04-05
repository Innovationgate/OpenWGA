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

import java.awt.Color;
import java.util.HashMap;

/**
 * A Log Level Object for the remote application log
  */
public class LogLevel {

    private static java.util.Map<String,LogLevel> levels = new HashMap<String,LogLevel>();
	public static final LogLevel LEVEL_ALL = new LogLevel("All", 0, "all", Color.BLACK);
	public static final LogLevel LEVEL_DEBUG = new LogLevel("Debug", 10, "DEBUG", Color.DARK_GRAY);
	public static final LogLevel LEVEL_INFO = new LogLevel("Info", 20, "INFO", Color.BLACK);
	public static final LogLevel LEVEL_WARN = new LogLevel("Warning", 50, "WARN", new Color(219, 69, 22));
	public static final LogLevel LEVEL_ERROR = new LogLevel("Error", 80, "ERROR", new Color(155, 38, 38));


	public static LogLevel getLevel(String str) {
	    LogLevel level = levels.get(str.toLowerCase());
	    if (level == null) {
	        level = LEVEL_DEBUG;
	    }
	    return level;
	}
	
	private final String _name;
	private final Color _color;
    private String _identifier;
    private int _severity;
    
    public static LogLevel isolateLogLevel(String line, int startColumn, LogLevel defaultLevel) {
        int levelEndPos = line.indexOf(" ", startColumn);
        if (levelEndPos != -1) {
            String levelStr = line.substring(startColumn, levelEndPos);
            LogLevel newLevel = LogLevel.getLevel(levelStr);
            if (newLevel != null) {
                return newLevel;
            }
        }
        return defaultLevel;
        
    }
	 

	LogLevel(String name, int severity, String identifier, Color color) {
		_name = name;
		_severity = severity;
		_identifier = identifier;
		_color = color;
		LogLevel.levels.put(_identifier.toLowerCase(), this);
	}
	
	public Color getColor() {
		return _color;
	}
	
	public String toString() {
		return _name;
	}

    public String getIdentifier() {
        return _identifier;
    }
    
    public boolean isHigherOrEqual(LogLevel level) {
        return (_severity >= level.getSeverity());
    }

    public int getSeverity() {
        return _severity;
    }


    public String getName() {
        return _name;
    }
}
