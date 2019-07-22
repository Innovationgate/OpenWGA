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
package de.innovationgate.wgpublisher.mail;


/**
 * An administrative notification mail
 */
public class WGAMailNotification {
	
	public static final String HEADERFIELD_TYPE = "WGANotificationType";
	
	public static final String TYPE_LOGIN_BLOCKED = "LoginBlocked";
	public static final String TYPE_DATABASE_INITIALISATION_ERRORS = "DatabaseInitialisationErrors";
	public static final String TYPE_LICENSE_VALIDITY = "LicenseValidity";
	public static final String TYPE_MEMORY_THRESHOLD = "MemoryThreshold";
	public static final String TYPE_MEMORY_FATAL = "MemoryThresholdFatal";
	public static final String TYPE_PROBLEM = "HighSeverityProblem";
	public static final String TYPE_CUSTOM = "Custom";
	
	
	public static final String MIME_TYPE_HTML = "text/html";
	public static final String MIME_TYPE_PLAINTEXT = "text/plain";
	
	private String _subject;
	private StringBuffer _message = new StringBuffer();
	private String _type;
	
	private boolean _attachLogfile = false;
	private int _logfileLines = 2000;
	
	/**
	 * Constructor, taking the notification type
	 * @param type
	 */
	public WGAMailNotification(String type) {
		_type = type;
	}
	
	/**
	 * Returns the subject of the mail
	 */
	public String getSubject() {
		return _subject;
	}
	/**
	 * Sets the subject of the mail
	 */
	public void setSubject(String subject) {
		_subject = subject;
	}
	
	/**
	 * Returns the mail body message
	 */
	public StringBuffer getMessage() {
		return _message;
	}
	/**
	 * Sets the mail body message
	 */
	public void setMessage(StringBuffer message) {
		_message = message;
	}
	
	/**
	 * Returns the notification type
	 */
	public String getType() {
		return _type;
	}

	/**
	 * Returns if a part of the application log is to be attached to this mail
	 */
	public boolean isAttachLogfile() {
		return _attachLogfile;
	}
	
	/**
	 * Returns if a part of the application log is to be attached to this mail
	 */
	public void setAttachLogfile(boolean attachLogfile) {
		_attachLogfile = attachLogfile;
	}
	
	/**
	 * Returns the number of lines from the end of the application log to append to this mail
	 */
	public int getLogfileLines() {
		return _logfileLines;
	}
	
	/**
     * Sets the number of lines from the end of the application log to append to this mail
     */
	public void setLogfileLines(int logfileLines) {
		_logfileLines = logfileLines;
	}

	/**
	 * Appends a string to the mail body
	 * @param aString
	 */
	public void append(String aString) {
		_message.append(aString);
	}

	
	/**
	 * Appends information about a throwable to the mail body
	 * @param throwable
	 */
	public void append(Throwable throwable) {

		_message.append("<b>Exception:</b><br>");
		_message.append(extractExceptionName(throwable) + ": ");
		_message.append(throwable.getMessage());
 
		Throwable root = retrieveAbsoulteRootCause(throwable);
		if (root != null) {
			_message.append("<br><b>Root cause:</b><br>");
			_message.append(extractExceptionName(root) + ": ");
			_message.append(root.getMessage());
			
			Throwable trace = throwable;
			_message.append("<br><b>Trace:</b>");	
			while (trace != null) {
				_message.append("<br>-");
				_message.append(extractExceptionName(trace) + ": ");			
				_message.append(trace.getMessage());
				trace = trace.getCause();
			}			
		}
		
				
	}
	
	private Throwable retrieveAbsoulteRootCause(Throwable throwable) {
		Throwable cause = throwable.getCause();
		if (cause == null) {
			return null;
		} else {
			while (cause != null) {
				if (cause.getCause() == null) {
					return cause;
				}
				cause = cause.getCause();				
			}
			return null;
		}
	}
	
	private String extractExceptionName(Throwable throwable) {
		String exceptionName = throwable.getClass().getName();
		exceptionName = exceptionName.substring(exceptionName.lastIndexOf(".") + 1);
		return exceptionName;
	}
	
}
