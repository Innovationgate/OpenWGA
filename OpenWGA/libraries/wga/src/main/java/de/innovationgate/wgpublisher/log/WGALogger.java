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


import javax.servlet.ServletRequest;

import de.innovationgate.wga.config.AccessLog;
import de.innovationgate.wgpublisher.WGACore;



/**
 * Interface to implement a WGA access logger
 */
public interface WGALogger {
    
    public void init(AccessLog config, WGACore core) throws WGALoggerException;

	/**
	 * Receives logger requests to log them.
	 */
	public void logRequest(ServletRequest request) throws WGALoggerException;
	
	/**
	 * Called when the logger is closed
	 */
	public void close() throws WGALoggerException;

}
