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

import org.apache.log4j.Logger;

import de.innovationgate.wga.config.AccessLog;
import de.innovationgate.wgpublisher.WGACore;


public class WGALoggerWrapper {
    
        public static final Logger LOG = Logger.getLogger("wga.accesslog");

		private WGALogger logger;

		public WGALoggerWrapper(AccessLog config, WGACore core) throws WGALoggerException {
		    
			Class loggerClass;
            try {
                loggerClass = core.getLibraryLoader().loadClass(config.getImplClassName());
            }
            catch (ClassNotFoundException e) {
                throw new WGALoggerException("Unknown class for access logger: " + config.getImplClassName());
            }
            
			try {
                logger = (WGALogger) loggerClass.newInstance();
            }
            catch (Exception e) {
                throw new WGALoggerException("Exception instantiating logger class " + config.getImplClassName(), e);
            }

            logger.init(config, core);
		}
		
		public void logRequest(ServletRequest request) {
			try {
    			if (this.logger == null) {
    				return;
    			}
    			
  				this.logger.logRequest(request);

            } 
			catch (Throwable e) {
                Logger.getLogger("wga.logger").error("Unable to log request.", e);
            }			
		}

		/**
		 * Returns the logger.
		 * @return WGALogger
		 */
		public WGALogger getLogger() {
			return logger;
		}
		
		public void close() {
			try {
                if (logger != null) {
                	logger.close();
                }
            }
            catch (Throwable e) {
                Logger.getLogger("wga.logger").error("Unable to close access logger.", e);       
            }
		}

}


