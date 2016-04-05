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
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.apache.log4j.Logger;

import de.innovationgate.wga.common.Constants;
import de.innovationgate.wgpublisher.WGACore;


public class LogServer extends Thread{

    public static final String SYSPROPERTY_REMOTE_APPLOG_PORT = "de.innovationgate.wga.remotelog.port";

    private static final String LOGSERVER_VERSION = "1.1";
    
	private ServerSocket _server;
    private WGACore _core;
    private boolean _active;

	public LogServer(WGACore core) {
	    _core = core;
	}
	
	private static int _logserverPort = Constants.LOGSERVER_PORT;
	static {
	    String portStr = System.getProperty(SYSPROPERTY_REMOTE_APPLOG_PORT);
	    try {
	        if (portStr != null) {
	            _logserverPort = Integer.parseInt(portStr);
	        }
	    }
	    catch (NumberFormatException e) {
	        Logger.getLogger("wga.logserver").error("Cannot parse logserver port as an integer: " + portStr);
	    }
	}

	public void run() {
		try {
			_server = new ServerSocket(_logserverPort);
			_core.getLog().info("Remote application log available on port " + _logserverPort);
			_active = true;
			
			while(true) {
				Socket s = _server.accept();
				LineNumberReader reader = new LineNumberReader(new InputStreamReader(s.getInputStream(), "UTF-8"));
				String admin = reader.readLine();
				String pwd = reader.readLine();
				String command = reader.readLine();
				Writer out = new OutputStreamWriter(s.getOutputStream(), "UTF-8");
				if (!_core.isAdminLogin(admin, pwd)) {
				    out.write("wronglogin\n");
				    out.flush();
				    s.close();
				    continue;
				}
				
				if (command.equals("tail")) {
				    out.write("ok:" + LOGSERVER_VERSION + "\n");
				    out.flush();
				    new TailClientThread(this, s, new AppLogTailHandler(_core));
				}
				else if (command.equals("log")) {
				    out.write("ok:" + LOGSERVER_VERSION + "\n");
				    out.flush();
	                new LogClientThread(this, s, new AppLogHandler(_core));
				}
				else {
				    out.write("unknown\n");
				    out.flush();
				    Logger.getLogger("wga.logserver").error("Unknown logserver command: " + command);
				}
				out.flush();
			}
		} 
		catch (SocketException e) {
		    if (isActive()) {
		        Logger.getLogger("wga.logserver").error("WGA remote applog server cancelled", e);
		    }
		}
		catch (Exception e) {
		    Logger.getLogger("wga.logserver").error("WGA remote applog server cancelled", e);
		}
	}

    public void shutdown() throws IOException {
        _active = false;
        if (_server != null) {
            _server.close();
        }
        _core.getLog().info("Remote application log is shutdown");
    }

    public boolean isActive() {
        return _active;
    }
}
