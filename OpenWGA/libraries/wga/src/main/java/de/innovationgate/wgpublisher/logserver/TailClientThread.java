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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;

import de.innovationgate.wga.common.LogLevel;

public class TailClientThread extends Thread {

	protected static final long interval = 1000;
	private final TailHandler handler;
	private Socket socket;
	private PrintWriter w;
	private BufferedReader r;
    private LogServer server;

	public TailClientThread(LogServer server, Socket s, TailHandler handler) {
	    this.server = server;
		this.socket = s;
		this.handler = handler;
		start();
	}

	public void run() {
	    
	    Thread.currentThread().setName("WGA Remote Applog Tail Client Thread for " + socket.getInetAddress().toString());
	    Logger.getLogger("wga.logserver").info("Serving remote applog tail client on " + socket.getInetAddress().toString());
	    
	    
		try {
			w = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), "UTF-8"), true);
			socket.setSoTimeout(100);
			
    		while (true) {
    		    
    		    // Testing if the connection still is valid. Trying a read is the only way to do that.
    		    try {
                    InputStream inputStream = socket.getInputStream();
                    try {
                        if (inputStream.read() == -1) {
                            break;
                        }
                    }
                    catch (SocketException e) {
                        break;
                    }
                }
                catch (SocketTimeoutException e) {
                }
                
                try {
    				Thread.sleep(interval);
    			} catch (InterruptedException e) {
    				e.printStackTrace();
    			}
    			
    			Iterator lines = handler.refresh().iterator();
				while (lines.hasNext()) {
					w.println(lines.next());
				}
    		}
    		Logger.getLogger("wga.logserver").info("Connection to remote applog tail client on " + socket.getInetAddress().toString() + " is closed");
		}
		catch (java.net.SocketException e) {
            if (server.isActive()) {
                Logger.getLogger("wga.logserver").error("Remote applog tail client thread stopped irregularly", e);
            }
        }
		catch (IOException e1) {
            Logger.getLogger("wga.logserver").error("Remote applog tail client thread stopped irregularly", e1);
        }
		finally {
		    handler.shutdown();
		    if (!socket.isClosed()) {
		        try {
                    socket.close();
                }
                catch (IOException e) {
                    Logger.getLogger("wga.logserver").error("Error closing remote applog socket", e);        
                }
		    }
		}
		
	}

}
