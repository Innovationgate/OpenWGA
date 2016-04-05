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
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.SocketException;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.LogHandler;
import de.innovationgate.wga.common.LogLevel;

public class LogClientThread extends Thread {
	
	private static final Pattern searchPattern = Pattern.compile("search:(\\d+):(true|false):(.*)");

	private final LogHandler handler;
	private Socket socket;
	private PrintWriter w;
	private BufferedReader r;

    private LogServer server;

	public LogClientThread(LogServer server, Socket s, LogHandler handler) {
	    this.server = server;
		this.socket = s;
		this.handler = handler;
		start();
	}
	
	public void run() {
	    Thread.currentThread().setName("WGA Remote Applog Client Thread for " + socket.getInetAddress().toString());
	    Logger.getLogger("wga.logserver").info("Serving remote applog client on " + socket.getInetAddress().toString());
		try {
			w = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), "UTF-8"), true);
			r = new BufferedReader(new InputStreamReader(socket.getInputStream(), "UTF-8"));
			String line;
			while ((line = r.readLine()) != null) {
				if (line.startsWith("get:")) {
					processGetRequest(line);
				} 
				else if(line.startsWith("search:")) {
					processSearchRequest(line);
				}
				else if (line.startsWith("lastpage:")) {
				    processLastPageRequest(line);
				}
				else if (line.startsWith("previouspage:")) {
				    processPreviousPageRequest(line);
				}
				
			}
			Logger.getLogger("wga.logserver").info("Connection to remote applog client on " + socket.getInetAddress().toString() + " is closed");
		} 
		catch (SocketException e) {
		    if (server.isActive()) {
		        Logger.getLogger("wga.logserver").error("Remote applog client thread stopped irregularly", e);
		    }
		}
		catch (IOException e) {
		    Logger.getLogger("wga.logserver").error("Remote applog client thread stopped irregularly", e);
		}
		finally {
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

    private void processPreviousPageRequest(String line) throws IOException {
        
        List tokens = WGUtils.deserializeCollection(line, ":");
        
        String[] range = line.substring(line.indexOf(":") + 1).split("-");
        int from = Integer.parseInt((String) tokens.get(1));
        int size = Integer.parseInt((String) tokens.get(2));
        
        LogLevel level = LogLevel.getLevel((String) tokens.get(3));
        List page = handler.getPreviousPage(from, size, level);
        Iterator lines = page.iterator();
        while (lines.hasNext()) {
            w.println(lines.next());
        }
        w.println("##END-LOG##");
        w.flush();
        
    }

    private void processLastPageRequest(String line) throws IOException {
        
        List tokens = WGUtils.deserializeCollection(line, ":");
        int size = Integer.parseInt((String) tokens.get(1));
        LogLevel level = LogLevel.getLevel((String) tokens.get(2));
        
        List page = handler.getLastPage(size, level);
        Iterator lines = page.iterator();
        while (lines.hasNext()) {
            w.println(lines.next());
        }
        w.println("##END-LOG##");
        w.flush();
        
    }

    private void processSearchRequest(String line) throws IOException {
        
        List tokens = WGUtils.deserializeCollection(line, ":");
        int offset = Integer.parseInt((String) tokens.get(1)); 
        int size = Integer.parseInt((String) tokens.get(2));
        int searchType = Integer.parseInt((String) tokens.get(3)); 
        boolean forward = Boolean.valueOf((String) tokens.get(4)).booleanValue();
        String searchString;
        if (tokens.size() > 6) {
            searchString = WGUtils.serializeCollection(tokens.subList(5, tokens.size() -1), ":");
        }
        else {
            searchString = (String) tokens.get(5);
        }
        
        List page = handler.search(offset, size, searchString, searchType, forward);
        
        Iterator lines = page.iterator();
        while (lines.hasNext()) {
            w.println(lines.next());
        }
        w.println("##END-LOG##");
        w.flush();
    }

    private void processGetRequest(String line) throws IOException {
        
        List tokens = WGUtils.deserializeCollection(line, ":");
        
        String[] range = line.substring(line.indexOf(":") + 1).split("-");
        int from = Integer.parseInt((String) tokens.get(1));
        int size = Integer.parseInt((String) tokens.get(2));
        
        LogLevel level = LogLevel.getLevel((String) tokens.get(3));
        List page = handler.getPage(from, size, level);
        Iterator lines = page.iterator();
        while (lines.hasNext()) {
        	w.println(lines.next());
        }
        w.println("##END-LOG##");
        w.flush();
    }
}
