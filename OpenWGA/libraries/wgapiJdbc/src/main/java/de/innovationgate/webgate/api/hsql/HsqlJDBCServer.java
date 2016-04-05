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

package de.innovationgate.webgate.api.hsql;

import java.io.IOException;
import java.io.PrintWriter;

import org.hsqldb.Server;
import org.hsqldb.persist.HsqlProperties;

public class HsqlJDBCServer {
    
    private Server _hsqlServer;
    
    public HsqlJDBCServer(String hsqlPath, int port) throws IOException {
        _hsqlServer = new Server();
        _hsqlServer.setNoSystemExit(true);
        _hsqlServer.setPort(port);
        _hsqlServer.setRestartOnShutdown(false);
        _hsqlServer.setSilent(true);
        _hsqlServer.setTls(false);
        _hsqlServer.setLogWriter(new PrintWriter(System.out));
        _hsqlServer.setErrWriter(new PrintWriter(System.out));
        _hsqlServer.setAddress(null);
        
        HsqlProperties hsqlProps = new HsqlProperties();
        hsqlProps.setProperty("server.remote_open", true);
        _hsqlServer.setProperties(hsqlProps);
        
        _hsqlServer.setDatabasePath(0, hsqlPath);
        _hsqlServer.setDatabaseName(0, "");
        

        _hsqlServer.start();
    }
    
    public int getPort() {
        return _hsqlServer.getPort();
    }
    
    public void shutdown() {
        _hsqlServer.shutdown();
    }

}
