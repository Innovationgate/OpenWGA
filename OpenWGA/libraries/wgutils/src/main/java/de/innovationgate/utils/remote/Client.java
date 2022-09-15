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
package de.innovationgate.utils.remote;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.utils.remote.commands.Command;
import de.innovationgate.utils.remote.commands.CommandException;
import de.innovationgate.utils.remote.commands.Connect;
import de.innovationgate.utils.remote.commands.Disconnect;

/**
 * The client object of the framework
 */
public class Client {        
    
    private String _host;
    private int _port;
    
    private String _version = "undefined";
    
    private XStream _xstream;
    
    private Socket _server;
            
    /**
     * Create a Client for the given server. The client is not yet connected on creation.
     * @param host The server host
     * @param port The server port
     */
    public Client(String host, int port) {
        _host = host;
        _port = port;
        _xstream = XStreamUtils.createXStream();        
    }
    
    /**
     * Create a Client for the given server. The client is not yet connected on creation.
     * @param host The server host
     * @param port The server port
     * @param classLoader The classloader to use for serialization of command-beans
     */
    public Client(String host, int port, ClassLoader classLoader) {
        _host = host;
        _port = port;
        _xstream = XStreamUtils.createXStream();
        if (classLoader != null) {
        	_xstream.setClassLoader(classLoader);
        }
    }
    
    /**
     * Connect to the server
     * @throws RemoteException
     */
    public void connect() throws RemoteException {
        try {
            _server = new Socket(_host, _port);
            Connect connect = new Connect(_version);
            sendCommand(connect);
        }
        catch (Exception e) {
            throw new RemoteException("Unable to connect to server." ,e);
        }
    }
    
    
    /**
     * Disconnect from the server
     * @throws RemoteException
     * @throws CommandException
     */
    public void disconnect() throws RemoteException, CommandException {
        try {
            Disconnect disconnect = new Disconnect();
            sendCommand(disconnect);
            if (disconnect.disconnected()) {
                _server.close();
            } else {
                throw new RemoteException("Unable to disconnect from server.");
            }
        } catch (RemoteException e) {
            throw e;
        } catch (CommandException e) {
            throw e;
        }
        catch (Exception e) {
            throw new RemoteException("Unable to disconnect from server.", e);
        }
    }
    
    
    /**
     * Sends a command object to the server. Response information will be available at the command object.
     * @param command The command object.
     * @throws RemoteException
     * @throws CommandException
     */
    public synchronized void sendCommand(Command command) throws RemoteException, CommandException {
        try {
            if (command == null) {
                throw new RemoteException("No command given.");
            }
            
            _xstream.toXML(command, new OutputStreamWriter(_server.getOutputStream()));             
            Object response = _xstream.fromXML(new InputStreamReader(_server.getInputStream()));
            if (response instanceof CommandException) {
                throw (CommandException)response;
            } else if (response instanceof RemoteException) {
                throw (RemoteException)response;
            } else {
                command.parseResponse(response);
            }
        } catch (CommandException e) {
            throw e;
        } catch (RemoteException e) {
            throw e;
        } catch (Exception e) {
            throw new RemoteException("Unexpected error sending command '" + command.getClass().getName() + "'.", e);
        }
    }

	/**
     * Sets the version of the client. 
     * If this is set prior to connecting the version will be transmitted on connection. The server then may choose
     * to indicate that it is unable to work with the client version.
	 * @param version A user defined version string.
	 */
	public void setVersion(String version) {
		_version = version;
	}

	/**
	 * Returns the user-defined version of the client
	 */
	public String getVersion() {
		return _version;
	}

	public String getHost() {
		return _host;
	}

	public int getPort() {
		return _port;
	}
}
