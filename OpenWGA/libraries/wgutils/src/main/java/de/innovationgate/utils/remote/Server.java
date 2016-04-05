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


import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;

import org.apache.log4j.Category;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.ThreadPool;
import de.innovationgate.utils.remote.commands.Command;
import de.innovationgate.utils.remote.commands.CommandException;
import de.innovationgate.utils.remote.commands.Connect;
import de.innovationgate.utils.remote.commands.Disconnect;

/**
 * The server object of the framework.
 * This is a {@link Runnable} that can be issued to any thread to run the server.
 */
public class Server implements Runnable {
        
        protected ServerSocket _server;        
        //protected ExecutorService _workerPool = Executors.newFixedThreadPool(5); -> Java 5
        protected ThreadPool _workerPool = new ThreadPool(5);

        private Category _log = Category.getInstance(this.getClass());
        private int _port;
        private CommandHandler _commandHandler;
        
        /**
         * The default timeout for socket connections
         */
        private static final int DEFAULT_CLIENT_SOCKET_TIMEOUT = 10000;
        
        private int _clientSocketTimeout = DEFAULT_CLIENT_SOCKET_TIMEOUT;
        
        private String _version = "undefined";
        
        /**
         * Creates a server that will run on the given port
         * @param port The port that the server will listen on
         * @param commandHandler The handler of commands to use
         */
        public Server(int port, CommandHandler commandHandler) {
            try {
                _commandHandler = commandHandler;
                _port = port;
                _server = new ServerSocket(_port);
                _log.info("Listening for client connections on port '" + _port + "'.");
                
            }
            catch (Exception e) {
                _log.error("Unable to start server on port '" +_port + "'.", e);
            }
        }   
        
        public void run() {
            try {
                // run forever
                while (true) {
                    Socket client = _server.accept();
                    // set timeout on socket
                    client.setSoTimeout(_clientSocketTimeout);
                    _log.info("client " + client.getInetAddress() + "  connected.");
                    _workerPool.execute(new ClientWorker(client));
                }
            } catch (Exception e) {
                _log.error(e);
            }
        }
        
        private class ClientWorker implements Runnable {
            
            Socket _client;
            XStream _xstream;
            int _failures;
            
            private static final int MAX_FAILURES = 5;
            
            public ClientWorker(Socket client) {
                _client = client;
                _xstream = new XStream(new Dom4JDriver()); 
                _failures = 0;
            }

            public void run() {                
                while (true) {
                    if (_failures > MAX_FAILURES) {
                        _log.error("Too many communication failures with client '" + _client.getInetAddress() + "'. Closing connection.");
                        break;
                    }
                    
                    // retrieve command from client
                    Command command = null;
                    try {
                        command = (Command) _xstream.fromXML(new InputStreamReader(_client.getInputStream()));
                    } catch (Exception e) {
                        if (e.getCause() != null) {
                            Throwable cause = e.getCause();
                            if (cause instanceof SocketTimeoutException) {
                                // client connection timed out
                                _log.warn("Connection for client '" + _client.getInetAddress() + "' timed out. Sending disconnect command.");
                                // send disconnect command to client
                                command = new Disconnect();
                            } else {
                                _log.error("Unable to retrieve command from client '" + _client.getInetAddress() + "'.", e);
                                _failures++;
                            }
                        } else {
                            _log.error("Unable to retrieve command from client '" + _client.getInetAddress() + "'.", e);
                            _failures++;
                        }
                        
                    }
                    
                    // execute command
                    if (command != null) {
                        try {
                        	if (command instanceof Connect) {
                                _log.info("connect command from client '" + _client.getInetAddress() + "' recieved");
                                String clientVersion = ((Connect)command).getClientVersion(); 
                                if (clientVersion.equals(getVersion())) {
                                	_log.info("client version accepted");
                                	sendResponse(new Boolean(true));
                                } else {
                                	throw new CommandException("Invalid client version '" + clientVersion + "'.");
                                }                                
                            } else if (command instanceof Disconnect) {
                                _log.info("disconnect command from client '" + _client.getInetAddress() + "' recieved.");
                                sendResponse(new Boolean(true));
                                break;
                            } else {
                            	_log.info("executing command '" + command.getClass().getName() + "' for client '" + _client.getInetAddress() + "'.");                                        
                            	Object response = _commandHandler.execute(command);
                            	_log.info("sending response to client '" + _client.getInetAddress() + "'.");
                            	sendResponse(response);
                            }
                        } catch (CommandException e) { 
                            _log.warn("executing command '" + command.getClass().getName() + "' for client '" + _client.getInetAddress() + "' failed.", e);
                            // send exception to client
                            sendResponse(e);
                        } catch (Exception e) {
                            _log.error("Unexpected error in ClientWorker.run().", e);
                            // send exception to client
                            sendResponse(new RemoteException("Unexpected error in ClientWorker", e));
                        }
                    }
                }
                try {
                    _client.close();
                }
                catch (IOException e) {
                    _log.error("Unable to close client-connection.");
                }
            }            
            
            private void sendResponse(Object response) {
                try {
                    _xstream.toXML(response, new OutputStreamWriter(_client.getOutputStream()));
                }
                catch (Exception e) {
                    _log.error("Unable to respond to client '" + _client.getInetAddress() + "'.", e);
                }        
            }        
            
        }

        /**
         * Returns the client timeout for socket connections in milliseconds
         */
        public int getClientSocketTimeout() {
            return _clientSocketTimeout;
        }

        /**
         * Sets the client timeout for socket connections in milliseconds. The default is defined as {@link #DEFAULT_CLIENT_SOCKET_TIMEOUT}
         * @param clientSocketTimeout
         */
        public void setClientSocketTimeout(int clientSocketTimeout) {
            _clientSocketTimeout = clientSocketTimeout;
        }

		/**
		 * Returns the server version
		 */
		public String getVersion() {
			return _version;
		}

        /**
         * Sets the version of the server. 
         * The server will deny clients that transmit different versions
         * than the one set here.
         * @param version A user defined version string.
         */
		public void setVersion(String version) {
			_version = version;
		}

    }
