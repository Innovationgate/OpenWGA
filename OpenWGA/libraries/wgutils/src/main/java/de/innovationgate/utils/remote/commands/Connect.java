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
package de.innovationgate.utils.remote.commands;

import de.innovationgate.utils.remote.Client;


/**
 * command to connect client to server.
 * This command is implicitly sent by the {@link Client} when calling {@link Client#connect()}.
 *
 */
public class Connect implements Command {
      
    
    private Boolean _connected;
	private String _clientVersion;


	/**
     * Creates a connect command for the given client version
	 * @param clientVersion
	 */
	public Connect(String clientVersion) {
        super();
        _clientVersion = clientVersion;
    }

    public void parseResponse(Object response) {
        _connected = (Boolean) response;        
    }

    /**
     * If client was connected this is true after sending the command
     */
    public boolean connected() {
        return _connected.booleanValue();
    }    
    
    /**
     * Returns the version of the Client
     */
    public String getClientVersion() {
		return _clientVersion;
	}

}
