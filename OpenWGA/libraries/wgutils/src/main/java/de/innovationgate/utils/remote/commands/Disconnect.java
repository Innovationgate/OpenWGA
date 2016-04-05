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
 * command to disconnect client from server.
 * This command is implicitly sent by the {@link Client} when calling {@link Client#disconnect()}.
 *
 */
public class Disconnect implements Command {
      
    
    private Boolean _disconnected;
    
    public Disconnect() {
        super();
    }

    public void parseResponse(Object response) {
        _disconnected = (Boolean) response;        
    }

    /**
     * If the client was disconnected this is true after sending the command
     */
    public boolean disconnected() {
        return _disconnected.booleanValue();
    }

}
