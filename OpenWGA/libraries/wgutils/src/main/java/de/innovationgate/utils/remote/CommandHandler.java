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

import de.innovationgate.utils.remote.commands.Command;
import de.innovationgate.utils.remote.commands.CommandException;

/**
 * interface for command handlers. A command handler is the serverside object that executes commands.
 *
 */
public interface CommandHandler {
    
    /**
     * called for command execution
     * @param command - command to execute
     * @return response-object must be serializable via XStream
     * @throws CommandException - exception is send back to client and thrown in client code
     */    
    public Object execute(Command command) throws CommandException;
    
}
