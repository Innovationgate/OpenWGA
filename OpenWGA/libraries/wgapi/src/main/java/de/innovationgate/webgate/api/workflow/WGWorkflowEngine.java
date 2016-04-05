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
package de.innovationgate.webgate.api.workflow;

import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGConfigurationException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;

/**
 * Interface for a worklow engine, generating workflow objects for specific contents.
 * 
 * Every workflow engine is specific to a WGDatabase object.
 * A workflow engine maintains workflow definitions, providing methods to create, modify and delete them.
 * It is up to the concrete engine what objects to use for definition. 
 * 
 */
public interface WGWorkflowEngine {
	
	/**
	 * Called when the engine object is created to do initial operations.
	 * @param db The database that this engine will be bound to
	 * @throws WGWorkflowException
	 * @throws WGBackendException 
	 * @throws WGConfigurationException 
	 */
	public void init(WGDatabase db) throws WGAPIException;
	/**
	 * Creates a workflow object for the given content
	 * @param content
	 * @throws WGAPIException 
	 */
	public WGWorkflow getWorkflow(WGContent content) throws WGAPIException;
	/**
	 * Returns the name of the workflow engine
	 */
	public String getName();
	/**
	 * Returns a description of the workflow engine
	 */
	public String getDescription();
	/**
	 * Returns a map of the stored workflow definitions, keyed by their names.
	 */
	public Map getWorkflowDefinitions();
	/**
	 * Create a workflow definition.
	 * @param name Name of the workflow definition.
	 * @throws WGWorkflowException
	 * @throws WGAuthorisationException
	 * @throws WGBackendException 
	 * @throws WGAPIException 
	 */
	public Object createWorkflowDefinition(String name) throws WGAPIException;
	/**
	 * Saves the state of a workflow definition object
	 * @param wfDef The workflow definition object to save
	 * @throws WGWorkflowException
	 */
	public boolean saveWorkflowDefinition(Object wfDef) throws WGAPIException;
	/**
	 * Deletes a workflow definition.
	 * @param wfDef The workflow definition to delete
	 * @throws WGWorkflowException
	 * @throws WGBackendException 
	 */
	public boolean removeWorkflowDefinition(Object wfDef) throws WGAPIException;
	/**
	 * Returns the class that is used to represent workflow definitions in this engine
	 */
	public Class getWorkflowDefinitionType();

}
