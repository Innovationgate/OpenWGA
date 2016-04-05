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

import java.util.Collections;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;

/**
 * The default Workflow engine for WGAPI, which is a no-op that just releases contents when they are published.
 */
public class WGDefaultWorkflowEngine implements WGWorkflowEngine {
	
	class WGDefaultWorkflow implements WGWorkflow {
		
		private WGContent content;
		
		private WGDefaultWorkflow(WGContent content) {
			this.content = content;
		}
		
		
		/**
		 * @see de.innovationgate.webgate.api.WGWorkflow#approve(String)
		 */
		public boolean approve(String comment) {
			return true;
		}

		/**
		 * @see de.innovationgate.webgate.api.WGWorkflow#getWorkflowRole()
		 */
		public int getWorkflowRole() {
			return ROLE_ADMINISTRATOR;
		}

		/**
		 * @see de.innovationgate.webgate.api.WGWorkflow#publish(String)
		 */
		public boolean publish(String comment) {
			return true;
		}

		/**
		 * @see de.innovationgate.webgate.api.WGWorkflow#reject(String)
		 */
		public void reject(String comment) {}

		/**
		 * @throws WGAPIException 
		 * @see de.innovationgate.webgate.api.WGWorkflow#releaseImmediately(String)
		 */
		public void release(String comment) throws WGAPIException {
		}
        

		/**
		 * @throws WGAPIException 
		 * @see de.innovationgate.webgate.api.WGWorkflow#archive(String)
		 */
		public void archive(String comment) throws WGAPIException {
		}

		/**
		 * @see de.innovationgate.webgate.api.WGWorkflow#initialize()
		 */
		public void initialize() {}


        public boolean isApprovableByUser() throws WGAPIException {
            return true;
        }

	}

	public String getDescription() {
		return "This workflow engine directly releases published content";
	}

	public String getName() {
		return "Direct release workflow";
	}

	public WGWorkflow getWorkflow(WGContent content) {
		return new WGDefaultWorkflow(content);
	}



	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.workflow.WGWorkflowEngine#createWorkflowDefinition(java.lang.String)
	 */
	public Object createWorkflowDefinition(String name) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.workflow.WGWorkflowEngine#getWorkflowDefinitions(de.innovationgate.webgate.api.WGDatabase)
	 */
	public Map getWorkflowDefinitions() {
		return Collections.emptyMap();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.workflow.WGWorkflowEngine#init(de.innovationgate.webgate.api.WGDatabase)
	 */
	public void init(WGDatabase db) {}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.workflow.WGWorkflowEngine#saveWorkflowDefinition(java.lang.Object)
	 */
	public boolean saveWorkflowDefinition(Object wfDef) { return false;}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.workflow.WGWorkflowEngine#removeWorkflowDefinition(java.lang.Object)
	 */
	public boolean removeWorkflowDefinition(Object wfDef) { return false; }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.workflow.WGWorkflowEngine#getWorkflowDefinitionType()
	 */
	public Class getWorkflowDefinitionType() {
		return null;
	}

}
