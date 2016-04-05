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

import de.innovationgate.webgate.api.WGAPIException;

/**
 * Interface for workflows allowing auto-approve and serving the necessary interface methods for this to accomplish
 */
public interface WGAutoApprovalCapableWorkflow extends WGWorkflow {
    
    /**
     * Returns the role of the current user on the current workflow level only. This excludes admin approver rights.
     * @return The workflow role as constant {@link WGWorkflow#ROLE_APPROVER} or {@link WGWorkflow#ROLE_NONE}
     * @throws WGAPIException
     */
    public int getWorkflowLevelRole() throws WGAPIException;
    
    /**
     * Returns if the current user has already approved the current workflow
     * @throws WGAPIException
     */
    public boolean isAlreadyApprovedByUser() throws WGAPIException;

}
