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
 * Interface for a workflow object.
 * The workflow object is a single instance of a workflow.
 * It is always created in context of a single content object.
 */
public interface WGWorkflow {
	
	/**
	 * User has no role on this workflow level
	 */
	public static int ROLE_NONE = 0;
	/**
	 * User is approver on this workflow level
	 */
	public static int ROLE_APPROVER = 1;
	/**
	 * User is admin reviewer of the workflow
	 */
	public static int ROLE_ADMINISTRATOR = 9;
    /**
     * Item to store past approvers
     */
    public static final String ITEM_APPROVEDBY = "WFLevelHatGenehmigt";
    /**
     * Item to store the archiving date
     */
    public static final String ITEM_ARCHIVINGDATE = "WFArchivingDate";
    /**
     * Item to store workflow comment.
     */
    public static final String ITEM_COMMENT = "WFComment";
    /**
     * Item to store the current workflow level
     */
    public static final String ITEM_CURRENTLEVEL = "WFCurrentLevel";  // Real sequence number of level
    /**
     * Item to store the workflow initiator.
     */
    public static final String ITEM_INITIATOR = "WFInitiator";
    /**
     * Item to store the unique name of the current workflow level
     */
    public static final String ITEM_LEVELNAME = "WFLevelNr";
    /**
     * Item to store the publishing date
     */
    public static final String ITEM_PUBLISHINGDATE = "WFPublishingDate";
    /**
     * Item to store the release date
     */
    public static final String ITEM_RELEASEDATE = "WFReleaseDate";
    /**
     * Item to store replacement reason.
     */
    public static final String ITEM_REPLACEMENT_REASON = "Ersetzungsgrund";
    /**
     * Item to store the number of reviewers.
     */
    public static final String ITEM_REVIEWERCOUNT = "WFLevelReviewerAnzahl";
    /**
     * Item to store reviewer names
     */
    public static final String ITEM_REVIEWERS = "WFReviewer";
    /**
     * Item to store workflow name
     */	
    public static final String ITEM_WFNAME = "WFName";
    /**
     * Item to store people with write access to the document.
     */
    public static final String ITEM_WRITEACCESS = "WFWriteAccess";
	
	/**
	 * Called when a content enters workflow and should be initialized for it.
	 * @throws WGAPIException
	 */
	public void initialize() throws WGAPIException;
	/**
	 * Publishes a draft content so it can enter authorization workflow.
	 * @param comment Comment on publishing
	 * @return true, if the content can be immediately released, false if there is another workflow level yet to pass
	 * @throws WGAPIException
	 */
	public boolean publish(String comment) throws WGAPIException;
	/**
	 * Rejects a content back to draft status.
	 * @param comment Comment on rejection
	 * @throws WGAPIException 
	 */
	public void reject(String comment) throws WGAPIException;
	/**
	 * Approves the content
	 * @param comment A comment on the approval operation
	 * @throws WGWorkflowException
	 * @return true, if the content can be immediately published, false if there is another workflow level
	 * @throws WGAPIException 
	 */
	public boolean approve(String comment) throws WGAPIException;
	/**
	 * Called when a content is released (i.e. has completely passed the workflow)
	 * @param comment Comment on releasing.
	 * @throws WGAPIException
	 */
	public void release(String comment) throws WGAPIException;
	/**
	 * Archives the content.
	 * @param comment Comment on archiving the content
	 * @throws WGAPIException
	 */
	public void archive(String comment) throws WGAPIException;
	
	/**
	 * Returns the workflow role of the currently logged in user for the current workflow level of the content.
	 * @return Workflow role as constant ROLE_....
	 * @throws WGAPIException 
	 */
	public int getWorkflowRole() throws WGAPIException;
	
	 /**
     * Checks all conditions that are necessary for the current user to approve this document.
     * @throws WGAPIException 
     */
    public boolean isApprovableByUser() throws WGAPIException;

}
