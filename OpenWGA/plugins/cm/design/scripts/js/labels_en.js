/*******************************************************************************
 *Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 *
 *This file is part of the OpenWGA server platform.
 *
 *OpenWGA is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *In addition, a special exception is granted by the copyright holders
 *of OpenWGA called "OpenWGA plugin exception". You should have received
 *a copy of this exception along with OpenWGA in file COPYING.
 *If not, see <http://www.openwga.com/gpl-plugin-exception>.
 *
 *OpenWGA is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with OpenWGA in file COPYING.
 *If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
$L={
	cancel: "Cancel",
	close: "Close",
	save: "Save",
	saveandclose: "Save & Close",
	edit: "Edit",
	edit_field: "Edit Field",
	cancel_edit: "Cancel editing",
	please_wait: "Please wait",
	filename: "Filename",
	filesize: "Size",
	remove: "Delete",
	deleting: "Deleting ..."
}

$L.headings=[
	"Heading 1",
	"Heading 2",
	"Heading 3",
	"Heading 4",
	"Heading 5",
	"Heading 6"
]

$L.goto={
	title: "GoTo ...",
	msg: "Please enter a key or unique name"
}

$L.layout={
	sitesstructure: "Sitestructure"
}

$L.statusbar={
	contentdocument: "Contentdocument",
	loaded: "loaded",
	no_content_for_page: "No content for page",
	found: "found",
	no_wga_page: "The shown page is no OpenWGA page.",
	loading_page: "loading page"
}

$L.editors= {
	click_to_edit: "Click to edit this page element"
}

$L.searchpanel= {
	searchresult: "Searchresult",
	button_text : "New Search ...",
	click_here_to_search: "<h1>New Search</h1>click here to start a new search with new parameters"
}

$L.propertypannel = {
	title: "Content Properties", 
	button_tooltip: "<h1>Save</h1>click here to save your changes" 
}

$L.contentviewpanel = {
	title: "Contents"
}

$L.se={
	tooltip_reload_se : "Reload Siteexplorer",
	tooltip_copy: "Copy page to clipboard",
	tooltip_paste: "Paste page from clipboard",
	tooltip_create_rootpage: "Create New Rootpage",
	tooltip_create_childpage: "Create New Childpage",
	tooltip_delete_page: "Delete Page",
	tooltip_page_settings: "Page Settings"
}

$L.dialogs=[
	{
		id: "login",
		title: "Change User or Access Rights ...",
		submitButtonText: "Login",
		submitOnEnter:		true
	},
	{
		id: 				"admin-login",
		title: 				"Login as OpenWGA Administrator ...",
		submitButtonText: 	"Login",
		submitOnEnter: 		true		
	},
	{
		id: 				"linkchecker-config",
		title: 				"Linkchecker Configuration ...",
		submitButtonText: 	"Save Configuration",
		submitOnEnter: 		false		
	},
	{
		id: 				"quit",
		title: 				"Quit Content Manager ...",
		submitButtonText: 	"Quit",
		submitOnEnter: 		true
	},
	{
		id: "publish-content",
		title: 				"Publish Content ...",
		submitButtonText:	"Publish",
		submitOnEnter:		false
	},

	{
		id: "create-page",
		title: 				"Create New Page  ...",
		submitButtonText:	"Create Page",
		submitOnEnter:		true
	},
	
	{
		id: "create-content",
		title: 				"Create New Language Version ...",
		submitButtonText:	"Create New Version",
		submitOnEnter:		true
	},
	
	{	id: "delete-page",
		title: 				"Delete Page and all Childpages ...",
		submitButtonText:	"Delete Page",
		submitOnEnter:		true
	},
	
	{	id: "delete-content",
		title: 				"Delete Content Version ...",
		submitButtonText:	"Delete",
		submitOnEnter:		true
	},
	
	{	id: "archive-content",
		title: 				"Archive Content ...",
		submitButtonText:	"Archive",
		submitOnEnter:		true
	},
	
	{	id: "show-access",
		title: 				"Access ..."
	},
	
	{	id: "approve-content",
		title: 				"Approve Content ...",
		submitButtonText:	"Approve"
	},
	
	{	id: "reject-content",
		title: 				"Reject Content ...",
		submitButtonText:	"Reject"
	},

	{	id: "reject-pending-content",
		title: 				"Reset pending document ...",
		submitButtonText:	"Set to Draft"
	},	
	
	{	id: "edit-content",
		title: 				"Edit Content ...",
		submitButtonText:	"Create Draft Copy",
		submitOnEnter:		true
	},
	
	{	id: "settings",
		title: 				"Edit Properties ...",
		submitButtonText:	"Save",
		submitOnEnter:		true
	},
	
	{	id: "search", 
		title: 				"Search for Content ...",
		submitButtonText:	"Search",
		submitOnEnter:		true
	},
	
	{	id: "move-page", 
		title: 				"Move Page ...",
		submitButtonText:	"Move",
		submitOnEnter:		true
	},
	
	{	id: "clear-remote", 
		title: 				"Clear Remote connection ...",
		submitButtonText:	"Clear",
		submitOnEnter:		true
	},

	{	id: "ldap-lookup",
		title: 				"LDAP Directory ..."
	},
	
	{	id: "upload",
		title: 				"Fileupload ...",
		submitButtonText:	"upload",
		closeButtonText:	"Close"
	},

	{	id: "forward-content",
		title: 				"Forward Content ...",
		submitButtonText:	"Send Mail"
	},

	{	id: "userdefined-metas",
		title: 				"Userdefined Settings ...",
		submitButtonText:	"Save"
	},

	{	id: "workflow-level",
		title: 				"Workflow Level ...",
		submitButtonText:	"Save"
	},

	{	id: "paste-page",
		title: 				"Insert page from clipboard ...",
		submitButtonText:	"Insert"
	},

	{	id: "paste-content",
		title: 				"Insert content elements from clipboard ...",
		submitButtonText:	"Insert"
	},

	{	id: "about",
		title: 				"About OpenWGA Content Manager ..."
	},
	
	{
		id: "rtf:insert-image",
		title: 				"Insert image...",
		submitButtonText: 	"Insert",
		submitOnEnter:		true
	},
	
	{
		id: "rtf:insert-link",
		title: 				"Insert link...",
		submitButtonText: 	"Insert",
		submitOnEnter:		true
	},
	
	{
		id: "bookmarks:add-bookmark",
		title: 				"Add bookmark...",
		submitButtonText: 	"Save",
		submitOnEnter: 		true
	},
	
	{
		id: "bookmarks:manage-bookmarks",
		title: "Manage bookmarks...",
		submitOnEnter: true
	},

	{
		id: "check-links",
		title: "Validate Links ...",
		submitButtonText: 	"Validate Links"
	}	

	,{
		id: "start-agent",
		title: "Stare Agent ...",
		submitButtonText: 	"Execute"
	}	
	
	,{
		id: "tag-editor",
		title: "Keywords",
		submitButtonText: "Save"
	}

	,{
		id: "init-db-design",
		title: "Database Design setup",
		submitButtonText: "Create Designelements"
	}

	,{
		id: "rtf:edit-rtf-node",
		title: "Edit RTF Element",
		submitButtonText: "Update"
	}

	,{
		id: "scale-image",
		title: "Scale Image",
		submitButtonText: "Scale Image"
	}
	
	,{
		id: "view-image",
		title: "View Image",
		cancelButtonText: "Close"
	}
	
	,{
		id: "userdefined-section",
		title: "Content Sections",
		submitButtonText: "Insert"
	}

	,{
		id: "content-modules",
		title: "Content Modules",
		submitButtonText: "Save"
	}	

	,{
		id: "empty-trash",
		title: "Trash",
		submitButtonText: "Empty"
	}

	,{
		id: "restore-deleted-page",
		title: "Restore Deleted Page",
		submitButtonText: "Restore"
	}

	,{
		id: "user-info",
		title: "User Informations"
	}

	,{
		id: "page-settings",
		title: "Page Settings",
		submitButtonText: "Save"
	}

	,{
		id: "image-item-editor",
		title: "Select Image",
		submitButtonText: "Save"
	}

	,{
		id: "custom-dialog",
		title: "Custom Dialog",
		submitButtonText: "Submit"
	}

	,{
		id: "file-meta-data",
		title: "File Metadata",
		submitButtonText: "Update"
	}
	
	,{
		id: "edit-video",
		title: "Edit Video",
		submitButtonText: "Save"
	}
	
];


/*
 * designer labels (ses)
 */

$L.toolbarmenu = {
	newContentType: "Create Contenttype",
	newTmlModule: "Create TML Module",
	newArea: "Create Area",
	newLanguage: "Create Language",
	newCssJs: "Create Script/CSS Module",
	newFileContainer: "Create Filecontainer",
	newWorkflow: "Create Workflow"
}

$L.designExplorer = {
	designTabTitle: "Management",
	refreshTooltip: "Refresh"
}

$L.panels = {
	tml: "TML",
	contenttype: "Contenttype",
	area: "Area",
	language: "Language",
	cssjs: "Script/CSS",
	filecontainer: "Filecontainer",
	workflow: "Workflow",
	
	alternativeParam: "(new)"
}


$L.contentComparePanel={
	title: "Compare Versions"
}


$L.RTFToolbar={
	zoom: "Zoom edit field",
	editHelper: "Show/Hide Paragraph Marker",
	undo: "Undo",
	redo: "Redo",
	paste: "Paste without Formats",
	removeFormatting: "Cleanup HTML",
	bold: "Bold",
	italic: "Italic",
	underline: "Underline",
	justifyLeft: "Justify Left",
	justifyRight: "Justify Right",
	justifyCenter: "Justify Center",
	justifyFull: "Justify Full",
	indent: "Indent",
	outdent: "Outdent",
	bulletList: "Bullet List",
	enumList: "Enumerated List",
	insertLink: "Create/Edit Link",
	insertTable: "Insert Table",
	insertImage: "Create/Edit Image",
	editImage: "Edit Image",
	unlink: "Delete Link",
	save_and_continue: "Save and Continue",
	save_and_close: "Save and Close" 
}

$L.linktype={
	exturl: "External link",
	int: "Internal link",
	intname: "Internal Link via unique name",
	intfile: "Attachment from this document",
	extfile: "Attachment from any document",
	file: "Attachment from file container",
	primaryfile: "Primary Attachment from Document",
	exp: "Context-Expression"
}
