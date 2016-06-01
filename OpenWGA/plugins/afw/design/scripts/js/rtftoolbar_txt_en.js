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
// rtftoolbar_txt_en.js

AFW.RTFToolbar.txt={

	tooltips:{
		save:				"Save",
		paste:				"Paste text from clipboard without any formats",
		bold: 				"Bold",
		italic:				"Italic",
		underline:			"Underline",
		forecolor:			"Text color",
		removeformat:		"Remove all formats",
		indent:				"Indent text",
		outdent:			"Outdent text",
		justifyleft:		"Left justify text",
		justifycenter:		"Center text",
		justifyright:		"Right justify text",
		insertunorderedlist:"Unordered list",
		insertorderedlist:	"Ordered list",
		inserttable:		"Insert Table",
		tableproperties:	"Edit table properties",
		inserttablerow:		"Insert table row",
		deletetablerow:		"Delete table row",
		inserttablecol:		"Inset table column",
		deletetablecol:		"Delete table column",
		mergetablecells:	"Merge table cells",
		splittablecell:		"Split table cells",
		insertlink:			"Select link",
		insertsimplelink:	"Insert or edit link",
		unlink:				"Delete link",
		insertimg:			"Select grafics from library",
		insertsimpleimg:	"Insert grafics"
	},

	linkDialog:{
		title:				"Insert Link",
		linktitle:			"Title",
		url:				"Url:",
		target:				"Target Window:",
		linktype:			"Link Type:",
		linktypeValues:		["extern|exturl", "Attachment|intfile"]
	},
	
	imgDialog:{
		title:				"Insert Image",
		url:				"Url:",
		alt:				"Title:",
		border:				"Border:",
		align:				"Alignment:",
		alignValues:		["bottom|bottom", "middle|middle", "top|top", "left|left", "right|right"]
	},

	fileDialog:{
		title:				"Manage Attachments",
		files:				"File Attachments"
	},
	
	tableDialog:{
		title:				"Insert Table",
		width:				"Width:",
		align:				"Alignment:",
		alignValues:		["none|", "left|left", "center|center", "right|right"],
		rows:				"Rows:",
		cols:				"Columns:"
	},

	tablePropDialog:{
		title:				"Edit Table Properties",
		change:				"Change",
		to:					"to",
		applyto:			"apply to",
		preview:			"Preview",
		none:				["none|"],
		tableStyleList:		["Table style", "Row style", "Cell style"],
		applyToTableList:	["selected table|table"],
		applyToTrList:		["selected row|selected", "all rows in table|all"],
		applyToTdList:		["selected cell|selected", "all cells in selected row|row", "all cells in table|all"]
	},

	viewmodes_html:			["WYSIWYG|wysiwyg", "HTML|html", "Preview|preview"],
	viewmodes:				["WYSIWYG|wysiwyg", "Preview|preview"]
}
