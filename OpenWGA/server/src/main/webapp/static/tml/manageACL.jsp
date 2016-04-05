<%------------------------------------------------------------------------------
  Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
  
  This file is part of the OpenWGA server platform.
  
  OpenWGA is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  In addition, a special exception is granted by the copyright holders
  of OpenWGA called "OpenWGA plugin exception". You should have received
  a copy of this exception along with OpenWGA in file COPYING.
  If not, see <http://www.openwga.com/gpl-plugin-exception>.
  
  OpenWGA is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with OpenWGA in file COPYING.
  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------%>
<%@ page pageEncoding="ISO-8859-1" contentType="text/html" buffer= "24kb" autoFlush="true" isThreadSafe="true" session="true" errorPage="../../error.jsp" %>
<%-- 
Function:
Form to edit the ACL of some database.

--%>
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
%>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<tml:root resource="manageACL.jsp">
<HEAD>
<tml:htmlhead/>
<META http-equiv="Content-Style-Type" content="text/css">

<TITLE>Manage ACL for <tml:meta type="database" name="title"/> (<tml:meta type="database" name="dbkey"/>)</TITLE>

<tml:action id="close">
this.removeSessionVar("currentEntry");
this.removeSessionVar("ACLEntryName");
this.removeSessionVar("ACLEntryType");
</tml:action>

<tml:action id="select">
var db = this.content().database;
var acl = db.getACL();
var entry = acl.getEntry(this.tmlparam1);
this.setSessionVar("currentEntry", entry);
this.removeSessionVar("ACLEntryName");
this.removeSessionVar("ACLEntryType");
</tml:action>

<tml:action id="rename">
try {
	if (!this.isdefined("currentEntry")) {
		this.setvar("msg", "No entry selected!");
		return;
	}
	
	var newName = tmlform.renameentry;
	var db = this.content().database;
	var acl = db.getACL();
	var oldEntry = acl.getEntry(this.currentEntry.name);
	var newEntry = acl.createEntry(newName, oldEntry.getType(), oldEntry.getLevel());
	newEntry.setFlags(oldEntry.getFlags());
	acl.save(newEntry);
	acl.remove(oldEntry);
	
	callAction("close");
}
catch (e) {
	this.setvar("msg", e.message);
}
</tml:action>

<tml:action id="delete">
try {
	if (!this.isdefined("currentEntry")) {
		this.setvar("msg", "No entry selected!");
		return;
	}
	var db = this.content().database;
	var acl = db.getACL();
	var entry = acl.getEntry(this.currentEntry.name); // Re-retrieval
	acl.remove(entry);
	callAction("close");
}
catch (e) {
	this.setvar("msg", e.message);
}
</tml:action>

<tml:action id="create">
var error = null;
if (isEmptyValue(this.tmlform.field("name"))) {
	this.setvar("msg", "Name cannot be empty!");
	return;
}
var db = this.content().database;
var acl = db.getACL();

// Validate role name
var type = parseInt(this.tmlform.field("type"));
var accessLevel;
if (type == Packages.de.innovationgate.webgate.api.WGACLEntry.TYPE_ROLE) {
	accessLevel = 0;
	if (acl.isValidRoleName(this.tmlform.field("name")) == false) {
		error = "Role names may only consist of characters a-z, A-Z, 0-9, -, _, $, #, [, ]";
	}
}
else {
	accessLevel = parseInt(this.tmlform.field("accessLevel"));
}

try {
	if (error == null) {
		var entry = acl.createEntry(this.tmlform.field("name"), type, accessLevel);
	}
	else {
		this.setvar("msg", error);
	}
}
catch (e) {
	this.setvar("msg", e.message);
}
</tml:action>

<tml:action id="init">
var db = this.content().database;
var acl = db.getACL();
if (db.getSessionContext().isMasterSession()) {
	acl.createUserEntry("*", db.ACCESSLEVEL_MANAGER);
	this.setvar("msg", "The database is now generally opened for manager access, since you currently are not logged in. Please ensure to reduce access before this database is used in a production environment!");
}
else if (!db.getSessionContext().isAnonymous()) {
	acl.createUserEntry(db.getSessionContext().getUser(), db.ACCESSLEVEL_MANAGER);
	acl.createUserEntry("*", db.ACCESSLEVEL_READER);
}
else {
	this.setvar("msg", "You cannot initialize the ACL as anonymous user. Please login as database user or as WGA admin on admin page!");
}
</tml:action>

<tml:action id="store">
try {
	var flagsClass = Packages.de.innovationgate.webgate.api.WGACLEntryFlags;
	if (!this.isdefined("currentEntry")) {
		this.setvar("msg", "No entry selected!");
		return;
	}
	var db = this.content().database;
	var acl = db.getACL();
	var entry = acl.getEntry(this.currentEntry.name); // Re-retrieval
	entry.level = parseInt(this.tmlform.field("accessLevel"));
	
	var aclFlags = new Packages.de.innovationgate.webgate.api.WGACLEntryFlags();
	aclFlags.getRoles().addAll(this.tmlform.fieldList("roles"));
	aclFlags.setMayDeleteDocs(tmlform.fieldList("privileges").contains(flagsClass.TYPE_DELETEDOCS));
	aclFlags.setMayMoveStructs(tmlform.fieldList("privileges").contains(flagsClass.TYPE_MOVESTRUCTS));
	aclFlags.setNoRoleInheritance(tmlform.fieldList("privileges").contains(flagsClass.TYPE_NOROLEINHERITANCE));
	entry.flags = aclFlags.toString();
	
	acl.save(entry);
	callAction("close");
}
catch (e) {
	this.setvar("msg", e.message);
}
</tml:action>

<script type="text/javascript">
	function openLookup(prefix, target) {
		if (prefix == '') {
			alert("<tml:label key="lookupUserGroups_ext_info_noPrefixGiven" bundle="forms"/>");
			return;
		}
		var parameter="height=500,width=500" 
        +",screenX=0,left=5,screenY=0,top=5"
        +",dependent=0,directories=0"
        +",fullscreen=0,location=0,menubar=0"
        +",resizable=1,scrollbars=1,status=1,toolbar=0";
		open("<%= request.getContextPath() %>/admintml/<tml:meta type="database" name="dbkey"/>/lookupUserGroups?prefix="+prefix+"&target="+target+"&append=false&valueBackButton=Back to acl" ,"_blank",parameter);
	}

	function renameUser(oldName) {
		var newUserName = prompt("Enter the new name of the user");
		if (newUserName == null) {
			return false;
		}
		document.forms['editEntry'].elements['renameentry'].value = newUserName;
		callAction('<tml:action ref="rename" form="editEntry"/>');
	}
</script>
</HEAD>
<BODY topmargin="5" marginheight="5" leftmargin="5" marginwidth="5">
<%
de.innovationgate.wgpublisher.webtml.utils.TMLContext context = jspHelper.getMainContext();
de.innovationgate.webgate.api.WGDatabase db = context.content().getDatabase();
	
String dbKey = (String) db.getAttribute(de.innovationgate.wgpublisher.WGACore.DBATTRIB_DBKEY);
String docTitle = "Manage ACL for " + db.getTitle() + "(" + dbKey + ")";
context.setvar("acl", db.getACL());
%>
	<jsp:include page="../inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="<%= docTitle %>"/>	
	</jsp:include>	
<%
if (!db.getSessionContext().isAnonymous() && db.getSessionContext().getAccessLevel() == de.innovationgate.webgate.api.WGDatabase.ACCESSLEVEL_MANAGER) {
	
	if (db.hasFeature(de.innovationgate.webgate.api.WGDatabase.FEATURE_ACL_MANAGEABLE)) {
%>


<tml:case isdefined="msg">
<div style="font-weight:bold; color:darkred; padding:5px; background-color:white; border-color:darkred; border-width:3px; border-style:solid;"><tml:item name="msg"/></div>
</tml:case>

	<tml:if isdefined="currentEntry">
		<tml:then>
			<!-- Display modification form -->
			<tml:form id="editEntry" source="none">
			<tml:script>
			var flagsClass = Packages.de.innovationgate.webgate.api.WGACLEntryFlags;
			
			// Fill form if this is the first request
			if (!isDefined("ACLEntryName")) {
				var entry = this.currentEntry;
				this.setSessionVar("ACLEntryName", entry.name);
				this.setSessionVar("ACLEntryType", (entry.type == entry.TYPE_USER ? "User" : "Role"));
				var flags = this.acl.parseFlags(entry);
				this.tmlform.setField("accessLevel", entry.level.toString());
				this.tmlform.setField("roles", flags.getRoles());
				var privileges = createList();
				if (flags.isMayDeleteDocs()) {
					privileges.add(flagsClass.TYPE_DELETEDOCS);
				}
				if (flags.isMayMoveStructs()) {
					privileges.add(flagsClass.TYPE_MOVESTRUCTS);
				}
				if (flags.isNoRoleInheritance()) {
					privileges.add(flagsClass.TYPE_NOROLEINHERITANCE);
				}
				this.tmlform.setField("privileges", privileges);
				
				privilegeOptions = createList();
				privilegeOptions.add("May delete documents|" + flagsClass.TYPE_DELETEDOCS);
				
				if (entry.level >= db().ACCESSLEVEL_EDITOR) {
				privilegeOptions.add("May move struct entries|" + flagsClass.TYPE_MOVESTRUCTS);
				}
				
				privilegeOptions.add("Do not inherit roles from less specific ACL entries|" + flagsClass.TYPE_NOROLEINHERITANCE);
			}
			</tml:script>

			<table border="0" width="100%" height="100%" class="tabforms" cellpadding="2" cellspacing="2"> 
				<tr>
					<td colspan="2" bgcolor="white" class="orangeBorder"><NOBR><b style="font-size:14px">Edit entry: <tml:item name="ACLEntryName"/> (<tml:item name="ACLEntryType"/>)</b></TD>
				</tr>
				<tr><td colspan="2">&nbsp;</td></tr>
				<tml:if condition="this.currentEntry.type == this.currentEntry.TYPE_USER">
					<tml:then>
						<tr height="100%" valign="top">
							<TD><NOBR style="font-size:14px">Access level:</NOBR></TD>
							<TD width="100%">
							
								<tml:input name="accessLevel" 
										   type="select"
										   changeaction="$refresh"
										   options="No access|0,
										   			Reader|10,
										   			Author|20,
										   			Author/Designer|25,
										   			Editor|30,
										   			Editor/Designer|35,
										   			Manager|90">
									style="width:80%"
								</tml:input>
							</TD>
						</tr>
						
						<tr height="100%" valign="top">
							<TD><NOBR style="font-size:14px">Assigned roles:</NOBR></TD>
							<TD width="100%">
								<tml:if condition="this.acl.getRoles().size() > 0">
									<tml:then>
										<tml:input name="roles" 
												   type="checkbox" 
												   options="{this.acl.getRoles().keySet()}"/>
									</tml:then>
									<tml:else>
										(No roles defined)
									</tml:else>
								</tml:if>
							</TD>
						</tr>

						<tr height="100%" valign="top">
							<TD><NOBR style="font-size:14px">Privileges and flags:</NOBR></TD>
							<TD width="100%">
							
								<tml:input name="privileges"
										   mode="{tmlform.accessLevel < 25 ? 'readonly' : 'edit'}"
										   type="checkbox" 
										   optionsitem="privilegeOptions"/>
							</TD>
						</tr>


					</tml:then>
					<tml:else>
						<tr height="100%" valign="top">
							<TD><NOBR style="font-size:14px">Users/Groups owning this role:</NOBR></TD>
							<TD width="100%">
								<tml:script var="owners">
									return acl.getOwnersOfRole(currentEntry);
									/*var result  = "";
									for (var i=0; i < owners.size(); i++) {
										result+= owners.get(i).getName() + "<br/>";
									} 
									return result;*/
								</tml:script>
								<tml:foreach type="itemvalue" item="owners" currentvalue="owner">
									<a href="<tml:url action="select" param1="{ owner.getName() }"/>"><tml:item name="owner" xpath="name"/></a><br/>
								</tml:foreach>
							</TD>
						</tr>
					</tml:else>
				</tml:if>
				<tr><td colspan="2"><hr></td></tr>
				<tr>
					<td colspan="2">
						<tml:case condition="this.currentEntry.type == this.currentEntry.TYPE_USER">
							<tml:button clickaction="store" cssclass="button">Store</tml:button>&nbsp;
							<button onclick="renameUser(); return false;" class="button">Rename</button>&nbsp;
						</tml:case>
						<tml:button clickaction="delete" cssclass="button">Delete</tml:button>&nbsp;
						<tml:button clickaction="close" cssclass="button">Back to list</tml:button><br/>		
					</td>
				</tr>
			</table>
			<tml:input name="renameentry" type="hidden"/>
			</tml:form>			
		</tml:then>
		
		<tml:else>
			<!-- Display user/roles list and form to create new un's -->
			<div class="tabforms" style="font-size:10px;padding:3px">Info: You are logged in as <tml:meta type="database" name="username"/></div>
			<br/>
			<table border="0" width="100%" class="tabforms" cellpadding="2" cellspacing="2"> 
				<tr>
					<td width="100%"><b>Name</b></td>
					<td><b>Type</b></td>
					<td><nobr><b>Access Level</b></nobr></td>
				</tr>
<%
		de.innovationgate.webgate.api.WGACL acl = db.getACL();
		java.util.Iterator entries = acl.getAllEntries().iterator();
		de.innovationgate.webgate.api.WGACLEntry entry;
			
		if( acl.getAllEntries().isEmpty() ){
%>
				<tr><td colspan="2">No entries. No access is allowed.<br/>
				<!--<p>The ACL should be initialized by creating a manager entry for you. This will ensure your further access as a manager of this database while editing the ACL.</p>
				<tml:button clickaction="init" cssclass="button">Initialize ACL</tml:button></td></tr>-->
				<%
		}
		else {
			boolean reachedRoles = false;
			while (entries.hasNext()) {
					entry = (de.innovationgate.webgate.api.WGACLEntry) entries.next();
					if (reachedRoles == false && entry.getType() == de.innovationgate.webgate.api.WGACLEntry.TYPE_ROLE) {
						reachedRoles = true;
						out.write("<tr><td>&nbsp;</td></tr>");
					}
%>
				<tr>
					<td class="orangeBorder" bgcolor="white">
						<a style="width:100%" 
						   alt="Click here to edit this entry"
						   href="<tml:url action="select" param1="<%= entry.getName() %>"/>"><%= entry.getName() %></a></td>
        			  
        			  
        			<td class="orangeBorder" bgcolor="white"><%= (entry.getType() == 1 ? "User" : "Role") %></td>
        			<% if (entry.getType() == de.innovationgate.webgate.api.WGACLEntry.TYPE_USER) { %>
					<td class="orangeBorder" bgcolor="white"><%= de.innovationgate.webgate.api.WGDatabase.accessLevelText(entry.getLevel()) %></td>
					<% } else { %>
					<td>&nbsp;</td>	
					<% } %>
				</tr>
<%
			}
	}
%>
			</table>
			<br/>
			<tml:form id="newuser" source="none" onsubmit="return false;">
				<tml:input type="hidden" name="type" default="<%= new Integer(de.innovationgate.webgate.api.WGACLEntry.TYPE_USER).toString() %>"/>
				<table border="0" width="100%" class="tabforms" cellpadding="2" cellspacing="2"> 
					<tr><TD colspan="2"><b>Create new user or group entry</b></TD></tr>
					<tr>
						<td><NOBR>User name:</NOBR></td>
						<td width="100%"><NOBR><tml:input name="name">style="width:75%"</tml:input></NOBR>
						<input type="button" onclick="openLookup(document.forms['newuser'].name.value, 'document.forms[\'newuser\'].name')" class="button" value="Lookup"/>
					</tr>
					<tr>
						<td><NOBR>at level</NOBR></td>
						<td width="100%"><NOBR><tml:input name="accessLevel" 
									   type="select" 
									   options="No access|0,
									   			Reader|10,
									   			Author|20,
									   			Author/Designer|25,
									   			Editor|30,
									   			Editor/Designer|35,
									   			Manager|90">

							</tml:input></NOBR>
							<tml:button clickaction="create" cssclass="button">Create</tml:button>
						</td>
					</tr>
					<TR><td colspan="2">
					
							<hr>
							<p>Use user name <b>&quot;anonymous&quot;</b> to determine anonymous access settings.</p>
							<p>Use group name <b>&quot;authenticated&quot;</b> to determine access for all authenticated users (i.e. all but "anonymous").</p>
							<p>Use group name <b>&quot;&#42;&quot;</b> (Star character) to determine access settings for all users</p>
						</td></TR>
				</table>
			</tml:form>
			<br/>
			<tml:form id="newrole" source="none" onsubmit="return false;">
				<tml:input type="hidden" name="type" default="<%= new Integer(de.innovationgate.webgate.api.WGACLEntry.TYPE_ROLE).toString() %>"/>
				<table border="0" width="100%" class="tabforms" cellpadding="2" cellspacing="2"> 
					<tr><TD colspan="2"><b>Create new role</b></TD></tr>
					<tr>
						<td><NOBR>Role name:</NOBR></td>
						<td width="100%"><NOBR><tml:input name="name">style="width:75%"</tml:input>
					</tr>
					<tr>
						<td>
							<tml:button clickaction="create" cssclass="button">Create</tml:button>
						</td>
					</tr>
				</table>
			</tml:form>
		</tml:else>	
	</tml:if>
<%
	}
	else {
		out.write("<nobr><b>The ACL of this database cannot be managed!</b></nobr>");
	}
}
else {
		out.write("<nobr><b>You are not authorized to manage the ACL of this database as user '" + db.getSessionContext().getUser() + "'. Use the start page to switch to another user.</b></nobr>");
}
%>

<!--<tml:warnings/>-->

<jsp:include page="../inc_foot.jsp" flush="true"/>
</BODY>
</tml:root>
</HTML>

