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
<%@page import="de.innovationgate.wga.server.api.WGA"%>
<%@page import="de.innovationgate.wgpublisher.log.AppLog"%>
<%@ page language="java" pageEncoding="ISO-8859-1" %>
<jsp:directive.page import="java.text.SimpleDateFormat"/>
<%@page import="de.innovationgate.webgate.api.WGFactory"%>
<%@page import="java.io.StringWriter"%>
<%@page import="de.innovationgate.wga.common.LogLevel"%>
<%@page import="de.innovationgate.wgpublisher.design.sync.DesignSyncManager"%>

<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
%>
<%@ page import="de.innovationgate.wga.common.beans.csconfig.v1.*" %>
<%@ page import="de.innovationgate.wgpublisher.jsputils.JspHelper" %>
<%@ page import="de.innovationgate.wgpublisher.*" %>
<%@ page import="de.innovationgate.wgpublisher.cache.FileCache" %>
<%@ page import="de.innovationgate.webgate.api.WGDatabase" %>
<%@ page import="javax.servlet.http.HttpSession" %>
<%@ page import="de.innovationgate.utils.WGUtils" %>
<%@ page import="de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory" %>
<%@ page import="java.util.Arrays" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Iterator" %>
<%@ page import="java.net.URL" %>
<%@ page import="java.util.Date" %>
<%@ page import="java.util.Map" %>
<%@ page import="java.text.DateFormat" %>
<%@ page import="java.text.NumberFormat" %>
<%@ page import="java.util.Collections" %>


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<TITLE><%= WGACore.getReleaseString() %> Administration Page</TITLE>

<%
try {
String message = "";
WGPDispatcher dispatcher = jspHelper.getDispatcher();
de.innovationgate.wgpublisher.WGACore core = jspHelper.getCore();
if (!core.getWgaConfiguration().isAdminPageEnabled() || !core.isAdministrativePort(request.getLocalPort())) {
	response.sendError(403, "Administration page is disabled");
	return;
}

if (dispatcher == null) {
	response.sendRedirect("wga4admin");
	return;
}

String timeNow = jspHelper.longDateTimeFormat(new java.util.Date());
String instanceActive = jspHelper.longDateTimeFormat(core.getInstanceActiveSince());
Runtime runtime = Runtime.getRuntime();

String adminName = (String) session.getAttribute(WGACore.SESSION_ADMINNAME);
if (adminName == null) {
	adminName = (String) pageContext.getRequest().getParameter(WGACore.SESSION_ADMINNAME);
}
String adminPassword = (String) session.getAttribute(WGACore.SESSION_ADMINPASSWORD);
if (adminPassword == null) {
	adminPassword = (String) pageContext.getRequest().getParameter(WGACore.SESSION_ADMINPASSWORD);
}

boolean isLoggedIn = core.isAdminLogin(adminName, adminPassword, request);
if (isLoggedIn) {
	session.setAttribute(WGACore.SESSION_ADMINNAME, adminName);
	session.setAttribute(WGACore.SESSION_ADMINPASSWORD, adminPassword);
	if (!WGUtils.isEmpty(request.getParameter("wheretogo"))) {
		response.sendRedirect(request.getParameter("wheretogo"));
		return;
	}
}
else if (adminName != null) {
		message = "Invalid login. Please try again";
}	
		
String showMode = pageContext.getRequest().getParameter("mode");
if (showMode == null ) {
	if(isLoggedIn){
		showMode = "general";	
	}
	else{	
		showMode = "nothing";
	}
}

int index = 1;
String indexText = pageContext.getRequest().getParameter("index");
if (indexText != null && !indexText.trim().equals("")) {
	index = Integer.parseInt(indexText);
}

String action = request.getParameter("action");
String param = request.getParameter("parameter");

if (isLoggedIn && action != null && !action.equals("")) {
	
	if (action.equals("resetContentDB")) {
		de.innovationgate.webgate.api.WGDatabase db = (de.innovationgate.webgate.api.WGDatabase) core.getContentdbs().get(param);
		if (db == null) {
			message = "No content database with key '" + param + "' found.";
			return;
		}
		core.removeContentDB(param);
		core.updateContentDBs();
		message = "Successfully reconnected content database '" + param + "'.";
	}
	
	else if (action.equals("switchDesignEnabling")) {
		de.innovationgate.webgate.api.WGDatabase db = (de.innovationgate.webgate.api.WGDatabase) core.getContentdbs().get(param);
		if (db == null) {
			message = "No content database with key '" + param + "' found.";
			return;
		}
		
		action = "showContentDetails";
		
	}
	
	else if (action.equals("resetPersDB")) {
		de.innovationgate.webgate.api.WGDatabase db = (de.innovationgate.webgate.api.WGDatabase) core.getPersonalisationdbs().get(param);
		if (db == null) {
			message = "No personalisation database for domain '" + param + "' found.";
			return;
		}
		core.removePersonalisationDB(param);
		core.updateConfig();
		message = "Successfully reconnected personalisation database for domain '" + param + "'.";
	}
	
	else if (action.equals("restartApp")) {
		core.shutdown();
		core.startup();
		message = "Successfully restarted application.";
	}
	
	else if (action.equals("clearLog")) {
		core.initLoggingFile();
		message = "Successfully cleared application log.";
	}
	
	else if (action.equals("clearCache")) {
		de.innovationgate.webgate.api.WGDatabase db = (de.innovationgate.webgate.api.WGDatabase) core.getContentdbs().get(param);
		if (db == null) {
			message = "No content database with key '" + param + "' found.";
			return;
		}
		db.openSession();
		List caches = WGUtils.deserializeCollection(request.getParameter("parameter2"), ",");
		if (caches.contains("data")) {
			db.refresh();
		}
		if (caches.contains("file")) {
			FileCache fileCache = (FileCache) db.getAttribute(WGACore.DBATTRIB_FILECACHE);
			fileCache.clear();
		}
		if (caches.contains("tml")) {
		    core.getWebTMLCache().clearForDatabase(db.getDbReference());
		}
		if (caches.contains("user")) {
		    db.clearUserCaches();
		}
		message = "Cleared caches on database " + param + ": " + WGUtils.serializeCollection(caches, ", ");
	}
	
	else if (action.equals("cacheDump")) {
		try {
			String fileName = core.tmlCacheDump();
			message = "WebTML cache dump file written to: " + fileName;
		}
		catch (java.io.IOException exc) {
			message = "ERROR CREATING CACHE DUMP: " + exc.getMessage();
			core.getLog().error("Error creating WebTML cache dump", exc);
		}

	}
	
	else if (action.equals("reloadConfig")) {
		core.updateConfig();
		message = "Reloaded configuration";
	}
	
	else if (action.equals("rebuildLuceneIndex")) {
		if (core.getLuceneManager() != null) {
   			try {
				core.getLuceneManager().rebuildIndex();
				message = "Rebuilding lucene index, check application log for details.";
			} catch (Exception e) {
				core.getLog().error(e);
				message = "Rebuilding lucene index failed. Exception: '" + e.getClass().getName() + "' " + e.getMessage();
			}				
		} else {
				message = "Lucene fulltext index is disabled.";
		}			
	}
	
	else if (action.equals("rebuildCSIndex")) {
		if (core.getLuceneManager() != null) {
   			try {
				core.getLuceneManager().rebuildIndex(param);
				message = "Rebuilding lucene index for contentDB '" + param + "' - check application log for details.";
			} catch (Exception e) {
				core.getLog().error(e);
				message = "Rebuilding lucene index for contentDB '" + param + "' failed. Exception: '" + e.getClass().getName() + "' " + e.getMessage();
			}				
		} else {
				message = "Lucene fulltext index is disabled.";
		}			
	}	
	
}
%>

<style type="text/css">

div.menu {
	padding: 0px;
	position:absolute; 
	font-size:8pt; 
	text-align:left; 
	visibility:hidden; 
	border:ridge 2px black; 
	background:url('<%= request.getContextPath() %>/static/images/wgabg.gif');
}

a.menu {
	display: block;
	border: 1px solid gray;
	padding: 5px;
	margin: 0px;
	font-size: 8pt;
	text-align: left;
	text-decoration: none;
}

a.menu:hover {
	background-color: white;
}
	
</style>

<script type="text/javascript">

function switchTo(mode, index, parameter) {

	var controlForm = document.forms['control'];
	controlForm.mode.value = mode;
	
	if (index == null) {
		index = 1;
	}	
	controlForm.index.value = index;
	
	if (parameter) {
		controlForm.parameter.value = parameter;
	}
	
	controlForm.submit();

}
// --------------------------------------- 
function action(action, parameter) {
	var controlForm = document.forms['control'];
	controlForm.action.value = action;
	controlForm.parameter.value = parameter;
	switchTo('<%= showMode %>');
}

function openACL(databaseKey){
	if(!databaseKey){
		if( selectedContentDB != null ){
			databaseKey = selectedContentDB;
		}
		else{
			alert("Please select a database first.");
			return false;
		}
	}	
	var parameter="height=500,width=500" 
        +",screenX=0,left=5,screenY=0,top=5"
        +",dependent=0,directories=0"
        +",fullscreen=0,location=0,menubar=0"
        +",resizable=1,scrollbars=1,status=1,toolbar=0";
	open("<%= request.getContextPath() %>/admintml/" + databaseKey + "/manageACL","_blank",parameter);
}

// ------------------- Content Stores Actions -----------------
<%
	String defaultDBKey = "null";
	if (action != null && action.equals("showContentDetails") && param != null) {
		defaultDBKey = "'" + param + "'";
	}
%>
var selectedContentDB = <%= defaultDBKey %>;

function selectContentDB(contentDbKey,hasDesign,aclManageable){	
	
	document.getElementById("manageACLButton").disabled = !(aclManageable);

	
	if( selectedContentDB!=null){
		document.getElementById("contentDB_row_"+selectedContentDB).style.background = "";
	}
	selectedContentDB = contentDbKey;
	document.getElementById("contentDB_row_"+contentDbKey).style.background = "white";	
}

function contentDbAction(sAction){
	if( selectedContentDB != null ){
		action(sAction, selectedContentDB);
	}
	else{
		alert("Please select a content store first, by clicking on it.");
	}
}

// -------------------------------------------------------------

isLoggedIn = <%= isLoggedIn %>;

function init(){
  if( document.forms[0] && document.forms[0].adminname){
	  document.forms[0].adminname.focus(); 
	  document.forms[0].adminname.select();
  }  
  
}

function restartApp() {

	if (confirm("Are you sure, you want to restart the complete application?")) {
		action("restartApp", null);
	}
}

function clearLog() {

	if (confirm("Are you sure, you want to clear the application log?")) {
		action("clearLog", null);
	}
}

function cacheDump() {

	if (confirm("Are you sure, you want to create a WebTML cache dump?")) {
		action("cacheDump", null);
	}
}

function rebuildLuceneIndex() {

	if (confirm("Are you sure, you want to rebuild the whole lucene index? This will delete all files from the index directory and reindex all dbs.")) {
		action("rebuildLuceneIndex", null);
	}
}

function rebuildCSIndex() {
	if (confirm("Are you sure, you want to rebuild the lucene index of the selected content store?")) {
		contentDbAction('rebuildCSIndex');
	}
}


function showDetails(key, cachePages) {
	alert("Database " + key + "\nCache pages: " + cachePages);
}

function tmlScriptConsole(databaseKey) {
	if(!databaseKey){
			if( selectedContentDB != null ){
				databaseKey = selectedContentDB;
			}
			else{
				alert("Please select a database first.");
				return false;
			}
		}	
		var parameter="height=700,width=600" 
	        +",screenX=0,left=5,screenY=0,top=5"
	        +",dependent=0,directories=0"
	        +",fullscreen=0,location=0,menubar=0"
	        +",resizable=1,scrollbars=1,status=1,toolbar=0";
		open("<%= request.getContextPath() %>/admintml/" + databaseKey + "/tmlscriptConsole","_blank",parameter);
}

function csDump() {

		document.getElementById('csDumpDialog').style.visibility = 'hidden';
	
		if( selectedContentDB != null ){
			databaseKey = selectedContentDB;
		}
		else{
			alert("Please select a database first.");
			return false;
		}
			
		var parameter="height=500,width=700" 
	        +",screenX=0,left=5,screenY=0,top=5"
	        +",dependent=0,directories=0"
	        +",fullscreen=0,location=0,menubar=0"
	        +",resizable=1,scrollbars=1,status=1,toolbar=0";
		open("<%= request.getContextPath() %>/admintml/" + databaseKey + "/csDump","_blank",parameter);
}

function loadDump() {

		document.getElementById('csDumpDialog').style.visibility = 'hidden';
	
		if( selectedContentDB != null ){
			databaseKey = selectedContentDB;
		}
		else{
			alert("Please select a database first.");
			return false;
		}
			
		var parameter="height=500,width=700" 
	        +",screenX=0,left=5,screenY=0,top=5"
	        +",dependent=0,directories=0"
	        +",fullscreen=0,location=0,menubar=0"
	        +",resizable=1,scrollbars=1,status=1,toolbar=0";
		open("<%= request.getContextPath() %>/admintml/" + databaseKey + "/loadDump","_blank",parameter);
}

function calcAbsoluteLeftOffset(elem) {

	var offset = elem.offsetLeft;
	if (elem.offsetParent) {
		offset += calcAbsoluteLeftOffset(elem.offsetParent);
	}
	return offset;

}

function calcAbsoluteTopOffset(elem) {

	var offset = elem.offsetTop;
	if (elem.offsetParent) {
		offset += calcAbsoluteTopOffset(elem.offsetParent);
	}
	return offset;

}

function showClearCacheDialog(e, button) {

	var dialog = document.getElementById('clearCacheDialog');
	if (dialog.style.visibility == 'hidden') {
		if( selectedContentDB == null ){
			alert("Please select a content store first, by clicking on it.");
			return;
		}
		dialog.style.left = calcAbsoluteLeftOffset(button) + "px";
		dialog.style.top = calcAbsoluteTopOffset(button) + button.offsetHeight + "px";
		dialog.style.visibility = 'visible';
	}
	else {
		dialog.style.visibility = 'hidden';
	}
}

function showQueriesDialog(e, button) {

	var dialog = document.getElementById('queriesDialog');
	if (dialog.style.visibility == 'hidden') {
		if( selectedContentDB == null ){
			alert("Please select a content store first, by clicking on it.");
			return;
		}
		dialog.style.left = calcAbsoluteLeftOffset(button) + "px";
		dialog.style.top = calcAbsoluteTopOffset(button) + button.offsetHeight + "px";
		dialog.style.visibility = 'visible';
	}
	else {
		dialog.style.visibility = 'hidden';
	}
}

function queryTestPage(databaseKey) {

	document.getElementById('queriesDialog').style.visibility = 'hidden';

	if(!databaseKey){
			if( selectedContentDB != null ){
				databaseKey = selectedContentDB;
			}
			else{
				alert("Please select a database first.");
				return false;
			}
		}	
		var parameter="height=700,width=700" 
	        +",screenX=0,left=5,screenY=0,top=5"
	        +",dependent=0,directories=0"
	        +",fullscreen=0,location=0,menubar=0"
	        +",resizable=1,scrollbars=1,status=1,toolbar=0";
		open("<%= request.getContextPath() %>/admintml/" + databaseKey + "/frmQueryTest","_blank",parameter);
}

function showCSDumpDialog(e, button) {

	var dialog = document.getElementById('csDumpDialog');
	if (dialog.style.visibility != 'visible') {
		if( selectedContentDB == null ){
			alert("Please select a content store first, by clicking on it.");
			return;
		}
		dialog.style.left = calcAbsoluteLeftOffset(button) + "px";
		dialog.style.top = calcAbsoluteTopOffset(button) + button.offsetHeight + "px";
		dialog.style.visibility = 'visible';
	}
	else {
		dialog.style.visibility = 'hidden';
	}
}

function doClearCache() {

	var form = document.forms['clearCacheForm'];
	var caches = new Array();
	for (var idx=0; idx < form.elements.length; idx++) {
		var elem = form.elements[idx];
		if (elem.checked == true) {
			caches[caches.length] = elem.value;
		}
	}
	var submitForm = document.forms['control'];
	submitForm.elements['parameter2'].value = caches.join(",");
	contentDbAction("clearCache");

}

function resetAPICache() {

	if (confirm("This will clear the WGAPI data cache for all databases. Are you sure?")) {
		action("resetAPICache", null);
	}

}

function showApplog(index) {
	document.forms['logForm'].index.value = index;
	document.forms['logForm'].submit();
}

</script>
<script type="text/javascript">
// ---------------------------- Scheduler ------------------------------
function execute() {		
	var form = document.forms["frmScheduler"];
	
	if( form.job.value == "" ){
		alert("Please select a job");
		return false;			
	}
	
	if (confirm("Are you sure you want to execute the job '" + form.job.value + "'?")) {
		form.run.value = "yes";
		form.submit();
	}			
}

function viewLog() {		
	var form = document.forms["frmScheduler"];
	if( form.job.value == "" ){
		alert("Please select a job");
		return false;			
	}
	form.run.value = "";
	form.submit();
	
}
var selectedJobTR = null;
function selectJob(jobName,trTag){
	var form = document.forms["frmScheduler"];
	form.job.value = jobName;
	if( selectedJobTR!=null){
		selectedJobTR.style.background = "";
	}
	trTag.style.background = "white";	
	selectedJobTR = trTag;
}

</script>
</HEAD>
<BODY onload="init()">
<% 
	
	String docTitle = "Administration Page<br>";
	
	if (showMode.equals("general")) {
		docTitle += "General Information";
	}
	else if (showMode.equals("java")) {
		docTitle += "Java Environment";
	}
	else if (showMode.equals("statistics")) {
		docTitle += "Usage Statistics";
	}
	else if (showMode.equals("licenses")) {
		docTitle += "Licensing";
	}
	else if (showMode.equals("contentdbs")) {
		docTitle += "Content Store Databases";
	}
	else if (showMode.equals("persdbs")) {
		docTitle += "Personalisation Databases";
	}
	else if (showMode.equals("plugins")) {
		docTitle += "Plugins";
	}
	else if (showMode.equals("applog")) {
		docTitle += "Application Log";
	}
	else if (showMode.equals("scheduler")) {
		docTitle += "Jobs";
	}
	else if (showMode.equals("tools")) {
		docTitle += "Tools";
	}
%>
<jsp:include page="static/inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="<%= docTitle %>"/>
</jsp:include>

<FORM METHOD="POST" NAME="control" style="display:inline">
<% if (!isLoggedIn) { %>

	<!-------------- Login ------------------------------------------------------------->		
	
	<table border="0" class="staticforms" cellspacing="0" cellpadding="10" width="100%">
		<tr><td colspan="2">Please login with an administrator account:</td></tr>
		<tr><td colspan="2">
		<% if (!message.equals("")) {
				out.write("<div style=\"color:red;font-size:12px;font-weight:bold;\">");
				out.write(message);
				out.write("</div>");
		   }
		%></td>
		</tr>
		<tr><td>Username:</td>
			<td width="100%">
				<input style="width:200px" type="text" SIZE="40" MAXLENGTH="256" 
							name="<%= WGACore.SESSION_ADMINNAME %>" 
							value="<%= (adminName != null ? adminName : "") %>"></td></tr>
		<tr><td>Password:</td>
			<td><input style="width:200px" type="password" SIZE="40" MAXLENGTH="256" name="<%= WGACore.SESSION_ADMINPASSWORD %>" value="<%= (adminPassword != null ? adminPassword : "") %>"></td>
				</tr>
		<tr><td colspan="2"><input class="button" type="submit" value="OK" width="50px"></td></tr>
		<tr><td colspan="2">
				<hr/>
				<a href="<%= WGACore.WGAMANAGER_URL %>">WebGate Anywhere Manager</a></td></tr>
	</table>	
	<input type="hidden" name="wheretogo" value="<%= (request.getParameter("wheretogo") != null ? request.getParameter("wheretogo") : "") %>"/>
	
	
	<!-------------- /Login ------------------------------------------------------------->
<% } else { %>
	<input type="hidden" name="mode" value="<%= showMode %>"/>
	<input type="hidden" name="index" value="<%= index %>"/>
	<input type="hidden" name="action" value=""/>
	<input type="hidden" name="parameter" value=""/>
	<input type="hidden" name="parameter2" value=""/>
<% } %>
</FORM>

<% if (isLoggedIn) { %>

<table border="0" cellpadding="0" cellspacing="0" width="100%">

<tr><td align="right">

<!-------------- Tabs ------------------------------------------------->
	<% 
		String tabTDStyle = "class=\"reiter\" style=\"border-style:outset\""; 
		String tabAStyle  =  "class=\"reiter\"";
		String tabTDSelectedStyle ="class=\"reiter\" style=\"border-style:inset;background-color:#999999\""; 
		String tabASelectedStyle  = "class=\"reiterSelected\"";
	%>
	<TABLE border="0" cellpadding="0" cellspacing="0">
		<tr>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("general")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("general")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %> 
					href="javascript:switchTo('general')"><nobr>Information</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("java")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("java")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('java')"><nobr>Java</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("statistics")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("statistics")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('statistics')"><nobr>Statistics</nobr></a>&nbsp;</td>			
			<td>&nbsp;</td>
			<td <% if (showMode.equals("contentdbs")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("contentdbs")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('contentdbs')"><nobr>Content</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("persdbs")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("persdbs")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('persdbs')"><nobr>Personalisation</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("plugins")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("plugins")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('plugins')"><nobr>Plugins</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("applog")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("applog")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('applog')"><nobr>Log</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("scheduler")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("scheduler")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('scheduler')"><nobr>Jobs</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
			<td <% if (showMode.equals("tools")){out.write(tabTDSelectedStyle);}else{out.write(tabTDStyle);} %>>
				&nbsp;
				<a <% if (showMode.equals("tools")){out.write(tabASelectedStyle);}else{out.write(tabAStyle);} %>
					href="javascript:switchTo('tools')"><nobr>Tools</nobr></a>&nbsp;</td>
			<td>&nbsp;</td>
		</tr>
	</table></td>
</tr>
<!-------------- /Tabs ------------------------------------------------->

<!-------------- Message Box ------------------------------------------------->
<tr><td style="padding:5px" class="staticForms">
			<div  class="orangeBorder" style="padding:5px;background-color:white;font-size:12px;font-weight:bold;">
			<% 
			   AppLog applog = WGA.get(core).service(AppLog.class);
			   if (showMode.equals("applog")) { 
					if (index == -1) {
						index = (applog.getLinesCount() / 1000 * 1000) + 1;						
			   		}
			   		out.write("Line " + index + " - " + (index+999));
			   }
			   if (!message.equals("")) {					
					out.write(message);					
			   }
			   else{
			   		out.write("&nbsp;");
			   }
			%>
		</div></td>
</tr>

<!-------------- Forms ------------------------------------------------->
<tr><td>		
	<table border="0" cellspacing="0" cellpadding="0" width="100%" class="listTable">
			
	<% if (showMode.equals("general")) { %>
			<TR>
				<TD>Application actions:</TD>
				<TD>
					<input type="button" class="button" style="width:200" value="Restart application" onClick="restartApp()"/>
					&nbsp;
					<input type="button" class="button" style="width:200" value="Clear application log" onClick="clearLog()"/>
					&nbsp;
					<input type="button" class="button" style="width:200" value="WebTML cache dump" onClick="cacheDump()"/>
					&nbsp;
					<input type="button" class="button" style="width:200" value="Reload configuration" onClick="action('reloadConfig', null)"/>
					&nbsp;
 					<input type="button" class="button" style="width:200" value="Rebuild whole lucene index" onClick="rebuildLuceneIndex()"/>
 					&nbsp;
 					<input type="button" class="button" style="width:200" value="Reset WGAPI data cache" onClick="resetAPICache()"/>
				</TD>		
			</TR>
			<!-- hr -->
			
			<TR>
				<TD><nobr>WGA Publisher Version:</nobr></TD>
				<TD width="100%"><%=  WGACore.getReleaseString() %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD><nobr>Build signature:</nobr></TD>
				<TD width="100%"><%=  WGACore.getBuildSignature() %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Application Server:</TD>
				<TD><%=  pageContext.getServletContext().getServerInfo() %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Operating system:</TD>
				<TD><%= (System.getProperty("os.name") + " Version " + System.getProperty("os.version") + " (" + System.getProperty("os.arch") + ")") %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Operating system user:</TD>
				<TD><%= System.getProperty("user.name") %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Default system locale:</TD>
				<TD><%= java.util.Locale.getDefault().toString() %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Configuration file:</TD>
				<TD><%= core.getConfigFilePath() %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Log output directory:</TD>
				<TD><%= (core.getLoggingDir() != null ? core.getLoggingDir().getPath() : "(none)") %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Directory for temporary files:</TD>
				<TD><%= core.getWgaTempDir().getPath() %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Instance active since:</TD>
				<TD><%= instanceActive %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Time now on server:</TD>
				<TD><%= timeNow %></TD>
			</TR>
			<TR>
				<TD><nobr>WebTML requests served:</nobr></TD>
				<TD><%= jspHelper.numberFormat(new Long(core.getUsageStatistics().getAllRequests())) %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Lucene indexer status:</TD>
				<TD><%= (core.getLuceneManager() == null ? "Not activated" : core.getLuceneManager().isIndexerRunning() ? "Indexing content" : "Idle") %></TD>
			</TR>
			<TR>
				<TD>Brute force login blocker status</TD>
				<TD><% 
					int blockedSize = core.getBruteForceLoginBlocker().getBlockedLogins().size(); 
					if (blockedSize > 0) {
						out.write("<a target=\"blank\" href=\"blockedlogins.jsp\" style=\"color:darkred; font-weight:bold\">");
						out.write(jspHelper.numberFormat(new Integer(blockedSize)));
						out.write(" login(s) are blocked");
						out.write("</a>");
					}
					else {
						out.write("All logins are available");
					}
				%></TD>
			</TR>			
	<% } %>
	
	<% if (showMode.equals("java")) { %>
			
			<TR>
			<% long maxHeap = de.innovationgate.utils.WGUtils.getMaxHeap(); 
			   List wgaLibraries = new ArrayList(Arrays.asList(core.getBaseLibraryLoader().getURLs()));
			   Iterator libsIt = wgaLibraries.iterator();
			   URL libURL;
			   while (libsIt.hasNext()) {
			   		libURL = (URL) libsIt.next();
			   		if (libURL.toString().indexOf("/WEB-INF/isolated/") != -1) {
			   			libsIt.remove();
			   		}
			   }
			
			
			%>
				<TD>Maximum heap size:</TD>
				<TD><%= (maxHeap != -1 ? jspHelper.numberFormat(new Double(maxHeap / 1024)) + " KB" : "(Not determinable in this Java VM. Must be Version 1.4 or higher)") %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Current heap size:</TD>
				<TD><%= jspHelper.numberFormat(new Double(runtime.totalMemory() / 1024)) %> KB</TD> 
			</TR>
			<!-- hr -->
			<TR>			
				<TD>Unused memory in heap:</TD>
				<TD><%= jspHelper.numberFormat(new Double(runtime.freeMemory() / 1024)) %> KB</TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Java Runtime Vendor:</TD>
				<TD><%=System.getProperty("java.vendor")  %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Java Runtime Version:</TD>
				<TD><%=System.getProperty("java.version")%></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD>Java Virtual Machine:</TD>
				<TD><%=( System.getProperty("java.vm.name") + " Version " + System.getProperty("java.vm.version") + " (" + System.getProperty("java.vm.vendor") + ")") %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD><nobr>Java File Encoding:</nobr></TD>
				<TD><%=(System.getProperty("file.encoding") == null ? "(not set)" : System.getProperty("file.encoding"))%></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD><nobr>Java JIT Compiler:</nobr></TD>
				<TD><%=(System.getProperty("java.compiler") == null ? "(none)" : System.getProperty("java.compiler"))%></TD>
			</TR>
			<!-- hr -->			
			<TR>
				<TD valign="top">Class path from app server:</TD>
				<TD><%= de.innovationgate.utils.WGUtils.strReplace(System.getProperty("java.class.path"), System.getProperty("path.separator"), "<br/>", true) %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD valign="top">Libraries from WGA configuration:</TD>
				<TD><%= de.innovationgate.utils.WGUtils.serializeCollection(wgaLibraries, "<br/>") %>&nbsp;</TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD valign="top">Libraries from WGA Content Stores:</TD>
				<TD><%= de.innovationgate.utils.WGUtils.serializeCollection(core.getSystemContainerManager().getJARDescriptions(), "<br/>") %></TD>
			</TR>			<!-- hr -->
			<TR>
				<TD valign="top">Native library path:</TD>
				<TD><%= de.innovationgate.utils.WGUtils.strReplace(System.getProperty("java.library.path"), System.getProperty("path.separator"), "<br/>", true) %></TD>
			</TR>
			<!-- hr -->
			<TR>
				<TD valign="top">WGA related system properties:</TD>
				<TD><%
				
					java.util.Enumeration propNames = System.getProperties().propertyNames();
					String propName;
					int count=0;
					while (propNames.hasMoreElements()) {
						propName = (String) propNames.nextElement();
						if (propName.startsWith("de.innovationgate.wga")) {
							out.print(propName + " := " + System.getProperty(propName) + "<br/>");
							count++;
						}
					}
					if (count == 0) {
						out.print("(none)");
					}
				
				%>
				</TD>
			</TR>	
			<TR>
				<TD valign="top">TMLScript code cache:</TD>
				<TD><%= ExpressionEngineFactory.getTMLScriptEngine().getScriptCacheCurrentSize() %> cached scripts (Max size: <%= ExpressionEngineFactory.getTMLScriptEngine().getScriptCacheMaxSize() %>) </TD>
			</TR>
			<TR>
				<TD valign="top">WGA design file dache:</TD>
				<TD><%= core.getDesignFileCache().getSize() %> Entries (Max: <%= core.getDesignFileCache().getMaxSize() %>)<br><%= core.getDesignFileCache().getUtilisation() %>% Utilisation</TD>
			</TR>
			
			
	<% } %>
	
	<% if (showMode.equals("statistics")) { %>
	
			<TR>
				<TD valign="top"><div style="overflow:auto;height:450px">
					<TABLE border="0" width="100%" cellspacing="0" cellpadding="0" class="listTable">
						<TR>
							<TD><b>Day</b></TD>
							<TD><b>Time</b></TD>
							<TD><b>WebTML Requests served</b></TD>
						</TR>
			<%
			
				WGAUsageStatistics stats = core.getUsageStatistics();
				Iterator days = stats.getAvailableDays().iterator();
								
				while (days.hasNext()) {
					Date day = (Date) days.next();
					String dayStr = jspHelper.shortDateFormat(day);
					Map hoursMap = stats.getHoursMapForDay(day);
					Iterator hours = hoursMap.keySet().iterator();
					long total = 0;
					while (hours.hasNext()) {
						Integer hour = (Integer) hours.next();
						WGAUsageStatistics.HourStatistic hourStat = (WGAUsageStatistics.HourStatistic) hoursMap.get(hour);
						total+=hourStat.getRequests();
						
						 %>

						
						<TR>
							<TD style="border-bottom:0px; border-top:0px;"><%= dayStr %></TD>
							<TD><%= hour %>:00 - <%= hour %>:59</TD>
							<TD><%= jspHelper.numberFormat(new Long(hourStat.getRequests())) %></TD>
						</TR>		
						
					<%
						dayStr = "&nbsp;";
				    }
				    %>
				    
				    <TR style="line-height:200%">
							<TD style="border-bottom:2px white groove">&nbsp;</TD>
							<TD style="font-weight:bold;">Total for <%= jspHelper.shortDateFormat(day) %></TD>
							<TD style="font-weight:bold;"><%= jspHelper.numberFormat(new Long(total)) %></TD>
					</TR>
				    
			<%
			    } 
			  %>
				</TABLE></div></td>
			</TR>
		
	<% } %>
	
	<% if (showMode.equals("plugins")) { 
	
			String pluginsManagementURL = dispatcher.getPublisherURL(request) + "/plugin-management/html/plugins:main";
			if (core.getContentdbs().containsKey("management-work")) {
			    pluginsManagementURL = dispatcher.getPublisherURL(request) + "/management-work/html/plugins:main";
			}
	
	%>
		
			<TR style="overflow:auto;height:500px">
				<TD align="center" style="width:100%; height:100%">
					<IFRAME STYLE="width:100%; height:500px;" SRC="<%= pluginsManagementURL %>"></IFRAME>			
				</TD>
			</TR>
	<% } %>
	
	<% if (showMode.equals("contentdbs")) { %>			
			<TR>
				<TD align="center" >
					<nobr>
						<input type="button" class="button" value="Details" onclick="contentDbAction('showContentDetails')"/>
						&nbsp;
						<input type="button" class="button" value="Reconnect" onClick="contentDbAction('resetContentDB')"/>
						&nbsp;
						<input id="manageACLButton" style="width:50px" type="button" class="button" value="ACL" onClick="openACL()"/>	
						&nbsp;								
						<button id="clearCacheButton" class="menubutton" onClick="showClearCacheDialog(event, this)">Clear caches</button>
						<div id="clearCacheDialog" style="position:absolute; font-size:8pt; text-align:left; padding:5px; visibility:hidden; border:ridge 2px black; background:url('<%= request.getContextPath() %>/static/images/wgabg.gif');">
							<form id="clearCacheForm">
								<input name="data" checked="true" type="checkbox" value="data"> Data cache<br/>
								<input name="file" checked="true" type="checkbox" value="file"> File cache<br/>
								<input name="tml" checked="true" type="checkbox" value="tml"> WebTML cache<br/>
								<input name="data" checked="true" type="checkbox" value="user"> User caches<br/>
							</form>
							<br/>
							<button style="font-size:8pt" onclick="doClearCache()">Clear</button> <button style="font-size:8pt;" onclick="showClearCacheDialog(event, this)">Cancel</button>
						</div>
						&nbsp;								
						<input id="scriptConsoleButton" type="button" class="button" value="TMLScript console" onClick="tmlScriptConsole()"/>
						&nbsp;						
						<button id="queriesButton" class="menubutton" onClick="showQueriesDialog(event, this)">Querying</button>
						<div class="menu" id="queriesDialog" style="visibility:hidden;">
								<a class="menu" href="javascript:void(0)" onclick="rebuildCSIndex()">Rebuild lucene index</a>
								<a class="menu" href="javascript:void(0)" onclick="queryTestPage()">Query Test Page</a>
						</div>		
						&nbsp;
						<input id="csDumpButton" type="button" class="menubutton" value="CS Dump" onClick="showCSDumpDialog(event, this)"/>
						<div class="menu" id="csDumpDialog">
							<a class="menu" href="javascript:void(0)" onclick="csDump()">Create content store dump</a>
							<a class="menu" href="javascript:void(0)" onclick="loadDump()">Load content store dump</a>
						</div>						
					</nobr>
				</TD>
			</TR>
			<TR>
				<TD valign="top"><div style="overflow:auto;height:450px">
					<TABLE border="0" width="100%" cellspacing="0" cellpadding="0" class="listTable">
						<TR>
							<TD><b>Key</b></TD>
							<TD><b>Title</b></TD>
							<TD><b>Type</b></TD>
							<TD><b>Path</b></TD>							
							<TD><b>Connected</b></TD>
							<TD><b>Master login</b></TD>				
							<TD><b>Login domain</b></TD>							
							
						</TR>
						

			<%
				Map contentdbs = core.getContentdbs();
				List contentKeys = new ArrayList(contentdbs.keySet());
				Collections.sort(contentKeys);
				Iterator keysIt = contentKeys.iterator();
				
				de.innovationgate.webgate.api.WGDatabase contentdb;
				String key = null;
				while (keysIt.hasNext()) {
					contentdb = (de.innovationgate.webgate.api.WGDatabase) contentdbs.get(keysIt.next()); 
					key = (String) contentdb.getAttribute(WGACore.DBATTRIB_DBKEY);
					
					if (key.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
					    	continue;
					}
					
					boolean hasDesign = contentdb.getRoles().contains(de.innovationgate.webgate.api.WGDatabase.ROLE_DESIGN);
					boolean aclManageable = contentdb.hasFeature(de.innovationgate.webgate.api.WGDatabase.FEATURE_ACL_MANAGEABLE);				
					
					
			%>
						<TR onclick="selectContentDB('<%= key %>',<%= ""+hasDesign %>,<%= ""+aclManageable %>)" id="contentDB_row_<%= key %>" style="cursor:pointer">
							<TD><%= key %></TD>
							<TD><%= contentdb.getTitle() %></TD>
							<TD><%= contentdb.getTypeName() %></TD>
							<TD><%= contentdb.getPath() %></TD>
							<TD><%= contentdb.isConnected() %></TD>
							<TD><%= contentdb.getMasterLoginName() %></TD>
							<TD><%= (contentdb.getAttribute(WGACore.DBATTRIB_DOMAIN) == null ? "(none)" : (String) contentdb.getAttribute(WGACore.DBATTRIB_DOMAIN)) %></TD>

							
						</TR>		
						
					<%
					if (action != null && action.equals("showContentDetails") && param.equalsIgnoreCase(key)) {						
					%>
						<script type="text/javascript">
							selectContentDB('<%= key %>',<%= ""+hasDesign %>,<%= ""+aclManageable %>);
						</script>					
						<%
					}
			  } %>
				</TABLE></div></td>
			</TR>
			
			
		
	
	<% } %>
	
	<% if (showMode.equals("persdbs")) { %>
			<TR>
				<TD>
					<TABLE border="0" width="100%" cellspacing="0" cellpadding="0" class="listTable">
						<TR>
							<TD><b>Type</b></TD>							
							<TD><b>Path</b></TD>
							<TD><b>Connected</b></TD>
							<TD align="middle"><b>Actions</b></TD>
						</TR>		
			
			<%
			Map persdbs = core.getPersonalisationdbs();
			List domains = new ArrayList(persdbs.keySet());
			Collections.sort(domains);
			Iterator domainsIt = domains.iterator();
			
			de.innovationgate.webgate.api.WGDatabase persdb;
			while (domainsIt.hasNext()) {
				persdb = (de.innovationgate.webgate.api.WGDatabase) persdbs.get(domainsIt.next());
			 %>
				
						<TR>
							<TD><%= persdb.getTypeName() %></TD>
							<TD><%= persdb.getPath() %></TD>
							<TD><%= persdb.isConnected() %></TD>
							<TD align="center"><input type="button" class="button" value="Reconnect" onClick="action('resetPersDB', '<%= (String) persdb.getAttribute(WGACore.DBATTRIB_DOMAIN) %>')"/></TD>
						</TR>
						<tr><td colspan="5">Master Login: <%= persdb.getMasterLoginName() %> (Domain &quot;<%= (String) persdb.getAttribute(WGACore.DBATTRIB_DOMAIN) %>&quot;)</td></tr>
						
						
			<% } %>
				</table></td>
			</tr>
		
	
	<% } %>
	
	<% if (showMode.equals("applog")) { 
			/*
			if (index == -1) {
				// index = (core.getApplogLines() / 1000 * 1000) + 1;
				index = 0;
			}; 
			*/
			%>		
			<TR>
				<TD>
				
				<%
					StringWriter log = new StringWriter();
					AppLog appLog = WGA.get(core).service(AppLog.class);
					AppLog.Page applogPage = appLog.writePage(index, 1000, log, LogLevel.LEVEL_ALL, false);
					int endLine = applogPage.getEndIndex();
				
				%>
				
				<FORM id="logForm">
					<INPUT TYPE="BUTTON" class="button" VALUE="Refresh" ONCLICK="switchTo('applog', <%= index %>)"/> &nbsp;
				</FORM>
				
				<FORM id="logOutput">
					<TEXTAREA id="logTextArea" ROWS="30" style="font-family:arial,verdana,monospace;font-size:12px;float:left;width:100%;" readonly="readonly" ><%= log.toString() %></TEXTAREA><BR><BR>
				</FORM>
				
				<div width="100%"  align="middle">			
					<INPUT TYPE="BUTTON" class="button" VALUE="First page" ONCLICK="switchTo('applog', 1)"/>
					<INPUT TYPE="BUTTON" class="button<% if (index <= 1) { %>Disabled<% } %>" VALUE="Previous page" ONCLICK="switchTo('applog', <%= (index > 1000 ? index - 1000 : 1) %>)" <% if (index <= 1) { %>DISABLED<% } %>/> 
					<INPUT TYPE="BUTTON" class="button" VALUE="Next page" ONCLICK="switchTo('applog', <%= index + 1000 %>)" />
					<INPUT TYPE="BUTTON" class="button" VALUE="Last page" ONCLICK="switchTo('applog', -1)"/>
					<INPUT TYPE="BUTTON" class="button" VALUE="Scroll to end" ONCLICK="document.forms['logOutput'].logTextArea.scrollTop = document.forms['logOutput'].logTextArea.scrollHeight - 31;"/>
				</div>
								
				</TD>
			</TR>
		
	
	<% } %>
		
	<% if (showMode.equals("scheduler")) { %>
		
		
		<tr><td colspan="2">
		<form name="frmScheduler" method="post" action="scheduler.jsp" target="_blank">
			<input type="hidden" name="job" value=""/>
			<input type="hidden" name="run" value=""/>					
			
			<table border="0" cellspacing="0" cellpadding="0" width="100%"> 
				<tr>
				<td colspan="5" align="middle">
					<input type="button" class="button" value="Execute" onClick="execute();return false"/>&nbsp;
					<input type="button" class="button" value="View log" onClick="viewLog();return false"/></td>
				</tr>
				<tr>
					<td><b>Job name</b></td>
					<td><b>Description</b></td>
					<td><b>Last run</b></td>
					<td><b>Next scheduled run</b></td>
					<td><b>Running now</b></td>					
				</tr>
				
				<%
				de.innovationgate.wgpublisher.scheduler.Scheduler scheduler = core.getScheduler();
				List jobNamesList = new ArrayList(scheduler.getJobNames());
				Collections.sort(jobNamesList);
				java.util.Iterator jobNames = jobNamesList.iterator();
				de.innovationgate.wgpublisher.scheduler.Job job;
				java.util.Date nextScheduledRun;
				while (jobNames.hasNext()) {
					job = (de.innovationgate.wgpublisher.scheduler.Job) scheduler.getJob((String) jobNames.next());
					nextScheduledRun = job.nextScheduledRun();
				%>
				<tr onclick="selectJob('<%= job.getName() %>',this)" style="cursor:pointer">
					<td><%= job.getName() %></td>
					<td><%= job.getDescription() %></td>
					<td><%= (job.getLastRun() != null ? String.valueOf(job.getLastRun()) : "(Never in this WGA runtime)") %></td>
					<td><%= (nextScheduledRun != null ? nextScheduledRun.toString() : "(Not scheduled)") %></td>
					<td><%= (job.isRunning() ? "YES" : "no") %></td>
					
				</tr>
				
				<% 
				} %>
				
				
			</TABLE>
		</FORM></td></tR>
	<% } %>
		
	<% if (showMode.equals("tools")) { %>
	
		
			<tr><td colspan="2">&nbsp;</td></tr>
			<% if (!message.equals("")) {
					out.write("<tr><td colspan=\"2\" style=\"color:red;font-size:12px;font-weight:bold;\">");
					out.write(message);
					out.write("</td></tr>");
			   }
			%>
			<TR>
				<TD colspan="2">
					<ul>
						<li><a class="tab" style="font-size:12px; font-weight: bold;" href="blockedlogins.jsp">Blocked logins</a> <div class="small" style="padding-bottom: 8px">Displays a list of logins that are blocked because of too many wrong login attempts. Allows resetting login blockade.</div>
						<li><a class="tab" style="font-size:12px; font-weight: bold;" href="lookuptml.jsp">Lookup WebTML module name</a> <div class="small" style="padding-bottom: 8px">Allows reverse lookups of WebTML module names for given jsp names.</div>
					</ul>
				</TD>
			</TR>
		
	
	<% } %>
	</TABLE></TD>
	</tr>
	</TABLE>
<% } %>

<jsp:include page="static/inc_foot.jsp" flush="true"/>
<!-- Content database details -->
<% if (action != null && action.equals("showContentDetails")) {

	de.innovationgate.webgate.api.WGDatabase db = (de.innovationgate.webgate.api.WGDatabase) core.getContentdbs().get(param);
	
	if (db != null) {
	    
	    if (!db.isSessionOpen()) {
			db.openSession();   
		}
	    
		de.innovationgate.webgate.api.WGDatabase.WGDatabaseStatistics statistics = db.getStatistics();
		FileCache fileCache = (FileCache) db.getAttribute(WGACore.DBATTRIB_FILECACHE);
		boolean designUpdate = true;
		
		String lastChanged = (db.hasFeature(de.innovationgate.webgate.api.WGDatabase.FEATURE_LASTCHANGED) && db.getLastChanged() != null ? jspHelper.shortDateTimeFormat(db.getLastChanged()) : "(Not supported)");
		String title = db.getTitle();
		String server = db.getServerName();
		String masterLoginName = db.getMasterLoginName();
		String defaultLanguage = db.getDefaultLanguage();
		if (defaultLanguage == null) {
			defaultLanguage = "(none)";
		}
		
		boolean monitorLastChange = db.monitorLastChange();
		int maxCores = db.getMaxCores();
		int exceedingSessions = db.getSessionStatistics().size();
		String exceedingSessionStr = (exceedingSessions >= 100 ? "100 or more" : String.valueOf(exceedingSessions));
		String designSource = "internal";
		
		if (db.getDesignProvider() != null) {
		    designSource = db.getDesignProvider().getName(); 
		}
		else {
			DesignSyncManager syncManager = (DesignSyncManager) db.getAttribute(WGACore.DBATTRIB_DESIGNSYNC);
			if (syncManager != null) {
			    designSource = "Full Design synchronisation with folder \"" + syncManager.getDesignPath() + "\"";
			}
		}
		
		
		String authSource = (db.getAuthenticationModule() != null ? db.getAuthenticationModule().getAuthenticationSource() : db.hasFeature(WGDatabase.FEATURE_EXTERNAL_AUTHENTICATION) ? "none" : "internal");
		String dbDomain = (db.getAttribute(WGACore.DBATTRIB_DOMAIN) == null ? "(none)" : (String) db.getAttribute(WGACore.DBATTRIB_DOMAIN));
		
		String languageBehaviour = (String) db.getAttribute(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR);
		if (languageBehaviour == null) {
		    languageBehaviour = WGACore.LANGUAGEBEHAVIOUR_DEFAULT;
		}
		boolean isMultiLanguageContent = db.getBooleanAttribute(WGACore.DBATTRIB_MULTILANGUAGE_CONTENT, true);
		String multiLanguageContent = (isMultiLanguageContent ? "ENABLED" : "disabled");
		
		CSConfig csConfig = (CSConfig) db.getAttribute(WGACore.DBATTRIB_CSCONFIG);
		String designBehaviour = db.getComplianceVersion().getMainVersionString();
		
		String homePage = "(not set)";
		String hpAttribute = (String) db.getAttribute(WGACore.DBATTRIB_HOME_PAGE);
		if (!WGUtils.isEmpty(hpAttribute)) {
		    homePage = hpAttribute;
		}

		String loginPage = "(not set)";
		String lpAttribute = (String) db.getAttribute(WGACore.DBATTRIB_LOGIN_PAGE);
		if (!WGUtils.isEmpty(lpAttribute)) {
		    loginPage = lpAttribute;
		}
		
		String projectMode = (db.isProjectMode() ? "ENABLED" : "disabled");
		String csVersion = Double.valueOf(db.getContentStoreVersion()).toString();
		
	
%>

<div id="contentDetails" style="background-color:gainsboro; position:absolute;left:100px;top:10px;width:500px; border:2px outset darkgray">
<nobr><span style="font-size:11pt; font-weight:bold; line-height:120%;">Content Database: <%= param %></span></nobr>
<div style="height:400px; overflow:scroll">
	<table cellspacing="0" cellpadding="0" class="listTable" background="<%= request.getContextPath() %>/static/images/wgabg.gif">
	
		<tr>
			<td>Title:</td><td><%= title %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Path:</td><td><%= db.getPath() %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Database server:</td><td><%= server %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Last changed:</td><td><%= lastChanged %> (<%= (monitorLastChange ? "monitored" : "not monitored") %>)</td>
		</tr>
		<!-- hr -->
		<tr>
			<td colspan="2" style="background-color:gainsboro; font-weight: bold;">Configuration</td>
		</tr>
		<tr>
			<td>Home page:</td><td><%= homePage %></td>
		</tr>
		<tr>
			<td>Login page:</td><td><%= loginPage %></td>
		</tr>
		<tr>
			<td>Project mode:</td><td><%= projectMode %></td>
		</tr>
		<tr>
			<td>Content Store Version:</td><td><%= csVersion %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td colspan="2" style="background-color:gainsboro; font-weight: bold;">Authentication</td>
		</tr>
		<tr>
			<td>Master Login:</td><td><%= masterLoginName %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Login Domain:</td><td><%= dbDomain %></td>
		</tr>
		<!-- hr -->	
		<tr>
			<td>Authentication source:</td><td><%= authSource %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td colspan="2" style="background-color:gainsboro; font-weight: bold;">Languages</td>
		</tr>
		<tr>
			<td>Default language:</td><td><%= defaultLanguage %></td>
		</tr>
			<!-- hr -->
		<tr>
			<td>Language behaviour:</td><td><%= languageBehaviour %></td>
		</tr>
		<tr>
			<td>Multi Language Support:</td><td><%= multiLanguageContent %></td>
		</tr>
		<tr>
			<td colspan="2" style="background-color:gainsboro; font-weight: bold;">Resources and Caches</td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Max docs per session:</td><td><%= maxCores %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Sessions exceeding max docs:</td><td><%= exceedingSessionStr %> (See activity for details)</td>
		</tr>
		<!-- hr -->
		<% if (statistics != null) { %>
		<tr>
			<td>Document cache size:</td><td><%= statistics.getDocumentCount() %> Documents</td>
		</tr>
		<!-- hr -->
		<% } %>
		<tr>
			<td>File cache size:</td><td><%= fileCache.getActualSize()  %> Entries (Maxs: <%= fileCache.getMaxSize() %> Entries)</td>
		</tr>
		<!-- hr -->
		<tr>
			<td>File cache threshold:</td><td><%= fileCache.getThreshold() / 1024 %> KB</td>
		</tr>
		<!-- hr -->
		<tr>
			<td>User cache latency:</td><td><%= db.getUserCacheLatency()  %> minutes</td>
		</tr>
		<!-- hr -->
		<tr>
			<td colspan="2" style="background-color:gainsboro; font-weight: bold;">Design</td>
		</tr>
		<tr>
			<td>Design source:</td><td><%= designSource %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Design version compliance:</td><td><%= designBehaviour %></td>
		</tr>
		<!-- hr -->
		<tr>
			<td>Design updates:</td><td><a href="javascript:action('switchDesignEnabling','<%= param %>')"><%= (designUpdate ? "enabled (click to disable)" : "disabled (click to enable)") %></a></td>
		</tr>
	</table>
</div>

<button class="button" onclick="switchTo('contentdbs')">Close</button>

</div>

<%
			}
		} 

%>

<%
}
finally {
	de.innovationgate.webgate.api.WGFactory.getInstance().closeSessions();
}
%>

</BODY>
</HTML>
