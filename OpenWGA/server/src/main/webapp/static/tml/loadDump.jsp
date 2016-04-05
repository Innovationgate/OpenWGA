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
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>
<%
	String errorMsg = null;
	de.innovationgate.wgpublisher.WGACore core = de.innovationgate.wgpublisher.WGACore.retrieve(pageContext);
	if (!core.isAdminLoggedIn(request)) {
		try {
			response.resetBuffer();
			response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN, "You must first login to the admin page");
		}
		catch (java.io.IOException e) {
			core.getLog().error("Error redirecting to admin page", e);
		}
		return;
	}

	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
%>
<tml:root resource="csDump.jsp">
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>

<HEAD>
<tml:htmlhead/>
<META http-equiv="Content-Style-Type" content="text/css">

<TITLE>Load Dump into "<tml:meta type="database" name="title"/>" (<tml:meta type="database" name="dbkey"/>)</TITLE>

<style type="text/css">

table.msgbox {
	padding: 5px;
	border-width: 1px;
	border-style:solid;
	border-color: white;
	font-weight:bold;
	font-size:12pt;
}

</style>

<script language="javascript">
function doLoad(actionRef) {
	if (confirm('Loading a dump will delete all data that this content store currently contains. Sure to continue?')) {
		callAction(actionRef);
	}
}
</script>

<tml:action id="execute">
	var file = null;
	var fileCreated = false;
	var options = createLookupTable();
	try {
	
		// Store dump file to disk
		var tempFile = java.io.File.createTempFile("dmp", ".tmp");
		var inStr = new java.io.FileInputStream(tmlform.getfile(tmlform.dump));
		var outStr = new java.io.FileOutputStream(tempFile);
		Packages.de.innovationgate.utils.WGUtils.inToOut(inStr, outStr, 2048);
		inStr.close();
		outStr.close();
		
		// Perform update
		
		options.put("dbkey", meta("db", "dbkey"));
		options.put("filename", tempFile.getAbsolutePath());
		options.put("deletefile", "true");
		
		if (tmlform.filterExpression != "") {
			options.put("filter", tmlform.filterExpression);
		}
		
		var name = wgacore.runTransientTask("de.innovationgate.wgpublisher.scheduler.LoadDumpTask", "loadContentStoreDump", options); 
		redirectTo(meta("request", "wgaurl") + "/scheduler.jsp?job=" + name);
		
	}
	catch (e) {
			logException(e);
			errormsg = "An error occured: " + e.message;
			if (e.javaException && e.javaException.cause != null) {
				errormsg += ". Cause: " + e.javaException.cause.message;
			}
	}
</tml:action>

</HEAD>
<BODY topmargin="5" marginheight="5" leftmargin="5" marginwidth="5">

<%
de.innovationgate.wgpublisher.webtml.utils.TMLContext context = jspHelper.getMainContext();
de.innovationgate.webgate.api.WGDatabase db = context.content().getDatabase();
	
String dbKey = (String) db.getAttribute(de.innovationgate.wgpublisher.WGACore.DBATTRIB_DBKEY);
String docTitle = "Content Store Dump of " + db.getTitle() + " (" + dbKey + ")";
%>
<jsp:include page="../inc_head.jsp" flush="true">	
<jsp:param name="frmTitle" value="<%= docTitle %>"/>
</jsp:include>

<tml:script>
if (!db().hasFeature(db().FEATURE_FULLCONTENTFEATURES)) {
	errormsg = "You cannot load a dump to this database because it is no WGA Content Store";
	disableDisplay = true;
}

</tml:script>

<tml:case isdefined="errormsg">
	<table class="msgbox" style="background-color:red;color:white;">
		<tr><td>
			Error: <tml:item name="errormsg"/>
		</td></tr>
	</table>
</tml:case>

<tml:case isdefined="infomsg">
	<table class="msgbox" style="background-color:lightblue;color:black;">
		<tr><td>
			<tml:item name="infomsg"/>
		</td></tr>
	</table>
</tml:case>

<tml:case isfalse="disableDisplay">
	<tml:form id="dump" source="none">
		<p>
			<b>Dump file: </b> <br/><tml:input name="dump" type="file"/>
		</p>
		<p>
			<button onclick="doLoad('<tml:action ref="execute"/>')">Execute</button>
		</p>
	</tml:form>

	<table style="background-color:lightblue;color:black;font-weight:bold; font-size:12pt; border-style:solid; border-width:1px; ">
			<tr><td>
				NOTE: Loading a dump will delete all content data and schema from this content store!
			</td></tr>
	</table>
</tml:case>

<tml:warnings autohide="true"/>
<jsp:include page="../inc_foot.jsp" flush="true"/>
</BODY>
</HTML>
</tml:root>


