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
<%@ page language="java" pageEncoding="ISO-8859-1" errorPage="error.jsp"%>
<%
	WGACore core = WGACore.retrieve(pageContext);
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
%>
<%@ page isThreadSafe="true" errorPage="error.jsp" %>
<%@ page import="de.innovationgate.webgate.api.*,de.innovationgate.wgpublisher.*,java.util.*" %>
<%
	String errorMsg = null;
	if (!core.isAdminLoggedIn(request)) {
		response.sendRedirect("wgadmin.jsp");
		return;
	}		
	else{
		if (request.getParameter("job") == null ){
			errorMsg = "Missing Parameter";
		}
	}
	
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<TITLE>WGA Job <%= request.getParameter("job") %></TITLE>
<link rel="stylesheet" type="text/css" href="<%= request.getContextPath() %>/static/css/wga.css">

<script language="javascript">

function toggleAutoUpdate() {
	if(document.forms['schedulerForm'].elements['autoUpdate'].checked) {
		interval = setInterval("autoUpdateTask()", 3000);	
	}
	else {
		clearInterval(interval);
	}
}

function autoUpdateTask() {
	document.getElementById("iframe").contentWindow.location.reload();
}

var interval = setInterval("autoUpdateTask()", 3000);
</script>

</HEAD>
<BODY>
<% String docTitle = "WGA Job '" + request.getParameter("job") + "'"; %>
<jsp:include page="static/inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="<%= docTitle %>"/>	
</jsp:include>

<table border="0" class="staticforms" cellspacing="0" cellpadding="10" width="100%" height="100%"> 
<% if ( errorMsg != null ) { 
			out.write("<tr><td style=\"color:red;font-size:12px;font-weight:bold;\">");
			out.write(errorMsg);
			out.write("</td></tr>");
   }else{ %>
<tr>
		<td><b>Execution log</b></td>
</tr>		
<% 
	if ( request.getMethod().equalsIgnoreCase("post") && request.getParameter("run") != null && request.getParameter("run").equals("yes")) {
		core.getScheduler().run(request.getParameter("job"), "WGA Administrator", null, null);
	}
%>
	<tr>		
		<td width="100%" height="100%">
			<iframe id="iframe" scrolling="auto" height="100%" width="100%" src="<%= core.getDispatcher().getPublisherURL(request) %>/joblog?name=<%= request.getParameter("job") %>#bottom">
			</iframe>
		</td>
	</tr>
<% } %>
	<tr>
		<td>
			<form id="schedulerForm">
				<button class="button" onclick="self.close();return false">Close</button> &nbsp;
				<input type="checkbox" name="autoUpdate" onclick="toggleAutoUpdate()" CHECKED> Automatic update
			</form>			
		</td>
	</tr>	
</TABLE>
<jsp:include page="static/inc_foot.jsp" flush="true"/>

</BODY>
</HTML>
