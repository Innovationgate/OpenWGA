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
<%@ page language="java" pageEncoding="ISO-8859-1"%>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
%>
<%@ page isThreadSafe="true" errorPage="error.jsp" %>
<%@ page import="de.innovationgate.webgate.api.*,de.innovationgate.wgpublisher.*,java.util.*" %>
<%@ page import="de.innovationgate.wgpublisher.WGPDispatcher" %>
<%@ page import="java.util.ConcurrentModificationException" %>
<%@ page import="de.innovationgate.wgpublisher.auth.LoginAttemptInformation" %>
<%@ page import="java.util.List" %>
<%@ page import="de.innovationgate.wgpublisher.auth.BruteForceLoginBlocker" %>
<%

	String errorMsg = null;
	WGACore core = WGACore.retrieve(pageContext);
	
	if (!core.isAdminLoggedIn(request)) {
		response.sendRedirect("wgadmin.jsp?wheretogo=blockedlogins.jsp");
		return;
	}
		
	
	BruteForceLoginBlocker blocker = core.getBruteForceLoginBlocker();
	String resetName = request.getParameter("resetName");
	String resetDomain = request.getParameter("resetDomain");
	if (request.getMethod().equals("POST")) {
		blocker.clearFailedLoginAttempts(resetDomain, resetName);
	}

%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<TITLE>WGA Blocked Logins</TITLE>
<link rel="stylesheet" type="text/css" href="<%= request.getContextPath() %>/static/css/wga.css">

<script type="text/javascript">

function resetName(domain, name) {
	var form = document.forms[0];
	form.resetDomain.value = domain;
	form.resetName.value = name;
	form.submit();
}

</script>

</HEAD>
<BODY>
<form method="post">
<jsp:include page="static/inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="Currently blocked logins"/>	
</jsp:include>

<input type="hidden" name="resetName"/>
<input type="hidden" name="resetDomain"/>

<table border="0" class="staticforms" cellspacing="0" cellpadding="10" width="100%"> 

<% if ( errorMsg != null ) { 
			out.write("<tr><td colspan=\"3\" style=\"color:red;font-size:12px;font-weight:bold;\">");
			out.write(errorMsg);
			out.write("</td></tr>");
	   } %>	


   			
	
 	<tr>
		<td colspan="4">&nbsp;</td>
	</tr>
		   

	<tr>
		<td><b>Domain</b></td>
		<td><b>Username</b></td>
		<td><b>Blocked since</b></td>
		<td><b>Actions</b></td>
	</tr>

	<%
	try {
	List logins = blocker.getBlockedLogins();
	Iterator loginsIt = logins.iterator();
	LoginAttemptInformation inf;
	while (loginsIt.hasNext()) {
		inf = (LoginAttemptInformation) loginsIt.next();		
	%>
	<tr>
		<td nowrap><%= inf.getDomain() %></td>
		<td nowrap><%= inf.getName() %></td>
		<td nowrap><%= inf.getBlockedDate() %></td>
		<td nowrap><a href="javascript:resetName('<%= inf.getDomain() %>', '<%= inf.getName() %>')">Reset</a></td>
	</tr>
	<tr><td colspan="4"><hr/></td></tr>
	<% } %>
	<tr>
		<td colspan="3">&nbsp;</td>
	</tr>
	
	<% }
	catch (ConcurrentModificationException e) {
		out.write("Information about blocked logins at the moment not available. Pleasy try again.");
	}
	%>
	
	
</TABLE>
<jsp:include page="static/inc_foot.jsp" flush="true"/>
</form>
</BODY>
</HTML>
