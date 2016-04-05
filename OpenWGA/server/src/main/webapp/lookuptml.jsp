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
		response.sendRedirect("wgadmin.jsp?wheretogo=lookuptml.jsp");
		return;
	}

	String jspName = request.getParameter("jspName");
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<TITLE>Lookup tml module name</TITLE>
<link rel="stylesheet" type="text/css" href="<%= request.getContextPath() %>/static/css/wga.css">

</HEAD>
<BODY>
<form method="post">
<jsp:include page="static/inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="Lookup tml module name"/>	
</jsp:include>


<table border="0" class="staticforms" cellspacing="0" cellpadding="10" width="100%"> 

	<tr>
		<td nowrap>JSP file:</td>
		<td nowrap><input type="text" size="40" maxlength="32" name="jspName" <% if (jspName != null) {%> value="<%=jspName%>" <% } %> />.jsp</td>
		<td nowrap><input class="button" type="submit" value="lookup"/></td>
	</tr>				
		<%	
		if (jspName != null) {
			%>
			<tr>
				<td nowrap>TML module:</td>
				<td nowrap colspan="2"><%=core.getDeployer().filenameToTMLModuleName(jspName.toLowerCase() + ".jsp")%></td>
			</tr>
			<%
		}
	%>
</TABLE>
<jsp:include page="static/inc_foot.jsp" flush="true"/>
</form>
</BODY>
</HTML>
