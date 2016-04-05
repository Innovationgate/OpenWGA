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
<%@page import="de.innovationgate.wgpublisher.WGACore"%>
<%@page import="de.innovationgate.wgpublisher.WGAError"%>
<%@page import="de.innovationgate.wga.server.api.WGA"%>
<%@page import="de.innovationgate.wgpublisher.webtml.utils.AjaxInfo"%>
<%@ page language="java" pageEncoding="ISO-8859-1"%>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
	WGAError error = (WGAError) request.getAttribute(WGACore.ATTRIB_WGAERROR);
	WGA wga = WGA.get(jspHelper.getCore());
	
%>
<% if (!"norefresh".equals(request.getParameter("$ajaxMode"))) { %>
<script type="text/javascript">
<% } %>

<% if ("true".equals(System.getProperty("de.innovationgate.license.DevelopmentModeEnabled"))) { 
    String msg1 = wga.encode("javascript", jspHelper.getLabels("errors").getString("ajaxerror.message.dev.1"));
    String msg2 = wga.encode("javascript", error.getSubMessage());
    String msg3 = wga.encode("javascript", jspHelper.getLabels("errors").getString("ajaxerror.message.dev.2"));
%>
	if (confirm("<%= msg1 + "\\n\\n" + msg2+ "\\n\\n" + msg3 %>")) {
		location.reload();
	}
<% } else {
    String msg = wga.encode("javascript", jspHelper.getLabels("errors").getString("ajaxerror.message"));
%>
	alert("<%= msg %>");
	location.reload();
<% } %>
<% if (!"norefresh".equals(request.getParameter("$ajaxMode"))) { %>
</script>
<% } %>
