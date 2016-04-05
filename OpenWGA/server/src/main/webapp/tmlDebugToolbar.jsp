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
<%@ page import="de.innovationgate.wgpublisher.WGACore" %>
<%
if (!jspHelper.isAdminLoggedIn()) {
	response.sendError(HttpServletResponse.SC_FORBIDDEN, "You must be logged in to WGA admin page to use this");
	return;
}

String path = request.getContextPath();
String basePath = request.getScheme()+"://"+request.getServerName()+":"+request.getServerPort()+path+"/";
%>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <base href="<%=basePath%>">
    
    <title>WebTML debug toolbar</title>
    
    <meta http-equiv="pragma" content="no-cache">
    <meta http-equiv="cache-control" content="no-cache">
    <meta http-equiv="expires" content="0">
    
    <script language="javascript">
    
    	function cmd(param) {
    		top.main.location.href = '<%=path%>/tmlDebug?command=' + param;
    	}
    
    </script>
    
    <!--
    <link rel="stylesheet" type="text/css" href="styles.css">
    -->
  </head>
  
  <body>
  	<span style="font-size:14pt; font-weight:bold;">WebTML debugging console</span>&nbsp;&nbsp;
    <button onclick="cmd('status')">Status</button>&nbsp;
    <button onclick="cmd('toggletmlscriptoptimization')">Switch TMLScript stack traces on/off</button>&nbsp;
    <button onclick="cmd('toggledebug')">Switch debug on/off</button>&nbsp;
    <button onclick="cmd('toggleresulttrace')">Switch result tracing on/off</button>&nbsp;
     <button onclick="cmd('toggleoptionstrace')">Switch options tracing on/off</button>&nbsp;
    <button onclick="cmd('list')">List</button>&nbsp;
  </body>
</html>
