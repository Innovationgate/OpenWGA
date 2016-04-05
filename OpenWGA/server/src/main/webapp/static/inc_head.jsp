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
<%@ page language="java" contentType="text/html" pageEncoding="ISO-8859-1" %>
<% 
	String title = request.getParameter("frmTitle");
	String frmTabWidth = request.getParameter("frmTabWidth");
	String frmTabHeight = request.getParameter("frmTabHeight");
	
	if( frmTabWidth==null ) frmTabWidth = "100%";
	if( frmTabHeight==null ) frmTabHeight = "100%";
	
%>
<LINK href="<%= request.getContextPath() %>/static/css/wga.css" rel="stylesheet" type="text/css">
<style type="text/css">
<!-- 
.button {
	padding-left:10px;
	padding-right:10px;
	height:24px;
	background-image:url("<%= request.getContextPath() %>/static/images/wgabgButton.gif");		
	font-family:Arial,Verdana,Helvetia;
	font-size: 12px;
	font-weight:bold;	
	color:#000000;
	background-color:#ffc800;
	border:inset 2px white;
}

.button:hover {
	background-image:url("<%= request.getContextPath() %>/static/images/wgabgButton_hover.gif");;
}

.menuButton {
	padding-left:10px;
	padding-right:10px;
	height:24px;
	background-image:url("<%= request.getContextPath() %>/static/images/wgabgMenuButton.gif");		
	font-family:Arial,Verdana,Helvetia;
	font-size: 12px;
	font-weight:bold;	
	color:#000000;
	background-color:#ffc800;
	border:inset 2px white;
}

.menuButton:hover {
	background-image:url("<%= request.getContextPath() %>/static/images/wgabgMenuButton_hover.gif");;
}

.buttonDisabled {
	padding-left:10px;
	padding-right:10px;
	height:24px;
	background-image:url("<%= request.getContextPath() %>/static/images/wgabgButton_disabled.gif");		
	font-family:Arial,Verdana,Helvetia;
	font-size: 12px;
	font-weight:bold;	
	color:#000000;
	background-color:#ffc800;
	border:inset 2px white;
}

-->
</style>
<table width="<%= frmTabWidth %>" height="<%= frmTabHeight %>" border="0" cellpadding="0" cellspacing="0">
<tr>
	<td height="80" background="<%= request.getContextPath() %>/static/images/wgabg.gif" valign="top">
	<table border="0" cellpadding="0" cellspacing="0" width="100%" height="100%">
		<tr><td width="100%">
			<table border="0" cellpadding="0" cellspacing="0" width="100%" height="100%" background="<%= request.getContextPath() %>/static/images/tab_head_middle.gif">
			<tr>
				<td rowspan="2"><img src="<%= request.getContextPath() %>/static/images/tab_head_left.gif" border="0"/></td>
				<td><img src="<%= request.getContextPath() %>/static/images/trans.gif" width="1" height="20"/></td>
				<TD><img src="<%= request.getContextPath() %>/static/images/trans.gif" width="1" height="20"/></TD>
			</tr>		
			<tr>
				<TD width="1" height="60"><img src="<%= request.getContextPath() %>/static/images/trans.gif" width="1" height="60"/></TD>
				<td width="100%" height="100%" valign="top"><b style="font-size:15px"><%= title %></b></td></tr>
			</table></td>
			<td><img src="<%= request.getContextPath() %>/static/images/tab_head_right.gif" border="0"/></td>
		</tr>
	</table></td>		
</tr>
<tr>
	<td background="<%= request.getContextPath() %>/static/images/wgabg.gif" valign="top" class="mainContentTable">
		
