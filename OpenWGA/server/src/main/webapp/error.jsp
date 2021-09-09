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
<%@ page session="false" %>
<%@ page isErrorPage="true" %>
<%@ page import="de.innovationgate.wgpublisher.webtml.utils.HttpErrorException,de.innovationgate.wgpublisher.jsputils.*" %>
<%@ page import="de.innovationgate.wgpublisher.*" %>
<%@ page import="de.innovationgate.wga.server.api.*" %>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%
ErrorPageManager manager = new ErrorPageManager(pageContext);
if (manager.isDisplayDefaultErrorPage()) {
	WGAError interpreter = manager.getError();
	WGA wga = manager.getWga();
	JspHelper jspHelper = manager.getJspHelper();
%>
<!DOCTYPE html>
<html>
	<head>
		<META name="robots" content="noindex, nofollow">
		<TITLE><%= wga.encode("html", WGABrand.getName()) %> Error</TITLE>
		<%= jspHelper.getContentInfoScript() %>
		<style>
			body {
				width:70%;
				margin:20px auto;
			}
			body *{
				font-size:18px;
				font-family: verdana, helvetica;
			}
			.msg{
				margin: 10px 0				
			}
			.msg label{
				float:left;
				width: 220px;
				color: gray;
			}
			.msg div{
				margin-left: 230px;
			}
		</style>
	</head>
	<body>
		<div style="text-align:right">
			<img src="<%= request.getContextPath() %>/static/images/brand/logo_200.png" style="width:100px">
		</div>

		<div style="font-size:125px;text-shadow:1px 1px 5px gray;color:#ffc800">
			<%= interpreter.getErrorCode() %>
		</div>		
		
		<% if (interpreter.getErrorCode() == 200) { %>
			<p>Alles klar soweit.</p>
			<p>Everything works fine.</p>
		<% }else{ %>
			<h1>Es ist leider ein Fehler aufgetreten
			<br>
			<%= interpreter.getMainMessage() %></h1>
			
			<% if (interpreter.getErrorCode() == 404) { %>
				<p style="padding:5px 0 25px 0;border-bottom:dotted silver 1px">
					Die angeforderte Seite konnte nicht gefunden werden.
					<br>The requested page could not be found.
				</p>
			<% }else{ %>
				<p style="padding:5px 0 25px;border-bottom:dotted silver 1px">
					<%= interpreter.getException().getMessage() %>
				</p>
			<% } %>
			
			<% if (interpreter.getDetailMessage() != null) { %>
				<H1>Message:</H1>
				<code><%= interpreter.getDetailMessageHTML() %></code>
			<% } %>
			
			<% if (interpreter.getErrorCode() == 403 && interpreter.getDbHint() != null) {
				
				de.innovationgate.wga.server.api.Domain domain = wga.database(interpreter.getDbHint()).domain();
				if (domain != null) {
					de.innovationgate.wga.server.api.Auth auth = domain.auth();
					if (auth.getLoginName() != null && auth.getAuthenticationType().equals("password")) {		
					%>
						<p style="padding:5px 0 25px 0">
							Your are currently logged in as user '<%= wga.encode("html", auth.getLoginName()) %>'.
							<br>
							<button onclick="location.href='<%= wga.getRequest().getContextPath() %>/<%= interpreter.getDbHint() %>/logout'">Logout</button>
						</p>
					<%}
				 }
			}
		} %>
		
		<a href="#" onclick="document.getElementById('stackTrace').style.display='block';this.style.display='none';return false">Show details</a>
		<div id="stackTrace" style="display:none">
			<p style="">
				<strong><%= interpreter.getSubMessage() %></strong>
			</p>
			<%= interpreter.getTechnicalInformation() %> 
		</div>
	</body>
</html>
<% } %>