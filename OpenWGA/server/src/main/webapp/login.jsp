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
<%@page import="de.innovationgate.wgpublisher.WGABrand"%>
<%@page import="de.innovationgate.wgpublisher.WGAVersion"%>
<%@page import="org.apache.commons.httpclient.util.URIUtil"%>
<%@page import="de.innovationgate.wgpublisher.WGACore"%>
<%@ page language="java" pageEncoding="ISO-8859-1"%>
<%@ page isThreadSafe="true" errorPage="error.jsp" %>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	de.innovationgate.wga.server.api.WGA wga = 	de.innovationgate.wga.server.api.WGA.get(jspHelper);
	jspHelper.setContentType("text/html");
	String domain = request.getParameter("domain");
	if (domain == null) {
		domain = "default";
	}
	domain = wga.encode("html", domain);
	
	if (WGACore.DOMAIN_ADMINLOGINS.equals(domain) && !jspHelper.getCore().isAdministrativePort(request.getLocalPort())) {
	    response.sendError(403, "Access to administrative resources is disabled");
	    return;
	}
	
	String authDomain = request.getParameter("authDomain");
	if (authDomain != null) {
		authDomain = wga.encode("html", authDomain);
	}

	String redirect = "";
	if (request.getParameter("redirect") != null) {
		redirect = wga.encode("html", jspHelper.getCore().getURLEncoder().decode(request.getParameter("redirect")));
	}
	
	
	String errorMsg = (String) request.getAttribute(WGACore.ATTRIB_LOGINERROR);
	if (errorMsg != null) {
		errorMsg = wga.encode("html", errorMsg);
	}
	
	// if domain has certauth enabled redirect immediately
	// per definition a domain is enabled for certauth if
    // - the authmodule configured on domainlevel has certauth enabled
    // - or one db in this domain has an certauth-enabled authmodule	
	if (jspHelper.getCore().certAuthEnabledForDomain(domain)) {
		response.sendRedirect(redirect);
	}
	
	// Enforce login redirection, if configured
	java.util.Map<String,String> serverOptions = wga.getCore().getWgaConfiguration().getServerOptions();
	String redirectProtocol = serverOptions.get(WGACore.SERVEROPTION_LOGINREDIRECTPROTOCOL);
	String redirectHost = serverOptions.get(WGACore.SERVEROPTION_LOGINREDIRECTHOST);
	String redirectPort = serverOptions.get(WGACore.SERVEROPTION_LOGINREDIRECTPORT);
	java.net.URL originalURL = de.innovationgate.wgpublisher.WGPRequestPath.buildCompleteURL(request);
	java.net.URL redirectURL = de.innovationgate.wgpublisher.WGPRequestPath.determineRedirectionURL(originalURL, redirectProtocol, redirectHost, redirectPort);
	if (redirectURL != null) {
		response.sendRedirect(redirectURL.toString());
	}

%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">

<%@page import="de.innovationgate.wgpublisher.WGACore"%><html>
<head>
	<title><%= wga.encode("html", WGABrand.getName()) %> Login</title>
	<meta name="robots" content="noindex">

	<script type="text/javascript">
		function init(){
		  document.forms[0].username.focus(); 
		}
	</script>
	
	<style>
		body{
			text-align: center;
		}
		body *{
			font: 16px helvetica;
		}
		img{
			max-width: 100%;
		}
		#content{
			text-align: left;
			margin:100px auto 0 auto; 
			width:500px;
			border: solid silver 1px;
			padding: 10px;
			background: #efefef;
			-moz-border-radius: 10px;
			-moz-box-shadow: 1px 1px 10px silver;
			-webkit-border-radius: 10px;
			-webkit-box-shadow: 1px 1px 10px silver;
			border-radius: 10px;
			box-shadow: 1px 1px 10px silver;
		}
		
		.adminlogin #content{
			background-color: white;
			color: black;
			-moz-box-shadow: 1px 1px 10px rgba(0,0,0, 0.3);
			-webkit-box-shadow: 1px 1px 10px rgba(0,0,0, 0.3);
			box-shadow: 1px 1px 10px rgba(0,0,0, 0.3);
		}
		.adminlogin #content div{
			color: white;
		}
		h1{
			font-size: 125%;
		}
		h1.servername{
			text-align: center;
			color: gray;
		}
		form{
			border-top: solid #dfdfdf 1px;
			display: block;
			margin-top: 10px;
		}
		form label{
			float: left;
			width: 100px;
			margin-top: 5px;
		}
		form .data{
			margin-left: 110px;
			margin-bottom: 5px;
			line-height: 32px;
		}
		form input{
			width: 100%;
			border: solid gray 1px;
			padding: 4px;
		}
		form button{
			border: 1px solid gray;
    		margin-top: 20px;
    		padding: 5px 20px;
   		}
		
		#error{
			display: <%= errorMsg != null ? "block" : "none" %>;
			margin-bottom: 15px;
			padding:10px;
			border:1px red solid;
			background-color:white;
			color:red !important;
			font-weight:bold;
			-moz-border-radius: 5px;
			-moz-box-shadow: 1px 1px 10px silver;
			-webkit-border-radius: 5px;
			-webkit-box-shadow: 1px 1px 10px silver;
			border-radius: 5px;
			box-shadow: 1px 1px 10px silver;
		}
		#submit-button{
			color: silver;
			font-style: italic;
		}
	</style>

</head>
<body onLoad="init();" <%= jspHelper.getCore().DOMAIN_ADMINLOGINS.equals(domain) ? "class='adminlogin'":"" %>>

	<div id="content">
		<div align="center">
			<img src="<%= request.getContextPath() %>/static/images/brand/logo_600.png">
		</div>
		
		<form method="post" action="login" onsubmit="document.getElementById('error').style.display='none';document.getElementById('submit-button').innerHTML='processing login request ...'" accept-charset="<%= jspHelper.getCore().getCharacterEncoding() %>">
	
			<input type="hidden" name="domain" value="<%= domain %>">
			<input type="hidden" name="redirect" value="<%= redirect %>">
			<input type="hidden" name="flag" value="true">

			<% if (domain.equals(jspHelper.getCore().DOMAIN_ADMINLOGINS)) { %>
				<h1 class="servername"><%= jspHelper.getCore().getWgaConfiguration().getServerName() %></h1>
				<h1>Administrative login ...</h1>
			<% } else if (domain.startsWith("plugin-")) { %>
				<% if (authDomain != null && !authDomain.equals(domain)) { %>
					<h1>Login to plugin <%= domain.substring("plugin-".length()) %> ...</h1>
					<label>Domain:</label>
					<div class="data"><%= authDomain %></div>	
					<div style="clear:both"></div>
				<%} else { %>							
					<h1>Login to plugin <%= domain.substring("plugin-".length()) %>...</h1>
				<% } %>
			<% } else { %>
					<h1>Login to domain <%= domain %> ...</h1>	
			<% } %>

			<div id="error"><%= errorMsg != null ? errorMsg : ""  %></div>
			
			<label>Username:</label>
			<div class="data">
				<input name="username" value="">
			</div>
			<div style="clear:both"></div>
			
			<label>Password:</label>
			<div class="data">
				<input name="password" type="password" value="">
			</div>
			<div style="clear:both"></div>
			
			<div id="submit-button">
				<button id="loginButton" type="submit">Login</button>
			</div>
			
		</form>
	</div>
</body>
</html>