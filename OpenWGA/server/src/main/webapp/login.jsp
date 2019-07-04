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
<!DOCTYPE html>
<%@page import="de.innovationgate.wgpublisher.WGACore"%><html>
<head>
	<title><%= wga.encode("html", WGABrand.getName()) %> Login</title>
	<meta name="robots" content="noindex,nofollow">

	<script>
		function init(){
			document.forms[0].username.focus(); 
		}
	</script>
	
	<style>
		body{
			padding: 0;
			margin: 0;
			font-size: 16px;
			font-family: -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Oxygen-Sans,Ubuntu,Cantarell,"Helvetica Neue",sans-serif;
		}
		.header{
			padding: 20px;
			font-size: 24px;
			background: #286090;
			color: white;
			text-align: right;
		}
		form{
			margin: 50px auto;
			width: 300px;
		}
		label{
			display: inline-block;
			width: 110px;
			margin-right: 10px;
			color: gray;
			font-size: 16px;
		}
		input{
			font-size: inherit;
			border: none;
			border-bottom: dashed #286090 1px;
			width: 100%;
			padding: 5px 0px;
		}
		.section{
			margin: 40px 0;
		}
		.section.error{
			color: brown;
			<% if (errorMsg == null) {%>
				display: none;
			<% } %>
		}
		button{
			font-size: inherit;
			border: none;
			padding: 5px 50px;
			background: #286090;
			color: white;
			border-radius: 3px;
		}
		button:hover{
			background: brown;
		}

		p.big{
			font-size:1.5em
		}				
		
		@media (min-width: 768px){
			body{
				font-size: 20px;
			}
			.header{
				font-size: 30px;
			}			
			form{
				margin: 100px auto;
				width: 500px;
			}
			p.big{
				font-size:2em
			}				
		}
	</style>

</head>
<body onLoad="init()">

	<div class="header">
		<%= WGABrand.getName() %> Login
	</div>

	<form method="post" action="login" onsubmit="document.getElementById('error').style.display='none';document.getElementById('submit-button-section').innerHTML='processing login request ...'" accept-charset="<%= jspHelper.getCore().getCharacterEncoding() %>">

		<input type="hidden" name="domain" value="<%= domain %>">
		<input type="hidden" name="redirect" value="<%= redirect %>">
		<input type="hidden" name="flag" value="true">

		<div class="section">
			<p class="big">

				<% if (domain.equals(jspHelper.getCore().DOMAIN_ADMINLOGINS)) { %>
					Administrative login to server <%= jspHelper.getCore().getWgaConfiguration().getServerName() %> ...
				<% } else if (domain.startsWith("plugin-")) { %>
					<% if (authDomain != null && !authDomain.equals(domain)) { %>
						Login to plugin <%= domain.substring("plugin-".length()) %>  in Domain <%= authDomain %> ...	
					<%} else { %>							
						Login to plugin <%= domain.substring("plugin-".length()) %> ...
					<% } %>
				<% } else { %>
					Login to domain <%= domain %> ...	
				<% } %>

			</p>
		</div>
		<div class="section error" id="error">
			<p><%= errorMsg != null ? errorMsg : ""  %></p>
		</div>
		<div class="section">
			<label>Username:</label>
			<input name="username" type="text">
		</div>
		<div class="section">
			<label>Password:</label>
			<input name="password" type="password">
		</div>
		<div class="section" id="submit-button-section">
			<button type="submit">Login</button>
		</div>

	</form>
	
</body>
</html>