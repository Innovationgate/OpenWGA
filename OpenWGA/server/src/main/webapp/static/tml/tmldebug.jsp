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
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	if (!jspHelper.isAdminLoggedIn()) {
		response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN);
		return;
	}
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<tml:root resource="tmldebug.jsp" debug="false">
<HTML>
<HEAD>
<tml:htmlhead/>
<META http-equiv="Content-Style-Type" content="text/css">
<LINK href="../css/wga.css" rel="stylesheet"
	type="text/css">
<TITLE>tmldebug.jsp</TITLE>
</HEAD>
<BODY>

<tml:action id="setDebug">

if (this.tmlparam1 == "true") {
	this.httpsession.setAttribute(Packages.de.innovationgate.wgpublisher.WGACore.ATTRIB_TMLDEBUG, true);
}
else {
	this.httpsession.setAttribute(Packages.de.innovationgate.wgpublisher.WGACore.ATTRIB_TMLDEBUG, false);
}

</tml:action>

<tml:script>
var tmlDebug = false;
var tmlDebugSetting = this.httpsession.getAttribute(Packages.de.innovationgate.wgpublisher.WGACore.ATTRIB_TMLDEBUG);
if (tmlDebugSetting != null) {
	tmlDebug = tmlDebugSetting;
}
this.setvar("debug", tmlDebug);
this.setvar("debugDisplay", (tmlDebug ? "ON" : "OFF"));
</tml:script>

TML-Debugging is <tml:item name="debugDisplay"/><br/>
<tml:button clickaction="setDebug" param1="true">Enable debugging</tml:button><br/>
<tml:button clickaction="setDebug" param1="false">Disable debugging</tml:button><br/>

<tml:warnings autohide="true"/>

</BODY>
</HTML>
</tml:root>
