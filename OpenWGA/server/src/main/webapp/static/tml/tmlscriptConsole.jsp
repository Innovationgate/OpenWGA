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
	String errorMsg = null;
	de.innovationgate.wgpublisher.WGACore core = de.innovationgate.wgpublisher.WGACore.retrieve(pageContext);
	if (!core.isAdminLoggedIn(request)) {
		try {
			response.resetBuffer();
			response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN, "You must first login to the admin page");
		}
		catch (java.io.IOException e) {
			core.getLog().error("Error redirecting to admin page", e);
		}
		return;
	}

	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
%>
<tml:root resource="tmlscriptConsole.jsp">
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>

<HEAD>
<tml:htmlhead/>
<META http-equiv="Content-Style-Type" content="text/css">

<TITLE>TMLScript Console for <tml:meta type="database" name="title"/> (<tml:meta type="database" name="dbkey"/>)</TITLE>

<tml:action id="execute">
if (this.tmlform.type == 's') {
	this.script = this.tmlform.script;
}
else {
	this.expr = this.tmlform.script;
}
if (this.tmlform.context != "") {
	this.scrcontext = this.tmlform.context;
}
else {
	this.scrcontext = "this";
}

</tml:action>

<tml:case isdefined="script">
<tml:script context="{item:scrcontext}" var="result"><tml:item name="script" encode="none"/></tml:script>
</tml:case>

<tml:case isdefined="expr">
<tml:script context="{item:scrcontext}" var="result">return (<tml:item name="expr" encode="none"/>);</tml:script>
</tml:case>

</HEAD>
<BODY topmargin="5" marginheight="5" leftmargin="5" marginwidth="5">

<%
de.innovationgate.wgpublisher.webtml.utils.TMLContext context = jspHelper.getMainContext();
de.innovationgate.webgate.api.WGDatabase db = context.content().getDatabase();
	
String dbKey = (String) db.getAttribute(de.innovationgate.wgpublisher.WGACore.DBATTRIB_DBKEY);
String docTitle = "TMLScript Console for " + db.getTitle() + "(" + dbKey + ")";
%>
<jsp:include page="../inc_head.jsp" flush="true">	
<jsp:param name="frmTitle" value="<%= docTitle %>"/>
</jsp:include>

<tml:form id="tmlscript" source="none">
<tml:script>if (!this.tmlform.hasField("type")) this.tmlform.type = "e";</tml:script>
<p>
<b>Context:</b> <tml:input name="context" type="text" encode="none">size="70"</tml:input>
</p>
<p>
<b>TMLScript-Code to execute:</b><br/>
<tml:input name="script" type="textarea" cssstyle="width:500px" encode="none">rows="10"</tml:input>
</p>
<p>
<b>Script-Type:</b><br/><tml:input name="type" type="radio" encode="none" options="Expression - Like in WebTML conditions. No multiple commands allowed.)|e,Script - Like in &lt;tml:script&gt;-Tags. You need to explicitly return your results)|s"/>
</p>
<p>
<tml:button clickaction="execute">Execute</tml:button>
</p>
</tml:form>

<tml:case isdefined="result">
<p>
	<b>Result of execution:</b><br/>
</p>
<table style="background-color:gainsboro; border-width:2px; border-color:grey; border-style:outset; width:500px; padding:5px"><tr><td>
	<tml:item name="result" divider="<br>"/>
</td></tr></table>

<p>
	<b>Data type of result:</b> 
	<tml:script>
		var res = itemList("result");
		var firstElement = (res.size() >= 1 ? res.get(0) : null);
		var firstElementType = (firstElement == null ? "null" : javaObject(firstElement).getClass().getName());
		
		if (res.size() > 1) {
			return "List with first element of type " + firstElementType;
		}
		else {
			return firstElementType;
		}
	</tml:script>
</p>
</tml:case>

<tml:warnings/>
<jsp:include page="../inc_foot.jsp" flush="true"/>
</BODY>
</HTML>
</tml:root>


