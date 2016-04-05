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
<%-- 
Function:
JSP to display a virtual document in BI. 
Displays the target and provides the possibility to edit it.
Used by:
Authoring-Frameset
--%>
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>

<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	if (!jspHelper.isBrowserInterface()) {
		response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN);
		return;
	}
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>

<tml:root resource="dispVirtual.jsp">
<HEAD>
<TITLE><tml:label bundle="forms" key="dispVirtual_title"/>: <tml:meta name="title"/></TITLE>


<tml:htmlhead/>



<script type="text/javascript">

function changeVLink(){
	self.location.href = "<tml:meta type="request" name="absolutewgaurl"/>/statictml/<tml:meta type="database" name="dbkey"/>/frmContProp/<tml:meta name="key"/>?page=VIRTUALLINK";
}
function execVLink(url){
	self.location.href = url;
}
<tml:script>
	var vLink = this.contenturl('html',null);	
	var previewURL = "";
	var execLink = "";
	
	/* NO MORE NECCESSARY: contenturl puts out correct URL in all cases
	if( vLink.indexOf("../../file") == 0 ){							
		vLink = this.meta("request","absolutewgaurl")+"/"+this.meta("database","dbkey") + vLink.substring( 5 );
	}
	
	// Cut Query-String or Anchor-Link form URL
	var pos = vLink.indexOf("?");
	if( pos == -1 ) {
		pos = vLink.indexOf("#");
		if( pos != -1 ) {
			vLink = vLink.split("#")[0];			
		}		
	}
	else{
		vLink = vLink.split("?")[0];			
	}*/

	this.setvar("previewURL",vLink+"?forceVLink=1&disableBISync=1");
	this.setvar("execLink",vLink+"?forceVLink=1");
	
</tml:script>

</script>

</HEAD>
<body>

<!--------------------------- HEAD ------------------------------------------------------------->

<tml:include type="statictml" ref="incFrmHeader">
	<tml:option name="frmTitle"><tml:label bundle="forms" key="dispVirtual_title"/>: <tml:meta name="title"/></tml:option>
	<tml:option name="size">big</tml:option>
</tml:include>

<table class="tabforms" height="100%" width="100%" border="0" cellspacing="5" cellpadding="0">
<tr>
	<td colspan="2">
			<button onclick="changeVLink()" class="button"><tml:label bundle="forms" key="dispVirtual_btn_change"/></button>&nbsp;
			<button onclick="execVLink('<tml:item name="execLink"/>')" class="button"><tml:label bundle="forms" key="dispVirtual_btn_execute"/></button>
	</td>
</tr>
<tr><TD colspan="2"><hr/></TD></tr>
<tr>
	<td width="150"><nobr><tml:label bundle="forms" key="dispVirtual_infotext"/>:</nobr></td>
	<td><b><tml:meta name="virtuallink"/></b>
		<tml:case condition="this.item('virtuallinktitle')!=null && this.item('virtuallinktitle')!=''">&nbsp;(<tml:item name="virtuallinktitle"/>)</tml:case>
	</td>
	
</tr>
<tr height="100%">
	<td colspan="3">
		<iframe id="iframe" width="100%" height="100%" src="<tml:item name="previewURL"/>"></iframe>
	</td>
</tr>
</table>
<tml:include type="statictml" ref="incFrmFooter"/>
<!--
<tml:warnings/>

-->
</BODY>
</tml:root>
</HTML>
