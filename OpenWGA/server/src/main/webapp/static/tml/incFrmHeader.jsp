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
Header for all dialogs used by BI.
--%>
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>
<%@ page import="de.innovationgate.wgpublisher.jsputils.JspHelper" %>
<tml:root resource="incFrmHeader.jsp">
<tml:option name="frmTitle" var="frmTitle" output="false"/>

<% 
	de.innovationgate.wgpublisher.jsputils.JspHelper helper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	de.innovationgate.wgpublisher.webtml.utils.TMLContext thisContext = helper.getMainContext();	
	
	String docTitle = (String)thisContext.option("frmTitle");
	String tableWidth = java.net.URLEncoder.encode("100%");
	String tableHeight = java.net.URLEncoder.encode("100%");
	
	if( thisContext.option("size") == null || ( thisContext.option("size") != null && !((String)thisContext.option("size")).equalsIgnoreCase("big") ) ){
		tableWidth = "550";
		tableHeight = "400";
	}	
%>

<jsp:include page="../inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="<%= docTitle %>"/>
	<jsp:param name="frmTabWidth" value="<%= tableWidth %>"/>
	<jsp:param name="frmTabHeight" value="<%= tableHeight %>"/>
</jsp:include>
<tml:case isdefined="tmlerror">
<table width="100%" style="background-color:white; color:red; border-color:red; border-width:3px; border-style:solid">
<tr><td>
<tml:script>
if (this.tmlerror.javaException) {
	log.error("Error in static tml module", tmlerror.javaException);
}
return this.tmlerror.message;
</tml:script></td></tr>
</tml:case>
</tml:root>
