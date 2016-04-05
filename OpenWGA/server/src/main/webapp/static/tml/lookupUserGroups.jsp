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
Dialog to lookup users and groups from authentication-module

--%>
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<tml:root resource="lookupUserGroups.jsp">
<LINK href="<%= request.getContextPath() %>/static/css/wga.css" rel="stylesheet" type="text/css">

<script type="text/javascript">
	function transmitSelectionToOpener(value) {
		<%if (request.getParameter("newline") != null && new Boolean(request.getParameter("newline")).booleanValue()) {%>
			value+="\n";
		<%}%>
		if (<%=request.getParameter("append")%>) {
			opener.<%=request.getParameter("target")%>.value+=value;
		} else {
			opener.<%=request.getParameter("target")%>.value=value;
		}
		self.close();
	}
	
	function setTR_BG(obj, color){	
		var tds = obj.childNodes;
		if( tds.length > 0 ){				
			for( var i=0; i < tds.length ;i++ ){
			  if( tds[i].nodeName.toLowerCase() == "td" ){
			  	defaultBGColor = tds[i].style.backgroundColor;
			  	tds[i].style.backgroundColor = color;
			  }
			}
		}
	}
	
	
	function MOver(obj){	
		setTR_BG(obj,'#C0C0C0');		 
		return;
	}

	function MOut(obj){
		setTR_BG(obj,"white");
		return;
	}	
	
</script>

<HEAD>
<tml:htmlhead/>
	

<%

	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	de.innovationgate.wgpublisher.webtml.utils.TMLContext context = jspHelper.getMainContext();
	de.innovationgate.webgate.api.WGDatabase db = context.content().getDatabase();	
%>
<%	
	// deny anonymous users
	if (db.getSessionContext().isAnonymous()) {
		response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN);
		return;
	} 
	
	// if dialog is not used in bi -> wgadmin-page
	if (!jspHelper.isBrowserInterface()) { 
		// check for manager-access		
		if (db.getSessionContext().getAccessLevel() < de.innovationgate.webgate.api.WGDatabase.ACCESSLEVEL_MANAGER) {
			response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN);
			return;
		}
	}			
%>
<META http-equiv="Content-Style-Type" content="text/css">


<TITLE><tml:label key="lookupUserGroups_title_part1" bundle="forms"/> '<tml:script>return request.getParameter('prefix')</tml:script>' <tml:label key="lookupUserGroups_title_part2" bundle="forms"/> <tml:script> return this.db().getTitle() + "(" + this.db().getDbReference() + ")";</tml:script></TITLE>
</HEAD>


<tml:include type="statictml" ref="incFrmHeader">
	<tml:option name="frmTitle">
	  <tml:label key="lookupUserGroups_title_part1" bundle="forms"/> '<tml:script>return request.getParameter('prefix')</tml:script>' <tml:label key="lookupUserGroups_title_part2" bundle="forms"/> <tml:script> return this.db().getTitle() + "(" + this.db().getDbReference() + ")";</tml:script>
	</tml:option>
	<tml:option name="size">big</tml:option>
</tml:include>

<BODY topmargin="5" marginheight="5" leftmargin="5" marginwidth="5">
<tml:script>
	// retrieve request parameters
	var prefix = request.getParameter("prefix");
	
	// lookup user and groups
	var authModule = this.db().getAuthenticationModule();
	var queryType = Packages.de.innovationgate.webgate.api.auth.AuthenticationModule.QUERY_USERS_AND_GROUPS;
	if (authModule.isQueryable(queryType)) {
		usersGroups = authModule.query(prefix, queryType);			
	} else {
		usersGroups = null;
	}
</tml:script>


<tml:if condition="usersGroups != null"> {
	<tml:then>		
		<div class="tabforms" style="font-size:10px;padding:3px"><tml:label key="lookupUserGroups_info" bundle="forms"/></div>
		<table border="0" width="100%" class="tabforms" cellpadding="2" cellspacing="2"> 
						<tr>
							<td width="100%"><b><tml:label key="lookupUserGroups_td_hdr_fqdn" bundle="forms"/></b></td>
							<td><b><tml:label key="lookupUserGroups_td_hdr_type" bundle="forms"/></b></td>
							<td><nobr><b><tml:label key="lookupUserGroups_td_hdr_alias" bundle="forms"/></b></nobr></td>
						</tr>
		<tml:foreach type="itemvalue" item="usersGroups" currentvalue="current">
		  <TR onclick="transmitSelectionToOpener('<tml:script>return current.getFullQualifiedName();</tml:script>')" 
		      onmouseover="MOver(this)" onmouseout="MOut(this)"
		      style="cursor: pointer;">
		
		    <td class="orangeBorder"  bgcolor="white"><tml:script>return current.getFullQualifiedName();</tml:script></TD>
		    <td class="orangeBorder"  bgcolor="white">
		    	<tml:script>
		    		if (current.isUser()) { 
		    			return "User" 
		    		} else {
		    			return "Group"
		    		}
		    	</tml:script>
		    </TD>    
		    <td class="orangeBorder" bgcolor="white"><tml:script divider=",">return current.getAliasNames();</tml:script></TD>
		   </TR> 
		</tml:foreach>
		</table>		
	</tml:then>
    <tml:else>
	    <div class="tabforms" style="font-size:10px;padding:3px"><tml:label key="lookupUserGroups_unsupported" bundle="forms"/></div>
    </tml:else>
</tml:if>      
<br>
<input type="button" onclick="self.close()" class="button" value="<%=request.getParameter("valueBackButton")%>"/>

</BODY>
<tml:include type="statictml" ref="incFrmFooter"/>>
</tml:root>
</HTML>
