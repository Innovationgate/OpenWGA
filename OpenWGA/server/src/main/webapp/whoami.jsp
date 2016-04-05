<%@ page language="java" pageEncoding="ISO-8859-1"%>
<%@page import="java.util.Iterator"%>
<%@page import="java.util.Map"%>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
%>
<%@ page import="de.innovationgate.webgate.api.*" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>

<%
String wgaBaseURL = jspHelper.getPublisherPath();
String dbKey = request.getParameter("db");
if (dbKey == null) {
	throw new javax.servlet.jsp.JspException("Missing parameter db=dbKey");
}

de.innovationgate.webgate.api.WGDatabase db = jspHelper.openDatabase(dbKey);
if (db == null) {
	throw new javax.servlet.jsp.JspException("No database for db key '" + dbKey + "'");
}
if (!db.isSessionOpen()) {
	throw new javax.servlet.jsp.JspException("User has no access to database '" + dbKey + "'");
}

int accessLevel = db.getSessionContext().getAccessLevel();
WGUserAccess userAccess = db.getSessionContext().getUserAccess();
de.innovationgate.webgate.api.WGUserDetails userDetails = db.getSessionContext().getUserDetails();


String aliasesText = "(none)";
String groupsText = "(none)";
String rolesText = "(none)";
String matchingEntriesText = "(none)";
String labeledNamesText = null;
if (userDetails != null) {
	if (userDetails.getAliases().size() > 0) {
		aliasesText = de.innovationgate.utils.WGUtils.serializeCollection(userDetails.getAliases(), "<br/>");
	}
	if (userDetails.getGroups().size() > 0) {
		groupsText = de.innovationgate.utils.WGUtils.serializeCollection(userDetails.getGroups(), "<br/>");
	}
	if (userDetails.getRoles().size() > 0) {
		rolesText = de.innovationgate.utils.WGUtils.serializeCollection(userDetails.getRoles(), "<br/>");
	}
	if (userDetails.getMatchingEntries().size() > 0) {
		matchingEntriesText = de.innovationgate.utils.WGUtils.serializeCollection(userDetails.getMatchingEntries(), "<br/>");
	}
	if (userDetails.getLabeledNames().size() > 0) {
	    StringBuffer namesOut = new StringBuffer();
		Iterator names = userDetails.getLabeledNames().entrySet().iterator();
		while (names.hasNext()) {
			Map.Entry entry = (Map.Entry) names.next();
			namesOut.append(entry.getKey()).append(" := ").append(entry.getValue()).append("<br>");
		}
		labeledNamesText = namesOut.toString();
	}

}
%>
<META http-equiv="Content-Style-Type" content="text/css">
<LINK href="static/css/wga.css" rel="stylesheet"
	type="text/css">
<TITLE>Who am I</TITLE>
<STYLE type="css">
td {
	vertical-align: top;
}
</STYLE>

</HEAD>
<BODY>

<%
	String docTitle = "You are logged in to database '" + db.getTitle() +"' ("+ dbKey + ") as user '"+ db.getSessionContext().getUser() +"'";
%>

<jsp:include page="static/inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="<%= docTitle %>"/>
</jsp:include>
<table border="0" class="listTable" cellspacing="0" cellpadding="0" width="100%">
    		
	<tr valign="top"><td width="200">Primary username:</td><td style="font-weight:bold;"><%= db.getSessionContext().getUser() %></td></tr>
	<tr valign="top"><td>Access level:</td><td style="font-weight:bold;" ><%= de.innovationgate.webgate.api.WGDatabase.accessLevelText(accessLevel) %></td></tr>
	<tr valign="top">
		<td>Access privileges:</td>
		<td style="font-weight:bold; " >
			<p style="line-height:200%">
				<img style="padding-right:5px;" src="<%= wgaBaseURL + "/static/images/" + (userAccess.mayDeleteDocuments() ? "flag.gif" : "forbidden.gif")  %>">
				<span style="color:<%= userAccess.mayDeleteDocuments() ? "black" : "grey" %>">May delete documents</span><br/>
				<img style="padding-right:5px;" src="<%= wgaBaseURL + "/static/images/" + (userAccess.mayMoveStructEntries() ? "flag.gif" : "forbidden.gif")  %>">
				<span style="color:<%= userAccess.mayMoveStructEntries() ? "black" : "grey" %>">May move struct entries</span>
			</p>
		</td>
	</tr>
	
<%
if (userDetails != null) {
%>
	<tr valign="top"><td>Aliases:</td><td style="font-weight:bold;" ><%= aliasesText %></td></tr>
	<%
		if (labeledNamesText != null) { %>
		
     <tr valign="top"><td>Labeled names:</td><td style="font-weight:bold;" ><%= labeledNamesText %></td></tr>
		    
	<%	}
	%>
	<tr valign="top"><td>User groups:</td><td style="font-weight:bold;" ><%= groupsText %></td></tr>
	<tr valign="top"><td>User roles:</td><td style="font-weight:bold;" ><%= rolesText %></td></tr>
	<tr valign="top"><td>Matching ACL entries:</td><td style="font-weight:bold;" ><%= matchingEntriesText %></td></tr>
	<tr valign="top"><td>E-Mail address:</td><td style="font-weight:bold;" ><%= (userDetails.getEMailAddress() != null ? userDetails.getEMailAddress() : "(none)") %></td></tr>
	<tr valign="top"><td>Authentication source:</td><td style="font-weight:bold;" ><%= (userDetails.getAuthSource() != null ? userDetails.getAuthSource() : "(none)") %></td></tr>
	<% if (userDetails.getLabeledNames().size() > 0) { %>
		<tr valign="top"><td>Labeled names:</td><td style="font-weight:bold"><%= userDetails.getLabeledNames() %></td></tr>
	<% } %>
	
<%
}
%>
	<tr valign="top"><td colspan="2">
	<% if (jspHelper.getDispatcher() != null) { %>
		<button type="button" class="button" onclick="location.href='<%= jspHelper.getDispatcher().getLoginURL(request, db, request.getRequestURL() + (request.getQueryString() != null ? "?" + request.getQueryString() : "")) %>'">Login as another user</button>	
	<% } %>
</TABLE>
<jsp:include page="static/inc_foot.jsp" flush="true"/>
</BODY>
</HTML>
