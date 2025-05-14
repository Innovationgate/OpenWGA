<%@ page language="java" pageEncoding="ISO-8859-1"%>
<%@page import="java.util.Iterator"%>
<%@page import="java.util.Map"%>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
%>
<%@ page import="de.innovationgate.webgate.api.*" %>

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


String aliasesText = "-none-";
String groupsText = "-none-";
String rolesText = "-none-";
String matchingEntriesText = "-none-";
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
String docTitle = "You are logged in to database '" + db.getTitle() +"' ("+ dbKey + ") as user '"+ db.getSessionContext().getUser() +"'";
%>

<!doctype html>
<html>
	<head>
		<title>Who am I</title>
	</head>

	<body>

		<p><b><%= docTitle %></b></p>
		<p>
			<b>Primary username:</b><br><%= db.getSessionContext().getUser() %>
		</p>
		<p>
			<b>Access level:</b><br><%= de.innovationgate.webgate.api.WGDatabase.accessLevelText(accessLevel) %>
		</p>

		<%
		if (userDetails != null) {
		%>
			<p><b>Groups:</b><br><%=groupsText%></p>
			<p><b>Roles:</b><br><%=rolesText%></p>
		<%
		}
		%>

	</body>
</html>
