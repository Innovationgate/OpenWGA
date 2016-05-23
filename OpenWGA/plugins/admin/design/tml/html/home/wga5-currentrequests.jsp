<%@ page language="java" pageEncoding="ISO-8859-1"%>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	jspHelper.setContentType("text/html");
%>
<%@ page isThreadSafe="true" errorPage="error.jsp" %>
<%@ page import="de.innovationgate.webgate.api.*,de.innovationgate.wgpublisher.*,java.util.*" %>
<%@ page import="de.innovationgate.wgpublisher.WGPDispatcher.RequestInformation" %>
<%@ page import="de.innovationgate.wgpublisher.WGPDispatcher" %>
<%@ page import="java.util.ConcurrentModificationException" %>
<%@ page import="de.innovationgate.wgpublisher.webtml.Range" %>
<%

	String errorMsg = null;
	WGACore core = WGACore.retrieve(pageContext);
	if (!core.isAdminLoggedIn(session)) {
		response.sendRedirect("wgadmin.jsp?wheretogo=currentrequests.jsp");
		return;
	}
	
	if (request.getParameter("clearCacheLocks") != null) {
	    Range.clearCacheLocks();
	}
	
	
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<HTML>
<HEAD>
<TITLE>WGA Current Requests</TITLE>
<link rel="stylesheet" type="text/css" href="<%= request.getContextPath() %>/static/css/wga.css">

<script type="text/javascript">

function clearCacheLocks() {
	if (!confirm("Are you sure to clear all cache locks?")) {
		return;
	}
	
	location.href = location.href + "?clearCacheLocks";
	
}

</script>

</HEAD>
<BODY>
<form method="post">
<jsp:include page="static/inc_head.jsp" flush="true">	
	<jsp:param name="frmTitle" value="Currently processed requests"/>	
</jsp:include>

<input type="hidden" name="index"/>

<h4>Time now on server: <%= new Date() %></h4>


<table border="0" class="staticforms" cellspacing="0" cellpadding="10" width="100%"> 

<% if ( errorMsg != null ) { 
			out.write("<tr><td colspan=\"3\" style=\"color:red;font-size:12px;font-weight:bold;\">");
			out.write(errorMsg);
			out.write("</td></tr>");
	   } %>	


   			
	
 	<tr>
		<td colspan="4">&nbsp;</td>
	</tr>
		   

	<tr>
		<td><b>Date started</b></td>
		<td><b>Status</b></td>
		<td><b>HTTP method</b></td>
		<td><b>Request URI</b></td>
		<td><b>Currently evaluating TML module</b></td>
	</tr>

	<%
	try {
	Map requests = core.getDispatcher().getCurrentRequests();
	WGPDispatcher.RequestInformation reqInfo;
	Iterator requestsIt = requests.keySet().iterator();
	javax.servlet.http.HttpServletRequest aRequest;
	while (requestsIt.hasNext()) {
		aRequest = (javax.servlet.http.HttpServletRequest) requestsIt.next();
		reqInfo = (WGPDispatcher.RequestInformation) requests.get(aRequest);
		String tml = "(none)";
		if (aRequest.getAttribute(WGACore.ATTRIB_CURRENTTML) != null) {
			tml = aRequest.getAttribute(WGACore.ATTRIB_CURRENTTML) + " in database " + aRequest.getAttribute(WGACore.ATTRIB_CURRENTTMLDB);
		}
		
	%>
	<tr>
		<td nowrap><%= reqInfo.getDate() %></td>
		<td nowrap><%= (reqInfo.isCommitted() ? "Committed" : "Processing") %></td>
		<td nowrap><%= aRequest.getMethod() %></td>
		<td nowrap><%= aRequest.getAttribute(WGACore.ATTRIB_REQUESTPATH) %></td>
		<td nowrap><%= tml %></td>
	</tr>
	<tr><td colspan="4"><hr/></td></tr>
	<% } %>
	<tr>
		<td colspan="3">&nbsp;</td>
	</tr>
	
	<% }
	catch (ConcurrentModificationException e) {
		out.write("Request information at the moment not available. Pleasy try again.");
	}
	%>
	
	
</TABLE>

<br>
<h4>Currently evaluated WebTML caches <button onclick="clearCacheLocks()">Clear all locks</button></h4>

<table border="0" class="staticforms" cellspacing="0" cellpadding="10">

	<tr>
		<td><b>Range ID / Cache key</b></td>
		<td><b>Evaluating since</b></td>
	</tr>
	
	<%
	Iterator caches = Range.getCurrentlyEvaluatedCaches().entrySet().iterator();
	while (caches.hasNext()) {
		Map.Entry cacheEntry  = (Map.Entry) caches.next();
	%>
	
	<tr>
		<td><%= cacheEntry.getKey() %></td>
		<td><%= new Date(((Long) cacheEntry.getValue()).longValue()).toString() %></td>
	</tr>
	
	<% 
	} 
	%>

</table>

<br>
<h4>Current operations using the database backend</h4>


<table border="0" class="staticforms" cellspacing="0" cellpadding="10">

	<tr>
		<td><b>Database</b></td>
		<td><b>Operation</b></td>
		<td><b>Key</b></td>
	</tr>
	
	<%
	Iterator dbs = core.getContentdbs().values().iterator();
	while (dbs.hasNext()) {
	    WGDatabase db = (WGDatabase) dbs.next();
	    Iterator ops = db.getCurrentBackendOperations().iterator();
	    while (ops.hasNext()) {
	        WGOperationKey op = (WGOperationKey) ops.next();
	    
	    
	%>

	<tr>
		<td><%= db.getDbReference()  %></td>
		<td><%= WGOperationKey.getOperationName(op.getOperation()) %></td>
		<td><%= op.getKey() %></td>
	</tr>
	
	<%
	    }
	} 
	%>

</table>

<jsp:include page="static/inc_foot.jsp" flush="true"/>
</form>
</BODY>
</HTML>
