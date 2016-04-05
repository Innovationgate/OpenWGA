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
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>
<tml:root resource="frmQueryTest.jsp">
<%

	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	if (!jspHelper.isAdminLoggedIn()) {
		response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN);
		return;
	}
	String docTitle = "Query database " + jspHelper.getMainContext().meta("db", "dbkey");
%>


<HTML>
<HEAD>
<TITLE><%= docTitle  %></TITLE>
<LINK href="<%= request.getContextPath() %>/static/css/wga.css" rel="stylesheet" type="text/css">
<style type="text/css">
<!-- 
.button {
	height:24px;	
	background-image:url("<%= request.getContextPath() %>/static/images/wgabgButton.gif");		
	font-family:Arial,Verdana,Helvetia;
	font-size: 10pt;
	font-weight:bold;	
	color:#000000;
	background-color:#ffc800;
	border:inset 2px white;
}
body {
	margin:0px;
	padding:0px;
}

table.staticforms tr td {
	vertical-align: top;
}

table.staticforms tr td.label {
	font-weight: bold;
}

-->
</style>

<script language="javascript">

function luceneTermFinder(databaseKey, contentkey) {

		var parameter="height=600,width=600" 
	        +",screenX=0,left=5,screenY=0,top=5"
	        +",dependent=0,directories=0"
	        +",fullscreen=0,location=0,menubar=0"
	        +",resizable=1,scrollbars=1,status=1,toolbar=0";
	        
	    var url = "<%= request.getContextPath() %>/admintml/" + databaseKey + "/luceneTermFinder?contentkey=" + contentkey; 
		open(url,"_blank",parameter);
}

function showQueryOptions() {
	document.getElementById("queryOptionsButton").style.display = 'none';
	document.getElementById("queryOptions").style.display = 'block';
}

</script>
</HEAD>

<tml:if condition="tmlformbyid('queryParameter') != null && !isEmptyValue(tmlformbyid('queryParameter').query)">
	<tml:then>
		<tml:script var="qform" expression="tmlformbyid('queryParameter')"/>
		<tml:collection id="searchResult">	
		      <tml:query id="query"
		      			 context="{isEmptyValue(qform.context) ? 'this' : qform.context}"
		      		     includecurrent="true"
		      			 alllanguages="{qform.alllanguages }" 
		      			 onlypublished="{ qform.onlypublished }"
		      			 role="{ qform.role }"
		      			 type="{ qform.type }"
		      			 options="{ qform.options }"
		      			 max="{ qform.max }"
		      			 cache="{ qform.cache }">
		      			 
		      			 <tml:script expression="qform.query" encode="none"/>
		      </tml:query> 		      
		</tml:collection>   
 	</tml:then>
</tml:if> 

<BODY>

<jsp:include page="../inc_head.jsp" flush="true">	
<jsp:param name="frmTitle" value="<%= docTitle %>"/>
</jsp:include>

<tml:form id="queryParameter" persist="true">

<TABLE width="100%" height="150px" cellpadding="5" cellspacing="5" class="staticforms" >
<tr>
	<TD class="label" width="*">Query:</TD>
	<TD width="100%"><tml:input type="textarea" name="query">rows="3" wrap="soft" style="width:90%"</tml:input></TD>
</tr>
<tr>
	<TD class="label">Type:</TD>
	<TD>
		<tml:input type="text" name="type"></tml:input>	
	</TD>
</tr>
<tr>
	<TD class="label">Display field data:</TD>
	<TD>
		<tml:input type="text" name="items">size="40"</tml:input><br>
		<div style="padding-top: 3px;">Comma separated field names, items lowercased, metas uppercased. Leave empty to see default document data.</div>
	</TD>
</tr>
<tr>
	<td class="label">More options:</td>
	<td>
			<div id="queryOptionsButton">
				<a href="javascript:showQueryOptions()">Click to show</a>
			</div>
			<div id="queryOptions" style="display: none">
				<table cellpadding="3">
					<tr>
						<TD>Execution context:</TD>
						<TD>
							<tml:input type="text" name="context">size="40"</tml:input>	
						</TD>
					</tr>
					<tr>
						<TD>All languages:</TD>
						<TD>
							<tml:input type="select" options="true, false" name="alllanguages" default="false"></tml:input>
						</TD>
					</tr>
					<tr>
						<TD>Only published contents:</TD>
						<TD>
							<tml:input type="select" options="true, false" name="onlypublished" default="true"></tml:input>
						</TD>
					</tr>
					<tr>
						<TD>Use query cache:</TD>
						<TD>
							<tml:input type="select" options="true, false" name="cache" default="false"></tml:input>
						</TD>
					</tr>
					
					<tr>
						<TD>Query role:</TD>
						<TD>
							<tml:input type="select" options="none, nav, search, sitemap" name="role" default="search"></tml:input>
						</TD>
					</tr>
					<tr>
						<TD>Custom query options:</TD>
						<TD><tml:input type="text" name="options">size="40"</tml:input></TD>
					</tr>
					<tr>
						<TD>Max number of documents returned:</TD>
						<TD><tml:input type="text" name="max" default="500"/></TD>
					</tr>
					<tr>
						<TD>Output pagesize:</TD>
						<TD><tml:input type="text" name="pagesize" default="10"/></td>
				</tr>
			</table>
		</div>
	</TD>	
</tr>
<tr>
	<TD width="*" height="100%"  valign="bottom">
		<INPUT type="submit"  class="button" value="Execute query">	
	</TD>
</tr>

<tr>
	<td colspan="2">
		<hr>
	</td>

<tml:case condition="tmlform != null && tmlform.isSubmitted()" conditionlanguage="tmlscript">
	
		<tr>
			<td colspan="2">
				<h2>Query results</h2>
			</td>
		</tr>
		<tr>
			<td class="label">Full query:</td>
			<td><tml:taginfo sourcetag="query" name="fullquery"/></td>
		</tr>
		
		<tml:taginfo sourcetag="query" name="executiontime" var="executionTime"/>
		<tml:case condition="executionTime != -1">
		<tr>
			<td class="label">Query execution time:</td>
			<td> <tml:item name="executionTime" format="#,##0" /> Milliseconds</td>
		</tr>
		</tml:case>

		<tml:taginfo sourcetag="query" name="postprocessingtime" var="postProcessingTime"/>
		<tml:case condition="postProcessingTime != -1">
		<tr>
			<td class="label">Post processing time:</td>
			<td> <tml:item name="postProcessingTime" format="#,##0" /> Milliseconds</td>
		</tr>
		</tml:case>

		<tml:taginfo sourcetag="query" name="totalprocessingtime" var="totalProcessingTime"/>
		<tml:case condition="totalProcessingTime != -1">
		<tr>
			<td class="label">Total processing time:</td>
			<td> <tml:item name="totalProcessingTime" format="#,##0" /> Milliseconds</td>
		</tr>
		</tml:case>
		
		<tr>
			<td class="label">Result documents:</td>
			<td><tml:taginfo sourcetag="searchResult" name="count" format="#,##0"/></td>
		</tr>
		
		<tr>
			<td class="label">Used query cache:</td>
			<td><tml:taginfo sourcetag="query" name="cacheused"/></td>
		</tr>

		<tr>
			<td colspan="2">
			<TABLE width="100%" border="1" cellpadding="3" cellspacing="0" style="background-color:white">
				<tml:if condition="tmlform != null && tmlform.isSubmitted() && !isEmptyValue(tmlform.items)">
					<tml:then>
						<tml:script>
							itemNames = Packages.de.innovationgate.utils.WGUtils.deserializeCollection(tmlform.items, ",", true);
						</tml:script>
						<TR>	
							<TD><B>Key</B></TD>
							<tml:foreach type="itemvalue" item="itemNames" currentvalue="itemName">
								<TD><B><tml:item name="itemName"/></B></TD>
							</tml:foreach>
						</TR>      
						<tml:foreach id="searchFE" sourcetag="searchResult" pagesize="{tmlform.pagesize}" filllastpage="false">
						    <TR>
						      <TD>
						        <tml:meta name="key"/>
						      </TD>
						     <tml:foreach type="itemvalue" item="itemNames" currentvalue="itemName">
								<TD>
									<tml:if condition="java.lang.Character.isUpperCase(itemName.charAt(0))">
										<tml:then>
											<tml:meta name="{itemName}" divider="<br>"/>
										</tml:then>
										<tml:else>
											<tml:item name="{itemName}" divider="<br>"/>
										</tml:else>
									</tml:if>
							</TD>									
							</tml:foreach>
						     				      	          
						    </TR>
						</tml:foreach>
					</tml:then>
					<tml:else>
						<TR>	
							<TD><B>Title</B></TD>
							<TD><B>Key</B></TD>
							<TD><B>Created</B></TD>
							<TD><B>Lastmodified</B></TD>
							<TD><B>Status</B></TD>
							<TD><B>Score</B></TD>
						</TR>      
						<tml:foreach id="searchFE" sourcetag="searchResult" pagesize="{tmlform.pagesize}" filllastpage="false">
						    <TR>
						      <TD>
						        <tml:link astext="true">target="_blank"</tml:link>
						      </TD>
						      <TD>
						        <tml:meta name="KEY"/>
						      </TD>		
						      <TD>
						        <tml:meta name="CREATED"/>
						      </TD>		           
						      <TD>
						        <tml:meta name="LASTMODIFIED"/>
						      </TD>		               
						      <TD>
						        <tml:meta name="STATUS"/>
						      </TD>
						      <TD>
						      	<tml:case condition="SEARCHSCORE != 0">
									<tml:script expression="SEARCHSCORE * 100" format="##0"/>%
						        </tml:case>
						      </TD>				      	          
						    </TR>
						</tml:foreach>
					</tml:else>
				</tml:if>
			</TABLE>
		</td>
	</tr>
	<tr>
		<td>
				Page <tml:taginfo sourcetag="searchFE" name="currentPage" format="#,##0"/> of <tml:taginfo sourcetag="searchFE" name="pages" format="#,##0"/> pages
			
			<tml:case haspreviouspage="searchFE">
			<a href="<tml:url type="previouspage" sourcetag="searchFE"/>">Previous Page</a>
			</tml:case>
			
			<tml:case hasnextpage="searchFE">
			<a href="<tml:url type="nextpage" sourcetag="searchFE"/>">Next Page</a>
			</tml:case>
	</td>
</tr>


	<tml:case condition="taginfo('query', 'error') != null">
		<tr>
			<td colspan="2">
				<b>An error happened while executing the query:</b>
				<p>
					<code>
						<tml:script encode="html">
						var error = taginfo('query', 'error');
						var stringWriter = new java.io.StringWriter();
						var writer = new java.io.PrintWriter(stringWriter, true);
						error.printStackTrace(writer);
						var trace = stringWriter.toString();
						return trace;
						</tml:script>
					</code>
				</p>
			</td>
		</tr>
	</tml:case>
	
	
</tml:case>
</TABLE>
</tml:form>



<br><br>



<tml:warnings autohide="true"/>
<jsp:include page="../inc_foot.jsp" flush="true"/>
</BODY>
</HTML>
</tml:root>
