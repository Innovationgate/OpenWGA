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
JSP to display the message: "This structEntry has no content, would you like to create one?"
Used by:
Authoring-Frameset
Appears if you click on a structEntry that has no content.
--%>
<%@ taglib uri="http://www.innovationgate.de/wgpublisher/webtml/2.2" prefix="tml" %>
<%
	de.innovationgate.wgpublisher.jsputils.JspHelper jspHelper = new de.innovationgate.wgpublisher.jsputils.JspHelper(pageContext);
	if (!jspHelper.isBrowserInterface()) {
		response.sendError(javax.servlet.http.HttpServletResponse.SC_FORBIDDEN);
		return;
	}
%>

<tml:root resource="dispNoContent.jsp">
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    
    <tml:urlparameter name="key" var="key"/>
    
    <tml:script>
    	/* This code is actually obsolete because it's allready checked in WGPDispatcher.sendNoContentNotification()
    	   This JSP should only be displayed if there is a structEntry with the key.    	
    	*/
    	var db = this.content().getDatabase();
    	var key = this.item("key");
    	
    	this.setvar( "isStructKey", false );
    	
    	if( key != null && key != "" ){
	    	var struct = db.getStructEntryByKey(key);	    	
	    	if( struct != null ){
	    		this.setvar( "isStructKey", true );
	    		this.setvar( "structTitle", struct.getTitle() );
	    		this.setvar( "hasContent", struct.hasContent(null,null) );
	    	}
	    }
    	
    </tml:script>
    
    
	<tml:if istrue="isStructKey">
	<tml:then>
		<title><tml:label bundle="toolbars" key="isNothing"/></title>
		<tml:comment>
		<script type="text/javascript">
			top.myContKey='nothing';
			top.myStructKey='<tml:item name="key"/>';
			top.myContTitle='';
			top.myStructTitle='<tml:item name="structTitle"/>';
			top.myStatus=null;
			top.myLanguage=null;
			top.myModifiedDate=null;			
			top.myDbKey='<tml:meta type="database" name="dbkey"/>';
		</script>
		</tml:comment>
		
		<tml:htmlhead context="{'$struct:'+this.item('key')}"/>		
	</tml:then>
	<tml:else>
		<title>404 Error: No Content of key <tml:item name="key"/> found in database <tml:meta type="database" name="dbkey"/></title>
	</tml:else>
	</tml:if>     
    
    <meta http-equiv="pragma" content="no-cache">
    <meta http-equiv="cache-control" content="no-cache">
    <meta http-equiv="expires" content="0">
    
    <style type="text/css">
	    body{
			font-family:Arial,Verdana,Helvetia;
			font-size: 12px;	
			color:#000000;
		}
		
		td{
			font-family:Arial,Verdana,Helvetia;
			font-size:11px;	
			color:#000000;
		}
		
		/*-- alle Links --*/
		a{
			font-family:Arial,sans-serif;
			font-size: 11px;
			text-decoration:none;
			color:#000000;
		}
		a:hover{
			text-decoration:underline;
		}
		
		.forms{	
			background-color:#DDDDDD;
			border:1px outset white;
			border-top:1px solid white;
			position:absolute;
			top:22px;
			left:0px;
			z-index:100;
			visibility:hidden;
		}
		td.reiter{
			position:relative;
			border:2px outset white;
			border-bottom:0px;
			
			padding-top:0px;
			padding-bottom:2px;
			padding-left:4px;
			padding-right:4px;
			z-index:50;
		}
		td.reiterSelected{
			position:relative;	
			border:2px outset white;
			border-bottom:5px solid #DDDDDD;
			
			background-color:#DDDDDD;
			padding-top:0px;
			padding-bottom:2px;
			padding-left:4px;
			padding-right:4px;
		
			z-index:1000;
		}
		td.reiter a{
			font-family:Arial,sans-serif;
			font-size: 12px;
			font-weight:bold;
			text-decoration:none;
			color:#000000;
		}
		td.reiter a:hover{
			color:#FFFFFF;
			text-decoration:none;
		}
		td.reiterSelected a{
			font-family:Arial,sans-serif;
			font-size: 12px;
			font-weight:bold;
			text-decoration:none;
			color:#000000;
		}
		td.reiterSelected a:hover{
			color:#000000;
			text-decoration:none;
		}
		.red{
			color:red;
		}
		img.megaBtnIMG{
			height:50px;
			width:50px;
			float:left; 
			margin:2px;
			margin-right:5px;
			margin-top:2px;
		}
		.megaButton{
			clear:left;
			display:block;
			padding:0px;
			padding-top:2px;
			padding-bottom:2px;
			height:54px;
			border:2px solid #DDDDDD;
			width:95%;	
			cursor:pointer;
			overflow:hidden;
			font-family:Arial,Verdana,Helvetia;
			font-size:11px;	
			color:#000000;
		}
    </style>
  </head>
  
  <body>
  	
    <tml:if istrue="isStructKey">
	<tml:then>
		<tml:comment>############### Is Struct ################</tml:comment>
		
		<tml:include type="statictml" ref="incFrmHeader">
			<tml:option name="frmTitle"><tml:label bundle="toolbars" key="isNothing"/>: <tml:item name="key"/></tml:option>
			<tml:option name="size">small</tml:option>
		</tml:include>
		
		<div class="warning">
			<tml:if istrue="hasContent">
			<tml:then>
				<tml:label bundle="toolbars" key="noReaderAccess"/>
			</tml:then>
			<tml:else>
				<tml:label bundle="toolbars" key="isNothing"/>
			</tml:else>
			</tml:if>
		</div>
		<hr>
		<table>
		<TR>
			<TD><tml:label key="struct"/> <tml:label key="key"/>:</td>
			<TD><b><tml:item name="key"/></b></TD>		
		</TR>
		<tr>
			<td><tml:label key="struct"/> <tml:label key="title"/>:</td>
			<td><b><tml:item name="structTitle"/></b></td>
		</tr>
		</table>
		<hr>
		<button onclick="self.WGABI_historyBackClicked = true;return false" class="button"><tml:label key="btn_back"/></button>
		<tml:include type="statictml" ref="incFrmFooter"/>
	</tml:then>
	<tml:else>
		<tml:comment>############### Is unknown ################</tml:comment>
		<h1>Not found</h1>
		<p>HTTP Status: 404</p>
		<hr>
		<p>
		Error: No content of key &quot;<tml:item name="key"/>&quot; found in database &quot;<tml:meta type="database" name="dbkey"/>&quot;</p>
	</tml:else>
	</tml:if>  <!--
		<tml:warnings/>   	-->
  </body>
</html>
</tml:root>
