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
<%@ page language="java" contentType="text/html" pageEncoding="ISO-8859-1" %>
</td>	
</tr>
<tr>	
	<td background="<%= request.getContextPath() %>/static/images/wgabg.gif">		
		<table border="0" cellpadding="0" cellspacing="0" width="100%" >
			<tr><td colspan="3" style="border-left:2px solid #DEDEDE;border-right:2px outset silver"><hr/></td></tr>
			<tr>
			<td><img src="<%= request.getContextPath() %>/static/images/tab_foot_left.gif" height="36px" width="40px" border="0"/></td>
			<td width="100%" align="middle" valign="top" style="font-size:7pt;padding:3px;height:36px;border-bottom:solid 2px black">		
			<%= de.innovationgate.wgpublisher.WGACore.getReleaseString() %>, 
			&copy; 2002 - <%					
				java.util.GregorianCalendar c = new java.util.GregorianCalendar();				
				String thisYear = "" + c.get(java.util.Calendar.YEAR);
				out.write(thisYear);				
			%>
			Innovation Gate GmbH. All rights reserved. <a style="font-size:7pt; font-weight:bold;" target="_blank" href="<%= request.getContextPath() %>/static/copyright.jsp">Third party copyright information</a></td>
			<td><img src="<%= request.getContextPath() %>/static/images/tab_foot_right.gif" height="36px" width="40px" border="0"/></td>
			</tr>
		</table></td>
</tr>
</table>
