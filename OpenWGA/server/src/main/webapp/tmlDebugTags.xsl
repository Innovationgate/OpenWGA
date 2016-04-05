<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    
    	<xsl:output method="html"/>
    	
        <xsl:template match="/">
            
            <html>
                
                <head>
                    <title>WebTML Tags</title>
                    <link rel="stylesheet" href="static/css/tmldebugger.css"/>
                    
                <script language="javascript">
                
                function showTagDetails(event, startStarted, startSpecStarted, startSpecEnded, startEnded, endStarted, endSpecStarted, endSpecEnded, endEnded) {
                	document.getElementById('detailsStartStarted').innerHTML = startStarted;
                	document.getElementById('detailsStartSpecificStarted').innerHTML = startSpecStarted;
                	document.getElementById('detailsStartSpecificEnded').innerHTML = startSpecEnded;
                	document.getElementById('detailsStartEnded').innerHTML = startEnded;
                	document.getElementById('detailsEndStarted').innerHTML = endStarted;
                	document.getElementById('detailsEndSpecificStarted').innerHTML = endSpecStarted;
                	document.getElementById('detailsEndSpecificEnded').innerHTML = endSpecEnded;
                	document.getElementById('detailsEndEnded').innerHTML = endEnded;
                	document.getElementById('detailDiv').style.display='block';
                }
                
                function hideTagDetails() {
                	document.getElementById('detailDiv').style.display='none';
                }
                
                </script>
                    
                </head>
                
                <body>
                    
                    <h2>WebTML Tags in WebTML module  <xsl:value-of select="/tmltag/@resource"></xsl:value-of></h2>
                    
                    Number of WebTML Tags: <xsl:value-of select="/tmltag/@subtags"/><br/>
                    Number of WebTML Modules <xsl:value-of select="count(//tmltag[@name='root'])"/><br/>
                    Backend-Docs: <xsl:value-of select="/tmltag/@backenddocs"></xsl:value-of>
                    <p>
                    <table width="100%" class="tmltags">
                    	<thead>
                    		<th>Time</th>
                    		<th>Line</th>
                    		<th>Tag</th>
                    	</thead>
                        <xsl:apply-templates select="/tmltag/*/tmltag[@name!='root']"></xsl:apply-templates>
                    </table>
                    </p>
                </body>
                
                <div id="detailDiv" style="display:none; position: fixed; left:100px; top:100px; background-color: lightgrey; border:2px black solid">
                	<h3>Tag details <a href="javascript:hideTagDetails()">close</a></h3>
                	<table width="400" border="1">
                		<tr>
                			<td>Starttag started:</td>
                			<td id="detailsStartStarted"></td>
                		</tr>
                		<tr>
                			<td>Starttag tag specific code started:</td>
                			<td id="detailsStartSpecificStarted"></td>
                		</tr>
						<tr>
                			<td>Starttag tag specific code ended:</td>
                			<td id="detailsStartSpecificEnded"></td>
                		</tr>
                		<tr>
                			<td>Starttag ended:</td>
                			<td id="detailsStartEnded"></td>
                		</tr>
                		<tr>
                			<td>Endtag started:</td>
                			<td id="detailsEndStarted"></td>
                		</tr>
                		<tr>
                			<td>Endtag tag specific code started:</td>
                			<td id="detailsEndSpecificStarted"></td>
                		</tr>
						<tr>
                			<td>Endtag tag specific code ended:</td>
                			<td id="detailsEndSpecificEnded"></td>
                		</tr>
                		<tr>
                			<td>Endtag ended:</td>
                			<td id="detailsEndEnded"></td>
                		</tr>
                	</table>
                </div>
                
            </html>
            
        </xsl:template>
    
        <xsl:template match="tmltag">
        		
        		<tr>
        			<td class="tmltags" style="white-space:nowrap">
        				<xsl:value-of select="@started"/>
        			</td>
        			<td class="tmltags">
        				<xsl:value-of select="@sourceline"/>
        			</td>
        			<td class="tmltags">
        	
                        <xsl:element name="span">
                               <xsl:attribute name="style">padding-left: <xsl:value-of  select="count(ancestor::tmltag)"/>cm</xsl:attribute>
        			    
				                &lt;
				                	<xsl:element name="a">
				                		<xsl:attribute name="href">javascript:void(0)</xsl:attribute>
				                		<xsl:attribute name="onclick">
				                			showTagDetails(event, 
				                						   '<xsl:value-of select="@started"/>', 
				                						   '<xsl:value-of select="starttag/@startedTagSpecific"/>', 
				                						   '<xsl:value-of select="starttag/@endedTagSpecific"/>', 
				                						   '<xsl:value-of select="starttag/@ended"/>', 
				                						   '<xsl:value-of select="endtag/@started"/>', 
				                						   '<xsl:value-of select="endtag/@startedTagSpecific"/>', 
				                						   '<xsl:value-of select="endtag/@endedTagSpecific"/>',
				                						   '<xsl:value-of select="@ended"/>');
										</xsl:attribute>
				                		tml:<xsl:value-of select="@name"/>
				                	</xsl:element>
				                	<xsl:text> </xsl:text>
				                
				                    <xsl:for-each select="attribute">
				                        <xsl:value-of select="@name"/>="<xsl:value-of select="text()"/>"
				                    </xsl:for-each>
				                    
				                    <xsl:if test="@subtags = 0">
				                        /
				                    </xsl:if>
				                    
				                    &gt;
                    
                        </xsl:element>       			    
                    
                    </td>
                                
                </tr>
                
                <xsl:apply-templates select="*/tmltag[@name!='root']"></xsl:apply-templates>
                
                <xsl:if test="@subtags > 0">
				                    
	                <tr>
	                	<td class="tmltags">
	        				<xsl:value-of select="@ended"/>
	        			</td>
	        			<td class="tmltags">
	        				<xsl:value-of select="@sourceline"/>
	        			</td>
	        			<td class="tmltags">
		        			<xsl:element name="span">
		                    	<xsl:attribute name="style">padding-left: <xsl:value-of  select="count(ancestor::tmltag)"/>cm</xsl:attribute>
		        			    
		                		&lt;/tml:<xsl:value-of select="@name"/>&gt;
		                    
		                    </xsl:element>
	        			</td>
	                </tr>
                
                </xsl:if>
                
                
                
        </xsl:template>
    
</xsl:stylesheet>
