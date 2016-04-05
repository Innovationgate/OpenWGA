<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    
    <xsl:template match="/">
        
            <html>
                
                <head>
                	<title>WebTML Portlets</title>
                	<link rel="stylesheet" href="static/css/tmldebugger.css"/>
                
                
                </head>
                
                <body>

                        <h2>WebTML Portlets in Request <xsl:value-of select="/tmldebugdocument/@url"></xsl:value-of></h2>

						<p>
						    <xsl:element name="a">
	            				<xsl:attribute name="href">tmlDebug?command=showModules&amp;index=<xsl:value-of select="/tmldebugdocument/@index"/></xsl:attribute>
	            				Show WebTML modules
        					</xsl:element>
						</p>
                    
                        Request started: <xsl:value-of select="/tmldebugdocument/@started"/><br/>
                        Request ended: <xsl:value-of select="/tmldebugdocument/@ended"/><br/>
                        Number of WebTML Tags: <xsl:value-of select="/tmldebugdocument/@tags"/><br/>
                        Number of WebTML Modules <xsl:value-of select="count(//tmltag[@name='root'])"/><br/>
                        Number of WebTML Portlets <xsl:value-of select="count(//tmltag[@name='root' and @portletname!=''])"/><br/>
                        Loaded backend docs <xsl:value-of select="/tmldebugdocument/@backenddocs"/><br/>
                        
                        <p>
	                        <xsl:apply-templates select="/tmldebugdocument/tmltag[@name='root']"></xsl:apply-templates>
                        </p>
                </body>
                
            </html>
        
    </xsl:template>
    
    <xsl:template  match="tmltag[@name='root' and @portletname]" name="root">
        
        <span class="tmlportlet">
        	
	        <xsl:element name="a">
	            <xsl:attribute name="href">tmlDebug?command=showTags&amp;root=<xsl:value-of select="@path"/>&amp;index=<xsl:value-of select="/tmldebugdocument/@index"/></xsl:attribute>
	            <xsl:value-of select="@portletname"></xsl:value-of>
	        </xsl:element>

            <xsl:if test="@actiontime">
            	<xsl:if test="@actionid">
            		Action ID: <xsl:value-of select="@actionid"/>,
            	</xsl:if>
            	Action execution time: <xsl:value-of select="@actiontime"/>, 
            </xsl:if>
            Resource: <xsl:value-of select="@resource"></xsl:value-of>,
            Portletkey: <xsl:value-of select="@portletkey"></xsl:value-of>,
            Portletmode: <xsl:value-of select="@portletmode"></xsl:value-of>
            <xsl:if test="@portletcontext!=''">
            	, Portletcontext: <xsl:value-of select="@portletcontext"></xsl:value-of>
            </xsl:if>
            <xsl:if test="@skippedforreload!=''">
            	, Skipped for AJAX reload
            </xsl:if>
        </span>
        
        <ul>
			<xsl:apply-templates select="*/tmltag"/>
        </ul>
        
        
    </xsl:template>
    
    <xsl:template match="tmltag" name="tmltag">
            <xsl:for-each select="*/tmltag">
                 <xsl:if test="@name='root'">
                     <li class="tmlportlet"><xsl:apply-templates select="."/></li>
                 </xsl:if>
                 <xsl:if test="@name!='root'">
                         <xsl:apply-templates select="."/>
                 </xsl:if>
            </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>

