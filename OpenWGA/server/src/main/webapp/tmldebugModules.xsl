<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    
    <xsl:template match="/">
        
            <html>
                
                <head>
                	<title>WebTML Modules</title>
                	<link rel="stylesheet" href="static/css/tmldebugger.css"/>
                
                
                </head>
                
                <body>

                        <h2>WebTML Modules in Request <xsl:value-of select="/tmldebugdocument/@url"></xsl:value-of></h2>

						<p>
	                        <xsl:element name="a">
	            				<xsl:attribute name="href">tmlDebug?command=showPortlets&amp;index=<xsl:value-of select="/tmldebugdocument/@index"/></xsl:attribute>
	            				Show WebTML portlets
	        				</xsl:element>
        				</p>
                    
                        Request started: <xsl:value-of select="/tmldebugdocument/@started"/><br/>
                        Request ended: <xsl:value-of select="/tmldebugdocument/@ended"/><br/>
                        Number of WebTML Tags: <xsl:value-of select="/tmldebugdocument/@tags"/><br/>
                        Number of WebTML Modules <xsl:value-of select="count(//tmltag[@name='root'])"/><br/>
                        Loaded backend docs <xsl:value-of select="/tmldebugdocument/@backenddocs"/><br/>
                        
                        
                        <p>
	                        <xsl:apply-templates select="/tmldebugdocument/tmltag[@name='root']"></xsl:apply-templates>
                        </p>
                </body>
                
            </html>
        
    </xsl:template>
    
    <xsl:template  match="tmltag[@name='root']" name="root">
        
        <span class="tmlmodule">
        	
        <xsl:element name="a">
            <xsl:attribute name="href">tmlDebug?command=showTags&amp;root=<xsl:value-of select="@path"/>&amp;index=<xsl:value-of select="/tmldebugdocument/@index"/></xsl:attribute>
            <xsl:value-of select="@resource"></xsl:value-of>
        </xsl:element>

            <xsl:if test="@actiontime">
            	Action execution time: <xsl:value-of select="@actiontime"/>, 
            </xsl:if>
            Duration: <xsl:value-of select="@duration"></xsl:value-of>,
            Subtags: <xsl:value-of select="@subtags"></xsl:value-of>,
            Backend-Docs: <xsl:value-of select="@backenddocs"></xsl:value-of>
            <xsl:if test="@directOutput">
            , DirectOutput
            </xsl:if>
        </span>
        
        <ul>
			<xsl:apply-templates select="*/tmltag"/>
        </ul>
        
        
    </xsl:template>
    
    <xsl:template match="tmltag" name="tmltag">
            <xsl:for-each select="*/tmltag">
                 <xsl:if test="@name='root'">
                     <li class="tmlmodule"><xsl:apply-templates select="."/></li>
                 </xsl:if>
                 <xsl:if test="@name!='root'">
                         <xsl:apply-templates select="."/>
                 </xsl:if>
            </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>
