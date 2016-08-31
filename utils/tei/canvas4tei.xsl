<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0" 
    xmlns:eg="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    xmlns:exsl="http://exslt.org/common"
    xmlns:msxsl="urn:schemas-microsoft-com:xslt"
    xmlns:fn="http://www.w3.org/2005/xpath-functions"
    extension-element-prefixes="exsl msxsl"
    xmlns="http://www.w3.org/1999/xhtml" 
    xmlns:html="http://www.w3.org/1999/xhtml" 
    exclude-result-prefixes="xsl tei xd eg fn #default">
    
  <xsl:import href="teibp.xsl"/>

  <xsl:output method="xml"/>

  <xsl:param name="standoffCSS" select="concat($filePrefix,'/css/standoff.css')"/>

  <xsl:template match="/" name="htmlShell" priority="99">
    <html>
      <xsl:call-template name="htmlHead"/>
      <body>
	<xsl:if test="$includeToolbox = true()">
	  <xsl:call-template name="teibpToolbox"/>
	  <xsl:call-template name="standoffInfobox"/>
	</xsl:if>
	<canvas id="canvas" style="position:absolute; float:left;"><br/></canvas>
	<div id="tei_wrapper" style="z-index:20000;">
	  <xsl:apply-templates/>
	</div>
	<xsl:copy-of select="$htmlFooter"/>
	<script type="text/javascript" src="{$teibpJS}"></script>
	<script src="https://code.jquery.com/jquery-1.10.2.js"></script>
	<script src="https://raw.githubusercontent.com/caleb531/jcanvas/master/jcanvas.min.js"></script>
	<script src="jannotation.js"></script>
	<script src="static.js"></script>
	<script type="text/javascript">
	  window.onload = function () {
	       drawStatic('canvas', 'tei_wrapper');
	  };
	</script>
	</body>
    </html>
  </xsl:template>

  <xsl:template name="standoffInfobox">
    <div id="standoffInfoboxWrapper">
      <h1>Stand-Off</h1>
      <div id="standoffInfobox"/>
    </div>
  </xsl:template>

  <xsl:template name="htmlHead">
    <head>
      <meta charset="UTF-8"/>
      <script src="{$lessJS}"></script>
      <link id="maincss" rel="stylesheet" type="text/css" href="{$teibpCSS}"/>
      <link id="customcss" rel="stylesheet" type="text/css" href="{$customCSS}"/>
      <link id="standoffcss" rel="stylesheet" type="text/css" href="{$standoffCSS}"/>
      <xsl:call-template name="tagUsage2style"/>
      <xsl:call-template name="rendition2style"/>
      <title><!-- don't leave empty. --></title>
      <xsl:if test="$includeAnalytics = true()">
	<xsl:call-template name="analytics"/>
      </xsl:if>
    </head>
  </xsl:template>

  <!-- add a relation anchor in front of the contents of each text
       range -->
  <xsl:template match="tei:span[@eid]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <a class="relationanchor"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
