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

  <xsl:template match="/" name="htmlShell" priority="99">
    <html>
      <xsl:call-template name="htmlHead"/>
      <body>
	<xsl:if test="$includeToolbox = true()">
	  <xsl:call-template name="teibpToolbox"/>
	</xsl:if>
	<canvas id="canvas" style="position:absolute; float:left;
				   z-index:30000;"><br/></canvas>
	<div id="tei_wrapper" style="z-index:20000;">
	  <xsl:apply-templates/>
	</div>
	<xsl:copy-of select="$htmlFooter"/>
	<script type="text/javascript" src="{$teibpJS}"></script>
	<script src="https://code.jquery.com/jquery-1.10.2.js"></script>
	<script type="application/x-javascript" src="arrows.js"></script>
	<script type="text/javascript">
	  window.onload = function () { drawStatic('canvas',
	  'tei_wrapper')};
	</script>
	</body>
    </html>
  </xsl:template>

</xsl:stylesheet>
