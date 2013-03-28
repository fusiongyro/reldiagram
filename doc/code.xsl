<?xml version="1.0"?>
<xsl:stylesheet version="1.0" 
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:db="http://docbook.org/ns/docbook"
		db:version="5.0">
  <xsl:output method="text"/> 
  <xsl:param name="target">source</xsl:param> 

  <xsl:template match="/">
    <xsl:apply-templates select="//db:programlisting"/>
  </xsl:template>
</xsl:stylesheet>
