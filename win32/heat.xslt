<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:wix="http://schemas.microsoft.com/wix/2006/wi"
        xmlns="http://schemas.microsoft.com/wix/2006/wi"
        exclude-result-prefixes="wix">

  <xsl:output method="xml" indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='/wix:Wix/wix:Fragment/wix:DirectoryRef[@Id="INSTALLDIR"]/wix:Directory[@Name="bin"]/wix:Component/wix:File[@Source="SourceDir\bin\leksah.exe"]/@Id'>
    <xsl:attribute name="Id">
      <xsl:text>LeksahExe</xsl:text>
    </xsl:attribute>
  </xsl:template>
</xsl:stylesheet>
