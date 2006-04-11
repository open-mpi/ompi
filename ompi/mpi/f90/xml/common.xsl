<!--
 ...........................................................................
 Copyright (c) 2004-2006 The Regents of the University of California.
                         All rights reserved.
 $COPYRIGHT$
 
 Additional copyrights may follow
 
 $HEADER$
 ...........................................................................
 -->

<!--
  - common templates for Chasm language interoperability system
  -
  - variables:
  -   nl - a newline "\n"
  -
  - templates:
  -
  - include-files
  - include-file (filename)
  - include-file-local (filename)
  -
  - upper-case (symbol)
  - lower-case (symbol)
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="text"/>


<!--
  - nl: a newline "\n"
  -->
<xsl:variable name="nl">
<xsl:text>
</xsl:text>
</xsl:variable>


<!--
  - include-files: include header files (to be overridden, this is a noop)
  -->
<xsl:template name="include-files"/>


<!--
  - include-file (filename): include named header file
  -->
<xsl:template name="include-file">
  <xsl:param name="filename"/>
  <xsl:text>#include &lt;</xsl:text> <xsl:value-of select="$filename"/>
  <xsl:text>&gt;</xsl:text> <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - include-file-local (filename): include named header file
  -->
<xsl:template name="include-file-local">
  <xsl:param name="filename"/>
  <xsl:text>#include &quot;</xsl:text> <xsl:value-of select="$filename"/>
  <xsl:text>&quot;</xsl:text> <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - upper-case (symbol): output symbol in upper case
  -->
<xsl:template name="upper-case">
  <xsl:param name="symbol"/>
  <xsl:value-of select="translate($symbol,
                                  'abcdefghijklmnopqrstuvwxyz',
                                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
</xsl:template>


<!--
  - lower-case (symbol): output symbol in lower case
  -->
<xsl:template name="lower-case">
  <xsl:param name="symbol"/>
  <xsl:value-of select="translate($symbol,
                                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
                                  'abcdefghijklmnopqrstuvwxyz')"/>
</xsl:template>


<!--
  - name-ifnot-alias : choose alias or name if no alias
  -->
<xsl:template name="name-ifnot-alias">
  <xsl:choose>
    <xsl:when test="@alias">
      <xsl:value-of select="@alias"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
