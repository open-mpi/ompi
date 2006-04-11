<!--
  - templates for f90 type conversions
  -
  - variables:
  -
  - templates:
  -
  - type-spec-hidden
  - decl-type-spec <arg>
  -
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="text"/>


<!--
  - type-spec-hidden
  -->
<xsl:template name="type-spec-hidden">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>
    <!-- Fortran types -->
    <xsl:when test="@kind = 'fchar'">
      <xsl:if test="@clen = '*'">
        <xsl:text>int</xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:when test="@kind = 'farray'">
      <xsl:call-template name="type-spec-hidden-farray"/>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<!--
  - type-spec-hidden-farray
  -->
<xsl:template name="type-spec-hidden-farray">
  <xsl:if test="array/dimension/@extent = '*'">
    <xsl:text>void*</xsl:text>
  </xsl:if>
</xsl:template>


<!--
  - decl-type-spec <arg>
  -->
<xsl:template name="decl-type-spec">
  <xsl:choose>
    <xsl:when test="@kind = 'fint'">
      <xsl:text>integer</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ffloat'">
      <xsl:text>real</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fcmplx'">
      <xsl:text>complex</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'flogic'">
      <xsl:text>logical</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fchar'">
      <xsl:text>character (len=</xsl:text>
      <xsl:value-of select="@clen"/>
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'farray'"/>
    <xsl:when test="@kind = 'usertype'">
      <xsl:value-of select="concat('type(', @usertype, ')')"/>
    </xsl:when>
    <xsl:when test="@kind = 'fptr'"/>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
