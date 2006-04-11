<!--
  - user-defined templates for type conversions
  -
  - Users can add new types here (for types that are not already specified
  - elsewhere).
  -
  - variables:
  -
  - templates:
  -   type-spec-user
  -
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="text"/>


<!--
  - type-spec-user <type>
  -   Enter user-defined tests to add to types not already specified
  -   in type-conversions.xsl (or elsewhere).
  -->
<xsl:template name="type-spec-user">
  <xsl:param name="out_default" select="'UNSUPPORTED'"/>

  <xsl:choose>
    <xsl:when test="@usertype = 'NOT_A_REAL_TYPE'">
      <xsl:value-of select="$out_default"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$out_default"/>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


</xsl:stylesheet>
