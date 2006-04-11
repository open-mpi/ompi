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
  - templates for type conversions
  -
  - variables:
  -
  - templates:
  -
  - type-qual-rtn   
  - type-spec-rtn   
  -
  - type-qual   
  - type-spec
  - type-spec-ptr
  - type-spec-farray 
  -
  - pointer
  - address-operator <arg/type>
  -
  - rtn-id <return/type>
  - assignment-operator <return/type>
  -
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="type-conv-user.xsl"/>

<xsl:output method="text"/>


<!--
  - type-qual-rtn <return/type>
  -->
<xsl:template name="type-qual-rtn"/>


<!--
  - type-spec-rtn <return/type>
  -->
<xsl:template name="type-spec-rtn">
  <xsl:call-template name="type-spec"/>
</xsl:template>


<!--
  - type-qual
  -->
<xsl:template name="type-qual"/>


<!--
  - type-spec <type>
  -->
<xsl:template name="type-spec">
  <xsl:param name="depth" select="0"/>

  <xsl:variable name="out_text">

  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'">
      <xsl:text>void</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'int'">
      <xsl:choose>
        <xsl:when test="@ikind = 'char'">
          <xsl:text>char</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'uchar'">
          <xsl:text>unsigned char</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'short'">
          <xsl:text>short</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'ushort'">
          <xsl:text>unsigned short</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'int'">
          <xsl:text>int</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'uint'">
          <xsl:text>unsigned int</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'long'">
          <xsl:text>long</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'ulong'">
          <xsl:text>unsigned long</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'longlong'">
          <xsl:text>long long</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'ulonglong'">
          <xsl:text>unsigned long long</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>UNSUPPORTED</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:choose>
        <xsl:when test="@fkind = 'float'">
          <xsl:text>float</xsl:text>
        </xsl:when>
        <xsl:when test="@fkind = 'dbl'">
          <xsl:text>double</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>UNSUPPORTED</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'">
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="type-spec-ptr">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'ref'">
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="type-spec">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:when>

    <!-- Fortran types -->

    <xsl:when test="@kind = 'fvoid'">
      <xsl:text>void</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'flogic'">
      <xsl:text>int</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fint'">
      <xsl:choose>
        <xsl:when test="@ikind = 'char'">
          <xsl:text>char</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'uchar'">
          <xsl:text>unsigned char</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'short'">
          <xsl:text>short</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'ushort'">
          <xsl:text>unsigned short</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'int'">
          <xsl:text>int</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'uint'">
          <xsl:text>unsigned int</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'long'">
          <xsl:text>long</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'ulong'">
          <xsl:text>unsigned long</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'longlong'">
          <xsl:text>long long</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'ulonglong'">
          <xsl:text>unsigned long long</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>UNSUPPORTED</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'ffloat'">
      <xsl:choose>
        <xsl:when test="@fkind = 'float'">
          <xsl:text>float</xsl:text>
        </xsl:when>
        <xsl:when test="@fkind = 'dbl'">
          <xsl:text>double</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>UNSUPPORTED</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'fcmplx'">
      <xsl:choose>
        <xsl:when test="@fkind = 'float'">
          <xsl:text>F90_ComplexFloat</xsl:text>
        </xsl:when>
        <xsl:when test="@fkind = 'dbl'">
          <xsl:text>F90_ComplexDouble</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>UNSUPPORTED</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'fchar'">
      <xsl:text>char*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'farray'">
      <xsl:call-template name="type-spec-farray"/>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fptr'">
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$out_text = 'UNSUPPORTED'">
      <xsl:call-template name="type-spec-user">
        <xsl:with-param name="out_default" select="$out_text"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$out_text"/>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<!--
  - type-spec-ptr <type> - is a pointer
  -->
<xsl:template name="type-spec-ptr">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'usertype'">
      <xsl:value-of select="@usertype"/> <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  - type-spec-farray 
  -->
<xsl:template name="type-spec-farray">
  <xsl:if test="array/dimension/@extent = '*'">
    <xsl:text>void*</xsl:text>
  </xsl:if>
</xsl:template>


<!--
  - pointer
  -->
<xsl:template name="pointer">
  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'"/>
    <xsl:when test="@kind = 'int'">
      <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'"/>
    <xsl:when test="@kind = 'ref'"/>
    <xsl:when test="@kind = 'usertype'">
      <xsl:text>*</xsl:text>
    </xsl:when>

    <!-- Fortran types -->

    <xsl:when test="@kind = 'fvoid'"/>
    <xsl:when test="@kind = 'flogic'">
      <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fint'">
      <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ffloat'">
      <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fcmplx'">
      <xsl:text>*</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fchar'"/>
    <xsl:when test="@kind = 'farray'"/>
    <xsl:when test="@kind = 'usertype'"/>
    <xsl:when test="@kind = 'fptr'"/>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  - address-operator <arg/type>
  -->
<xsl:template name="address-operator"/>


<!--
  - rtn-id <return/type>
  -->
<xsl:template name="rtn-id">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'"/>
    <xsl:when test="@kind = 'ptr'">
      <xsl:text>rtn_</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ref'">
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="rtn-id">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:choose>
        <xsl:when test="@usertype = 'std::string'">
          <xsl:text>rtn_</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>rtn_</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>

    <!-- Fortran types -->

    <xsl:when test="@kind = 'fvoid'"/>
    <xsl:when test="@kind = 'fint'">
      <xsl:text>rtn_</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ffloat'">
      <xsl:text>rtn_</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'flogic'">
      <xsl:text>rtn_</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fchar'"/>
    <xsl:when test="@kind = 'farray'"/>
    <xsl:when test="@kind = 'usertype'"/>
    <xsl:when test="@kind = 'fptr'"/>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  - assignment-operator <return/type>
  -->
<xsl:template name="assignment-operator">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'"/>
    <xsl:when test="@kind = 'int'">
      <xsl:text> = </xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:text> = </xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'ref'">
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="assignment-operator">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:choose>
        <xsl:when test="@usertype = 'std::string'">
          <xsl:text> = </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> = </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>

    <!-- Fortran types -->

    <xsl:when test="@kind = 'fvoid'"/>
    <xsl:when test="@kind = 'fint'">
      <xsl:text> = </xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ffloat'">
      <xsl:text> = </xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'flogic'">
      <xsl:text> = </xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'fchar'"/>
    <xsl:when test="@kind = 'farray'"/>
    <xsl:when test="@kind = 'usertype'"/>
    <xsl:when test="@kind = 'fptr'"/>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  - arg-id <arg/type>
  -->
<xsl:template name="arg-id">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'"/>
    <xsl:when test="@kind = 'int'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'ref'">
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="arg-id">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:choose>
        <xsl:when test="$depth > 0">
          <xsl:value-of select="../../../@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="../@name"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>

    <!-- Fortran types -->

    <xsl:when test="@kind = 'fvoid'"/>
    <xsl:when test="@kind = 'flogic'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'fint'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'ffloat'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'fcmplx'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'fchar'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'farray'">
      <xsl:value-of select="../@name"/>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'"/>
    <xsl:when test="@kind = 'fptr'"/>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
