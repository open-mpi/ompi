<!-- LANL:license
 ...........................................................................
 - This SOFTWARE has been authored by an employee or employees of the
 - University of California, operator of the Los Alamos National Laboratory
 - under Contract No. W-7405-ENG-36 with the U.S. Department of Energy.
 - The U.S. Government has rights to use, reproduce, and distribute this
 - SOFTWARE.  The public may copy, distribute, prepare derivative works and
 - publicly display this SOFTWARE without charge, provided that this Notice
 - and any statement of authorship are reproduced on all copies.  Neither
 - the Government nor the University makes any warranty, express or implied,
 - or assumes any liability or responsibility for the use of this SOFTWARE.
 - If SOFTWARE is modified to produce derivative works, such modified
 - SOFTWARE should be clearly marked, so as not to confuse it with the
 - version available from LANL.
 ...........................................................................
 - LANL:license
 ...........................................................................
 -->

<!--
  - chasm-mpi.f90.xsl: creates F90 functions that call MPI equivalent
  -
  - Output should be directed to the file, Method_f90.f90
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="common.xsl"/>
<xsl:import href="common-C.xsl"/>
<xsl:import href="common-f90.xsl"/>

<xsl:output method="text"/>

<!-- global variables -->

<xsl:param name="test_function" select="unkown_function"/>


<!--
  - root level
  -->
<xsl:template match="/">
  <xsl:call-template name="openFile"/>
  <xsl:apply-templates select="/library/scope/method[@name=$test_function]"/>
  <xsl:call-template name="closeFile"/>
</xsl:template>


<!--
  - method level: define C test function>
  -->
<xsl:template match="method">
  <xsl:call-template name="compound-statement"/>
</xsl:template>


<!--
  - param-id-list <method>
  -->
<xsl:template name="param-id-list">
  <xsl:param name="module"/> 
  <xsl:for-each select="arg">
    <xsl:call-template name="param-decl">
      <xsl:with-param name="id" select="@name"/>
    </xsl:call-template>
    <xsl:if test="position() != last()">
      <xsl:text>, </xsl:text>
    </xsl:if>
  </xsl:for-each>
  <xsl:if test="return/@name= 'ierr'">
    <xsl:text>, int* ierr</xsl:text>
  </xsl:if>
</xsl:template>


<!--
  - compound-statement <method>
  -   decl-list statement-list
  -->
<xsl:template name="statement-list">
  <xsl:param name="function_id">
    <xsl:call-template name="function-id"/>
  </xsl:param>
  <xsl:param name="ws" select="'  '"/>

  <xsl:if test="return/@name = 'ierr'">
    <xsl:value-of select="concat($ws, '*ierr = 0;', $nl)"/>
  </xsl:if>
  <xsl:for-each select="arg">
    <xsl:call-template name="type-spec-if-stmt"/>
  </xsl:for-each>  

</xsl:template>


<!--
  - type-spec-if-stmt <arg>
  -->
<xsl:template name="type-spec-if-stmt">
  <xsl:param name="ws" select="'  '"/>
  <xsl:variable name="name">
    <xsl:call-template name="alias"/>
  </xsl:variable>
  <xsl:variable name="i" select="concat('i', position())"/>
  <xsl:variable name="a" select="concat('a', position())"/>

  <xsl:if test="type/@kind = 'ptr'">
    <xsl:choose>
      <xsl:when test="type/indirect/type/@ikind = 'char'"/>
      <xsl:otherwise>
        <xsl:value-of select="concat($ws, 'if ')"/>
        <xsl:for-each select="type[1]">
          <xsl:call-template name="type-spec-if-expr"/>
        </xsl:for-each>
        <xsl:value-of select="concat(' *ierr = 1;', $nl)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>

  <xsl:for-each select="type[1]">
    <xsl:choose>
      <xsl:when test="@kind = 'ptr'">
        <xsl:if test="indirect/type/@ikind = 'char'">
          <xsl:if test="@alias">
            <xsl:value-of select="concat($ws, 'if (STR_LEN != ')"/>
            <xsl:value-of select="concat(@alias, ') *ierr = 1;', $nl)"/>
          </xsl:if>
          <xsl:value-of select="concat($ws, 'if (strncmp(', $name)"/>
          <xsl:text>, STR_VAL, STR_LEN) != 0)</xsl:text>
          <xsl:value-of select="concat(' *ierr = 1;', $nl)"/>
        </xsl:if>
      </xsl:when>
      <xsl:when test="@kind = 'array'">
        <xsl:if test="array/@rank = 1">
          <xsl:value-of select="concat('  for (', $i, ' = 0; ')"/>
          <xsl:value-of select="concat($i, ' &lt; ARRAY_LEN1D; ')"/>
          <xsl:value-of select="concat($i, '++)', $nl)"/>
          <xsl:value-of select="concat('    if (', $name, '[', $i, ']')"/>
          <xsl:value-of select="concat(' != ', $a, '[', $i, '])')"/>
          <xsl:value-of select="concat(' *ierr = 1;', $nl)"/>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat($ws, 'if ')"/>
        <xsl:call-template name="type-spec-if-expr"/>
        <xsl:value-of select="concat(' *ierr = 1;', $nl)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>  
</xsl:template>


<!--
  - decl-list-decl-rtn ([rtn_id=rtn-id, ws='  ']) <return/type>
  -->
<xsl:template name="decl-list-decl-rtn">
</xsl:template>


<!--
  - decl-list-decl-arg ([arg_id=arg-id, ws='  ']) <arg/type>
  -->
<xsl:template name="decl-list-decl-arg">
  <xsl:if test="@kind = 'array'">
    <xsl:if test="array/@rank = 1">
      <xsl:value-of select="concat('  int i', position(), ';', $nl)"/>
      <xsl:value-of select="concat('  int a', position())"/>
      <xsl:value-of select="concat('[] = ARRAY_VAL1D;', $nl)"/>
    </xsl:if>
  </xsl:if>
</xsl:template>


<!--
  - type-spec-if-expr <type>
  -->
<xsl:template name="type-spec-if-expr">
  <xsl:param name="depth" select="0"/>
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="../@alias">
        <xsl:value-of select="../@alias"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="../@name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:if test="$depth = 0">
    <xsl:value-of select="concat('(*', $name)"/>
  </xsl:if>

  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'">
    </xsl:when>
    <xsl:when test="@kind = 'bool'">
      <xsl:text> != INT_VAL)</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'int'">
      <xsl:choose>
        <xsl:when test="@ikind = 'int'">
          <xsl:text> != INT_VAL)</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'char'">
          <xsl:if test="../../@kind != 'ptr'">
            <xsl:text> != CHAR_VAL)</xsl:text>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> != UNSUPPORTED_VAL)</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:text> != FLOAT_VAL)</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'">
      <xsl:for-each select="indirect[1]/type[1]">
        <xsl:call-template name="type-spec-if-expr">
          <xsl:with-param name="depth" select="indirect/@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'array'"/>
    <xsl:when test="@kind = 'ref'">
      <xsl:text> != REF_VAL)</xsl:text>
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="type-spec">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:choose>
        <xsl:when test="@usertype = 'MPI_Aint'">
          <xsl:text> != MPI_AINT_VAL)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'int64_t'">
          <xsl:text> != INT64_VAL)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Status'">
          <xsl:text> != STATUS_VAL)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:param name="prefix" select="substring-before(@usertype, '_')"/>
          <xsl:if test="$prefix = 'MPI'">
            <xsl:text> != USER_INT_VAL)</xsl:text>
          </xsl:if>
          <xsl:if test="$prefix != 'MPI'">
            <xsl:text> != UNSUPPORTED_VAL)</xsl:text>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> != UNSUPPORTED_VAL</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<!--
  - type-spec-rtn <return/type>
  -->
<xsl:template name="type-spec-rtn">
  <xsl:if test="../@name = 'ierr'">
    <xsl:text>void</xsl:text>
  </xsl:if>
</xsl:template>


<!--
  - alias : choose alias or name if no alias
  -->
<xsl:template name="alias">
  <xsl:choose>
    <xsl:when test="@alias">
      <xsl:value-of select="@alias"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  - openFile: print file header information
  -->
<xsl:template name="openFile">
  <xsl:param name="filename" select="''"/>
</xsl:template>


<!--
  - closeFile: finish up
  -->
<xsl:template name="closeFile">
  <xsl:param name="filename" select="''"/>
</xsl:template>


</xsl:stylesheet>
