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

<xsl:variable name="filename">
  <xsl:call-template name="lower-case">
    <xsl:with-param name="symbol" select="/library/scope/@name"/>
  </xsl:call-template>
  <xsl:text>_C.c</xsl:text>
</xsl:variable>

<xsl:variable name="header_file">
  <xsl:call-template name="lower-case">
    <xsl:with-param name="symbol" select="/library/scope/@name"/>
  </xsl:call-template>
  <xsl:text>_C.h</xsl:text>
</xsl:variable>


<!--
  - root level
  -->
<xsl:template match="/">
  <xsl:call-template name="openFile">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:call-template>

  <!-- output all include files -->

  <xsl:call-template name="include-files">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:call-template>

  <!-- define C bridging functions -->

  <xsl:for-each select="library/scope">
    <xsl:call-template name="defineFunctions"/>
  </xsl:for-each>

  <xsl:call-template name="closeFile">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:call-template>
</xsl:template>


<!--
  - defineFunctions: define functions to call Fortran procedures <scope>
  -->
<xsl:template name="defineFunctions">
  <xsl:param name="module" select="@name"/>
  <xsl:value-of select="$nl"/>
  <xsl:for-each select="method">

    <xsl:value-of select="$nl"/>
    <xsl:text>procedure='</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>'</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:value-of select="$nl"/>

    <xsl:text>echo "interface ${procedure}"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>echo</xsl:text>
    <xsl:value-of select="$nl"/>

    <xsl:choose>
      <xsl:when test="@template = 'yes'">
        <xsl:call-template name="defineArrayFunctionBody"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="defineFunctionBody"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text>echo "end interface ${procedure}"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>echo</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>echo</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:value-of select="$nl"/>

  </xsl:for-each>

</xsl:template>


<!--
  - defineFunctionBody
  -->
<xsl:template name="defineFunctionBody">

    <xsl:text>proc="${procedure}"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>echo "subroutine ${proc}(</xsl:text>
    <xsl:call-template name="arg-list"/> <xsl:text>)"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>echo "  use mpi_kinds"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:call-template name="decl-construct-list">
      <xsl:with-param name="ws" select="''"/>
    </xsl:call-template>

    <xsl:for-each select="return[1]">
      <xsl:if test="@name = 'ierr'">
       <xsl:text>echo "  integer, intent(out) :: ierr"</xsl:text>
      </xsl:if>
    </xsl:for-each>

<xsl:text>
echo "end subroutine ${proc}"
echo
</xsl:text>

</xsl:template>


<!--
  - defineArrayFunctionBody
  -->
<xsl:template name="defineArrayFunctionBody">

<xsl:text>
for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

</xsl:text>

    <xsl:text>  for kind in $ikinds</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>  do</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>    proc="${procedure}${rank}DI${kind}"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>    echo "subroutine ${proc}(</xsl:text>
    <xsl:call-template name="arg-list"/> <xsl:text>)"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>    echo "  use mpi_kinds"</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:call-template name="decl-construct-list">
      <xsl:with-param name="ws" select="'    '"/>
    </xsl:call-template>

    <xsl:for-each select="return[1]">
      <xsl:if test="@name = 'ierr'">
       <xsl:text>    echo "  integer, intent(out) :: ierr"</xsl:text>
      </xsl:if>
    </xsl:for-each>

<xsl:text>
    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
</xsl:text>

</xsl:template>


<!--
  - arg-list <method>
  -->
<xsl:template name="arg-list">
  <xsl:for-each select="arg">
    <xsl:value-of select="@name"/>
    <xsl:if test="position() != last()">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:if test="position() = 5">
      <xsl:text>&amp;</xsl:text>
      <xsl:value-of select="concat($nl, '        ')"/>
    </xsl:if>
  </xsl:for-each>
  <xsl:for-each select="return[1]">
    <xsl:if test="@name = 'ierr'">
      <xsl:if test="../arg[1]">
        <xsl:text>, </xsl:text>
      </xsl:if>
      <xsl:value-of select="@name"/>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<!--
  - decl-construct-list <method>
  -->
<xsl:template name="decl-construct-list">
  <xsl:param name="ws" select="'  '"/>
  <xsl:for-each select="arg">
    <xsl:call-template name="type-decl-stmt">
      <xsl:with-param name="ws" select="$ws"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - type-decl-stmt <arg>
  -->
<xsl:template name="type-decl-stmt">
  <xsl:param name="ws" select="'  '"/>

  <xsl:value-of select="$ws"/>
  <xsl:text>echo "  </xsl:text>
  <xsl:for-each select="type[1]">
    <xsl:call-template name="decl-type-spec"/>
  </xsl:for-each>

  <xsl:if test="type/@kind != 'void'">
    <xsl:value-of select="concat(', intent(', @intent, ')')"/>
  </xsl:if>
  <xsl:value-of select="concat(' :: ', @name, '&quot;', $nl)"/>

</xsl:template>


<!--
  - decl-type-spec <arg>
  -->
<xsl:template name="decl-type-spec">

  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'">
      <xsl:text>integer(kind=MPI_INTEGER${kind}_KIND)</xsl:text>
      <xsl:text>, dimension(${dim})</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'bool'">
      <xsl:text>integer</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'int'">
      <xsl:choose>
        <xsl:when test="@ikind = 'int'">
          <xsl:text>integer</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'char'">
          <xsl:text>character</xsl:text>
          <xsl:if test="../../@kind = 'ptr'">
            <xsl:text>(len=*)</xsl:text>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>UNSUPPORTED</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'">
      <xsl:for-each select="indirect[1]/type[1]">
        <xsl:call-template name="decl-type-spec"/>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'array'">
      <xsl:for-each select="array[1]/type[1]">
        <xsl:call-template name="decl-type-spec"/>
      </xsl:for-each>
      <xsl:text>, dimension(</xsl:text>
      <xsl:for-each select="array[1]/dimension">
        <xsl:value-of select="@extent"/>
        <xsl:if test="position() != last()">
          <xsl:text>, </xsl:text>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ref'">
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="type-spec">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:choose>
        <xsl:when test="@usertype = 'MPI_Aint'">
          <xsl:text>integer(kind=MPI_ADDRESS_KIND)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'int64_t'">
          <xsl:text>integer(kind=MPI_OFFSET_KIND)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Status'">
          <xsl:text>integer(MPI_STATUS_SIZE)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:param name="prefix" select="substring-before(@usertype, '_')"/>
          <xsl:if test="$prefix = 'MPI'">
            <xsl:text>integer</xsl:text>
          </xsl:if>
          <xsl:if test="$prefix != 'MPI'">
            <xsl:text>UNSUPPORTED</xsl:text>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>UNSUPPORTED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!--
  - openFile: print file header information
  -->
<xsl:template name="openFile">
  <xsl:param name="filename" select="''"/>
  <xsl:text>#! /bin/sh</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>
  <xsl:text>. fortran_kinds.sh</xsl:text>
  <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - closeFile: finish up
  -->
<xsl:template name="closeFile">
  <xsl:param name="filename" select="''"/>
</xsl:template>


</xsl:stylesheet>
