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
  - method level: define program to call Fortran procedures>
  -->
<xsl:template match="method">

    <xsl:choose>
      <xsl:when test="@template = 'yes'">
        <xsl:call-template name="defineArrayFunctionBody"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="defineFunctionBody"/>
      </xsl:otherwise>
    </xsl:choose>

</xsl:template>


<!--
  - defineFunctionBody
  -->
<xsl:template name="defineFunctionBody">

  <xsl:call-template name="decl-construct-list"/>

  <xsl:for-each select="return[1]">
    <xsl:if test="@name = 'ierr'">
     <xsl:text>  integer :: ierr</xsl:text>
     <xsl:value-of select="$nl"/>
    </xsl:if>
  </xsl:for-each>

  <xsl:call-template name="assign-stmt-list"/>

  <xsl:call-template name="call-stmt"/>

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
  - call-or-assign <return/type>
  -->
<xsl:template name="call-or-assign">
  <xsl:text>call </xsl:text>
</xsl:template>


<!--
  - act-arg-spec-list <method>
  -->
<xsl:template name="act-arg-spec-list">
  <xsl:for-each select="arg">
    <xsl:value-of select="@name"/>
    <xsl:if test="position() != last()">
      <xsl:text>, </xsl:text>
    </xsl:if>
  </xsl:for-each>

  <xsl:for-each select="return[1]">
    <xsl:if test="@name = 'ierr'">
      <xsl:if test="../arg[1]">
        <xsl:text>, </xsl:text>
      </xsl:if>
     <xsl:text>ierr</xsl:text>
    </xsl:if>
  </xsl:for-each>
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

  <xsl:for-each select="type[1]">
    <xsl:value-of select="$ws"/>
    <xsl:call-template name="decl-type-spec"/>
  </xsl:for-each>
  <xsl:value-of select="concat(' :: ', @name)"/>
  <xsl:for-each select="type[1]">
    <xsl:call-template name="type-spec-assign-val"/>
  </xsl:for-each>
  <xsl:value-of select="$nl"/>

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
            <xsl:text>(len=STR_LEN)</xsl:text>
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
        <xsl:text>ARRAY_LEN</xsl:text>
        <xsl:value-of select="position()"/>  <xsl:text>D</xsl:text>
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
  - type-spec-assign-val <type>
  -->
<xsl:template name="type-spec-assign-val">

  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'">
      <xsl:text> = VOID_VAL</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'bool'">
      <xsl:text> = INT_VAL</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'int'">
      <xsl:choose>
        <xsl:when test="@ikind = 'int'">
          <xsl:text> = INT_VAL</xsl:text>
        </xsl:when>
        <xsl:when test="@ikind = 'char'">
          <xsl:if test="../../@kind != 'ptr'">
            <xsl:text> = CHAR_VAL</xsl:text>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> = UNSUPPORTED_VAL</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'float'">
      <xsl:text> = FLOAT_VAL</xsl:text>
    </xsl:when>
    <xsl:when test="@kind = 'ptr'">
      <xsl:for-each select="indirect[1]/type[1]">
        <xsl:call-template name="type-spec-assign-val"/>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'array'"/>
    <xsl:when test="@kind = 'ref'">
      <xsl:text> = REF_VAL</xsl:text>
      <xsl:for-each select="indirect/type">
        <xsl:call-template name="type-spec">
          <xsl:with-param name="depth" select="../@depth"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="@kind = 'usertype'">
      <xsl:choose>
        <xsl:when test="@usertype = 'MPI_Aint'">
          <xsl:text> = MPI_AINT_VAL</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'int64_t'">
          <xsl:text> = INT64_VAL</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Status'">
          <xsl:text> = STATUS_VAL</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:param name="prefix" select="substring-before(@usertype, '_')"/>
          <xsl:if test="$prefix = 'MPI'">
            <xsl:text> = USER_INT_VAL</xsl:text>
          </xsl:if>
          <xsl:if test="$prefix != 'MPI'">
            <xsl:text> = UNSUPPORTED_VAL</xsl:text>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> = UNSUPPORTED_VAL</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<!--
  - assign-stmt <arg>
  -->
<xsl:template name="assign-stmt">
  <xsl:param name="ws" select="'  '"/>

  <xsl:choose>
    <xsl:when test="type/indirect/type/@ikind = 'char'">
      <xsl:value-of select="$ws"/>
      <xsl:value-of select="concat(@name, ' = STR_VAL')"/>
      <xsl:value-of select="$nl"/>
    </xsl:when>
    <xsl:when test="type/@kind = 'array'">
      <xsl:value-of select="$ws"/>
      <xsl:value-of select="concat(@name, ' = ARRAY_VAL')"/>
      <xsl:for-each select="type/array[1]/dimension">
        <xsl:if test="position() = last()">
          <xsl:value-of select="last()"/>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>D</xsl:text>
      <xsl:value-of select="$nl"/>
    </xsl:when>
  </xsl:choose>

</xsl:template>


<!--
  - openFile: print file header information
  -->
<xsl:template name="openFile">
  <xsl:param name="filename" select="''"/>
  <xsl:text>!</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text>! Fortran program to test F90 interface to C function </xsl:text>
  <xsl:value-of select="$test_function"/>
  <xsl:value-of select="$nl"/>
  <xsl:text>!</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text>program main</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text>  use mpi</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text>  implicit none</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text>  include 'F90TestValues.h'</xsl:text>
  <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - closeFile: finish up
  -->
<xsl:template name="closeFile">
  <xsl:param name="filename" select="''"/>
  <xsl:text>  if (ierr /= 0) print *, "ERROR running </xsl:text>
  <xsl:value-of select="$test_function"/> <xsl:text>"</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text>end program</xsl:text>
  <xsl:value-of select="$nl"/>
</xsl:template>


</xsl:stylesheet>
