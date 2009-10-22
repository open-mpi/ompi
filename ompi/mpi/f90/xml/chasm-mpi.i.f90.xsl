<!--
 ...........................................................................
 Copyright (c) 2004-2006 The Regents of the University of California.
                         All rights reserved.
 Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 $COPYRIGHT$
 
 Additional copyrights may follow
 
 $HEADER$
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
  <xsl:for-each select="method">

   <xsl:if test="@kind != 'No_F90'">

    <xsl:choose>
      <xsl:when test="@template = 'yes'">
        <xsl:call-template name="defineArrayFunctionBody"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="defineFunctionBody"/>
      </xsl:otherwise>
    </xsl:choose>

   </xsl:if>
  </xsl:for-each>

</xsl:template>


<!--
  - defineFunctionBody
  -->
<xsl:template name="defineFunctionBody">

<xsl:text>
#------------------------------------------------------------------------

output_</xsl:text>  <xsl:number/>  <xsl:text>() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat &lt;&lt;EOF

</xsl:text>

    <xsl:text>subroutine ${procedure}(</xsl:text>
    <xsl:call-template name="arg-list"/> <xsl:text>)</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>  include 'mpif-config.h'</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:call-template name="decl-construct-list">
      <xsl:with-param name="ws" select="''"/>
      <xsl:with-param name="void_type" select="''"/>
      <xsl:with-param name="void_kind" select="''"/>
    </xsl:call-template>

    <xsl:for-each select="return[1]">
      <xsl:if test="@name = 'ierr'">
       <xsl:text>  integer, intent(out) :: ierr</xsl:text>
      </xsl:if>
    </xsl:for-each>

<xsl:text>
end subroutine ${procedure}

EOF
}

start </xsl:text>
    <xsl:choose>
      <xsl:when test="@interface != ''">
        <xsl:value-of select="@interface"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@name"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text> </xsl:text>
    <xsl:call-template name="interface-size"/>
<xsl:text>
output_</xsl:text>  <xsl:number/>  <xsl:text> </xsl:text>
<xsl:value-of select="@name"/>  <xsl:text>
end </xsl:text>
    <xsl:choose>
      <xsl:when test="@interface != ''">
        <xsl:value-of select="@interface"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@name"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$nl"/>

</xsl:template>


<!--
  - defineArrayFunctionBody
  -->
<xsl:template name="defineArrayFunctionBody">

<xsl:text>
#------------------------------------------------------------------------

output_</xsl:text>  <xsl:number/>  <xsl:text>() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat &lt;&lt;EOF

</xsl:text>

    <xsl:text>subroutine ${proc}(</xsl:text>
    <xsl:call-template name="arg-list"/> <xsl:text>)</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:text>  include 'mpif-config.h'</xsl:text>
    <xsl:value-of select="$nl"/>
    <xsl:call-template name="decl-construct-list">
      <xsl:with-param name="ws" select="''"/>
      <xsl:with-param name="void_type" select="'logical'"/>
      <xsl:with-param name="void_kind" select="'INTEGER'"/>
      <xsl:with-param name="has_dim" select="0"/>
    </xsl:call-template>

    <xsl:for-each select="return[1]">
      <xsl:if test="@name = 'ierr'">
       <xsl:text>  integer, intent(out) :: ierr</xsl:text>
      </xsl:if>
    </xsl:for-each>

<xsl:text>
end subroutine ${proc}

EOF
}

start </xsl:text>  <xsl:value-of select="@name"/>  <xsl:text> </xsl:text>
<xsl:call-template name="interface-size"/>
<xsl:value-of select="$nl"/>

<xsl:text>
for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_</xsl:text>  <xsl:number/>  <xsl:text> </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> ${rank} CH "character${dim}"
  output_</xsl:text>  <xsl:number/>  <xsl:text> </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_</xsl:text>  <xsl:number/>  <xsl:text> </xsl:text>
    <xsl:value-of select="@name"/>
  <xsl:text> ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_</xsl:text>  <xsl:number/>  <xsl:text> </xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text> ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_</xsl:text>  <xsl:number/>  <xsl:text> </xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text> ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end </xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:value-of select="$nl"/>

</xsl:template>


<!--
  - interface-size <method>
  -->
<xsl:template name="interface-size">
  <xsl:variable name="count">
    <xsl:call-template name="choice-count"/>
  </xsl:variable>

  <xsl:choose>
    <!-- special cases-->
    <xsl:when test="@name = 'MPI_Alloc_mem'">
      <xsl:text>medium</xsl:text>
    </xsl:when>
    <xsl:when test="@name = 'MPI_SIZEOF'">
      <xsl:text>trivial</xsl:text>
    </xsl:when>
    <!-- end special cases-->
    <xsl:when test="$count = ''">
      <xsl:text>small</xsl:text>
    </xsl:when>
    <xsl:when test="$count = '1'">
      <xsl:text>medium</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>large</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<!--
  - choice-count <method>
  -->
<xsl:template name="choice-count">

  <xsl:for-each select="arg">
    <xsl:if test="type/@idl = 'choice'">
      <xsl:text>1</xsl:text>
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
  <xsl:param name="void_type"/>
  <xsl:param name="void_kind"/>
  <xsl:param name="has_dim" select="1"/>
  <xsl:for-each select="arg">
    <xsl:call-template name="type-decl-stmt">
      <xsl:with-param name="ws" select="$ws"/>
      <xsl:with-param name="void_type" select="$void_type"/>
      <xsl:with-param name="void_kind" select="$void_kind"/>
      <xsl:with-param name="has_dim" select="$has_dim"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - type-decl-stmt <arg>
  -->
<xsl:template name="type-decl-stmt">
  <xsl:param name="ws" select="'  '"/>
  <xsl:param name="void_type"/>
  <xsl:param name="void_kind"/>
  <xsl:param name="has_dim" select="1"/>

  <xsl:value-of select="$ws"/>
  <xsl:text>  </xsl:text>
  <xsl:for-each select="type[1]">
    <xsl:call-template name="decl-type-spec">
      <xsl:with-param name="void_type" select="$void_type"/>
      <xsl:with-param name="void_kind" select="$void_kind"/>
      <xsl:with-param name="has_dim" select="$has_dim"/>
    </xsl:call-template>
  </xsl:for-each>
  <xsl:for-each select="type[1]">
    <xsl:call-template name="decl-type-intent"/>
  </xsl:for-each>
  <xsl:value-of select="concat(' :: ', @name, $nl)"/>

</xsl:template>


<!--
  - decl-type-intent <type>
  -->
<xsl:template name="decl-type-intent">

  <xsl:choose>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'"/>
    <xsl:when test="@kind = 'ptr'">
      <xsl:choose>
        <xsl:when
             test="indirect/type/@usertype='MPI_Comm_errhandler_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Comm_copy_attr_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Comm_delete_attr_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Handler_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_File_errhandler_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Grequest_query_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Grequest_free_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Grequest_cancel_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Copy_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Delete_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_User_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Datarep_conversion_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Datarep_extent_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Type_copy_attr_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Type_delete_attr_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Win_errhandler_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Win_copy_attr_function'"/>
        <xsl:when
             test="indirect/type/@usertype='MPI_Win_delete_attr_function'"/>
        <xsl:otherwise>
          <xsl:value-of select="concat(', intent(', ../@intent, ')')"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="concat(', intent(', ../@intent, ')')"/>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<!--
  - decl-type-spec <type>
  -->
<xsl:template name="decl-type-spec">
  <xsl:param name="void_type"/>
  <xsl:param name="void_kind"/>
  <xsl:param name="has_dim" select="1"/>

  <xsl:choose>

    <!-- idl types (annotate xml to specify what this type is) -->

    <xsl:when test="@idl = 'MPI_ARGV'">
      <xsl:text>character(len=*), dimension(*)</xsl:text>
    </xsl:when>
    <xsl:when test="@idl = 'MPI_A_ARGV'">
      <xsl:text>character(len=*), dimension(</xsl:text>
      <xsl:value-of select="array/dimension[2]/@extent"/>
      <xsl:text>,*)</xsl:text>
    </xsl:when>
    <xsl:when test="@idl = 'MPI_Aint'">
      <xsl:text>integer(kind=MPI_ADDRESS_KIND)</xsl:text>
    </xsl:when>

    <!-- C++ types -->

    <xsl:when test="@kind = 'void'">
      <xsl:choose>
        <xsl:when test="../../../../@template = 'yes'">
          <xsl:text>${type}</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>integer</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@kind = 'bool'">
      <xsl:text>logical</xsl:text>
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
        <xsl:when test="@usertype = 'default_integer'">
          <xsl:text>integer</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Aint'">
          <xsl:text>integer(kind=MPI_ADDRESS_KIND)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'int64_t'">
          <xsl:text>integer(kind=MPI_OFFSET_KIND)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Status'">
          <xsl:text>integer, dimension(MPI_STATUS_SIZE)</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Comm_errhandler_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Comm_copy_attr_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Comm_delete_attr_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Handler_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_File_errhandler_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Grequest_query_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Grequest_free_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Grequest_cancel_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Copy_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Delete_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_User_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Datarep_conversion_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Datarep_extent_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Type_copy_attr_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Type_delete_attr_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Win_errhandler_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Win_copy_attr_function'">
          <xsl:text>external</xsl:text>
        </xsl:when>
        <xsl:when test="@usertype = 'MPI_Win_delete_attr_function'">
          <xsl:text>external</xsl:text>
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
  - copyright: print copyright
  -->
<xsl:template name="copyright">


<xsl:text>
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
</xsl:text>

</xsl:template>


<!--
  - info: print information about file
  -->
<xsl:template name="info">

<xsl:text>
# Do a little error checking

if test ! -f "$1/fortran_kinds.sh"; then
    echo "ERROR: Cannot find fortran_kinds.sh" >&amp;2
    exit 1
elif test -z "$1/fortran_kinds.sh"; then
    echo "ERROR: fortran_kinds.sh appears to be empty!" >&amp;2
    exit 1
fi

# Read in the KIND information

. "$1/fortran_kinds.sh"

# Setup

output=1
allranks="0 $ranks"

# A few hard-coded functions that cannot pass through to the F77
# equivalents

start MPI_Wtick small
if test "$output" = "1"; then
    cat <<EOF

function MPI_Wtick()
    double precision MPI_Wtick
end function MPI_Wtick

EOF
fi
end MPI_Wtick

start MPI_Wtime small
if test "$output" = "1"; then
    cat <<EOF

function MPI_Wtime()
    double precision MPI_Wtime
end function MPI_Wtime

EOF
fi
end MPI_Wtime

#------------------------------------------------------------------------

# Helper functions

start() {
    check_size $2
    if test "$output" = "1"; then        
        echo "interface $1"
    fi
}

end() {
    if test "$output" = "1"; then
        cat &lt;&lt;EOF
end interface


EOF
    fi
}
</xsl:text>

</xsl:template>


<!--
  - openFile: print file header information
  -->
<xsl:template name="openFile">
  <xsl:param name="filename" select="''"/>
  <xsl:text>#! /bin/sh</xsl:text>
  <xsl:call-template name="copyright"/>
  <xsl:call-template name="info"/>
</xsl:template>


<!--
  - closeFile: finish up
  -->
<xsl:template name="closeFile">
  <xsl:param name="filename" select="''"/>
</xsl:template>


</xsl:stylesheet>
