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
  - common templates for creating C files
  -
  - templates:
  -
  - defineInitMacro ([module=@name]) <scope>
  - defineMacros ([module=@name]) <scope>
  - def-macro-lower
  - def-macro-upper
  -
  - declareInitFunction <scope>
  -
  - declareFunctionPointers ([module=@name]) <scope>
  - decl-function-pointer
  -
  - defineSetFunctionPointers ([module=@name]) <scope>
  - def-set-function-pointer
  -
  - defineFunctions ([module=@name]) <scope>
  -
  - decl-arg-list <method>
  - param-decl-hidden <arg>
  -
  - use-stmt-list ([ws='  ']) <method>
  - use-stmt <arg>
  -
  - decl-construct-list ([ws='  ']) <method>
  - type-decl-stmt <arg>
  -
  - assign-stmt-list ([ws='  ']) <method>
  - assign-stmt <arg>
  -
  - call-stmt ([ws='  ']) <method>
  - call-or-assign <method>
  - proc-designator <method>
  -
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="type-conv-f90.xsl"/>

<!--
  - defineInitMacro: define macro for Fortran init module procedure <scope>
  -->
<xsl:template name="defineInitMacro">
  <xsl:param name="module" select="@name"/>
  <xsl:text>/**</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text> * Macros to create Fortran symbols</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text> */</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>

  <!-- lower case -->

  <xsl:text>#if defined(F90_SYM_CASE_LOWER)</xsl:text>
  <xsl:value-of select="$nl"/>

  <xsl:call-template name="def-macro-lower">
    <xsl:with-param name="symbol">
      <xsl:text>CH_INIT_</xsl:text> <xsl:value-of select="$module"/>
    </xsl:with-param>
  </xsl:call-template>

  <!-- upper case -->

  <xsl:text>#elif defined(F90_SYM_CASE_UPPER)</xsl:text>
  <xsl:value-of select="$nl"/>

  <xsl:call-template name="def-macro-upper">
    <xsl:with-param name="symbol">
      <xsl:text>CH_INIT_</xsl:text> <xsl:value-of select="$module"/>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:text>#endif /* F90_SYM_CASE */</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>

</xsl:template>


<!--
  - defineMacros: define macros for Fortran symbols <scope>
  -->
<xsl:template name="defineMacros">
  <xsl:param name="module" select="@name"/>
  <xsl:param name="symbol" select="''"/>
  <xsl:text>/**</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text> * Macros to create Fortran symbols</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text> */</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>

  <!-- lower case -->

  <xsl:text>#if defined(F90_SYM_CASE_LOWER)</xsl:text>
  <xsl:value-of select="$nl"/>

  <xsl:for-each select="method">
    <xsl:if test='string-length($symbol) &gt; 0'>
      <xsl:call-template name="def-macro-lower">
        <xsl:with-param name="symbol" select="$symbol"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:call-template name="def-macro-lower">
      <xsl:with-param name="symbol">
        <xsl:text>SET_</xsl:text> <xsl:value-of select="$module"/>
        <xsl:text>_</xsl:text> <xsl:value-of select="@name"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>

  <!-- upper case -->

  <xsl:text>#elif defined(F90_SYM_CASE_UPPER)</xsl:text>
  <xsl:value-of select="$nl"/>

  <xsl:for-each select="method">
    <xsl:if test='string-length($symbol) &gt; 0'>
      <xsl:call-template name="def-macro-upper">
        <xsl:with-param name="symbol" select="$symbol"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:call-template name="def-macro-upper">
      <xsl:with-param name="symbol">
        <xsl:text>SET_</xsl:text> <xsl:value-of select="$module"/>
        <xsl:text>_</xsl:text> <xsl:value-of select="@name"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>

  <xsl:text>#endif /* F90_SYM_CASE */</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>

</xsl:template>


<!--
  - def-macro-lower
  -->
<xsl:template name="def-macro-lower">
  <xsl:param name="symbol"/> 
  <xsl:variable name="sym_lower">
    <xsl:call-template name="lower-case">
      <xsl:with-param name="symbol" select="$symbol"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:text>#  define </xsl:text> <xsl:value-of select="$symbol"/>
  <xsl:text> F90_SYMBOL(</xsl:text> <xsl:value-of select="$sym_lower"/>
  <xsl:text>)</xsl:text> <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - def-macro-upper
  -->
<xsl:template name="def-macro-upper">
  <xsl:param name="symbol"/> 
  <xsl:variable name="sym_upper">
    <xsl:call-template name="upper-case">
      <xsl:with-param name="symbol" select="$symbol"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:text>#  define </xsl:text> <xsl:value-of select="$symbol"/>
  <xsl:text> F90_SYMBOL(</xsl:text> <xsl:value-of select="$sym_upper"/>
  <xsl:text>)</xsl:text> <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - declareFunctionPointers: declare pointers to Fortran procedures <scope>
  -->
<xsl:template name="declareFunctionPointers">
  <xsl:param name="module" select="@name"/>
  <xsl:text>/**</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text> * Pointers to Fortran procedures</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text> */</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>
  <xsl:for-each select="method">
    <xsl:call-template name="decl-function-pointer">
      <xsl:with-param name="id">
        <xsl:value-of select="$module"/> <xsl:text>_</xsl:text>
        <xsl:value-of select="@name"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - declareInitFunction <scope>
  -->                 
<xsl:template name="declareInitFunction">
  <xsl:param name="symbol">
    <xsl:text>CH_INIT_</xsl:text> <xsl:value-of select="@name"/>
  </xsl:param>
  <xsl:value-of select="concat('/**', $nl)"/>
  <xsl:text> * Declare Fortran initialization routine</xsl:text>
  <xsl:value-of select="concat($nl, ' */', $nl)"/>
  <xsl:text>void </xsl:text>
  <xsl:value-of select="$symbol"/> <xsl:text>(void);</xsl:text>
  <xsl:value-of select="concat($nl, $nl)"/>
</xsl:template>


<!--
  - decl-function-pointer
  -   type-qual type-spec ( * PTR_ function-id ) ( param-type-list );
  -->
<xsl:template name="decl-function-pointer">
  <xsl:param name="id"/>
  <xsl:param name="ws" select="''"/>
  <xsl:param name="module"/> 
  <xsl:value-of select="$ws"/>
  <xsl:for-each select="return[1]/type">
    <xsl:call-template name="type-spec"/>
  </xsl:for-each>
  <xsl:text> (*PTR_</xsl:text> <xsl:value-of select="$id"/>
  <xsl:text>)(</xsl:text>
  <xsl:call-template name="param-type-list">
    <xsl:with-param name="with_hidden" select="'yes'"/>
  </xsl:call-template>
  <xsl:text>);</xsl:text>
  <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - defineSetFunctionPointers: define functions to set pointers to Fortran
  -                            procedures <scope>
  -->
<xsl:template name="defineSetFunctionPointers">
  <xsl:param name="module" select="@name"/>
  <xsl:text>/**</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text> * Functions to set pointers to Fortran procedures</xsl:text>
  <xsl:text> (called from Fortran)</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text> */</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>
  <xsl:for-each select="method">
    <xsl:call-template name="def-set-function-pointer">
      <xsl:with-param name="id">
        <xsl:value-of select="$module"/> <xsl:text>_</xsl:text>
        <xsl:value-of select="@name"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - def-set-function-pointer
  -   void SET_ function-id ( type-qual type-spec (*fptr) ( param-type-list ) )
  -      { PTR_ function-id = fptr; }
  -->
<xsl:template name="def-set-function-pointer">
  <xsl:param name="id"/>
  <xsl:param name="ws" select="''"/>
  <xsl:param name="module"/> 
  <xsl:value-of select="$ws"/>
  <xsl:text>void SET_</xsl:text> <xsl:value-of select="$id"/>
  <xsl:text>(</xsl:text>
  <xsl:for-each select="return[1]/type">
    <xsl:call-template name="type-spec"/>
  </xsl:for-each>
  <xsl:text> (*fptr)(</xsl:text>
  <xsl:call-template name="param-type-list">
    <xsl:with-param name="with_hidden" select="'yes'"/>
  </xsl:call-template>
  <xsl:text>))</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text>{</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text>  PTR_</xsl:text> <xsl:value-of select="$id"/>
  <xsl:text> = fptr;</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text>}</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - defineFunctions: define functions to call Fortran procedures <scope>
  -->
<xsl:template name="defineFunctions">
  <xsl:param name="module" select="@name"/>
  <xsl:text>/**</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text> * Bridging functions to call Fortran procedures</xsl:text>
  <xsl:value-of select="$nl"/>
  <xsl:text> */</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:value-of select="$nl"/>
  <xsl:for-each select="method">
    <xsl:call-template name="function-def">
      <xsl:with-param name="id">
        <xsl:value-of select="$module"/> <xsl:text>_</xsl:text>
        <xsl:value-of select="@name"/>
      </xsl:with-param>
      <xsl:with-param name="module" select="$module"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - defineMacros: define macros for Fortran symbols <scope>
  -->
<xsl:template name="defineInitProc">
  <xsl:param name="module" select="@name"/>
  <xsl:param name="symbol" select="''"/>
  <xsl:text>subroutine </xsl:text> <xsl:value-of select="$symbol"/>
  <xsl:text>()</xsl:text> <xsl:value-of select="$nl"/>
  <xsl:text>  use </xsl:text> <xsl:value-of select="$module"/>
  <xsl:value-of select="$nl"/>
  <xsl:for-each select="method">
    <xsl:text>  call SET_</xsl:text> <xsl:value-of select="$module"/>
    <xsl:text>_</xsl:text> <xsl:value-of select="@name"/>
    <xsl:text>( </xsl:text> <xsl:value-of select="@name"/>
    <xsl:text> )</xsl:text> <xsl:value-of select="$nl"/>
  </xsl:for-each>
  <xsl:text>end subroutine </xsl:text> <xsl:value-of select="$symbol"/>
  <xsl:value-of select="$nl"/>
</xsl:template>


<!--
  - decl-list-decl-rtn ([rtn_id=rtn-id, ws='  ']) <return/type>
  -->
<xsl:template name="decl-list-decl-rtn">
  <xsl:param name="rtn_id">
    <xsl:call-template name="rtn-id"/>
  </xsl:param>
  <xsl:param name="ws" select="'  '"/>
  <xsl:if test='string-length($rtn_id) &gt; 0'>
    <xsl:value-of select="$ws"/> <xsl:call-template name="type-spec"/>
    <xsl:text> </xsl:text> <xsl:value-of select="$rtn_id"/>
    <xsl:text>;</xsl:text> <xsl:value-of select="$nl"/>
  </xsl:if>
</xsl:template>


<!--
  - decl-list-decl-arg ([arg_id=arg-id, ws='  ']) <arg/type>
  -->
<xsl:template name="decl-list-decl-arg">
  <xsl:param name="arg_id">
    <xsl:call-template name="arg-id"/>
  </xsl:param>
  <xsl:param name="ws" select="'  '"/>
  <xsl:call-template name="decl-list-decl-arg-f90">
    <xsl:with-param name="arg_id" select="$arg_id"/>
    <xsl:with-param name="ws" select="$ws"/>
  </xsl:call-template>
</xsl:template>


<!--
  - decl-list-decl-arg-f90 ([arg_id=arg-id, ws='  ']) <arg/type>
  -->
<xsl:template name="decl-list-decl-arg-f90">
  <xsl:param name="arg_id">
    <xsl:call-template name="arg-id"/>
  </xsl:param>
  <xsl:param name="ws" select="'  '"/>
  <xsl:variable name="ext">
    <xsl:call-template name="type-conv-name-ext"/>
  </xsl:variable>
  <xsl:if test="$ext != ''">
    <xsl:value-of select="$ws"/>
    <xsl:call-template name="type-spec"/>
    <xsl:value-of select="concat(' ', ../@name, $ext, ';', $nl)"/>
  </xsl:if>
  <xsl:variable name="hidden">
    <xsl:call-template name="type-spec-hidden"/>
  </xsl:variable>
  <xsl:variable name="ext_h">
    <xsl:call-template name="type-conv-name-ext-hidden"/>
  </xsl:variable>
  <xsl:if test="$hidden != ''">
    <xsl:value-of select="concat($ws, $hidden, ' ', ../@name, $ext_h, ';')"/>
    <xsl:value-of select="($nl)"/>
  </xsl:if>
</xsl:template>


<!--
  - statement-list-pre-call <method>
  -->
<xsl:template name="statement-list-pre-call">
  <xsl:param name="ws" select="'  '"/>
  <xsl:for-each select="arg/type">
    <xsl:variable name="ext">
      <xsl:call-template name="type-conv-name-ext"/>
    </xsl:variable>
    <xsl:if test="$ext != ''">
      <xsl:call-template name="type-conv-statement">
        <xsl:with-param name="ws" select="$ws"/>
        <xsl:with-param name="arg_name" select="../@name"/>
        <xsl:with-param name="ext" select="$ext"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:for-each>
  <xsl:for-each select="arg/type">
    <xsl:call-template name="type-statement-hidden-pre-call"/>
  </xsl:for-each>
</xsl:template>


<!--
  - statement-list-post-call <method>
  -->
<xsl:template name="statement-list-post-call">
  <xsl:param name="ws" select="'  '"/>
  <xsl:for-each select="arg/type">
    <xsl:call-template name="type-statement-hidden-post-call"/>
  </xsl:for-each>
</xsl:template>


<!--
  - param-decl-hidden <arg>
  -   type-spec pointer param-id
  -->
<xsl:template name="param-decl-hidden">
  <xsl:param name="id">
    <xsl:call-template name="param-id"/>
  </xsl:param>

  <xsl:for-each select="type[1]">
    <xsl:call-template name="type-spec-hidden"/>
  </xsl:for-each>
  <xsl:if test="$id != ''">
    <xsl:value-of select="concat(' ', $id)"/>
  </xsl:if>

</xsl:template>


<!--
  - type-statement-hidden-pre-call <arg/type>
  -->
<xsl:template name="type-statement-hidden-pre-call">
  <xsl:param name="ws" select="'  '"/>
  <xsl:choose>
    <!-- Fortran types -->
    <xsl:when test="@kind = 'fchar'">
      <xsl:if test="@clen = '*'">
        <xsl:value-of select="concat($ws, ../@name)"/>
        <xsl:text>_h = strlen(</xsl:text>
        <xsl:value-of select="concat(../@name, ');', $nl)"/>
      </xsl:if>
    </xsl:when>
    <xsl:when test="@kind = 'farray'">
      <xsl:call-template name="type-statement-hidden-pre-call-farray">
        <xsl:with-param name="ws" select="$ws"/>
      </xsl:call-template>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<!--
  - type-statement-hidden-post-call <arg/type>
  -->
<xsl:template name="type-statement-hidden-post-call">
  <xsl:param name="ws" select="'  '"/>
  <xsl:choose>
    <!-- Fortran types -->
    <xsl:when test="@kind = 'farray'">
      <xsl:call-template name="type-statement-hidden-post-call-farray">
        <xsl:with-param name="ws" select="$ws"/>
      </xsl:call-template>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<!--
  - type-statement-hidden-pre-call-farray <arg/type>
  -->
<xsl:template name="type-statement-hidden-pre-call-farray">
  <xsl:param name="ws" select="'  '"/>
  <xsl:if test="array/dimension/@extent = '*'">
    <xsl:value-of select="$ws"/>
    <xsl:text>APPEND_F90_COMPILER(createArrayDescAndHidden) (</xsl:text>
    <xsl:value-of select="concat(../@name, ', ', array/@rank)"/>
    <xsl:text>, F90_Array, &amp;</xsl:text>
    <xsl:value-of select="concat(../@name, '_dv, &amp;', ../@name, '_dvh')"/>
    <xsl:text>);</xsl:text>
    <xsl:value-of select="$nl"/>
  </xsl:if>
</xsl:template>


<!--
  - type-statement-hidden-post-call-farray <arg/type>
  -->
<xsl:template name="type-statement-hidden-post-call-farray">
  <xsl:param name="ws" select="'  '"/>
  <xsl:variable name="ext">
    <xsl:call-template name="type-conv-name-ext-hidden"/>
  </xsl:variable>
  <xsl:if test="array/dimension/@extent = '*'">
    <xsl:value-of select="$ws"/>
    <xsl:text>APPEND_F90_COMPILER(freeArrayDescAndHidden) (</xsl:text>
    <xsl:value-of select="concat('F90_Array, ',../@name, '_dv, ')"/>
    <xsl:value-of select="concat(../@name, $ext, ');', $nl)"/>
  </xsl:if>
</xsl:template>


<!--
  - arg-list-hidden <method>
  -->
<xsl:template name="arg-list-hidden">
  <xsl:for-each select="arg/type">
    <xsl:variable name="hidden">
      <xsl:call-template name="type-spec-hidden"/>
    </xsl:variable>
    <xsl:variable name="ext">
      <xsl:call-template name="type-conv-name-ext-hidden"/>
    </xsl:variable>
    <xsl:if test="$hidden != ''">
      <xsl:value-of select="concat(', ', ../@name, $ext)"/>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<!--
  - type-conv-name-ext <arg/type>
  -->
<xsl:template name="type-conv-name-ext">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>
    <!-- Fortran types -->
    <xsl:when test="@kind = 'farray'">
      <xsl:if test="array/dimension/@extent = '*'">
        <xsl:text>_dv</xsl:text>
      </xsl:if>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<!--
  - type-conv-name-ext-hidden <arg/type>
  -->
<xsl:template name="type-conv-name-ext-hidden">
  <xsl:param name="depth" select="0"/>
  <xsl:choose>
    <!-- Fortran types -->
    <xsl:when test="@kind = 'fchar'">
      <xsl:if test="@clen = '*'">
        <xsl:text>_h</xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:when test="@kind = 'farray'">
      <xsl:if test="array/dimension/@extent = '*'">
        <xsl:text>_dvh</xsl:text>
      </xsl:if>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<!--
  - use-stmt-list <method>
  -->
<xsl:template name="use-stmt-list">
  <xsl:param name="ws" select="'  '"/>
  <xsl:for-each select="arg">
    <xsl:call-template name="use-stmt">
      <xsl:with-param name="ws" select="$ws"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - use-stmt <arg>
  -->
<xsl:template name="use-stmt">
  <xsl:param name="ws" select="'  '"/>
  <xsl:if test="type/@kind = 'usertype'">
    <xsl:value-of select="$ws"/>  <xsl:text>use </xsl:text>
    <xsl:value-of select="concat(type/@context, ', only : ')"/>
    <xsl:value-of select="concat(type/@usertype, $nl)"/>
  </xsl:if>
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
  <xsl:for-each select="type[1]">
    <xsl:call-template name="decl-type-spec"/>
  </xsl:for-each>

  <xsl:value-of select="concat(', intent(', @intent, ')')"/>
  <xsl:value-of select="concat(' :: ', @name, '_l', $nl)"/>

</xsl:template>


<!--
  - assign-stmt-list <method>
  -->
<xsl:template name="assign-stmt-list">
  <xsl:param name="ws" select="'  '"/>
  <xsl:for-each select="arg">
    <xsl:call-template name="assign-stmt">
      <xsl:with-param name="ws" select="$ws"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>


<!--
  - assign-stmt <arg>
  -->
<xsl:template name="assign-stmt">
  <xsl:param name="ws" select="'  '"/>
  <xsl:param name="lowercase_name">
    <xsl:call-template name="lower-case">
      <xsl:with-param name="symbol" select="@name"/>
    </xsl:call-template>
  </xsl:param>

  <xsl:value-of select="$ws"/>
  <xsl:value-of select="concat(@name, '_l = ', $lowercase_name)"/>
  <xsl:value-of select="$nl"/>

</xsl:template>


<!--
  - call-stmt <method>
  -   call-or-assign proc-designator ( act-arg-spec-list )
  -->
<xsl:template name="call-stmt">
  <xsl:param name="ws" select="'  '"/>

  <xsl:value-of select="$ws"/>
  <xsl:for-each select="return[1]/type">
    <xsl:call-template name="call-or-assign"/>
  </xsl:for-each>
  
  <xsl:call-template name="proc-designator"/>
  <xsl:text>(</xsl:text>
  <xsl:call-template name="act-arg-spec-list"/>
  <xsl:text>)</xsl:text>
  <xsl:value-of select="$nl"/>

</xsl:template>


<!--
  - call-or-assign <return/type>
  -->
<xsl:template name="call-or-assign">
  <xsl:param name="rtn_id">
    <xsl:call-template name="rtn-id"/>
  </xsl:param>

  <xsl:choose>
    <xsl:when test="@kind = 'fvoid'">
      <xsl:text>call </xsl:text>
    </xsl:when>
    <xsl:when test="$rtn_id != ''">
      <xsl:value-of select="$rtn_id"/>
      <xsl:text> = </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="../../@name"/>
      <xsl:text> = </xsl:text>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<!--
  - act-arg-spec-list <method>
  -->
<xsl:template name="act-arg-spec-list">
  <xsl:for-each select="arg">
    <xsl:value-of select="@name"/>
    <xsl:text>_l</xsl:text>
    <xsl:if test="position() != last()">
      <xsl:text>, </xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<!--
  - proc-designator <method>
  -->
<xsl:template name="proc-designator">
  <xsl:value-of select="@name"/>
</xsl:template>


</xsl:stylesheet>
