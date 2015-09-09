dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2013      Intel, Inc. All rights reserved
dnl Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# PMIX_C_COMPILER_VENDOR(VENDOR_VARIABLE)
# ---------------------------------------
# Set shell variable VENDOR_VARIABLE to the name of the compiler
# vendor for the current C compiler.
#
# See comment for _PMIX_CHECK_COMPILER_VENDOR for a complete
# list of currently detected compilers.
AC_DEFUN([PMIX_C_COMPILER_VENDOR], [
    AC_REQUIRE([AC_PROG_CC])

    AC_CACHE_CHECK([for the C compiler vendor],
        [pmix_cv_c_compiler_vendor],
        [AC_LANG_PUSH(C)
         _PMIX_CHECK_COMPILER_VENDOR([pmix_cv_c_compiler_vendor])
         AC_LANG_POP(C)])

    $1="$pmix_cv_c_compiler_vendor"
])


# workaround to avoid syntax error with Autoconf < 2.68:
m4_ifndef([AC_LANG_DEFINES_PROVIDED],
	  [m4_define([AC_LANG_DEFINES_PROVIDED])])

# PMIX_IFDEF_IFELSE(symbol, [action-if-defined],
#                   [action-if-not-defined])
# ----------------------------------------------
# Run compiler to determine if preprocessor symbol "symbol" is
# defined by the compiler.
AC_DEFUN([PMIX_IFDEF_IFELSE], [
    AC_COMPILE_IFELSE([AC_LANG_DEFINES_PROVIDED
#ifndef $1
#error "symbol $1 not defined"
choke me
#endif], [$2], [$3])])


# PMIX_IF_IFELSE(symbol, [action-if-defined],
#                [action-if-not-defined])
# ----------------------------------------------
# Run compiler to determine if preprocessor symbol "symbol" is
# defined by the compiler.
AC_DEFUN([PMIX_IF_IFELSE], [
    AC_COMPILE_IFELSE([AC_LANG_DEFINES_PROVIDED
#if !( $1 )
#error "condition $1 not met"
choke me
#endif], [$2], [$3])])


# _PMIX_CHECK_COMPILER_VENDOR(VENDOR_VARIABLE)
# --------------------------------------------
# Set shell variable VENDOR_VARIABLE to the name of the compiler
# vendor for the compiler for the current language.  Language must be
# one of C, OBJC, or C++.
#
# thanks to http://predef.sourceforge.net/precomp.html for the list
# of defines to check.
AC_DEFUN([_PMIX_CHECK_COMPILER_VENDOR], [
    pmix_check_compiler_vendor_result="unknown"

    # GNU is probably the most common, so check that one as soon as
    # possible.  Intel pretends to be GNU, so need to check Intel
    # before checking for GNU.

    # Intel
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__INTEL_COMPILER) || defined(__ICC)],
               [pmix_check_compiler_vendor_result="intel"])])

    # Fujitsu
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__FUJITSU)], 
               [pmix_check_compiler_vendor_result="fujitsu"])])

    # GNU
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__GNUC__],
               [pmix_check_compiler_vendor_result="gnu"

               # We do not support gccfss as a compiler so die if
               # someone tries to use said compiler.  gccfss (gcc
               # for SPARC Systems) is a compiler that is no longer
               # supported by Oracle and it has some major flaws
               # that prevents it from actually compiling PMIX code.
               # So if we detect it we automatically bail.

               if ($CC --version | grep gccfss) >/dev/null 2>&1; then
                   AC_MSG_RESULT([gccfss])
                   AC_MSG_WARN([Detected gccfss being used to compile PMIx.])
                   AC_MSG_WARN([Because of several issues PMIx does not support])
                   AC_MSG_WARN([the gccfss compiler.  Please use a different compiler.])
                   AC_MSG_WARN([If you didn't think you used gccfss you may want to])
                   AC_MSG_WARN([check to see if the compiler you think you used is])
                   AC_MSG_WARN([actually a link to gccfss.])
                   AC_MSG_ERROR([Cannot continue])
               fi])])

    # Borland Turbo C
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__TURBOC__],
               [pmix_check_compiler_vendor_result="borland"])])

    # Borland C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__BORLANDC__],
               [pmix_check_compiler_vendor_result="borland"])])

    # Comeau C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__COMO__],
               [pmix_check_compiler_vendor_result="comeau"])])

    # Compaq C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__DECC) || defined(VAXC) || defined(__VAXC)],
               [pmix_check_compiler_vendor_result="compaq"],
               [PMIX_IF_IFELSE([defined(__osf__) && defined(__LANGUAGE_C__)],
                    [pmix_check_compiler_vendor_result="compaq"],
                    [PMIX_IFDEF_IFELSE([__DECCXX],
                         [pmix_check_compiler_vendor_result="compaq"])])])])

    # Cray C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([_CRAYC],
               [pmix_check_compiler_vendor_result="cray"])])

    # Diab C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__DCC__],
               [pmix_check_compiler_vendor_result="diab"])])

    # Digital Mars
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__DMC__) || defined(__SC__) || defined(__ZTC__)],
               [pmix_check_compiler_vendor_result="digital mars"])])

    # HP ANSI C / aC++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__HP_cc) || defined(__HP_aCC)],
               [pmix_check_compiler_vendor_result="hp"])])

    # IBM XL C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__xlC__) || defined(__IBMC__) || defined(__IBMCPP__)],
               [pmix_check_compiler_vendor_result="ibm"],
               [PMIX_IF_IFELSE([defined(_AIX) && !defined(__GNUC__)],
                    [pmix_check_compiler_vendor_result="ibm"])])])

    # KAI C++ (rest in peace)
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__KCC],
               [pmix_check_compiler_vendor_result="kai"])])

    # LCC
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__LCC__],
               [pmix_check_compiler_vendor_result="lcc"])])

    # MetaWare High C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__HIGHC__],
               [pmix_check_compiler_vendor_result="metaware high"])])

    # Metrowerks Codewarrior
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__MWERKS__],
               [pmix_check_compiler_vendor_result="metrowerks"])])

    # MIPSpro (SGI)
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(sgi) || defined(__sgi)],
               [pmix_check_compiler_vendor_result="sgi"])])

    # MPW C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__MRC__) || defined(MPW_C) || defined(MPW_CPLUS)],
               [pmix_check_compiler_vendor_result="mpw"])])

    # Norcroft C
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__CC_NORCROFT],
               [pmix_check_compiler_vendor_result="norcroft"])])

    # Pelles C
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__POCC__],
               [pmix_check_compiler_vendor_result="pelles"])])

    # Portland Group
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__PGI],
               [pmix_check_compiler_vendor_result="portland group"])])

    # SAS/C
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(SASC) || defined(__SASC) || defined(__SASC__)],
               [pmix_check_compiler_vendor_result="sas"])])

    # Sun Workshop C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IF_IFELSE([defined(__SUNPRO_C) || defined(__SUNPRO_CC)],
               [pmix_check_compiler_vendor_result="sun"])])

    # TenDRA C/C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__TenDRA__],
               [pmix_check_compiler_vendor_result="tendra"])])

    # Tiny C
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__TINYC__],
               [pmix_check_compiler_vendor_result="tiny"])])

    # USL C
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__USLC__],
               [pmix_check_compiler_vendor_result="usl"])])

    # Watcom C++
    AS_IF([test "$pmix_check_compiler_vendor_result" = "unknown"],
          [PMIX_IFDEF_IFELSE([__WATCOMC__],
               [pmix_check_compiler_vendor_result="watcom"])])

    $1="$pmix_check_compiler_vendor_result"
    unset pmix_check_compiler_vendor_result
])
