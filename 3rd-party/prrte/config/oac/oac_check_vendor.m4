dnl -*- autoconf -*-
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
dnl Copyright (c) 2014      Intel, Inc. All rights reserved
dnl Copyright (c) 2017-2021 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


dnl OAC_C_COMPILER_VENDOR: Determine compiler vendor
dnl
dnl <no arguments>
dnl
dnl Sets oac_cv_c_compiler_vendor shell variable to a string name of
dnl the in use C compiler.
AC_DEFUN([OAC_C_COMPILER_VENDOR], [
    AC_REQUIRE([AC_PROG_CC])
    AC_CACHE_CHECK([for the C compiler vendor],
        [oac_cv_c_compiler_vendor],
        [_OAC_CHECK_COMPILER_VENDOR()])
])


dnl workaround to avoid syntax error with Autoconf < 2.68:
m4_ifndef([AC_LANG_DEFINES_PROVIDED],
          [m4_define([AC_LANG_DEFINES_PROVIDED])])


dnl 1 -> symbol
dnl 2 -> [action-if-defined],
dnl 3 -> [action-if-not-defined])
dnl
dnl Run compiler to determine if preprocessor symbol "symbol" is
dnl defined by the compiler.
AC_DEFUN([_OAC_COMPILER_VENDOR_IF_IFELSE], [
    AC_COMPILE_IFELSE([AC_LANG_DEFINES_PROVIDED
#if !( $1 )
#error "condition $1 not met"
choke me
#endif], [$2], [$3])])


dnl <no arguments>
dnl
dnl Sets oac_cv_c_compiler_vendor to the detected compiler vendor.
dnl
dnl thanks to http://predef.sourceforge.net/precomp.html for the list
dnl of defines to check.
AC_DEFUN([_OAC_CHECK_COMPILER_VENDOR], [
    AC_LANG_PUSH(C)
    oac_cv_c_compiler_vendor="unknown"
    dnl Check GCC last, despite it being most common, because everyone
    dnl pretends to be GCC.

    # Intel
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__INTEL_COMPILER) || defined(__ICC)],
               [oac_cv_c_compiler_vendor="intel"])])

    # Portland Group
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__PGI)],
               [oac_cv_c_compiler_vendor="portland group"])])

    # Fujitsu
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__FUJITSU)],
               [oac_cv_c_compiler_vendor="fujitsu"])])

    # Clang
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__clang__)],
               [oac_cv_c_compiler_vendor="clang"])])

    # IBM XL C/C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__xlC__) || defined(__IBMC__) || defined(__IBMCPP__) || defined(__ibmxl__)],
               [oac_cv_c_compiler_vendor="ibm"],
               [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(_AIX) && !defined(__GNUC__)],
                    [oac_cv_c_compiler_vendor="ibm"])])])

    # Compaq C/C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__DECC) || defined(VAXC) || defined(__VAXC)],
               [oac_cv_c_compiler_vendor="compaq"],
               [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__osf__) && defined(__LANGUAGE_C__)],
                    [oac_cv_c_compiler_vendor="compaq"],
                    [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__DECCXX)],
                         [oac_cv_c_compiler_vendor="compaq"])])])])

    # Cray C/C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(_CRAYC)],
               [oac_cv_c_compiler_vendor="cray"])])

    # Diab C/C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__DCC__)],
               [oac_cv_c_compiler_vendor="diab"])])

    # HP ANSI C / aC++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__HP_cc) || defined(__HP_aCC)],
               [oac_cv_c_compiler_vendor="hp"])])

    # KAI C++ (rest in peace)
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__KCC)],
               [oac_cv_c_compiler_vendor="kai"])])

    # LCC
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__LCC__)],
               [oac_cv_c_compiler_vendor="lcc"])])

    # Metrowerks Codewarrior
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__MWERKS__)],
               [oac_cv_c_compiler_vendor="metrowerks"])])

    # MIPSpro (SGI)
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(sgi) || defined(__sgi)],
               [oac_cv_c_compiler_vendor="sgi"])])

    # Microsoft
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(_MSC_VER) || defined(__MSC_VER)],
               [oac_cv_c_compiler_vendor="microsoft"])])

    # Norcroft C
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__CC_NORCROFT)],
               [oac_cv_c_compiler_vendor="norcroft"])])

    # SAS/C
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(SASC) || defined(__SASC) || defined(__SASC__)],
               [oac_cv_c_compiler_vendor="sas"])])

    # Sun Workshop C/C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__SUNPRO_C) || defined(__SUNPRO_CC)],
               [oac_cv_c_compiler_vendor="sun"])])

    # TenDRA C/C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__TenDRA__)],
               [oac_cv_c_compiler_vendor="tendra"])])

    # Tiny C
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__TINYC__)],
               [oac_cv_c_compiler_vendor="tiny"])])

    # USL C
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__USLC__)],
               [oac_cv_c_compiler_vendor="usl"])])

    # Watcom C++
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__WATCOMC__)],
               [oac_cv_c_compiler_vendor="watcom"])])

    # GNU
    AS_IF([test "$oac_cv_c_compiler_vendor" = "unknown"],
          [_OAC_COMPILER_VENDOR_IF_IFELSE([defined(__GNUC__)],
               [oac_cv_c_compiler_vendor="gnu"
                # gccfss (gcc for SPARC Systems) is a compiler that is
                # no longer supported by Oracle.
                AS_IF([($CC --version | grep gccfss) >/dev/null 2>&1],
                      [oac_cv_c_compiler_vendor="gccfss"])])])
     AC_LANG_POP(C)])
])
