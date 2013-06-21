# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_CHECK_PMI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI],[
    AC_ARG_WITH([pmi],
                [AC_HELP_STRING([--with-pmi],
                                [Build PMI support (default: no)])],
	                        [], with_pmi=no)

    opal_enable_pmi=0
    opal_use_pmi2=0
    opal_pmi_rpath=
    opal_have_slurm_pmi2=0

    # save flags
    opal_check_pmi_$1_save_CPPFLAGS="$CPPFLAGS"
    opal_check_pmi_$1_save_LDFLAGS="$LDFLAGS"
    opal_check_pmi_$1_save_LIBS="$LIBS"

    # set defaults
    opal_check_pmi_$1_LDFLAGS=
    opal_check_pmi_$1_CPPFLAGS=
    opal_check_pmi_$1_LIBS=

    AC_MSG_CHECKING([if user requested PMI support])
    AS_IF([test "$with_pmi" = "no"],
          [AC_MSG_RESULT([no])
           $3],
          [AC_MSG_RESULT([yes])
           AC_MSG_CHECKING([if PMI or PMI2 support installed])
           # cannot use OMPI_CHECK_PACKAGE as its backend header
           # support appends "include" to the path, which won't
           # work with slurm :-(
           AS_IF([test ! -z "$with_pmi" -a "$with_pmi" != "yes"],
                 [AS_IF([test -d "$with_pmi/lib64"],
                        [opal_check_pmi_$1_LDFLAGS="-L$with_pmi/lib64"
                         opal_pmi_rpath="$with_pmi/lib64"],
                        [opal_check_pmi_$1_LDFLAGS="-L$with_pmi/lib"
                         opal_pmi_rpath="$with_pmi/lib"])
                  # default to using PMI-2 if it is present
                  AS_IF([test -f "$with_pmi/include/pmi2.h" -o -f "$with_pmi/include/pmi.h"],
                        [opal_check_pmi_$1_CPPFLAGS="-I$with_pmi/include"
                         AS_IF([test -f "$with_pmi/include/pmi2.h"],
                                [opal_use_pmi2=1
                                 AC_MSG_RESULT([PMI2 support found])],
                                [opal_use_pmi2=0
                                 AC_MSG_RESULT([PMI support found])])],
                        [AS_IF([test -f "$with_pmi/include/slurm/pmi2.h" -o -f "$with_pmi/include/slurm/pmi.h"],
                               [opal_check_pmi_$1_CPPFLAGS="-I$with_pmi/include/slurm"
                                AS_IF([test -f "$with_pmi/include/slurm/pmi2.h"],
                                      [opal_use_pmi2=1
                                       opal_have_slurm_pmi2=1
                                       AC_MSG_RESULT([Slurm PMI2 support found])],
                                      [opal_use_pmi2=0
                                       AC_MSG_RESULT([Slurm PMI support found])])],
                               [AC_MSG_RESULT([not found])
                                AC_MSG_WARN([PMI support requested (via --with-pmi) but neither pmi.h])
                                AC_MSG_WARN([nor pmi2.h were found under locations:])
                                AC_MSG_WARN([    $with_pmi/include])
                                AC_MSG_WARN([    $with_pmi/include/slurm])
                                AC_MSG_WARN([Specified path: $with_pmi])
                                AC_MSG_ERROR([Aborting])
                                $3])])])

           AS_IF([test $opal_use_pmi2 = 1],
                 [AS_IF([test $opal_have_slurm_pmi2 = 1],
                        [ # slurm puts pmi2 into a separate lib
                         opal_check_pmi_$1_LIBS="-lpmi2 -lpmi -Wl,-rpath=$opal_pmi_rpath"],
                        [opal_check_pmi_$1_LIBS="-lpmi -Wl,-rpath=$opal_pmi_rpath"])],
                 [opal_check_pmi_$1_LIBS="-lpmi -Wl,-rpath=$opal_pmi_rpath"])

           LDFLAGS="$LDFLAGS $opal_check_pmi_$1_LDFLAGS"
           CPPFLAGS="$CPPFLAGS $opal_check_pmi_$1_CPPFLAGS"
           LIBS="$LIBS $opal_check_pmi_$1_LIBS"
           opal_have_pmi_support=no
           AS_IF([test "$opal_use_pmi2" = "1"],
                 [AC_CHECK_HEADERS([pmi2.h],
                                   [AC_CHECK_LIB([pmi2], [PMI2_Init],
                                   [opal_have_pmi_support=yes])])],
                 [AC_CHECK_HEADERS([pmi.h],
                                   [AC_CHECK_LIB([pmi], [PMI_Init],
                                   [opal_have_pmi_support=yes])])])

           AC_MSG_CHECKING([PMI2 or PMI support enabled])
           AS_IF([test "$opal_have_pmi_support" = "yes"],
                 [AC_MSG_RESULT([yes])
                  opal_enable_pmi=1
                  $1_LDFLAGS="$opal_check_pmi_$1_LDFLAGS"
                  $1_CPPFLAGS="$opal_check_pmi_$1_CPPFLAGS"
                  $1_LIBS="-lpmi"
                  $2],
                 [AC_MSG_RESULT([no])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but not found.])
                  AC_MSG_ERROR([Aborting.])
                  $3])
           ])

    # restore flags - have to add CPPFLAGS so base functions can find pmi.h
    CPPFLAGS="$opal_check_pmi_$1_save_CPPFLAGS $opal_check_pmi_$1_CPPFLAGS"
    LDFLAGS="$opal_check_pmi_$1_save_LDFLAGS"
    LIBS="$opal_check_pmi_$1_save_LIBS"

   AC_DEFINE_UNQUOTED([WANT_PMI_SUPPORT],
                      [$opal_enable_pmi],
                      [Whether we want PMI support])
   AC_DEFINE_UNQUOTED([WANT_PMI2_SUPPORT],
                      [$opal_use_pmi2],
                      [Whether we want to use PMI2])
   AM_CONDITIONAL(WANT_PMI_SUPPORT, [test "$opal_enable_pmi" = 1])
])
