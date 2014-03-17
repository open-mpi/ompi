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
# Copyright (c) 2014      Intel, Inc. All rights reserved.
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
                [AC_HELP_STRING([--with-pmi(=DIR)],
                                [Build PMI support, optionally adding DIR to the search path (default: no)])],
	                        [], with_pmi=no)

    opal_enable_pmi=0
    opal_pmi_rpath=
    opal_have_pmi2=0
    opal_have_pmi1=0

    # save flags
    opal_check_pmi_$1_save_CPPFLAGS="$CPPFLAGS"
    opal_check_pmi_$1_save_LDFLAGS="$LDFLAGS"
    opal_check_pmi_$1_save_LIBS="$LIBS"

    # set defaults
    opal_check_pmi_$1_LDFLAGS=
    opal_check_pmi_$1_CPPFLAGS=
    opal_check_pmi_$1_LIBS=

    AC_MSG_CHECKING([if user requested PMI support])
    opal_have_pmi_support=no
    AS_IF([test "$with_pmi" = "no"],
          [AC_MSG_RESULT([no])
           $3],
          [AC_MSG_RESULT([yes])
           AC_MSG_CHECKING([if PMI or PMI2 headers installed])
           # cannot use OMPI_CHECK_PACKAGE as its backend header
           # support appends "include" to the path, which won't
           # work with slurm :-(
           AS_IF([test ! -z "$with_pmi" -a "$with_pmi" != "yes"],
                 [AS_IF([test -d "$with_pmi/lib64"],
                        [opal_check_pmi_$1_LDFLAGS="-L$with_pmi/lib64"
                         opal_pmi_rpath="$with_pmi/lib64"],
                        [opal_check_pmi_$1_LDFLAGS="-L$with_pmi/lib"
                         opal_pmi_rpath="$with_pmi/lib"])
                  # look for required headers - both pmi.h AND/OR pmi2.h
                  # may be present
                  AS_IF([test -f "$with_pmi/include/pmi2.h" -o -f "$with_pmi/include/pmi.h"],
                        [opal_check_pmi_$1_CPPFLAGS="-I$with_pmi/include"
                         AS_IF([test -f "$with_pmi/include/pmi2.h"],
                                [AC_MSG_RESULT([PMI2 header found])],
                                [AC_MSG_RESULT([PMI header found])])],
                        [AS_IF([test -f "$with_pmi/include/slurm/pmi2.h" -o -f "$with_pmi/include/slurm/pmi.h"],
                               [opal_check_pmi_$1_CPPFLAGS="-I$with_pmi/include/slurm"
                                AS_IF([test -f "$with_pmi/include/slurm/pmi2.h"],
                                      [AC_MSG_RESULT([Slurm PMI2 headers found])],
                                      [AC_MSG_RESULT([Slurm PMI headers found])])],
                               [AC_MSG_RESULT([not found])
                                AC_MSG_WARN([PMI support requested (via --with-pmi) but neither pmi.h])
                                AC_MSG_WARN([nor pmi2.h were found under locations:])
                                AC_MSG_WARN([    $with_pmi/include])
                                AC_MSG_WARN([    $with_pmi/include/slurm])
                                AC_MSG_WARN([Specified path: $with_pmi])
                                AC_MSG_ERROR([Aborting])
                                $3])])],
                 [AS_IF([test -f "/usr/include/slurm/pmi2.h" -o -f "/usr/include/slurm/pmi.h"],
                        [opal_check_pmi_$1_CPPFLAGS="-I/usr/include/slurm"])
                  AC_MSG_RESULT([in default locations])])

           # setup to check libraries
           LDFLAGS="$LDFLAGS $opal_check_pmi_$1_LDFLAGS"
           CPPFLAGS="$CPPFLAGS $opal_check_pmi_$1_CPPFLAGS"
           # reset the included libs so we only link in the
           # ones we successfully check
           opal_check_pmi_$1_LIBS=
           # check the PMI libs - both -lpmi and -lpmi2 may
           # be present. If both are present, then we need
           # to link against both
           LIBS="$opal_check_pmi_$1_save_LIBS -lpmi2"
           AC_CHECK_LIB([pmi2], [PMI2_Init],
                        [opal_have_pmi_support=yes
                         opal_have_pmi2=1
                         opal_check_pmi_$1_LIBS="$opal_check_pmi_$1_LIBS -lpmi2"])
          # if the pmi2 functions aren't in -lpmi2, they might
          # be in -lpmi. Nobody follows a convention here, so
          # all we can do is check both
          AS_IF([test "$opal_have_pmi2" = "0"],
                [LIBS="$opal_check_pmi_$1_save_LIBS -lpmi"
                 AC_CHECK_LIB([pmi], [PMI2_Init],
                              [opal_have_pmi_support=yes
                               opal_have_pmi2=1
                               opal_have_pmi1=1
                               opal_check_pmi_$1_LIBS="$opal_check_pmi_$1_LIBS -lpmi"])])
          # if we haven't already added -lpmi, look for the pmi1 functions
          AS_IF([test "$opal_have_pmi1" = "0"],
                [LIBS="$opal_check_pmi_$1_save_LIBS -lpmi"
                 AC_CHECK_LIB([pmi], [PMI_Init],
                              [opal_have_pmi_support=yes
                               opal_check_pmi_$1_LIBS="$opal_check_pmi_$1_LIBS -lpmi"])])

           AC_MSG_CHECKING([PMI2 and/or PMI support enabled])
           AS_IF([test "$opal_have_pmi_support" = "yes"],
                 [AC_MSG_RESULT([yes])
                  opal_enable_pmi=1
                  $1_LDFLAGS="$opal_check_pmi_$1_LDFLAGS"
                  $1_CPPFLAGS="$opal_check_pmi_$1_CPPFLAGS"
                  $1_LIBS="$opal_check_pmi_$1_LIBS  -Wl,-rpath=$opal_pmi_rpath"
                  AC_MSG_CHECKING([final added libraries])
                  AC_MSG_RESULT([$opal_check_pmi_$1_LIBS])

                  $2],
                 [AC_MSG_RESULT([no])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but not found.])
                  AC_MSG_ERROR([Aborting.])
                  $3])
           ])

    # restore flags
    CPPFLAGS="$opal_check_pmi_$1_save_CPPFLAGS"
    LDFLAGS="$opal_check_pmi_$1_save_LDFLAGS"
    LIBS="$opal_check_pmi_$1_save_LIBS"

   AC_DEFINE_UNQUOTED([WANT_PMI_SUPPORT],
                      [$opal_enable_pmi],
                      [Whether we want PMI support])
   AC_DEFINE_UNQUOTED([WANT_PMI2_SUPPORT],
                      [$opal_have_pmi2],
                      [Whether we have PMI2 support])
   AM_CONDITIONAL(WANT_PMI_SUPPORT, [test "$opal_enable_pmi" = 1])
   AM_CONDITIONAL(WANT_PMI2_SUPPORT, [test "$opal_have_pmi2" = 1])
])
