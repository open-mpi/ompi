# -*- shell-script ; indent-tabs-mode:nil -*-
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
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
# Copyright (c) 2014-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# define an internal function for checking the existence
# and validity of a PMI library
#
# OPAL_CHECK_PMI_LIB(installdir, libdir, pmi, function, [action-if-slurm], [action-if-valid], [action-if-not-valid])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI_LIB],
[
    # save flags
    opal_check_$3_save_CPPFLAGS=$CPPFLAGS
    opal_check_$3_save_LDFLAGS=$LDFLAGS
    opal_check_$3_save_LIBS=$LIBS
    opal_check_$3_hdr_happy=
    opal_check_$3_mycppflags=

    # check for the header
    AC_MSG_CHECKING([for $3.h in $1/include])
    AS_IF([test -f $1/include/$3.h],
          [AC_MSG_RESULT([found])
           opal_check_$3_mycppflags="-I$1/include"],
          [AC_MSG_RESULT([not found])
           AC_MSG_CHECKING([for $3.h in $1/include/slurm])
           AS_IF([test -f $1/include/slurm/$3.h],
                 [AC_MSG_RESULT([found])
                  opal_check_$3_mycppflags="-I$1/include/slurm"
                  $5],
                 [AC_MSG_RESULT([not found])
                  opal_check_$3_hdr_happy=no])])

    AS_IF([test "$opal_check_$3_hdr_happy" != "no"],
          [CPPFLAGS="$CPPFLAGS $opal_check_$3_mycppflags"
            AC_CHECK_HEADER([$3.h],
                            [opal_check_$3_hdr_happy=yes
                             $3_CPPFLAGS="$opal_check_$3_mycppflags"],
                            [opal_check_$3_hdr_happy=no])])

    # check for library and function
    opal_check_$3_lib_happy=
    LIBS="$LIBS -l$3"

    # check for the library in the given location in case
    # an exact path was given
    AC_MSG_CHECKING([for lib$3 in $2])
    files=`ls $2/lib$3.* 2> /dev/null | wc -l`
    AS_IF([test "$files" -gt "0"],
          [AC_MSG_RESULT([found])
           LDFLAGS="$LDFLAGS -L$2"
           AC_CHECK_LIB([$3], [$4],
                        [opal_check_$3_lib_happy=yes
                         $3_LDFLAGS=-L$2
                         $3_rpath=$2],
                        [opal_check_$3_lib_happy=no])],
          [opal_check_$3_lib_happy=no
           AC_MSG_RESULT([not found])])

    # check for presence of lib64 directory - if found, see if the
    # desired library is present and matches our build requirements
    files=`ls $2/lib64/lib$3.* 2> /dev/null | wc -l`
    AS_IF([test "$opal_check_$3_lib_happy" != "yes"],
          [AC_MSG_CHECKING([for lib$3 in $2/lib64])
           AS_IF([test "$files" -gt "0"],
                 [AC_MSG_RESULT([found])
                  LDFLAGS="$LDFLAGS -L$2/lib64"
                  AC_CHECK_LIB([$3], [$4],
                               [opal_check_$3_lib_happy=yes
                                $3_LDFLAGS=-L$2/lib64
                                $3_rpath=$2/lib64],
                               [opal_check_$3_lib_happy=no])],
                 [opal_check_$3_lib_happy=no
                  AC_MSG_RESULT([not found])])])


    # if we didn't find lib64, or the library wasn't present or correct,
    # then try a lib directory if present
    files=`ls $2/lib/lib$3.* 2> /dev/null | wc -l`
    AS_IF([test "$opal_check_$3_lib_happy" != "yes"],
          [AC_MSG_CHECKING([for lib$3 in $2/lib])
           AS_IF([test "$files" -gt "0"],
                 [AC_MSG_RESULT([found])
                  LDFLAGS="$LDFLAGS -L$2/lib"
                  AC_CHECK_LIB([$3], [$4],
                               [opal_check_$3_lib_happy=yes
                                $3_LDFLAGS=-L$2/lib
                                $3_rpath=$2/lib],
                               [opal_check_$3_lib_happy=no])],
                 [opal_check_$3_lib_happy=no
                  AC_MSG_RESULT([not found])])])

    # restore flags
    CPPFLAGS=$opal_check_$3_save_CPPFLAGS
    LDFLAGS=$opal_check_$3_save_LDFLAGS
    LIBS=$opal_check_$3_save_LIBS

    AS_IF([test "$opal_check_$3_hdr_happy" = "yes" && test "$opal_check_$3_lib_happy" = "yes"],
          [$6], [$7])

])

# OPAL_CHECK_PMI()
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI],[
    OPAL_VAR_SCOPE_PUSH([check_pmi_install_dir check_pmi_lib_dir default_pmi_loc default_pmi_libloc slurm_pmi_found])

    AC_ARG_WITH([pmi],
                [AC_HELP_STRING([--with-pmi(=DIR)],
                                [Build PMI support, optionally adding DIR to the search path (default: no)])],
                                [], with_pmi=no)

    AC_ARG_WITH([pmi-libdir],
                [AC_HELP_STRING([--with-pmi-libdir(=DIR)],
                                [Look for libpmi or libpmi2 in the given directory, DIR/lib or DIR/lib64])])

    check_pmi_install_dir=
    check_pmi_lib_dir=
    default_pmi_loc=
    default_pmi_libloc=
    slurm_pmi_found=

    AC_MSG_CHECKING([if user requested PMI support])
    AS_IF([test "$with_pmi" = "no"],
          [AC_MSG_RESULT([no])
           $3],
          [AC_MSG_RESULT([yes])
           # cannot use OPAL_CHECK_PACKAGE as its backend header
           # support appends "include" to the path, which won't
           # work with slurm :-(
           AS_IF([test ! -z "$with_pmi" && test "$with_pmi" != "yes"],
                 [check_pmi_install_dir=$with_pmi
                  default_pmi_loc=no],
                 [check_pmi_install_dir=/usr
                  default_pmi_loc=yes])
           AS_IF([test ! -z "$with_pmi_libdir"],
                 [check_pmi_lib_dir=$with_pmi_libdir
                  default_pmi_libloc=no],
                 [check_pmi_lib_dir=$check_pmi_install_dir
          AS_IF([test "$default_pmi_loc" = "no"],
                [default_pmi_libloc=no],
                [default_pmi_libloc=yes])])

           # check for pmi-1 lib */
           slurm_pmi_found=no
           OPAL_CHECK_PMI_LIB([$check_pmi_install_dir],
                              [$check_pmi_lib_dir],
                              [pmi], [PMI_Init],
                              [slurm_pmi_found=yes],
                              [opal_enable_pmi1=yes
                               opal_pmi1_LIBS="-lpmi"
                               AC_SUBST(opal_pmi1_LIBS)],
                              [opal_enable_pmi1=no])

           AS_IF([test "$opal_enable_pmi1" = "yes"],
                 [AS_IF([test "$default_pmi_loc" = "no" || test "$slurm_pmi_found" = "yes"],
                        [opal_pmi1_CPPFLAGS="$pmi_CPPFLAGS"
                         AC_SUBST(opal_pmi1_CPPFLAGS)])
                  AS_IF([test "$default_pmi_libloc" = "no" || test "$slurm_pmi_found" = "yes"],
                        [opal_pmi1_LDFLAGS="$pmi_LDFLAGS"
                         AC_SUBST(opal_pmi1_LDFLAGS)
                         opal_pmi1_rpath="$pmi_rpath"
                         AC_SUBST(opal_pmi1_rpath)])])

           # check for pmi2 lib */
           slurm_pmi_found=no
           OPAL_CHECK_PMI_LIB([$check_pmi_install_dir],
                              [$check_pmi_lib_dir],
                              [pmi2], [PMI2_Init],
                              [slurm_pmi_found=yes],
                              [opal_enable_pmi2=yes
                               opal_pmi2_LIBS="-lpmi2"
                               AC_SUBST(opal_pmi2_LIBS)],
                              [opal_enable_pmi2=no])

           AS_IF([test "$opal_enable_pmi2" = "yes"],
                 [AS_IF([test "$default_pmi_loc" = "no" || test "$slurm_pmi_found" = "yes"],
                        [opal_pmi2_CPPFLAGS="$pmi2_CPPFLAGS"
                         AC_SUBST(opal_pmi2_CPPFLAGS)])
                  AS_IF([test "$default_pmi_libloc" = "no" || test "$slurm_pmi_found" = "yes"],
                        [opal_pmi2_LDFLAGS="$pmi2_LDFLAGS"
                         AC_SUBST(opal_pmi2_LDFLAGS)
                         opal_pmi2_rpath="$pmi2_rpath"
                         AC_SUBST(opal_pmi2_rpath)])])

           # since support was explicitly requested, then we should error out
           # if we didn't find the required support
           AC_MSG_CHECKING([can PMI support be built])
           AS_IF([test "$opal_enable_pmi1" != "yes" && test "$opal_enable_pmi2" != "yes"],
                 [AC_MSG_RESULT([no])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but neither pmi.h])
                  AC_MSG_WARN([nor pmi2.h were found under locations:])
                  AC_MSG_WARN([    $check_pmi_install_dir])
                  AC_MSG_WARN([    $check_pmi_install_dir/slurm])
                  AC_MSG_WARN([Specified path: $with_pmi])
                  AC_MSG_WARN([OR neither libpmi nor libpmi2 were found under:])
                  AC_MSG_WARN([    $check_pmi_lib_dir/lib])
                  AC_MSG_WARN([    $check_pmi_lib_dir/lib64])
                  AC_MSG_WARN([Specified path: $with_pmi_libdir])
                  AC_MSG_ERROR([Aborting])],
                 [AC_MSG_RESULT([yes])])
           ])

    OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_CHECK_PMIX],[

    OPAL_VAR_SCOPE_PUSH([opal_external_pmix_save_CPPFLAGS opal_external_pmix_save_LDFLAGS opal_external_pmix_save_LIBS])

    AC_ARG_WITH([pmix],
                [AC_HELP_STRING([--with-pmix(=DIR)],
                                [Build PMIx support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of PMIx.  "external" forces Open MPI to use an external installation of PMIx.  Supplying a valid directory name also forces Open MPI to use an external installation of PMIx, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. Note that Open MPI does not support --without-pmix.])])

    AS_IF([test "$with_pmix" = "no"],
          [AC_MSG_WARN([Open MPI requires PMIx support. It can be built])
           AC_MSG_WARN([with either its own internal copy of PMIx, or with])
           AC_MSG_WARN([an external copy that you supply.])
           AC_MSG_ERROR([Cannot continue])])

    AC_MSG_CHECKING([if user requested external PMIx support($with_pmix)])
    AS_IF([test -z "$with_pmix" || test "$with_pmix" = "yes" || test "$with_pmix" = "internal"],
          [AC_MSG_RESULT([no])
           opal_external_pmix_happy=no],

          [AC_MSG_RESULT([yes])
           # check for external pmix lib */
           AS_IF([test "$with_pmix" = "external"],
                 [pmix_ext_install_dir=/usr],
                 [pmix_ext_install_dir=$with_pmix])

           # Make sure we have the headers and libs in the correct location
           OPAL_CHECK_WITHDIR([external-pmix], [$pmix_ext_install_dir/include], [pmix.h])
           OPAL_CHECK_WITHDIR([external-libpmix], [$pmix_ext_install_dir/lib], [libpmix.*])

           # check the version
           opal_external_pmix_save_CPPFLAGS=$CPPFLAGS
           opal_external_pmix_save_LDFLAGS=$LDFLAGS
           opal_external_pmix_save_LIBS=$LIBS

           # if the pmix_version.h file does not exist, then
           # this must be from a pre-1.1.5 version
           AC_MSG_CHECKING([PMIx version])
           CPPFLAGS="-I$pmix_ext_install_dir/include $CPPFLAGS"
           AS_IF([test "x`ls $pmix_ext_install_dir/include/pmix_version.h 2> /dev/null`" = "x"],
                 [AC_MSG_RESULT([version file not found - assuming v1.1.4])
                  opal_external_pmix_version_found=1
                  opal_external_pmix_version=114],
                 [AC_MSG_RESULT([version file found])
                  opal_external_pmix_version_found=0])

           # if it does exist, then we need to parse it to find
           # the actual release series
           AS_IF([test "$opal_external_pmix_version_found" = "0"],
                 [AC_MSG_CHECKING([version 3x])
                  AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                      #include <pmix_version.h>
                                                      #if (PMIX_VERSION_MAJOR != 3L)
                                                      #error "not version 3"
                                                      #endif
                                                      ], [])],
                                    [AC_MSG_RESULT([found])
                                     opal_external_pmix_version=3x
                                     opal_external_pmix_version_found=1],
                                    [AC_MSG_RESULT([not found])])])

           AS_IF([test "$opal_external_pmix_version_found" = "0"],
                 [AC_MSG_CHECKING([version 2x])
                  AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                      #include <pmix_version.h>
                                                      #if (PMIX_VERSION_MAJOR != 2L)
                                                      #error "not version 2"
                                                      #endif
                                                      ], [])],
                                    [AC_MSG_RESULT([found])
                                     opal_external_pmix_version=2x
                                     opal_external_pmix_version_found=1],
                                    [AC_MSG_RESULT([not found])])])

           AS_IF([test "$opal_external_pmix_version_found" = "0"],
                 [AC_MSG_CHECKING([version 1x])
                  AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                      #include <pmix_version.h>
                                                      #if (PMIX_VERSION_MAJOR != 1L)
                                                      #error "not version 1"
                                                      #endif
                                                      ], [])],
                                    [AC_MSG_RESULT([found])
                                     opal_external_pmix_version=1x
                                     opal_external_pmix_version_found=1],
                                    [AC_MSG_RESULT([not found])])])

           AS_IF([test "x$opal_external_pmix_version" = "x"],
                 [AC_MSG_WARN([External PMIx support requested, but version])
                  AC_MSG_WARN([information of the external lib could not])
                  AC_MSG_WARN([be detected])
                  AC_MSG_ERROR([cannot continue])])

           CPPFLAGS=$opal_external_pmix_save_CPPFLAGS
           LDFLAGS=$opal_external_pmix_save_LDFLAGS
           LIBS=$opal_external_pmix_save_LIBS

           opal_external_pmix_CPPFLAGS="-I$pmix_ext_install_dir/include"
           opal_external_pmix_LDFLAGS=-L$pmix_ext_install_dir/lib
           opal_external_pmix_LIBS=-lpmix
           opal_external_pmix_happy=yes])

    OPAL_VAR_SCOPE_POP
])
