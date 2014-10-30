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
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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
    opal_check_pmi_install_dir=
    opal_default_loc=0
    opal_pmi_added_cppflag=no
    opal_pmi_added_ldflag=no

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
           AC_MSG_CHECKING([if PMI installed])
           # cannot use OPAL_CHECK_PACKAGE as its backend header
           # support appends "include" to the path, which won't
           # work with slurm :-(
           AS_IF([test ! -z "$with_pmi" && test "$with_pmi" != "yes"],
                 [opal_check_pmi_install_dir=$with_pmi
                  opal_default_loc="no"],
                 [opal_check_pmi_install_dir="/usr"
                  opal_default_loc="yes"])
           # check for pmi-1 lib */
           files=`ls -U $opal_check_pmi_install_dir/lib64/libpmi.* 2> /dev/null | wc -l`
           AS_IF([test "$files" -gt "0"],
                 [opal_have_pmi1=1
                  AS_IF([test "$opal_default_loc" = "no"],
                        [opal_check_pmi_$1_LDFLAGS="-L$opal_check_pmi_install_dir/lib64"
                         opal_pmi_rpath="$opal_check_pmi_install_dir/lib64"
                         opal_pmi_added_ldflag=yes])
                  opal_check_pmi_$1_LIBS="-lpmi"],
                 [files=`ls -U $opal_check_pmi_install_dir/lib/libpmi.* 2> /dev/null | wc -l`
                  AS_IF([test "$files" -gt "0"],
                        [opal_have_pmi1=1
                         AS_IF([test "$opal_default_loc" = "no"],
                               [opal_check_pmi_$1_LDFLAGS="-L$opal_check_pmi_install_dir/lib"
                                opal_pmi_rpath="$opal_check_pmi_install_dir/lib"
                                opal_pmi_added_ldflag=yes])
                         opal_check_pmi_$1_LIBS="-lpmi"])])
           # check for pmi.h
           AS_IF([test -f "$opal_check_pmi_install_dir/include/pmi.h"],
               [AS_IF([test "$opal_default_loc" = "no"],
                      [opal_check_pmi_$1_CPPFLAGS="-I$opal_check_pmi_install_dir/include"
                       opal_pmi_added_cppflag=yes])],
               # this could be SLURM, which puts things in a different location
               [AS_IF([test -f "$opal_check_pmi_install_dir/include/slurm/pmi.h"],
                       # even if this was the default loc, we still need to add it in
                       # because of the slurm path addition
                      [opal_check_pmi_$1_CPPFLAGS="-I$opal_check_pmi_install_dir/include/slurm"
                       opal_pmi_added_cppflag=yes])])

           # check for pmi2 lib */
           files=`ls -U $opal_check_pmi_install_dir/lib64/libpmi2.* 2> /dev/null | wc -l`
           AS_IF([test "$files" -gt "0"],
                 [opal_have_pmi2=1
                  AS_IF([test "$opal_pmi_added_ldflag" != "yes" && test "$opal_default_loc" = "no"],
                        [opal_check_pmi_$1_LDFLAGS="-L$opal_check_pmi_install_dir/lib64"
                         opal_pmi_rpath="$opal_check_pmi_install_dir/lib64"])
                  opal_check_pmi_$1_LIBS="$opal_check_pmi_$1_LIBS -lpmi2"],
                 [files=`ls -U $opal_check_pmi_install_dir/lib/libpmi2.* 2> /dev/null | wc -l`
                  AS_IF([test "$files" -gt "0"],
                        [opal_have_pmi2=1
                         AS_IF([test "$opal_pmi_added_ldflag" != "yes" && test "$opal_default_loc" = "no"],
                               [opal_check_pmi_$1_LDFLAGS="-L$opal_check_pmi_install_dir/lib"
                                opal_pmi_rpath="$opal_check_pmi_install_dir/lib"])
                         opal_check_pmi_$1_LIBS="$opal_check_pmi_$1_LIBS -lpmi2"])])
           # check for pmi2.h
           AS_IF([test "$opal_pmi_added_cppflag" = "no"],
                 [AS_IF([test -f "$opal_check_pmi_install_dir/include/pmi2.h"],
                        [AS_IF([test "$opal_default_loc" = "no"],
                               [opal_check_pmi_$1_CPPFLAGS="-I$opal_check_pmi_install_dir/include"])],
                        # this could be SLURM, which puts things in a different location
                        [AS_IF([test -f "$opal_check_pmi_install_dir/include/slurm/pmi2.h"],
                               # even if this was the default loc, we still need to add it in
                               # because of the slurm path addition
                               [opal_check_pmi_$1_CPPFLAGS="-I$opal_check_pmi_install_dir/include/slurm"])])])

           # since support was explicitly requested, then we should error out
           # if we didn't find the required support
           AS_IF([test "$opal_have_pmi1" != "1" && test "$opal_have_pmi2" != "1"],
                 [AC_MSG_RESULT([not found])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but neither libpmi])
                  AC_MSG_WARN([nor libpmi2 were found under locations:])
                  AC_MSG_WARN([    $opal_check_pmi_install_dir/lib])
                  AC_MSG_WARN([    $opal_check_pmi_install_dir/lib64])
                  AC_MSG_WARN([Specified path: $with_pmi])
                  AC_MSG_ERROR([Aborting])
                  $3],
                 [AC_MSG_RESULT([yes])
                  opal_enable_pmi=1
                  $1_LDFLAGS="$opal_check_pmi_$1_LDFLAGS"
                  $1_CPPFLAGS="$opal_check_pmi_$1_CPPFLAGS"
                  $1_LIBS="$opal_check_pmi_$1_LIBS  -Wl,-rpath=$opal_pmi_rpath"
                  AC_MSG_CHECKING([final added libraries])
                  AC_MSG_RESULT([$opal_check_pmi_$1_LIBS])
                  $2])
           ])

    AC_DEFINE_UNQUOTED([WANT_PMI_SUPPORT],
                       [$opal_enable_pmi],
                       [Whether we want PMI support])
    AC_DEFINE_UNQUOTED([WANT_PMI2_SUPPORT],
                       [$opal_have_pmi2],
                       [Whether we have PMI2 support])
    AM_CONDITIONAL(WANT_PMI_SUPPORT, [test "$opal_enable_pmi" = "1"])
    AM_CONDITIONAL(WANT_PMI2_SUPPORT, [test "$opal_have_pmi2" = "1"])

    # restore flags
    CPPFLAGS="$opal_check_pmi_$1_save_CPPFLAGS"
    LDFLAGS="$opal_check_pmi_$1_save_LDFLAGS"
    LIBS="$opal_check_pmi_$1_save_LIBS"
])

#
# special check for cray pmi, uses macro(s) from pkg.m4
#
# OPAL_CHECK_CRAY_PMI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_CRAY_PMI],[
    if test -z "$opal_check_cray_pmi_happy" ; then
        if test -f "/usr/lib/alps/libalps.a" ; then
            using_cle5_install="no"
        else
            using_cle5_install="yes"
        fi

        if test "$using_cle5_install" = "no" ; then
            OPAL_CHECK_PMI([CRAY_PMI], [opal_check_cray_pmi_happy="yes"],
                [opal_check_cray_pmi_happy="no"])

            if test "$opal_check_cray_pmi_happy" = "yes" ; then
                # Check for extra libraries required by Cray PMI
                opal_check_alps_pmi_alps_happy=no
                for opal_check_cray_pmi_extra_dir in "/usr/lib/alps" "/opt/cray/xe-sysroot/default/usr/lib/alps" ; do
                    AC_MSG_CHECKING([for alps libraries required by PMI in "$opal_check_cray_pmi_extra_dir"])

                    # libalpslli and libalpsutil are needed by libpmi to compile statically
                    if test -f "$opal_check_cray_pmi_extra_dir/libalpslli.a" ; then
                        if test -f "$opal_check_cray_pmi_extra_dir/libalpsutil.a" ; then
                            opal_check_alps_pmi_alps_happy=yes
                            CRAY_PMI_LDFLAGS="$CRAY_PMI_LDFLAGS -L$opal_check_cray_pmi_extra_dir"
                            CRAY_PMI_LIBS="$CRAY_PMI_LIBS -lalpslli -lalpsutil"
                            AC_MSG_RESULT([found])
                            break
                        fi
                    fi
                    AC_MSG_RESULT([not found])
                done

                if test "$opal_check_alps_pmi_alps_happy" = "no" ; then
                    opal_check_cray_pmi_alps_happy="no"
                fi
            fi
        else
            # CLE5 uses package config
            PKG_CHECK_MODULES([CRAY_PMI], [cray-pmi],
                [opal_check_cray_pmi_happy="yes"],
                [opal_check_cray_pmi_happy="no"])
        fi
    fi

    AS_IF([test "$opal_check_cray_pmi_happy" = "yes"],
        [$1_LDFLAGS="$CRAY_PMI_LDFLAGS"
            $1_CPPFLAGS="$CRAY_PMI_CPPFLAGS"
            $1_LIBS="$CRAY_PMI_LIBS"
            $2], [$3])
])
