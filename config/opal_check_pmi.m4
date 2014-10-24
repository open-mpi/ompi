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

# define an internal function for checking the existence
# and validity of a PMI library
#
# OPAL_CHECK_PMI_LIB(installdir, pmi, function, [action-if-valid], [action-if-not-valid])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI_LIB],
[
    OPAL_VAR_SCOPE_PUSH([save_LDFLAGS save_CPPFLAGS save_LIBS hdr_happy lib_happy files mycppflags])

    # save flags
    save_CPPFLAGS="$CPPFLAGS"
    save_LDFLAGS="$LDFLAGS"
    save_LIBS="$LIBS"
    hdr_happy=
    mycppflags=

    # check for the header
    AC_MSG_CHECKING([for $2.h in $1/include])
    AS_IF([test -f $1/include/$2.h],
          [AC_MSG_RESULT([found])
           mycppflags="-I$1/include"],
          [AC_MSG_RESULT([not found])
           AC_MSG_CHECKING([for $2.h in $1/include/slurm])
           AS_IF([test -f $1/include/slurm/$2.h],
                 [AC_MSG_RESULT([found])
                  mycppflags="-I$1/include/slurm"],
                 [AC_MSG_RESULT([not found])
                  hdr_happy=no])])

    AS_IF([test "$hdr_happy" != "no"],
          [CPPFLAGS="$CPPFLAGS $mycppflags"
            AC_CHECK_HEADER([$2.h],
                            [hdr_happy=yes
                             $2_CPPFLAGS="$mycppflags"],
                            [hdr_happy=no])])

    # check for presence of lib64 directory - if found, see if the
    # desired library is present and matches our build requirements
    lib_happy=
    LIBS="$LIBS -l$2"
    AC_MSG_CHECKING([for lib$2 in $1/lib64])
    files=`ls $1/lib64/lib$2.* 2> /dev/null | wc -l`
    AS_IF([test "$files" -gt "0"],
          [AC_MSG_RESULT([found])
           LDFLAGS="$LDFLAGS -L$1/lib64"
           AC_CHECK_LIB([$2], [$3],
                        [lib_happy=yes.
                         $2_LDFLAGS="-L$1/lib64"
                         $2_rpath="$1/lib64"],
                        [lib_happy=no])],
           [AC_MSG_RESULT([not found])])
           

    # if we didn't find lib64, or the library wasn't present or correct,
    # then try a lib directory if present
    files=`ls $1/lib/lib$2.* 2> /dev/null | wc -l`
    AS_IF([test "$lib_happy" != "yes"],
          [AC_MSG_CHECKING([for lib$2 in $1/lib])
           AS_IF([test "$files" -gt "0"],
                 [AC_MSG_RESULT([found])
                  LDFLAGS="$LDFLAGS -L$1/lib"
                  AC_CHECK_LIB([$2], [$3],
                               [lib_happy=yes
                                $2_LDFLAGS="-L$1/lib"
                                $2_rpath="$1/lib"],
                               [lib_happy=no])],
                 [lib_happy=no
                  AC_MSG_RESULT([not found])])])

    # restore flags
    CPPFLAGS="$save_CPPFLAGS"
    LDFLAGS="$save_LDFLAGS"
    LIBS="$save_LIBS"

    AS_IF([test "$hdr_happy" = "yes" && test "$lib_happy" = "yes"],
          [$4], [$5])

    OPAL_VAR_SCOPE_POP
])

# OPAL_CHECK_PMI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI],[

    OPAL_VAR_SCOPE_PUSH([check_install_dir have_pmi1 have_pmi2 added_flags default_loc have_rpath local_libs])

    AC_ARG_WITH([pmi],
                [AC_HELP_STRING([--with-pmi(=DIR)],
                                [Build PMI support, optionally adding DIR to the search path (default: no)])],
                                [], with_pmi=no)

    opal_enable_pmi=0
    have_pmi2=no
    have_pmi1=no
    check_install_dir=
    added_flags=no
    $1_LIBS=
    have_rpath=
    local_libs=

    AC_MSG_CHECKING([if user requested PMI support])
    AS_IF([test "$with_pmi" = "no"],
          [AC_MSG_RESULT([no])
           $3],
          [AC_MSG_RESULT([yes])
           # cannot use OPAL_CHECK_PACKAGE as its backend header
           # support appends "include" to the path, which won't
           # work with slurm :-(
           AS_IF([test ! -z "$with_pmi" && test "$with_pmi" != "yes"],
                 [check_install_dir=$with_pmi
                  default_loc="no"],
                 [check_install_dir="/usr"
                  default_loc="yes"])

           # check for pmi-1 lib */
           OPAL_CHECK_PMI_LIB([$check_install_dir],
                              [pmi], [PMI_Init],
                              [have_pmi1=yes
                               AS_IF([test "$default_loc" = "no"],
                                     [$1_CPPFLAGS="$pmi_CPPFLAGS"
                                      $1_LDFLAGS="$pmi_LDFLAGS"
                                      have_rpath="$pmi_rpath"
                                      added_flags=yes])
                               local_libs="$pmi_LIBS"],
                              [have_pmi1=no])

           # check for pmi2 lib */
           OPAL_CHECK_PMI_LIB([$check_install_dir],
                              [pmi2], [PMI2_Init],
                              [have_pmi2=yes
                               opal_have_pmi2=1
                               AS_IF([test "$default_loc" = "no" && test "$added_flags" = "no"],
                                     [$1_CPPFLAGS="$pmi2_CPPFLAGS"
                                      $1_LDFLAGS="$pmi2_LDFLAGS"
                                      have_rpath="$pmi2_rpath"])
                               local_libs="local_libs $pmi2_LIBS"],
                              [have_pmi2=no])

           # since support was explicitly requested, then we should error out
           # if we didn't find the required support
           AS_IF([test "$have_pmi1" != "yes" && test "$have_pmi2" != "yes"],
                 [AC_MSG_RESULT([not found])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but neither libpmi])
                  AC_MSG_WARN([nor libpmi2 were found under locations:])
                  AC_MSG_WARN([    $check_install_dir/lib])
                  AC_MSG_WARN([    $check_install_dir/lib64])
                  AC_MSG_WARN([Specified path: $with_pmi])
                  AC_MSG_ERROR([Aborting])
                  $3],
                 [AC_MSG_RESULT([yes])
                  opal_enable_pmi=1
                  $1_LIBS="$local_libs -Wl,-rpath=$have_rpath"
                  AC_MSG_CHECKING([final added libraries])
                  AC_MSG_RESULT([$local_libs])
                  $2])
           ])

    OPAL_VAR_SCOPE_POP
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
