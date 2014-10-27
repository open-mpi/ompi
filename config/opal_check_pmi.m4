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
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# define an internal function for checking the existence
# and validity of a PMI library
#
# OPAL_CHECK_PMI_LIB(installdir, pmi, function, [action-if-slurm], [action-if-valid], [action-if-not-valid])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI_LIB],
[
    # save flags
    opal_check_$2_save_CPPFLAGS=$CPPFLAGS
    opal_check_$2_save_LDFLAGS=$LDFLAGS
    opal_check_$2_save_LIBS=$LIBS
    opal_check_$2_hdr_happy=
    opal_check_$2_mycppflags=

    # check for the header
    AC_MSG_CHECKING([for $2.h in $1/include])
    AS_IF([test -f $1/include/$2.h],
          [AC_MSG_RESULT([found])
           opal_check_$2_mycppflags="-I$1/include"],
          [AC_MSG_RESULT([not found])
           AC_MSG_CHECKING([for $2.h in $1/include/slurm])
           AS_IF([test -f $1/include/slurm/$2.h],
                 [AC_MSG_RESULT([found])
                  opal_check_$2_mycppflags="-I$1/include/slurm"
		  $4],
                 [AC_MSG_RESULT([not found])
                  opal_check_$2_hdr_happy=no])])

    AS_IF([test "$opal_check_$2_hdr_happy" != "no"],
          [CPPFLAGS="$CPPFLAGS $opal_check_$2_mycppflags"
            AC_CHECK_HEADER([$2.h],
                            [opal_check_$2_hdr_happy=yes
                             $2_CPPFLAGS="$opal_check_$2_mycppflags"],
                            [opal_check_$2_hdr_happy=no])])

    # check for presence of lib64 directory - if found, see if the
    # desired library is present and matches our build requirements
    opal_check_$2_lib_happy=
    LIBS="$LIBS -l$2"
    AC_MSG_CHECKING([for lib$2 in $1/lib64])
    files=`ls $1/lib64/lib$2.* 2> /dev/null | wc -l`
    AS_IF([test "$files" -gt "0"],
          [AC_MSG_RESULT([found])
           LDFLAGS="$LDFLAGS -L$1/lib64"
           AC_CHECK_LIB([$2], [$3],
                        [opal_check_$2_lib_happy=yes
                         $2_LDFLAGS=-L$1/lib64
                         $2_rpath=$1/lib64],
                        [opal_check_$2_lib_happy=no])],
           [AC_MSG_RESULT([not found])])
           

    # if we didn't find lib64, or the library wasn't present or correct,
    # then try a lib directory if present
    files=`ls $1/lib/lib$2.* 2> /dev/null | wc -l`
    AS_IF([test "$opal_check_$2_lib_happy" != "yes"],
          [AC_MSG_CHECKING([for lib$2 in $1/lib])
           AS_IF([test "$files" -gt "0"],
                 [AC_MSG_RESULT([found])
                  LDFLAGS="$LDFLAGS -L$1/lib"
                  AC_CHECK_LIB([$2], [$3],
                               [opal_check_$2_lib_happy=yes
                                $2_LDFLAGS=-L$1/lib
                                $2_rpath=$1/lib],
                               [opal_check_$2_lib_happy=no])],
                 [opal_check_$2_lib_happy=no
                  AC_MSG_RESULT([not found])])])

    # restore flags
    CPPFLAGS=$opal_check_$2_save_CPPFLAGS
    LDFLAGS=$opal_check_$2_save_LDFLAGS
    LIBS=$opal_check_$2_save_LIBS

    AS_IF([test "$opal_check_$2_hdr_happy" = "yes" && test "$opal_check_$2_lib_happy" = "yes"],
          [$5], [$6])

])

# OPAL_CHECK_PMI()
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_PMI],[

    AC_ARG_WITH([pmi],
                [AC_HELP_STRING([--with-pmi(=DIR)],
                                [Build PMI support, optionally adding DIR to the search path (default: no)])],
                                [], with_pmi=no)

    check_pmi_install_dir=
    default_pmi_loc=
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

           # check for pmi-1 lib */
           slurm_pmi_found=no
           OPAL_CHECK_PMI_LIB([$check_pmi_install_dir],
                              [pmi], [PMI_Init],
			      [slurm_pmi_found=yes],
                              [opal_enable_pmi1=yes
                               opal_pmi1_LIBS="-lpmi"
                               AC_SUBST(opal_pmi1_LIBS)],
                              [opal_enable_pmi1=no])

           AS_IF([test "$opal_enable_pmi1" = "yes"],
                 [AS_IF([test "$default_pmi_loc" = "no" || test "$slurm_pmi_found" = "yes"],
                        [opal_pmi1_CPPFLAGS="$pmi_CPPFLAGS"
                         AC_SUBST(opal_pmi1_CPPFLAGS)
                         opal_pmi1_LDFLAGS="$pmi_LDFLAGS"
                         AC_SUBST(opal_pmi1_LDFLAGS)
                         opal_pmi1_rpath="$pmi_rpath"
                         AC_SUBST(opal_pmi1_rpath)])])
		  
           # check for pmi2 lib */
           slurm_pmi_found=no
           OPAL_CHECK_PMI_LIB([$check_pmi_install_dir],
                              [pmi2], [PMI2_Init],
			      [slurm_pmi_found=yes],
                              [opal_enable_pmi2=yes
                               opal_pmi2_LIBS="-lpmi2"
                               AC_SUBST(opal_pmi2_LIBS)],
                              [opal_enable_pmi2=no])

           AS_IF([test "$opal_enable_pmi2" = "yes"],
                 [AS_IF([test "$default_pmi_loc" = "no" || test "$slurm_pmi_found" = "yes"],
                        [opal_pmi2_CPPFLAGS="$pmi2_CPPFLAGS"
                         AC_SUBST(opal_pmi2_CPPFLAGS)
                         opal_pmi2_LDFLAGS="$pmi2_LDFLAGS"
                         AC_SUBST(opal_pmi2_LDFLAGS)
                         opal_pmi2_rpath="$pmi2_rpath"
                         AC_SUBST(opal_pmi2_rpath)])])
		  
           # since support was explicitly requested, then we should error out
           # if we didn't find the required support
           AS_IF([test "$opal_enable_pmi1" != "yes" && test "$opal_enable_pmi2" != "yes"],
                 [AC_MSG_RESULT([not found])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but neither libpmi])
                  AC_MSG_WARN([nor libpmi2 were found under locations:])
                  AC_MSG_WARN([    $check_install_dir/lib])
                  AC_MSG_WARN([    $check_install_dir/lib64])
                  AC_MSG_WARN([Specified path: $with_pmi])
                  AC_MSG_ERROR([Aborting])],
                 [AC_MSG_RESULT([yes])])
           ])
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
