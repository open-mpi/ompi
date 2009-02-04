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
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_TM_LIBS_FLAGS(prefix, [LIBS or LDFLAGS])
# ---------------------------------------------------
AC_DEFUN([OMPI_CHECK_TM_LIBS_FLAGS],[
    ompi_check_tm_flags=`$ompi_check_tm_pbs_config --libs`
    for ompi_check_tm_val in $ompi_check_tm_flags; do
        if test "`echo $ompi_check_tm_val | cut -c1-2`" = "-l"; then
            if test "$2" = "LIBS"; then
                $1_$2="$$1_$2 $ompi_check_tm_val"
            fi
        else
            if test "$2" = "LDFLAGS"; then
                $1_$2="$$1_$2 $ompi_check_tm_val"
            fi
        fi
    done
])


# OMPI_CHECK_TM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_TM],[
    AC_ARG_WITH([tm],
                [AC_HELP_STRING([--with-tm(=DIR)],
                                [Directory where the tm software is installed])])
    OMPI_CHECK_WITHDIR([tm], [$with_tm], [include/tm.h])

    ompi_check_tm_found=no
    AS_IF([test "$with_tm" = "no"],
          [ompi_check_tm_happy="no"],
          [ompi_check_tm_happy="yes"
           AS_IF([test ! -z "$with_tm" -a "$with_tm" != "yes"],
                 [ompi_check_tm_dir="$with_tm"],
                 [ompi_check_tm_dir=""])])

    AS_IF([test "$ompi_check_tm_happy" = "yes"],
          [AC_MSG_CHECKING([for pbs-config])
           ompi_check_tm_pbs_config="not found"
           AS_IF([test "$ompi_check_tm_dir" != "" -a -d "$ompi_check_tm_dir" -a -x "$ompi_check_tm_dir/bin/pbs-config"],
                 [ompi_check_tm_pbs_config="$ompi_check_tm_dir/bin/pbs-config"],
                 [AS_IF([pbs-config --prefix >/dev/null 2>&1],
                        [ompi_check_tm_pbs_config="pbs-config"])])
           AC_MSG_RESULT([$ompi_check_tm_pbs_config])])

    # If we have pbs-config, get the flags we need from there and then
    # do simplistic tests looking for the tm headers and symbols

    AS_IF([test "$ompi_check_tm_happy" = "yes" -a "$ompi_check_tm_pbs_config" != "not found"],
          [$1_CPPFLAGS=`$ompi_check_tm_pbs_config --cflags`
           OMPI_LOG_MSG([$1_CPPFLAGS from pbs-config: $$1_CPPFLAGS], 1)

           OMPI_CHECK_TM_LIBS_FLAGS([$1], [LDFLAGS])
           OMPI_LOG_MSG([$1_LDFLAGS from pbs-config: $$1_LDFLAGS], 1)

           OMPI_CHECK_TM_LIBS_FLAGS([$1], [LIBS])
           OMPI_LOG_MSG([$1_LIBS from pbs-config: $$1_LIBS], 1)

           # Now that we supposedly have the right flags, try them out.

           CPPFLAGS_save="$CPPFLAGS"
           LDFLAGS_save="$LDFLAGS"
           LIBS_save="$LIBS"

           CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
           LIBS="$LIBS $$1_LIBS"
           LDFLAGS="$LDFLAGS $$1_LDFLAGS"

           AC_CHECK_HEADER([tm.h],
               [AC_CHECK_FUNC([tm_finalize],
                   [ompi_check_tm_found="yes"])])

           CPPFLAGS="$CPPFLAGS_save"
           LDFLAGS="$LDFLAGS_save"
           LIBS="$LIBS_save"])

    # If we don't have pbs-config, then we have to look around
    # manually.

    # Note that Torque 2.1.0 changed the name of their back-end
    # library to "libtorque".  So we have to check for both libpbs and
    # libtorque.  First, check for libpbs.

    ompi_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_package_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_package_$1_save_LIBS="$LIBS"

    ompi_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    ompi_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    ompi_check_package_$1_orig_LIBS="$$1_LIBS"

    AS_IF([test "$ompi_check_tm_found" = "no"],
          [AS_IF([test "$ompi_check_tm_happy" = "yes"],
                 [_OMPI_CHECK_PACKAGE_HEADER([$1], 
                       [tm.h],
                       [$ompi_check_tm_dir],
                       [ompi_check_tm_found="yes"],
                       [ompi_check_tm_found="no"])])

           AS_IF([test "$ompi_check_tm_found" = "yes"],
                 [_OMPI_CHECK_PACKAGE_LIB([$1],
                       [pbs],
                       [tm_init],
                       [],
                       [$ompi_check_tm_dir],
                       [$ompi_check_tm_libdir],
                       [ompi_check_tm_found="yes"],
                       [_OMPI_CHECK_PACKAGE_LIB([$1],
                             [torque],
                             [tm_init],
                             [],
                             [$ompi_check_tm_dir],
                             [$ompi_check_tm_libdir],
                             [ompi_check_tm_found="yes"],
                             [ompi_check_tm_found="no"])])])])

    CPPFLAGS="$ompi_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
    LIBS="$ompi_check_package_$1_save_LIBS"

    # Did we find the right stuff?
    AS_IF([test "$ompi_check_tm_happy" = "yes" -a "$ompi_check_tm_found" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_tm" -a "$with_tm" != "no"],
                 [AC_MSG_ERROR([TM support requested but not found.  Aborting])])
           $3])
])
