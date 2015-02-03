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
dnl Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2011 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
dnl Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# Internal helper macro to look for the verbs libdir
# --------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_VERBS_LIBDIR],[
    AS_IF([test -d "$1"],
          [AS_IF([test "x`ls $1/libibverbs.* 2> /dev/null`" != "x"],
                 [opal_verbs_libdir="$1"])
          ])
])

# Internal helper macro to look for the verbs dir
# --------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_VERBS_DIR],[
    AS_IF([test -d "$1"],
          [AS_IF([test -f "$1/include/infiniband/verbs.h"],
                 [opal_verbs_dir="$1"])
          ])
])

# OPAL_CHECK_VERBS_DIR
# --------------------------------------------------------
# Add --with-verbs options, and if directories are specified,
# sanity check them.
#
# At the end of this macro:
#
# 1. $opal_want_verbs will be set to:
#    "yes" if --with-verbs or --with-verbs=DIR was specified
#    "no" if --without-verbs was specified)
#    "optional" if neither --with-verbs* nor --without-verbs was specified
#
# 2. $opal_verbs_dir and $opal_verbs_libdir with either both be set or
# both be empty.
#
AC_DEFUN([OPAL_CHECK_VERBS_DIR],[

    # Add --with options
    AC_ARG_WITH([verbs],
        [AC_HELP_STRING([--with-verbs(=DIR)],
             [Build verbs support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([verbs-libdir],
       [AC_HELP_STRING([--with-verbs-libdir=DIR],
             [Search for verbs libraries in DIR])])

    # Sanity check the --with values
    OPAL_CHECK_WITHDIR([verbs], [$with_verbs], 
                       [include/infiniband/verbs.h])
    OPAL_CHECK_WITHDIR([verbs-libdir], [$with_verbs_libdir], 
                       [libibverbs.*])

    # Set standardized shell variables for OFED lovin' components to
    # use.  Either both of $opal_verbs_dir and
    # $verbs_libdir will be set, or neither will be set.
    opal_want_verbs=no
    AS_IF([test -z "$with_verbs"],
          [opal_want_verbs=optional],
          [AS_IF([test "$with_verbs" = "no"],
                 [opal_want_verbs=no],
                 [opal_want_verbs=yes])
          ])

    opal_verbs_dir=
    AS_IF([test -n "$with_verbs" && test "$with_verbs" != "yes" && test "$with_verbs" != "no"],
          [opal_verbs_dir=$with_verbs])

    opal_verbs_libdir=
    AS_IF([test -n "$with_verbs_libdir" && test "$with_verbs_libdir" != "yes" && test "$with_verbs_libdir" != "no"],
          [opal_verbs_libdir=$with_verbs_libdir])

    # If the top dir was specified but the libdir was not, look for
    # it.  Note that if the user needs a specific libdir (i.e., if our
    # hueristic ordering below is not sufficient), they need to
    # specify it.
    AS_IF([test -z "$opal_verbs_libdir" && test -n "$opal_verbs_dir"],
          [_OPAL_CHECK_VERBS_LIBDIR(["$opal_verbs_dir/lib"])])
    AS_IF([test -z "$opal_verbs_libdir" && test -n "$opal_verbs_dir"],
          [_OPAL_CHECK_VERBS_LIBDIR(["$opal_verbs_dir/lib64"])])
    AS_IF([test -z "$opal_verbs_libdir" && test -n "$opal_verbs_dir"],
          [_OPAL_CHECK_VERBS_LIBDIR(["$opal_verbs_dir/lib32"])])
    AS_IF([test -z "$opal_verbs_libdir" && test -n "$opal_verbs_dir"],
          [AC_MSG_WARN([Could not find libibverbs in the usual locations under $opal_verbs_dir])
           AC_MSG_ERROR([Cannot continue])
          ])

    # If the libdir was specified, but the top dir was not, look for
    # it.  Note that if the user needs a specific top dir (i.e., if
    # our hueristic below is not sufficient), they need to specify it.
    AS_IF([test -z "$opal_verbs" && test -n "$opal_verbs_libdir"],
          [_OPAL_CHECK_VERBS_DIR([`dirname "$opal_verbs_libdir"`])])
    AS_IF([test -z "$opal_verbs_dir" && test -n "$opal_verbs_libdir"],
          [AC_MSG_WARN([Could not find verbs.h in the usual locations under $opal_verbs_dir])
           AC_MSG_ERROR([Cannot continue])
          ])
])
