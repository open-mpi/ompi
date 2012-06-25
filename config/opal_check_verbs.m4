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
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2011 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
# Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


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
# NOTE: --with-openib* are deprecated synonyms for --with-verbs*.
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

    # Add deprecated --with options
    # JMS REMOVE THESE IN v1.9
    AC_ARG_WITH([openib],
        [AC_HELP_STRING([--with-openib(=DIR)],
             [DEPRECATED synonym for --with-verbs])])
    AC_ARG_WITH([openib-libdir],
       [AC_HELP_STRING([--with-openib-libdir=DIR],
             [DEPRECATED synonym for --with-verbs-libdir])])

    # Did we specify both --with-openib* and --with-verbs*?
    # JMS REMOVE THESE IN v1.9
    AS_IF([test -n "$with_openib" -a -n "$with_verbs"],
          [AC_MSG_WARN([Both --with-verbs and --with-openib specified.])
           AC_MSG_WARN([--with-openib value ignored.])
           with_openib=
          ])
    AS_IF([test -n "$with_openib_libdir" -a -n "$with_verbs_libdir"],
          [AC_MSG_WARN([Both --with-verbs-libdir and --with-openib-libdir specified.])
           AC_MSG_WARN([--with-openib-libdir value ignored.])
           with_openib_libdir=
          ])

    # Did we specify --with-openib* and not --with-verbs*?
    # JMS REMOVE THESE IN v1.9
    AS_IF([test -n "$with_openib" -a -z "$with_verbs"],
          [AC_MSG_WARN([**************************************************])
           AC_MSG_WARN([The use of --with-openib is deprecated.])
           AC_MSG_WARN([You should use --with-verbs instead.])
           AC_MSG_WARN([Proceeding with the assumption you meant])
           AC_MSG_WARN([--with-verbs=$with_openib])
           AC_MSG_WARN([(sleeping to let you read this message)])
           AC_MSG_WARN([**************************************************])
           sleep 10
           with_verbs=$with_openib
           with_openib=
          ])
    AS_IF([test -n "$with_openib_libdir" -a -z "$with_verbs_libdir"],
          [AC_MSG_WARN([**************************************************])
           AC_MSG_WARN([The use of --with-openib-libdir is deprecated.])
           AC_MSG_WARN([You should use --with-verbs-libdir instead.])
           AC_MSG_WARN([Proceeding with the assumption you meant])
           AC_MSG_WARN([--with-verbs-libdir=$with_openib_libdir])
           AC_MSG_WARN([(sleeping to let you read this message)])
           AC_MSG_WARN([**************************************************])
           sleep 10
           with_verbs_libdir=$with_openib_libdir
           with_openib_libdir=
          ])

    # Sanity check the --with values
    OMPI_CHECK_WITHDIR([verbs], [$with_verbs], 
                       [include/infiniband/verbs.h])
    OMPI_CHECK_WITHDIR([verbs-libdir], [$with_verbs_libdir], 
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
    AS_IF([test -n "$with_verbs" -a "$with_verbs" != "yes" -a "$with_verbs" != "no"],
          [opal_verbs_dir=$with_verbs])

    opal_verbs_libdir=
    AS_IF([test -n "$with_verbs_libdir" -a "$with_verbs_libdir" != "yes" -a "$with_verbs_libdir" != "no"],
          [opal_verbs_libdir=$with_verbs_libdir])

    # If the top dir was specified but the libdir was not, look for
    # it.  Note that if the user needs a specific libdir (i.e., if our
    # hueristic ordering below is not sufficient), they need to
    # specify it.
    AS_IF([test -z "$opal_verbs_libdir" -a -n "$opal_verbs_dir"],
          [_OPAL_CHECK_VERBS_LIBDIR(["$opal_verbs_dir/lib"])])
    AS_IF([test -z "$opal_verbs_libdir" -a -n "$opal_verbs_dir"],
          [_OPAL_CHECK_VERBS_LIBDIR(["$opal_verbs_dir/lib64"])])
    AS_IF([test -z "$opal_verbs_libdir" -a -n "$opal_verbs_dir"],
          [_OPAL_CHECK_VERBS_LIBDIR(["$opal_verbs_dir/lib32"])])
    AS_IF([test -z "$opal_verbs_libdir" -a -n "$opal_verbs_dir"],
          [AC_MSG_WARN([Could not find libibverbs in the usual locations under $opal_verbs_dir])
           AC_MSG_ERROR([Cannot continue])
          ])

    # If the libdir was specified, but the top dir was not, look for
    # it.  Note that if the user needs a specific top dir (i.e., if
    # our hueristic below is not sufficient), they need to specify it.
    AS_IF([test -z "$opal_verbs" -a -n "$opal_verbs_libdir"],
          [_OPAL_CHECK_VERBS_DIR([`dirname "$opal_verbs_libdir"`])])
    AS_IF([test -z "$opal_verbs_dir" -a -n "$opal_verbs_libdir"],
          [AC_MSG_WARN([Could not find verbs.h in the usual locations under $opal_verbs_dir])
           AC_MSG_ERROR([Cannot continue])
          ])
])
