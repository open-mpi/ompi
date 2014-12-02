# -*- shell-script -*-
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# --------------------------------------------------------
# OPAL_CHECK_LIBFABRIC([prefix (1)],
#                      [action-if-found (2)],
#                      [action-if-not-found (3)])
# --------------------------------------------------------
# Check if libfabric support can be found.  Sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found.
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_LIBFABRIC],[
    # Setup the --with switches to allow users to specify where
    # libfabric stuff lives.
    AC_REQUIRE([_OPAL_CHECK_LIBFABRIC_DIR])

    # usNIC: not allowed to say --with-libfabric=no or
    # --with-libfabric-libdir=no
    AS_IF([test "$opal_libfabric_dir" = "no" || \
           test "$opal_libfabric_libdir" = "no"],
          [AC_MSG_WARN([You cannot specify --without-libfabric[-libdir]])
           AC_MSG_ERROR([Cannot continue])])

    opal_check_libfabric_$1_save_CPPFLAGS=$CPPFLAGS
    opal_check_libfabric_$1_save_LDFLAGS=$LDFLAGS
    opal_check_libfabric_$1_save_LIBS=$LIBS

    # usNIC: Use the internal or external libfabric?
    AS_IF([test -z "$opal_libfabric_dir" && test -z "$opal_libfabric_libdir"],
          [_OPAL_USNIC_SETUP_LIBFABRIC_INTERNAL($1)],
          [_OPAL_USNIC_SETUP_LIBFABRIC_EXTERNAL($1)])
    _OPAL_USNIC_SETUP_LIBFABRIC_INTERNAL_CONDITIONALS

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
    LIBS="$LIBS $$1_LIBS"

    CPPFLAGS=$opal_check_libfabric_$1_save_CPPFLAGS
    LDFLAGS=$opal_check_libfabric_$1_save_LDFLAGS
    LIBS=$opal_check_libfabric_$1_save_LIBS

    AS_IF([test "$opal_check_libfabric_happy" = "yes"],
          [$2],
          [AS_IF([test "$opal_want_lifabric" = "yes"],
                 [AC_MSG_WARN([Libfabric support requested (via --with-libfabric) but not found.])
                  AC_MSG_ERROR([Cannot continue])])
           $3])
])


# --------------------------------------------------------
# _OPAL_CHECK_LIBFABRIC_DIR (internal)
# --------------------------------------------------------
# Add --with-libfabric options, and if directories are specified,
# sanity check them.
#
# At the end of this macro:
#
# 1. $opal_want_libfabric will be set to:
#    "yes" if --with-libfabric or --with-libfabric=DIR was specified
#    "no" if --without-libfabric was specified)
#    "optional" if neither --with-libfabric* nor --without-libfabric
#    was specified
#
# 2. $opal_libfabric_dir and $opal_libfabric_libdir with either both
#    be set or both be empty.
#
# --------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_LIBFABRIC_DIR],[

    # Add --with options
    AC_ARG_WITH([libfabric],
        [AC_HELP_STRING([--with-libfabric(=DIR)],
             [Build libfabric support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([libfabric-libdir],
       [AC_HELP_STRING([--with-libfabric-libdir=DIR],
             [Search for libfabric libraries in DIR])])

    # Sanity check the --with values
    OPAL_CHECK_WITHDIR([libfabric], [$with_libfabric],
                       [include/rdma/fabric.h])
    OPAL_CHECK_WITHDIR([libfabric-libdir], [$with_libfabric_libdir],
                       [libfabric.*])

    # Set standardized shell variables for libfabric lovin' components to
    # use.  Either both of $opal_libfabric_dir and
    # $libfabric_libdir will be set, or neither will be set.
    opal_want_libfabric=no
    AS_IF([test -z "$with_libfabric"],
          [opal_want_libfabric=optional],
          [AS_IF([test "$with_libfabric" = "no"],
                 [opal_want_libfabric=no],
                 [opal_want_libfabric=yes])
          ])

    opal_libfabric_dir=
    AS_IF([test -n "$with_libfabric" && \
           test "$with_libfabric" != "yes" && \
           test "$with_libfabric" != "no"],
          [opal_libfabric_dir=$with_libfabric])

    opal_libfabric_libdir=
    AS_IF([test -n "$with_libfabric_libdir" && \
           test "$with_libfabric_libdir" != "yes" && \
           test "$with_libfabric_libdir" != "no"],
          [opal_libfabric_libdir=$with_libfabric_libdir])
])

# --------------------------------------------------------
# Internal helper macro to setup the embedded libfabric.
#
# The internal libfabric is *TEMPORARY* and only for convenience of
# development.  Ultimately, the embedded libfabric will disappear and
# you will need to have libfabric installed.
# --------------------------------------------------------
AC_DEFUN([_OPAL_USNIC_SETUP_LIBFABRIC_INTERNAL_CONDITIONALS],[
    AM_CONDITIONAL(HAVE_LD_VERSION_SCRIPT, [/bin/false])
    AM_CONDITIONAL([HAVE_DIRECT], [/bin/false])
])
AC_DEFUN([_OPAL_USNIC_SETUP_LIBFABRIC_INTERNAL],[
    AC_MSG_NOTICE([Setting up for INTERNAL libfabric])

    # Mostly replicate relevant parts from the libfabric configure.ac
    # script.  Make a lot of simplifying assumptions, just for the
    # sake of embedding here.
    AC_DEFINE([INCLUDE_VALGRIND], 0, [no valgrind])
    AC_DEFINE([STREAM_CLOEXEC], 0, [no streamcloexec])
    AC_DEFINE([HAVE_ATOMICS], 0, [no atomics])
    AC_DEFINE([HAVE_SYMVER_SUPPORT], 1, [assembler has .symver support])

    usnic_happy=1
    AC_CHECK_HEADER([infiniband/verbs.h], [], [usnic_happy=0])
    AC_CHECK_HEADER([linux/netlink.h], [], [usnic_happy=0], [
#include <sys/types.h>
#include <net/if.h>
])
     AC_CHECK_LIB([nl], [nl_connect], [], [usnic_happy=0])

     AS_IF([test "$usnic_happy" -eq 1],
           [BTL_USNIC_EMBEDDED_LIBFABRIC=libusnic_fabric_embedded.la
            $1_CPPFLAGS="-I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/include"
            $1_LIBS="$OPAL_TOP_BUILDDIR/opal/mca/common/libusnic_fabric_embedded.la"])
     AC_SUBST(BTL_USNIC_EMBEDDED_LIBFABRIC)
     opal_check_libfabric_happy=yes
])

# --------------------------------------------------------
# Internal helper macro to setup for an external libfabric
# --------------------------------------------------------
AC_DEFUN([_OPAL_USNIC_SETUP_LIBFABRIC_EXTERNAL],[
    AC_MSG_NOTICE([Setting up for EXTERNAL libfabric])

    # If the top dir was specified but the libdir was not, look for
    # it.  Note that if the user needs a specific libdir (i.e., if our
    # hueristic ordering below is not sufficient), they need to
    # specify it.
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [_OPAL_CHECK_LIBFABRIC_LIBDIR(["$opal_libfabric_dir/lib"])])
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [_OPAL_CHECK_LIBFABRIC_LIBDIR(["$opal_libfabric_dir/lib64"])])
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [_OPAL_CHECK_LIBFABRIC_LIBDIR(["$opal_libfabric_dir/lib32"])])
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [AC_MSG_WARN([Could not find libiblibfabric in the usual locations under $opal_libfabric_dir])
           AC_MSG_ERROR([Cannot continue])
          ])

    # If the libdir was specified, but the top dir was not, look for
    # it.  Note that if the user needs a specific top dir (i.e., if
    # our hueristic below is not sufficient), they need to specify it.
    AS_IF([test -z "$opal_libfabric" -a -n "$opal_libfabric_libdir"],
          [_OPAL_CHECK_LIBFABRIC_INCDIR([`dirname "$opal_libfabric_libdir"`])])
    AS_IF([test -z "$opal_libfabric_dir" -a -n "$opal_libfabric_libdir"],
          [AC_MSG_WARN([Could not find libfabric.h in the usual locations under $opal_libfabric_dir])
           AC_MSG_ERROR([Cannot continue])
          ])

    # Now actually check to ensure that the external libfabric works
    OPAL_CHECK_PACKAGE([$1],
                       [rdma/fabric.h],
                       [fabric],
                       [fi_getinfo],
                       [],
                       [$opal_libfabric_dir],
                       [$opal_libfabric_libdir],
                       [opal_check_libfabric_happy="yes"],
                       [opal_check_libfabric_happy="no"])
])

# --------------------------------------------------------
# Internal helper macro to look for the libfabric libdir
# --------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_LIBFABRIC_LIBDIR],[
    AS_IF([test -d "$1"],
          [AS_IF([test "x`ls $1/libfabric.* 2> /dev/null`" != "x"],
                 [opal_libfabric_libdir="$1"])
          ])
])

# --------------------------------------------------------
# Internal helper macro to look for the libfabric dir
# --------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_LIBFABRIC_INCDIR],[
    AS_IF([test -d "$1"],
          [AS_IF([test -f "$1/include/rdma/fabric.h"],
                 [opal_libfabric_dir="$1"])
          ])
])
