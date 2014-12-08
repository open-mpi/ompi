# -*- shell-script -*-
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_common_libfabric_CONFIG([action-if-can-copalle],
#                                  [action-if-cant-copalle])
#
# Will also set $opal_common_libfabric_happy to 0 or 1 (0 = no
# libfabric support, 1 = libfabric support).  Will also set
# $opal_common_libfabric_build_embedded to 0 or 1 (1 = building
# embedded libfabric, 0 = not building embedded libfabric).
# ------------------------------------------------
AC_DEFUN([MCA_opal_common_libfabric_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/libfabric/Makefile])

    # Initially state that we're unhappy
    opal_common_libfabric_happy=0
    opal_common_libfabric_build_embedded=0

    # Setup the --with switches to allow users to specify where
    # libfabric stuff lives.
    AC_REQUIRE([_OPAL_COMMON_LIBFABRIC_WITH_FLAGS])

    AS_IF([test "$opal_want_libfabric" != "no"],
          [ # Regardless of whether we build embedded or external,
            # libfabric is only supported on Linux.
           AC_MSG_CHECKING([if we are on Linux])
           AS_CASE([$host_os],
               [*linux*], [AC_MSG_RESULT([yes])
                           _OPAL_COMMON_LIBFABRIC_CONFIGURE],
               [*],       [AC_MSG_RESULT([no])],
           )
          ])

    # AM conditionals must be executed unconditionally
    _OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EMBEDDED_CONDITIONALS
    AM_CONDITIONAL([OPAL_COMMON_LIBFABRIC_BUILD_EMBEDDED],
                   [test $opal_common_libfabric_build_embedded -eq 1])

    # This is for components that build with libfabric support
    AC_SUBST(opal_common_libfabric_CPPFLAGS)
    AC_SUBST(opal_common_libfabric_LIBADD)

    # This is for building the libfabric component itself
    opal_common_libfabric_embedded_CPPFLAGS=$opal_common_libfabric_CPPFLAGS
    AC_SUBST(opal_common_libfabric_embedded_CPPFLAGS)
    AC_SUBST(opal_common_libfabric_embedded_CFLAGS)
    AC_SUBST(opal_common_libfabric_embedded_LIBADD)

    # Did libfabric configure successfully?
    AS_IF([test $opal_common_libfabric_happy -eq 1],
          [$1],
          [AS_IF([test "$opal_want_libfabric" = "yes"],
                 [AC_MSG_WARN([Libfabric support requested (via --with-libfabric) but not found.])
                  AC_MSG_ERROR([Cannot continue])])
           $2])
])


# --------------------------------------------------------
# _OPAL_COMMON_LIBFABRIC_WITH_FLAGS (internal)
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
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_WITH_FLAGS],[

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
# Internal helper macro to configure an internal or external libfabric.
#
# arg 1: action if will build libfabric
# arg 2: action if will not build libfabric
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_CONFIGURE],[
    opal_check_libfabric_save_CPPFLAGS=$CPPFLAGS
    opal_check_libfabric_save_LDFLAGS=$LDFLAGS
    opal_check_libfabric_save_LIBS=$LIBS

    # Use the internal or external libfabric?
    AS_IF([test -z "$opal_libfabric_dir" && \
           test -z "$opal_libfabric_libdir"],
          [_OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EMBEDDED],
          [_OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EXTERNAL])

    CPPFLAGS=$opal_check_libfabric_save_CPPFLAGS
    LDFLAGS=$opal_check_libfabric_save_LDFLAGS
    LIBS=$opal_check_libfabric_save_LIBS

    AS_IF([test $opal_common_libfabric_happy -eq 1], [$1], [$2])
])

# --------------------------------------------------------
# Internal helper macros to setup the embedded libfabric.
#
# The internal libfabric is *TEMPORARY* and only for convenience of
# development.  Ultimately, the embedded libfabric will disappear and
# you will need to have libfabric installed.
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EMBEDDED_CONDITIONALS],[
    AM_CONDITIONAL([HAVE_LD_VERSION_SCRIPT], [false])
    AM_CONDITIONAL([HAVE_DIRECT], [false])

    _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_CONDITIONALS
])

AC_DEFUN([_OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EMBEDDED],[
    AC_MSG_NOTICE([Setting up for EMBEDDED libfabric])

    # Mostly replicate relevant parts from the libfabric configure.ac
    # script.  Make a lot of simplifying assumptions, just for the
    # sake of embedding here.
    AC_DEFINE([INCLUDE_VALGRIND], 0, [no valgrind])
    AC_DEFINE([STREAM_CLOEXEC], 0, [no streamcloexec])
    AC_DEFINE([HAVE_ATOMICS], 0, [no atomics])
    AC_DEFINE([HAVE_SYMVER_SUPPORT], 1, [assembler has .symver support])

    AC_CHECK_HEADER([infiniband/verbs.h],
        [opal_common_libfabric_happy=1],
        [opal_common_libfabric_happy=0])

    # Add flags for libfabric core
    AS_IF([test $opal_common_libfabric_happy -eq 1],
           [opal_common_libfabric_CPPFLAGS="-I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/include"
            opal_common_libfabric_build_embedded=1

            # OMPI's debugging compile flags are fairly aggressive,
            # and include -pedantic.  Unfortunately, there's a bunch
            # of code in libfabric that complains about -pedantic, so
            # remove it from the CFLAGS.
            for flag in $CFLAGS; do
                case $flag in
                -pedantic) ;;
                *) opal_common_libfabric_embedded_CFLAGS="$opal_common_libfabric_embedded_CFLAGS $flag" ;;
                esac
            done

            # Do stuff for specific providers
            _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC
           ])
])

# --------------------------------------------------------
# Internal helper macro to setup for an external libfabric
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EXTERNAL],[
    AC_MSG_NOTICE([Setting up for EXTERNAL libfabric])

    # If the top dir was specified but the libdir was not, look for
    # it.  Note that if the user needs a specific libdir (i.e., if our
    # hueristic ordering below is not sufficient), they need to
    # specify it.
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [_OPAL_COMMON_LIBFABRIC_CHECK_LIBDIR(["$opal_libfabric_dir/lib"])])
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [_OPAL_COMMON_LIBFABRIC_CHECK_LIBDIR(["$opal_libfabric_dir/lib64"])])
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [_OPAL_COMMON_LIBFABRIC_CHECK_LIBDIR(["$opal_libfabric_dir/lib32"])])
    AS_IF([test -z "$opal_libfabric_libdir" -a -n "$opal_libfabric_dir"],
          [AC_MSG_WARN([Could not find libiblibfabric in the usual locations under $opal_libfabric_dir])
           AC_MSG_ERROR([Cannot continue])
          ])

    # If the libdir was specified, but the top dir was not, look for
    # it.  Note that if the user needs a specific top dir (i.e., if
    # our hueristic below is not sufficient), they need to specify it.
    AS_IF([test -z "$opal_libfabric" -a -n "$opal_libfabric_libdir"],
          [_OPAL_COMMON_LIBFABRIC_CHECK_INCDIR([`dirname "$opal_libfabric_libdir"`])])
    AS_IF([test -z "$opal_libfabric_dir" -a -n "$opal_libfabric_libdir"],
          [AC_MSG_WARN([Could not find libfabric.h in the usual locations under $opal_libfabric_dir])
           AC_MSG_ERROR([Cannot continue])
          ])

    # Now actually check to ensure that the external libfabric works
    OPAL_CHECK_PACKAGE([opal_common_libfabric],
                       [rdma/fabric.h],
                       [fabric],
                       [fi_getinfo],
                       [],
                       [$opal_libfabric_dir],
                       [$opal_libfabric_libdir],
                       [opal_common_libfabric_happy=1],
                       [opal_common_libfabric_happy=0])

     opal_common_libfabric_LIBADD="-lfabric"
])

# --------------------------------------------------------
# Internal helper macro to look for the libfabric libdir
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_CHECK_LIBDIR],[
    AS_IF([test -d "$1"],
          [AS_IF([test "x`ls $1/libfabric.* 2> /dev/null`" != "x"],
                 [opal_libfabric_libdir="$1"])
          ])
])

# --------------------------------------------------------
# Internal helper macro to look for the libfabric dir
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_CHECK_INCDIR],[
    AS_IF([test -d "$1"],
          [AS_IF([test -f "$1/include/rdma/fabric.h"],
                 [opal_libfabric_dir="$1"])
          ])
])

# --------------------------------------------------------
# Internal helper macro to look for the things the usnic provider
# needs
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC],[
    opal_common_libfabric_usnic_happy=1
    AC_CHECK_HEADER([linux/netlink.h], [],
                    [opal_common_libfabric_usnic_happy=0], [
#include <sys/types.h>
#include <net/if.h>
])
    AC_CHECK_LIB([nl], [nl_connect], [],
                 [opal_common_libfabric_usnic_happy=0])

    opal_common_libfabric_CPPFLAGS="$opal_common_libfabric_CPPFLAGS -I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/prov/usnic/src -I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/prov/usnic/src/usnic_direct"
    opal_common_libfabric_LIBADD="\$(OPAL_TOP_BUILDDIR)/opal/mca/common/libfabric/lib${OPAL_LIB_PREFIX}mca_common_libfabric.la"

    opal_common_libfabric_embedded_LIBADD="-lnl"
])

# --------------------------------------------------------
# Internal helper macro for usnic AM conditionals (that must be run
# unconditionally)
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_CONDITIONALS],[
    AM_CONDITIONAL([OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_USNIC],
                   [test $opal_common_libfabric_usnic_happy -eq 1])
])
