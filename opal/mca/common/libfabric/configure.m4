# -*- shell-script -*-
#
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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
    AC_CONFIG_HEADERS([opal/mca/common/libfabric/libfabric/config.h])

    # Initially state that we're unhappy
    opal_common_libfabric_happy=0
    opal_common_libfabric_build_embedded=0
    _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_SETUP
    _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_PSM_SETUP

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
    AC_SUBST(opal_common_libfabric_LDFLAGS)
    AC_SUBST(opal_common_libfabric_LIBADD)

    # This is for building the libfabric component itself
    opal_common_libfabric_embedded_CPPFLAGS=$opal_common_libfabric_CPPFLAGS
    AC_SUBST(opal_common_libfabric_embedded_CPPFLAGS)
    AC_SUBST(opal_common_libfabric_embedded_CFLAGS)
    AC_SUBST(opal_common_libfabric_embedded_LIBADD)

    # Ensure that the wrappers get what they need (e.g., for static
    # builds).
    common_libfabric_WRAPPER_EXTRA_LIBS=$opal_common_libfabric_embedded_LIBADD

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
    _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_PSM_CONDITIONALS
])

AC_DEFUN([_OPAL_COMMON_LIBFABRIC_SETUP_LIBFABRIC_EMBEDDED],[
    AC_MSG_NOTICE([Setting up for EMBEDDED libfabric])

    # Replicate a few libfabric configure tests
    opal_common_libfabric_happy=1
    AC_CHECK_HEADER([infiniband/verbs.h], [],
        [opal_common_libfabric_happy=0])
    AC_CHECK_LIB(pthread, pthread_mutex_init, [],
        [opal_common_libfabric_happy=0])
    AC_CHECK_LIB(rt, clock_gettime, [],
        [opal_common_libfabric_happy=0])

    # Add flags for libfabric core
    AS_IF([test $opal_common_libfabric_happy -eq 1],
           [opal_common_libfabric_CPPFLAGS="-I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric -I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/include"
            opal_common_libfabric_build_embedded=1
            opal_common_libfabric_LIBADD="\$(OPAL_TOP_BUILDDIR)/opal/mca/common/libfabric/lib${OPAL_LIB_PREFIX}mca_common_libfabric.la"

            # OMPI's debugging compile flags are fairly aggressive,
            # and include -pedantic.  Unfortunately, there's a bunch
            # of code in libfabric that complains about -pedantic, so
            # remove it from the CFLAGS.
            for flag in $CFLAGS; do
                case $flag in
                -pedantic) ;;
                -Wmissing-prototypes) ;;
                -Wsign-compare) ;;
                *) opal_common_libfabric_embedded_CFLAGS="$opal_common_libfabric_embedded_CFLAGS $flag" ;;
                esac
            done

            # Specifically disabling (by not defining anything)
            # libfabric features: valgrind support, symbol versioning
            # support.

            # Check for gcc atomic intrinsics
            AC_MSG_CHECKING([if compiler support for c11 atomics])
            AC_TRY_LINK([#include <stdatomic.h>],
                        [atomic_int a;
   atomic_init(&a, 0);
#ifdef __STDC_NO_ATOMICS__
#error c11 atomics are not supported
#else
    return 0;
#endif
                        ],
                        [
                         AC_MSG_RESULT(yes)
                         AC_DEFINE(HAVE_ATOMICS, 1, [Set to use c11 atomic functions])
                        ],
                        [AC_MSG_RESULT(no)])

            AC_MSG_CHECKING([if linker supports the alias attribute])
            AC_LINK_IFELSE(
                [AC_LANG_SOURCE[
                        int foo(int arg);
                        int foo(int arg) { return arg + 3; };
                        int foo2(int arg) __attribute__ (( __alias__("foo")));
                        ]],
                [AC_MSG_RESULT([yes])
                 opal_common_libfabric_alias_symbols=1],
                [AC_MSG_RESULT([no])
                 opal_common_libfabric_alias_symbols=0])
            AC_DEFINE_UNQUOTED([HAVE_ALIAS_ATTRIBUTE],
                [$opal_common_libfabric_alias_symbols],
                [Define to 1 if the linker supports alias attribute.])

            AC_CHECK_FUNC([pthread_spin_init],
                [opal_common_libfabric_have_spinlock=1],
                [opal_common_libfabric_have_spinlock=0])

            AC_DEFINE_UNQUOTED([PT_LOCK_SPIN],
                [$opal_common_libfabric_have_spinlock],
                [Define PT_LOCK_SPIN to 1 if available.])

            # Do stuff for specific providers
            _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC
            _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_PSM

            # Hard-coding to not build the sockets or verbs providers
            AC_DEFINE([HAVE_SOCKETS], [0],
                [libfabric: do not build sockets provider])
            AC_DEFINE([HAVE_SOCKETS_DL], [0],
                [libfabric: do not build sockets provider])
            AC_DEFINE([HAVE_VERBS], [0],
                [libfabric: do not build verbs provider])
            AC_DEFINE([HAVE_VERBS_DL], [0],
                [libfabric: do not build verbs provider])
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

     opal_common_libfabric_LDFLAGS="-L$opal_libfabric_libdir"
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
# Internal helper macro to setup the embedded usnic provider
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_SETUP],[
    opal_common_libfabric_usnic_happy=0
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
    _OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_CHECK_LIBNL3(
        [opal_common_libfabric_LIBNL_CPPFLAGS],
        [opal_common_libfabric_LIBNL_LIBS], [0])
    AS_IF([test "$opal_common_libfabric_LIBNL_LIBS" == ""],
          [opal_common_libfabric_usnic_happy=0])

    AC_DEFINE_UNQUOTED([HAVE_USNIC], [$opal_common_libfabric_usnic_happy],
          [libfabric: whether to build the usnic provider or not])
    AC_DEFINE([HAVE_USNIC_DL], 0,
          [libfabric: do not build usnic provider as a DL])

    AS_IF([test $opal_common_libfabric_usnic_happy -eq 1],
          [opal_common_libfabric_CPPFLAGS="$opal_common_libfabric_CPPFLAGS $opal_common_libfabric_LIBNL_CPPFLAGS -I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/prov/usnic/src -I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/prov/usnic/src/usnic_direct -D__LIBUSNIC__ -DHAVE_LIBNL3=$HAVE_LIBNL3 -DWANT_DEBUG_MSGS=0"
           opal_common_libfabric_embedded_LIBADD=$opal_common_libfabric_LIBNL_LIBS])
])

# --------------------------------------------------------
# Internal helper macro for usnic AM conditionals (that must be run
# unconditionally)
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_CONDITIONALS],[
    AM_CONDITIONAL([OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_USNIC],
                   [test $opal_common_libfabric_usnic_happy -eq 1])
])

dnl --------------------------------------------------------
dnl
dnl Check for libnl; prefer version 3 instead of version 1.  Abort (i.e.,
dnl AC_MSG_ERROR) if neither libnl v1 or v3 can be found.
dnl
dnl Outputs:
dnl
dnl - Set $1 to the CPPFLAGS necessary to compile with libnl
dnl - Set $2 to the LIBS necessary to link with libnl
dnl - If $3 is 1, AC_MSG_ERROR (i.e., abort) if neither libnl or
dnl   libnl3 can be found
dnl - Set HAVE_LIBNL3 to 1 if libnl3 will be used; 0 if libnl1 will be used
dnl - AC_SUBST $HAVE_LIBNL3
dnl - AC_DEFINE HAVE_LIBNL3
dnl
dnl --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_USNIC_CHECK_LIBNL3],[
       # More libnl v1/v3 sadness: the two versions are not compatible
       # and will not work correctly if simultaneously linked into the
       # same applications.  Unfortunately, they *will* link into the
       # same image!  On platforms like SLES 12, libibverbs depends on
       # libnl-3.so.200 and friends, while a naive implementation of
       # our configure logic would link libnl.so.1 to libdaplusnic,
       # resulting in both versions in the dependency map at the same
       # time.  As a coarse fix, just check for libnl-3 first and use
       # it if present on the system.

       # GROSS: libnl wants us to either use pkg-config (which we
       # can't assume is always present) or we need to look in a
       # particular directory for the right libnl3 include files.  For
       # now, just hard code the special path into this logic.

       save_CPPFLAGS=$CPPFLAGS
       save_LIBS=$LIBS

       $1="-I/usr/include/libnl3"
       CPPFLAGS="$$1 $CPPFLAGS"
       AC_MSG_CHECKING([for /usr/include/libnl3])
       AS_IF([test -d "/usr/include/libnl3"],
             [AC_MSG_RESULT([present])
              AC_CHECK_HEADER(
                [netlink/version.h],
                [AC_COMPILE_IFELSE(
                    [AC_LANG_PROGRAM([[
#include <netlink/netlink.h>
#include <netlink/version.h>
#ifndef LIBNL_VER_MAJ
#error "LIBNL_VER_MAJ not defined!"
#endif
/* to the best of our knowledge, version.h only exists in libnl3 */
#if LIBNL_VER_MAJ < 3
#error "LIBNL_VER_MAJ < 3, this is very unusual"
#endif
                        ]],[[/* empty body */]])],
                    [HAVE_LIBNL3=1],    dnl our program compiled
                    [HAVE_LIBNL3=0])],  dnl our program failed to compile
                [HAVE_LIBNL3=0],  dnl AC_CHECK_HEADER failed
                [#include <netlink/netlink.h>
                ])],
             [AC_MSG_RESULT([missing])
              HAVE_LIBNL3=0])  dnl "/usr/include/libnl3" does not exist

       # nl_recvmsgs_report is a symbol that is only present in v3
       AS_IF([test "$HAVE_LIBNL3" -eq 1],
             [AC_SEARCH_LIBS([nl_recvmsgs_report], [nl-3],
                             [# We also need libnl-route-3
                              AC_SEARCH_LIBS([nl_rtgen_request], [nl-route-3],
                                             [$2="-lnl-3 -lnl-route-3"
                                              HAVE_LIBNL3=1],
                                             [HAVE_LIBNL3=0])],
                             [HAVE_LIBNL3=0])])

       AS_IF([test "$HAVE_LIBNL3" -eq 1],
             [AC_MSG_NOTICE([using libnl-3])],
             [# restore $1 since we are falling back to libnl (v1)
              $1=""
              AC_SEARCH_LIBS([nl_connect], [nl],
                             [$2="-lnl"],
                             [AC_MSG_WARN([Cannot find libnl-3 nor libnl])
                              AS_IF([test "$3" = "1"],
                                    [AC_MSG_ERROR([Cannot continue])])
                             ])
              AC_MSG_NOTICE([using libnl (v1)])])

       # libnl_utils.h does not include configure-generated config.h,
       # so it may not see the HAVE_LIBNL3 #define.  Hence, we set
       # HAVE_LIBNL3 as both a C preprocessor macro (in case some
       # other file includes config.h before libnl_utils.h) and a
       # Makefile macro (so that the app can set HAVE_LIBNL3 via
       # CPPFLAGS).  Also, this macro may be used in multiple
       # different libraries; setting HAVE_LIBNL3 both ways lets the
       # application choose which way to set it.
       AC_SUBST([HAVE_LIBNL3])
       AC_DEFINE_UNQUOTED([HAVE_LIBNL3],[$HAVE_LIBNL3],
                          [set to 1 if should use libnl v3, set to 0 for libnl v11])

       LIBS=$save_LIBS
       AS_UNSET([save_LIBS])
       CPPFLAGS=$save_CPPFLAGS
       AS_UNSET([save_CPPFLAGS])
])

# --------------------------------------------------------
# Internal helper macro to setup the embedded PSM provider
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_PSM_SETUP],[
    opal_common_libfabric_psm_happy=0
])

# --------------------------------------------------------
# Internal helper macro to look for the things the psm provider
# needs
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_PSM],[
    opal_common_libfabric_psm_happy=1
    AC_CHECK_HEADER([psm.h], [], [opal_common_libfabric_psm_happy=0])
    AC_CHECK_LIB([psm_infinipath], [psm_init], [],
                 [opal_common_libfabric_psm_happy=0])

    AC_DEFINE_UNQUOTED([HAVE_PSM], [$opal_common_libfabric_psm_happy],
          [libfabric: whether to build the PSM provider or not])
    AC_DEFINE([HAVE_PSM_DL], 0,
          [libfabric: do not build PSM provider as a DL])

    AS_IF([test $opal_common_libfabric_psm_happy -eq 1],
          [opal_common_libfabric_CPPFLAGS="$opal_common_libfabric_CPPFLAGS -I$OPAL_TOP_SRCDIR/opal/mca/common/libfabric/libfabric/prov/psm/src"
           opal_common_libfabric_embedded_LIBADD="-lpsm_infinipath"])
])

# --------------------------------------------------------
# Internal helper macro for psm AM conditionals (that must be run
# unconditionally)
# --------------------------------------------------------
AC_DEFUN([_OPAL_COMMON_LIBFABRIC_EMBEDDED_PROVIDER_PSM_CONDITIONALS],[
    AM_CONDITIONAL([OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_PSM],
                   [test $opal_common_libfabric_psm_happy -eq 1])
])
