# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2023 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2017 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2022 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
# Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2021      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2023-2024 Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

############################################################################
# Initialization, version number, and other random setup/init stuff
############################################################################

# Load in everything found by autogen.pl
m4_include([config/autogen_found_items.m4])

# Init autoconf
# We don't have the version number to put in here yet, and we can't
# call PRTE_GET_VERSION (etc.) before AC_INIT.  So use the shell
# version.  project_name_* comes from config/project_list.m4, which
# was set during autogen.pl.

AC_INIT([prte],
        [m4_normalize(esyscmd([config/prte_get_version.sh VERSION --tarball]))],
        [https://github.com/openpmix/prrte/], [prrte])

AC_PREREQ(2.69)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

OAC_PUSH_PREFIX([PRTE])

# Get the absolute version of the srcdir.  We don't use "readlink -f",
# because that unfortunately isn't portable (cough cough macOS cough
# cough).
save=$(pwd)
cd $srcdir
abs_srcdir=$(pwd)
cd $save

# autotools expects to perform tests without interference
# from user-provided CFLAGS, particularly -Werror flags.
# Search for them here and cache any we find
PRTE_CFLAGS_cache=
PRTE_CFLAGS_pass=
for val in $CFLAGS; do
    if echo "$val" | grep -q -e "-W"; then
        PRTE_CFLAGS_cache="$PRTE_CFLAGS_cache $val";
    else
        PRTE_CFLAGS_pass="$PRTE_CFLAGS_pass $val";
    fi
done
CFLAGS=$PRTE_CFLAGS_pass
# Load the version number code
m4_include([config/prte_get_version.m4])

PRTE_CAPTURE_CONFIGURE_CLI([PRTE_CONFIGURE_CLI])

# Get our platform support file.  This has to be done very, very early
# because it twiddles random bits of autoconf
PRTE_LOAD_PLATFORM

#
# Start it up
#

PRTE_CONFIGURE_SETUP

prte_show_title "Configuring PRTE"

#
# Setup some things that must be done before AM-INIT-AUTOMAKE
#

prte_show_subtitle "Startup tests"

AC_PROG_SED

AC_CHECK_PROG([PERL],[perl],[perl],[no])
AS_IF([test "X$PERL" = "Xno"],
      [AC_MSG_ERROR(["PRTE requires perl. Aborting"])])

AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_DEFINE_UNQUOTED(PRTE_ARCH, "$target", [PRTE architecture string])

AS_IF([test "$host" != "$target"],
      [AC_MSG_WARN([Cross-compile detected])
       AC_MSG_WARN([Cross-compiling is only partially supported])
       AC_MSG_WARN([Proceed at your own risk!])])

# AC_USE_SYSTEM_EXTENSIONS alters CFLAGS (e.g., adds -g -O2)

PRTE_VAR_SCOPE_PUSH([CFLAGS_save])
CFLAGS_save=$CFLAGS
AC_USE_SYSTEM_EXTENSIONS

# AC_USE_SYSTEM_EXTENSIONS will modify CFLAGS if nothing was in there
# beforehand.  We don't want that.  So if there was nothing in
# CFLAGS, put nothing back in there.

AS_IF([test -z "$CFLAGS_save"], [CFLAGS=])

PRTE_VAR_SCOPE_POP

#
# Init automake
#

AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.12.2])

# SILENT_RULES is new in AM 1.11, but we require 1.11 or higher via
# autogen.  Limited testing shows that calling SILENT_RULES directly
# works in more cases than adding "silent-rules" to INIT_AUTOMAKE
# (even though they're supposed to be identical).  Shrug.

AM_SILENT_RULES([yes])

# set the language
AC_LANG([C])
# find NM
AC_PROG_GREP
LT_PATH_NM

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

# Sanity checks
AC_DEFUN([PRTE_CHECK_DIR_FOR_SPACES],[
    dir="$1"
    article="$2"
    label="$3"

    AC_MSG_CHECKING([directory of $label])
    AC_MSG_RESULT([$dir])
    AS_IF([test -n "`echo $dir | grep ' '`"],
          [AC_MSG_WARN([This version of PRRTE does not support $article $label])
           AC_MSG_WARN([with a path that contains spaces])
           AC_MSG_ERROR([Cannot continue.])])
])

PRTE_VAR_SCOPE_PUSH(prte_checkdir)
prte_checkdir=`pwd`
PRTE_CHECK_DIR_FOR_SPACES([$prte_checkdir], [a], [build tree])
PRTE_CHECK_DIR_FOR_SPACES([$prefix], [a], [prefix])
PRTE_CHECK_DIR_FOR_SPACES([$srcdir], [a], [source tree])
PRTE_VAR_SCOPE_POP

prte_show_subtitle "Checking versions"

# Get the version of PRTE that we are installing
PRTE_SAVE_VERSION([PRTE], [PMIx Reference Run-Time Environment],
                          [$srcdir/VERSION],
                          [src/include/version.h])

# Get shared library version numbers
. $srcdir/VERSION

AC_SUBST(libprrte_so_version)
AC_SUBST(libprrte_common_alps_so_version)

#
# Get the versions of the autotools that were used to bootstrap us
# (helpful for debugging reports)
#

AC_MSG_CHECKING([for bootstrap Autoconf version])
acversion=`grep "Generated by GNU Autoconf" $0 | head -n 1 | awk '{ print $6 }'`
AC_MSG_RESULT([$acversion])

AC_MSG_CHECKING([for bootstrap Automake version])
AC_MSG_RESULT([$am__api_version])

AC_MSG_CHECKING([for boostrap Libtool version])
ltversion=`grep VERSION= $srcdir/config/ltmain.sh | head -n 1 | cut -d= -f2`
AC_MSG_RESULT([$ltversion])

# List header files to generate

AC_CONFIG_HEADERS([src/include/prte_config.h])

prte_show_subtitle "Initialization, setup"

PRTE_TOP_BUILDDIR="`pwd`"
AC_SUBST(PRTE_TOP_BUILDDIR)
cd "$srcdir"
PRTE_TOP_SRCDIR="`pwd`"
AC_SUBST(PRTE_TOP_SRCDIR)
cd "$PRTE_TOP_BUILDDIR"
AC_MSG_NOTICE([builddir: $PRTE_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $PRTE_TOP_SRCDIR])

if test "$PRTE_TOP_BUILDDIR" != "$PRTE_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# Setup the top of the src/include/prte_config.h file

AH_TOP([/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef PRTE_CONFIG_H
#define PRTE_CONFIG_H

#include "prte_config_top.h"
])

AH_BOTTOM([
#include "prte_config_bottom.h"
#endif /* PRTE_CONFIG_H */
])

# Other basic setup stuff (shared with components)

PRTE_BASIC_SETUP

PRTE_TOP_SRCDIR="$PRTE_TOP_SRCDIR"
PRTE_TOP_BUILDDIR="$PRTE_TOP_BUILDDIR"
AC_SUBST(PRTE_TOP_SRCDIR)
AC_SUBST(PRTE_TOP_BUILDDIR)

############################################################################
# Configuration options
############################################################################

PRTE_CHECK_OS_FLAVORS
PRTE_CONFIGURE_OPTIONS

if test "$enable_binaries" = "no" && test "$enable_dist" = "yes"; then
    AC_MSG_WARN([--disable-binaries is incompatible with --enable dist])
    AC_MSG_ERROR([Cannot continue])
fi


############################################################################
# Libtool: part one
# (before C compiler setup)
############################################################################

#
# Part one of libtool magic.  Default to: enable shared, disable static.
#

AC_ENABLE_SHARED([])
AC_DISABLE_STATIC([])

##################################
# Check for known incompatibility
##################################

# Do *not* print a message that we're checking the OS because this
# test is *not* meant to be an all-inclusive "if it passes this test,
# then configure must succeed" test.  This test is *only* mean to
# screen out the versions of OS X where we know OMPI will cause kernel
# panics because of bad implementations of pty's.  See
# https://svn.open-mpi.org/trac/ompi/ticket/1637 for details.
#   OS X name         OS X Version    $host_os value
# OS X Tiger             10.4.x           darwin8.x
# OS X Leopard           10.5.x           darwin9.x
# OS X Snow Leopard      10.6.x           darwin10.x
# OS X Lion              10.7.x           darwin11.x
# We do not support OS X before version 10.5 (Leopard)
case $host_os in
# Corresponds to OS X 10.0 - 10.4 (additional [] quoting for m4)
darwin[[45678]]*)
    AC_MSG_WARN([PRTE does not support OS X prior to version 10.5 (Leopard)])
    AC_MSG_ERROR([Cannot continue])
esac

############################################################################
# Check for compilers and preprocessors
############################################################################

prte_show_title "Compiler and preprocessor tests"

##################################
# C compiler characteristics
##################################

PRTE_SETUP_CC

# Does the compiler support "ident"-like constructs?

PRTE_CHECK_IDENT([CC], [CFLAGS], [c], [C])

#
# Check for type sizes
#

AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(void *)
AS_IF([test "$ac_cv_sizeof_void_p" -eq 4],
      [AC_MSG_WARN([PRRTE does not support 32 bit builds.])
       AC_MSG_ERROR([Cannot continue])])
AC_CHECK_SIZEOF(size_t)

#
# Check for type alignments
#

AC_CHECK_ALIGNOF(bool, [AC_INCLUDES_DEFAULT
                        #include <stdbool.h>])
AC_CHECK_ALIGNOF(int32_t)
AC_CHECK_ALIGNOF(int64_t)
AC_CHECK_ALIGNOF(int)
AC_CHECK_ALIGNOF(long)
AC_CHECK_ALIGNOF(long long)
AC_CHECK_ALIGNOF(double)
AC_CHECK_ALIGNOF(size_t)

# Check if we support the offsetof compiler directive
PRTE_CHECK_OFFSETOF

##################################
# Only after setting up C check compiler attributes.
##################################

prte_show_subtitle "Compiler characteristics"

PRTE_CHECK_ATTRIBUTES
PRTE_CHECK_COMPILER_VERSION_ID

# PRTE only supports GCC >=v4.8.1.  Notes:
#
# 1. The default compiler that comes with RHEL 7 is v4.8.5
#    (version ID 264197).
# 2. We regularly test with GCC v4.8.1 (version ID 264193).
# 3. GCC 4.8.0 probably also works; we just haven't tested it.
#
# Since we regularly test with 4.8.1, that's what we check for.
AS_IF([test "$prte_cv_compiler_FAMILYNAME" = "GNU" && \
           test "$prte_cv_compiler_VERSION" -lt 264193],
      [AC_MSG_WARN([PRTE no longer supports versions of the GNU compiler suite])
       AC_MSG_WARN([less than v4.8.1.])
       AC_MSG_WARN([Please upgrade your GNU compiler suite, or use])
       AC_MSG_WARN([a different compiler to build PRTE.])
       AC_MSG_ERROR([Cannot continue])
      ])

##################################
# Assembler Configuration
##################################

prte_show_subtitle "Atomics"

PRTE_CONFIG_ASM

##################################
# Header files
##################################

prte_show_title "Header file tests"

PRTE_VAR_SCOPE_PUSH(PRTE_CFLAGS_save_for_headers)
PRTE_CFLAGS_save_for_headers=$CFLAGS
_PRTE_CHECK_SPECIFIC_CFLAGS(-Werror, Werror)

AC_CHECK_HEADERS([arpa/inet.h dirent.h \
                  dlfcn.h endian.h execinfo.h fcntl.h \
                  libutil.h netdb.h netinet/in.h netinet/tcp.h \
                  poll.h  pty.h pwd.h sched.h \
                  strings.h linux/ethtool.h linux/sockios.h \
                  sys/fcntl.h \
                  sys/ioctl.h sys/param.h sys/queue.h \
                  sys/resource.h sys/select.h sys/socket.h \
                  sys/stat.h sys/time.h \
                  sys/types.h sys/uio.h sys/un.h net/uio.h sys/utsname.h sys/wait.h syslog.h \
                  termios.h unistd.h util.h malloc.h \
                  paths.h \
                  ioLib.h sockLib.h hostLib.h stdatomic.h])

# Needed to work around Darwin requiring sys/socket.h for
# net/if.h

AC_CHECK_HEADERS([net/if.h], [], [],
                 [#include <stdio.h>
                  #if STDC_HEADERS
                  # include <stdlib.h>
                  # include <stddef.h>
                  #else
                  # if HAVE_STDLIB_H
                  #  include <stdlib.h>
                  # endif
                  #endif
                  #if HAVE_SYS_SOCKET_H
                  # include <sys/socket.h>
                  #endif
                 ])

CFLAGS=$PRTE_CFLAGS_save_for_headers
PRTE_VAR_SCOPE_POP

# checkpoint results
AC_CACHE_SAVE

##################################
# Types
##################################

prte_show_title "Type tests"

AC_CHECK_TYPES([socklen_t, struct sockaddr_in, struct sockaddr_in6,
                struct sockaddr_storage, struct ifreq, struct ethtool_cmd],
               [], [], [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_NET_IF_H
#include <net/if.h>
#endif])

AC_CHECK_DECLS([ethtool_cmd_speed, SIOCETHTOOL],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_MEMBERS([struct ethtool_cmd.speed_hi],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif])

AC_CHECK_DECLS([AF_UNSPEC, PF_UNSPEC, AF_INET6, PF_INET6],
               [], [], [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

AC_CHECK_MEMBERS([siginfo_t.si_fd],,,[#include <signal.h>])

AC_CHECK_MEMBERS([siginfo_t.si_band],,,[#include <signal.h>])

# checkpoint results
AC_CACHE_SAVE

##################################
# Linker characteristics
##################################

AC_MSG_CHECKING([the linker for support for the -fini option])
PRTE_VAR_SCOPE_PUSH([LDFLAGS_save])
LDFLAGS_save=$LDFLAGS
LDFLAGS="$LDFLAGS_save -Wl,-fini -Wl,finalize"
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        void finalize (void) {}
        ]])],
        [AC_MSG_RESULT([yes])
         prte_ld_have_fini=1],
        [AC_MSG_RESULT([no])
         prte_ld_have_fini=0])
LDFLAGS=$LDFLAGS_save
PRTE_VAR_SCOPE_POP

##################################
# Libraries
##################################

prte_show_title "Library and Function tests"

PRTE_SEARCH_LIBS_CORE([gethostbyname], [nsl])

PRTE_SEARCH_LIBS_CORE([socket], [socket])

# Solaris has sched_yield in -lrt, usually in libc
PRTE_SEARCH_LIBS_CORE([sched_yield], [rt])

# Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
PRTE_SEARCH_LIBS_CORE([ceil], [m])

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf fork  setsid strsignal syslog setpgid fileno_unlocked])

# On some hosts, htonl is a define, so the AC_CHECK_FUNC will get
# confused.  On others, it's in the standard library, but stubbed with
# the magic glibc foo as not implemented.  and on other systems, it's
# just not there.  This covers all cases.

AC_CACHE_CHECK([for htonl define],
  [ompi_cv_htonl_define],
  [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif],[
#ifndef ntohl
#error "ntohl not defined"
#endif
])], [prte_cv_htonl_define=yes], [prte_cv_htonl_define=no])])

AC_CHECK_FUNC([htonl], [prte_have_htonl=yes], [prte_have_htonl=no])

AS_IF([test "$prte_cv_htonl_define" = "yes" || test "$prte_have_htonl" = "yes"],
    [AC_DEFINE_UNQUOTED([HAVE_UNIX_BYTESWAP], [1],
        [whether unix byteswap routines -- htonl, htons, nothl, ntohs -- are available])])

AC_CHECK_DECLS(__func__)

# checkpoint results
AC_CACHE_SAVE

##################################
# System-specific tests
##################################

prte_show_title "System-specific tests"

# all: endian
AC_C_BIGENDIAN

#
# Check out what thread support we have
#

PRTE_CONFIG_THREADS

CFLAGS="$CFLAGS $THREAD_CFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS="$LIBS $THREAD_LIBS"

#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S
AC_PROG_GREP
AC_PROG_EGREP

# This check must come after PRTE_CONFIG_THREADS
AC_CHECK_FUNCS([pthread_setaffinity_np])

#
# We need as and lex
#

AM_PROG_AS
AC_PROG_LEX(noyywrap)

# If we don't have Flex and we don't have a generated .c file
# (distribution tarballs will have the .c file included, but git
# clones will not), then error.  Must have Flex -- other versions of
# Lex are not workable (all things being equal, since this is *only*
# required for developers, we decided that it really was not worth it
# to be portable between different versions of lex ;-).

if test "x$LEX" != xflex && test ! -e $PRTE_TOP_SRCDIR/src/util/hostfile/hostfile_lex.c; then
    AC_MSG_WARN([*** Could not find Flex on your system.])
    AC_MSG_WARN([*** Flex is required for developer builds of PRRTE.])
    AC_MSG_WARN([*** Other versions of Lex are not supported.])
    AC_MSG_WARN([*** NOTE: If you are building from an official tarball])
    AC_MSG_WARN([*** (not the ones made by GitHub!) downloaded from the])
    AC_MSG_WARN([*** PRRTE web site, you do not need Flex.])
    AC_MSG_ERROR([Cannot continue])
fi

#
# Do we have RLIMIT_NPROC in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_NPROC], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_MEMLOCK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_MEMLOCK], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_NOFILE in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_NOFILE], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_MEMLOCK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_FSIZE], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_CORE in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_CORE], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_STACK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_STACK], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_AS in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_AS], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Setup Sphinx processing
#

OAC_SETUP_SPHINX([$srcdir/docs/_build/html/index.html], [],
                 [$srcdir/docs/requirements.txt])
AC_CHECK_PROGS(PYTHON, [python3 python python2])

AS_IF([test -n "$OAC_MAKEDIST_DISABLE"],
      [AS_IF([test -n "$PRTE_MAKEDIST_DISABLE"],
             [PRTE_MAKEDIST_DISABLE="$PRTE_MAKEDIST_DISABLE $OAC_MAKEDIST_DISABLE"],
             [PRTE_MAKEDIST_DISABLE=$OAC_MAKEDIST_DISABLE])])
AS_IF([test -n "$PRTE_MAKEDIST_DISABLE"],
      [AC_MSG_WARN(["make dist" will be disabled due to: $PRTE_MAKEDIST_DISABLE])])
AC_SUBST([PRTE_MAKEDIST_DISABLE])

# checkpoint results
AC_CACHE_SAVE

##################################
# Libevent
##################################
prte_show_title "Event libraries"

dnl Only one of Libev or Libevent can be used by PRTE.  The
dnl selection logic for the two is:
dnl
dnl   * libev is used if explicitly requested
dnl   * libevent is used if explicitly requested
dnl   * if both are explicitly requested, then we report the error
dnl     and abort
dnl   * if neither is explicitly requested, then we default to
dnl     using libevent if it is available. If libevent isn't
dnl     available, then we see if libev is available.
dnl
dnl poking at $with_libevent and $with_libev is a bit of an
dnl abstraction break, but makes implementing this logic
dnl significantly easier.
prte_libev_support=0
prte_libevent_support=0

AS_IF([test ! -z "$with_libevent" -a "$with_libevent" != "no"],
      [want_libevent=1])
AS_IF([test ! -z "$with_libev" -a "$with_libev" != "no"],
      [want_libev=1])

AS_IF([test "$want_libevent" = "1" -a "$want_libev" = "1"],
      [AC_MSG_WARN([Both libevent and libev support have been specified.])
       AC_MSG_WARN([Only one can be configured against at a time. Please])
       AC_MSG_WARN([remove one from the configure command line.])
       AC_MSG_ERROR([Cannot continue])])

prte_found_event_lib=0
dnl If libevent succeeds, then we don't need libev, but we skip
dnl libevent if libev was explicitly requested.
AS_IF([test "$want_libev" != "1"],
      [PRTE_LIBEVENT_CONFIG([prte_found_event_lib=1])])
AS_IF([test $prte_found_event_lib -eq 0],
      [PRTE_LIBEV_CONFIG([prte_found_event_lib=1])])

dnl The following must _always_ be defined, regardless of which
dnl event library was selected/requested
AC_DEFINE_UNQUOTED([PRTE_HAVE_LIBEV], [$prte_libev_support], [Whether we are building against libev])
AC_DEFINE_UNQUOTED([PRTE_HAVE_LIBEVENT], [$prte_libevent_support], [Whether we are building against libevent])

AS_IF([test $prte_found_event_lib -eq 0],
      [AC_MSG_WARN([Either libevent or libev support is required, but neither])
       AC_MSG_WARN([was found. Please use the configure options to point us])
       AC_MSG_WARN([to where we can find one or the other library])
       AC_MSG_ERROR([Cannot continue])])

##################################
# PMIx
##################################

prte_show_title "Configure PMIx"

PRTE_CHECK_PMIX

##################################
# HWLOC
##################################

prte_show_title "Configure HWLOC"

PRTE_SETUP_HWLOC

##################################
# MCA
##################################

prte_show_title "Modular Component Architecture (MCA) setup"

AC_MSG_CHECKING([for subdir args])
PRTE_CONFIG_SUBDIR_ARGS([prte_subdir_args])
AC_MSG_RESULT([$prte_subdir_args])

PRTE_MCA

# checkpoint results
AC_CACHE_SAVE

##################################
# Visibility
##################################
# Check the visibility declspec at the end to avoid problem with
# the previous tests that are not necessarily prepared for
# the visibility feature.

prte_show_title "Symbol visibility feature"

PRTE_CHECK_VISIBILITY

##################################
# STOP-ON-EXEC
##################################
# Check ptrace support for stop-on-exec

prte_show_title "Ptrace stop-on-exec support"

PRTE_CHECK_PTRACE

############################################################################
# Final top-level PRTE configuration
############################################################################

prte_show_title "Final top-level PRTE configuration"

############################################################################
# Libtool: part two
# (after C compiler setup = no compiler/linker tests after this)
############################################################################
prte_show_subtitle "Libtool configuration"

# Use the undocumented solaris_use_stlport4 libtool variable to turn off any
# Cstd/stlport4 linkage. This allows PRTE to be C++ STL agnostic.
if test "x$prte_cv_c_compiler_vendor" = "xsun"; then
    solaris_use_stlport4="yes"
fi

LT_INIT()
LT_LANG([C])


# What's the suffix of shared libraries?  Inspired by generated
# Libtool code (even though we don't support several of these
# platforms, there didn't seem to be any harm in leaving in some of
# them, alhtough I did remove some that we have never/will never
# support, like OS/2).
PRTE_DYN_LIB_PREFIX=lib
case $host_os in
cygwin*)
    PRTE_DYN_LIB_PREFIX=cyg
    PRTE_DYN_LIB_SUFFIX=dll
    ;;
mingw* | pw32* | cegcc*)
    PRTE_DYN_LIB_SUFFIX=dll
    ;;
darwin* | rhapsody*)
    PRTE_DYN_LIB_SUFFIX=dylib
    ;;
hpux9* | hpux10* | hpux11*)
    case $host_cpu in
        ia64*)
        PRTE_DYN_LIB_SUFFIX=so
        ;;
    *)
        PRTE_DYN_LIB_SUFFIX=sl
        ;;
    esac
    ;;
*)
   PRTE_DYN_LIB_SUFFIX=so
   ;;
esac

AC_SUBST(PRTE_DYN_LIB_PREFIX)
AC_SUBST(PRTE_DYN_LIB_SUFFIX)

# Need the libtool binary before the rpathify stuff
LT_OUTPUT

############################################################################
# final compiler config
############################################################################
prte_show_subtitle "Compiler flags"

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory.  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#

# Because prte_config.h is created by AC_CONFIG_HEADERS, we
# don't need to -I the builddir for prte/include. However, we do
# need to add it for prte as it doesn't have an AC_CONFIG_HEADERS that
# will install it for us. If we VPATH building, we do need to include the
# source directories, however.
#

if test "$PRTE_TOP_BUILDDIR" != "$PRTE_TOP_SRCDIR"; then
    # Note the embedded m4 directives here -- we must embed them
    # rather than have successive assignments to these shell
    # variables, lest the $(foo) names try to get evaluated here.
    # Yuck!
    cpp_includes="$PRTE_TOP_SRCDIR $PRTE_TOP_BUILDDIR $PRTE_TOP_SRCDIR/src/include $PRTE_TOP_BUILDDIR/src/include $PRTE_TOP_BUILDDIR/include"
else
    cpp_includes="$PRTE_TOP_SRCDIR $PRTE_TOP_SRCDIR/src/include"
fi
CPP_INCLUDES="$(echo $cpp_includes | $SED 's/[[^ \]]* */'"$prte_cc_iquote"'&/g')"
CPPFLAGS="$CPP_INCLUDES $CPPFLAGS $PRTE_FINAL_CPPFLAGS"
LDFLAGS="$LDFLAGS $PRTE_FINAL_LDFLAGS"
LIBS="$LIBS $PRTE_FINAL_LIBS"

# restore any user-provided Werror flags
AS_IF([test ! -z "$PRTE_CFLAGS_cache"], [CFLAGS="$CFLAGS $PRTE_CFLAGS_cache"])

# setup "picky" compiler options if enabled
PRTE_SETUP_PICKY_COMPILERS

#
# Delayed the substitution of CFLAGS and CXXFLAGS until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)

AC_MSG_CHECKING([final CPPFLAGS])
AC_MSG_RESULT([$CPPFLAGS])

AC_MSG_CHECKING([final CFLAGS])
AC_MSG_RESULT([$CFLAGS])

AC_MSG_CHECKING([final LDFLAGS])
AC_MSG_RESULT([$LDFLAGS])

AC_MSG_CHECKING([final LIBS])
AC_MSG_RESULT([$LIBS])


#
# Aggregate MCA parameters directory
#
AC_SUBST([AMCA_PARAM_SETS_DIR], ['$(prtedatadir)/amca-param-sets'])

# If there is a local hook, call it.
m4_ifdef([prte_CONFIG_LOCAL], [prte_CONFIG_LOCAL])

############################################################################
# Party on
############################################################################

AC_MSG_CHECKING([if libtool needs -no-undefined flag to build shared libraries])

case "`uname`" in
  CYGWIN*|MINGW*|AIX*)
    ## Add in the -no-undefined flag to LDFLAGS for libtool.
    AC_MSG_RESULT([yes])
    LDFLAGS="$LDFLAGS -no-undefined"
    ;;
  *)
    ## Don't add in anything.
    AC_MSG_RESULT([no])
    ;;
esac

# prtedatadir, prtelibdir, and prteinclude are essentially the same as
# pkg*dir, but will always be */prte.

prtedatadir='${datadir}/prte'
prtelibdir='${libdir}/prte'
prteincludedir='${includedir}/prte'
AC_SUBST(prtedatadir)
AC_SUBST(prtelibdir)
AC_SUBST(prteincludedir)

prte_want_prd=0

prte_show_subtitle "Final output"

AC_CONFIG_FILES([
    Makefile
    config/Makefile
    contrib/Makefile
    include/Makefile
    include/prte_version.h
    docs/Makefile
    src/docs/Makefile
    src/docs/show-help-files/Makefile
    src/docs/prrte-rst-content/Makefile
])

PRTE_CONFIG_FILES

AC_OUTPUT

PRTE_SUMMARY_PRINT
