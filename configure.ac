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
# Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2011 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
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

# Load the version number code
m4_include([config/opal_get_version.m4])
AC_LANG([C])

# Init autoconf
# We don't have the version number to put in here yet, and we can't
# call OPAL_GET_VERSION (etc.) before AC_INIT.  So use the shell
# version.  project_name_* comes from config/project_list.m4, which
# was set during autogen.pl.

AC_INIT([project_name_long],
        [m4_normalize(esyscmd([config/opal_get_version.sh VERSION --tarball]))],
        [https://pmix.github.io/master/], [project_name_short])

AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
OPAL_CAPTURE_CONFIGURE_CLI([OPAL_CONFIGURE_CLI])

#
# Start it up
#
OPAL_CONFIGURE_SETUP

opal_show_title "Configuring project_name_long"

#
# Setup some things that must be done before AM-INIT-AUTOMAKE
#

opal_show_subtitle "Startup tests"

AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_DEFINE_UNQUOTED(OPAL_ARCH, "$target", [PMIx Reference Server architecture string])

AS_IF([test "$host" != "$target"],
      [AC_MSG_WARN([Cross-compile detected])
       AC_MSG_WARN([Cross-compiling is only partially supported])
       AC_MSG_WARN([Proceed at your own risk!])])

# AC_USE_SYSTEM_EXTENSIONS alters CFLAGS (e.g., adds -g -O2)

OPAL_VAR_SCOPE_PUSH([CFLAGS_save])
CFLAGS_save=$CFLAGS
AC_USE_SYSTEM_EXTENSIONS

# AC_USE_SYSTEM_EXTENSIONS will modify CFLAGS if nothing was in there
# beforehand.  We don't want that.  So if there was nothing in
# CFLAGS, put nothing back in there.

AS_IF([test -z "$CFLAGS_save"], [CFLAGS=])

OPAL_VAR_SCOPE_POP

#
# Init automake
#

AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.12.2 tar-ustar])

# SILENT_RULES is new in AM 1.11, but we require 1.11 or higher via
# autogen.  Limited testing shows that calling SILENT_RULES directly
# works in more cases than adding "silent-rules" to INIT_AUTOMAKE
# (even though they're supposed to be identical).  Shrug.

AM_SILENT_RULES([yes])

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

opal_show_subtitle "Checking versions"

# Get the version of PMIx Reference Server that we are installing
OPAL_SAVE_VERSION([ORTE], [Open Run-Time Environment],
                          [$srcdir/VERSION],
                          [orte/include/orte/version.h])
OPAL_SAVE_VERSION([OPAL], [Open Portable Access Layer], [$srcdir/VERSION],
                  [opal/include/opal/version.h])

# Get shared library version numbers
. $srcdir/VERSION

AC_SUBST(libopen_rte_so_version)
AC_SUBST(libopen_pal_so_version)

# It's icky that we have to hard-code the names of the
# common components here.  :-( This could probably be done
# transparently by adding some intelligence in autogen.pl
# and/or opal_mca.m4, but I don't have the cycles to do this
# right now.

AC_SUBST(libmca_opal_common_libfabric_so_version)
AC_SUBST(libmca_opal_common_cuda_so_version)
AC_SUBST(libmca_opal_common_sm_so_version)
AC_SUBST(libmca_opal_common_ugni_so_version)
AC_SUBST(libmca_opal_common_verbs_so_version)

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

AC_CONFIG_HEADERS([opal/include/opal_config.h])

opal_show_subtitle "Initialization, setup"

OMPI_TOP_BUILDDIR="`pwd`"
AC_SUBST(OMPI_TOP_BUILDDIR)
cd "$srcdir"
OMPI_TOP_SRCDIR="`pwd`"
AC_SUBST(OMPI_TOP_SRCDIR)
cd "$OMPI_TOP_BUILDDIR"
AC_MSG_NOTICE([builddir: $OMPI_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $OMPI_TOP_SRCDIR])

if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# Setup the top of the opal/include/opal_config.h file

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

#ifndef OPAL_CONFIG_H
#define OPAL_CONFIG_H

#include "opal_config_top.h"
])

AH_BOTTOM([
#include "opal_config_bottom.h"
#endif /* OPAL_CONFIG_H */
])

# Other basic setup stuff (shared with components)

OPAL_BASIC_SETUP

OPAL_TOP_SRCDIR="$OMPI_TOP_SRCDIR"
OPAL_TOP_BUILDDIR="$OMPI_TOP_BUILDDIR"
AC_SUBST(OPAL_TOP_SRCDIR)
AC_SUBST(OPAL_TOP_BUILDDIR)

ORTE_TOP_SRCDIR="$OMPI_TOP_SRCDIR"
ORTE_TOP_BUILDDIR="$OMPI_TOP_BUILDDIR"
AC_SUBST(ORTE_TOP_SRCDIR)
AC_SUBST(ORTE_TOP_BUILDDIR)

############################################################################
# Configuration options
############################################################################

OPAL_CONFIGURE_OPTIONS
OPAL_CHECK_OS_FLAVORS
OPAL_CHECK_PMIX
ORTE_CONFIGURE_OPTIONS

if test "$enable_binaries" = "no" && test "$enable_dist" = "yes"; then
    AC_MSG_WARN([--disable-binaries is incompatible with --enable dist])
    AC_MSG_ERROR([Cannot continue])
fi

# The library prefixes must be set before we call OPAL MCA.  Here is
# as good a place as any.

OPAL_SET_LIB_PREFIX([])
ORTE_SET_LIB_PREFIX([])

############################################################################
# Libtool: part one
# (before C compiler setup)
############################################################################

#
# Part one of libtool magic.  Default to: enable shared, disable static.
#

AM_ENABLE_SHARED
AM_DISABLE_STATIC
OPAL_SETUP_WRAPPER_INIT

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
    AC_MSG_WARN([Open MPI does not support OS X prior to version 10.5 (Leopard)])
    AC_MSG_ERROR([Cannot continue])
esac

############################################################################
# Check for compilers and preprocessors
############################################################################

opal_show_title "Compiler and preprocessor tests"

##################################
# C compiler characteristics
##################################

OPAL_SETUP_CC

# If we build on a windows environment with the windows compiler and linker
# then we need some translation functions from the opal/win32 directory.

AM_CONDITIONAL(OMPI_NEED_WINDOWS_REPLACEMENTS,
               test "$opal_cv_c_compiler_vendor" = "microsoft" )

# Do all Interix detections if necessary

OMPI_INTERIX

# Does the compiler support "ident"-like constructs?

OPAL_CHECK_IDENT([CC], [CFLAGS], [c], [C])

#
# Check for some types
#

AC_CHECK_TYPES(int8_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(int16_t)
AC_CHECK_TYPES(uint16_t)
AC_CHECK_TYPES(int32_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES(int64_t)
AC_CHECK_TYPES(uint64_t)
AC_CHECK_TYPES(int128_t)
AC_CHECK_TYPES(__int128)
AC_CHECK_TYPES(uint128_t)
AC_CHECK_TYPES(long long)
AC_CHECK_TYPES(__float128)
AC_CHECK_TYPES(long double)
# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
AC_CHECK_HEADERS(complex.h)
AC_CHECK_TYPES(float _Complex)
AC_CHECK_TYPES(double _Complex)
AC_CHECK_TYPES(long double _Complex)
AC_CHECK_TYPES(intptr_t)
AC_CHECK_TYPES(uintptr_t)
AC_CHECK_TYPES(mode_t)
AC_CHECK_TYPES(ssize_t)
AC_CHECK_TYPES(ptrdiff_t)

#
# Check for type sizes
#

AC_CHECK_SIZEOF(_Bool)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)

if test "$ac_cv_type_long_long" = yes; then
    AC_CHECK_SIZEOF(long long)
fi

AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)

if test "$ac_cv_type_long_double" = yes; then
    AC_CHECK_SIZEOF(long double)
fi

if test "$ac_cv_type___float128" = yes; then
    AC_CHECK_SIZEOF(__float128)
fi

# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_float__Complex" = yes; then
    AC_CHECK_SIZEOF(float _Complex)
fi
if test "$ac_cv_type_double__Complex" = yes; then
    AC_CHECK_SIZEOF(double _Complex)
fi
if test "$ac_cv_type_long_double__Complex" = yes; then
    AC_CHECK_SIZEOF(long double _Complex)
fi
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(size_t)

if test "$ac_cv_type_ssize_t" = yes ; then
    AC_CHECK_SIZEOF(ssize_t)
fi

if test "$ac_cv_type_ptrdiff_t" = yes; then
    AC_CHECK_SIZEOF(ptrdiff_t)
fi

AC_CHECK_SIZEOF(wchar_t)
AC_CHECK_SIZEOF(pid_t)

#
# Check for type alignments
#

OPAL_C_GET_ALIGNMENT(bool, OPAL_ALIGNMENT_BOOL)
OPAL_C_GET_ALIGNMENT(int8_t, OPAL_ALIGNMENT_INT8)
OPAL_C_GET_ALIGNMENT(int16_t, OPAL_ALIGNMENT_INT16)
OPAL_C_GET_ALIGNMENT(int32_t, OPAL_ALIGNMENT_INT32)
OPAL_C_GET_ALIGNMENT(int64_t, OPAL_ALIGNMENT_INT64)

if test "$ac_cv_type_int128_t" = yes ; then
  OPAL_C_GET_ALIGNMENT(int128_t, OPAL_ALIGNMENT_INT128)
fi

OPAL_C_GET_ALIGNMENT(char, OPAL_ALIGNMENT_CHAR)
OPAL_C_GET_ALIGNMENT(short, OPAL_ALIGNMENT_SHORT)
OPAL_C_GET_ALIGNMENT(wchar_t, OPAL_ALIGNMENT_WCHAR)
OPAL_C_GET_ALIGNMENT(int, OPAL_ALIGNMENT_INT)
OPAL_C_GET_ALIGNMENT(long, OPAL_ALIGNMENT_LONG)

if test "$ac_cv_type_long_long" = yes; then
    OPAL_C_GET_ALIGNMENT(long long, OPAL_ALIGNMENT_LONG_LONG)
fi

OPAL_C_GET_ALIGNMENT(float, OPAL_ALIGNMENT_FLOAT)
OPAL_C_GET_ALIGNMENT(double, OPAL_ALIGNMENT_DOUBLE)

if test "$ac_cv_type_long_double" = yes; then
    OPAL_C_GET_ALIGNMENT(long double, OPAL_ALIGNMENT_LONG_DOUBLE)
fi

if test "$ac_cv_type___float128" = yes; then
    OPAL_C_GET_ALIGNMENT(__float128, OPAL_ALIGNMENT___FLOAT128)
fi

# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_float__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(float _Complex, OPAL_ALIGNMENT_FLOAT_COMPLEX)
fi
if test "$ac_cv_type_double__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(double _Complex, OPAL_ALIGNMENT_DOUBLE_COMPLEX)
fi
if test "$ac_cv_type_long_double__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(long double _Complex, OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX)
fi

OPAL_C_GET_ALIGNMENT(void *, OPAL_ALIGNMENT_VOID_P)
OPAL_C_GET_ALIGNMENT(size_t, OPAL_ALIGNMENT_SIZE_T)

#
# Check for other compiler characteristics
#

OPAL_C_WEAK_SYMBOLS
OPAL_C_MACRO_WEAK_SYMBOLS


if test "x$CC" = "xicc"; then
    OPAL_CHECK_ICC_VARARGS
fi

# Check if we support the offsetof compiler directive
OPAL_CHECK_OFFSETOF

##################################
# Only after setting up C check compiler attributes.
##################################

opal_show_subtitle "Compiler characteristics"

OPAL_CHECK_ATTRIBUTES
OPAL_CHECK_COMPILER_VERSION_ID

##################################
# Assembler Configuration
##################################

opal_show_subtitle "Assembler"

AM_PROG_AS
OPAL_CONFIG_ASM

##################################
# Header files
##################################

opal_show_title "Header file tests"

AC_CHECK_HEADERS([alloca.h aio.h arpa/inet.h dirent.h \
    dlfcn.h execinfo.h err.h fcntl.h grp.h libgen.h \
    libutil.h memory.h netdb.h netinet/in.h netinet/tcp.h \
    poll.h pthread.h pty.h pwd.h sched.h \
    strings.h stropts.h linux/ethtool.h linux/sockios.h \
    sys/fcntl.h sys/ipc.h sys/shm.h \
    sys/ioctl.h sys/mman.h sys/param.h sys/queue.h \
    sys/resource.h sys/select.h sys/socket.h sys/sockio.h \
    sys/stat.h sys/statfs.h sys/statvfs.h sys/time.h sys/tree.h \
    sys/types.h sys/uio.h sys/un.h net/uio.h sys/utsname.h sys/vfs.h sys/wait.h syslog.h \
    termios.h ulimit.h unistd.h util.h utmp.h malloc.h \
    ifaddrs.h crt_externs.h regex.h mntent.h paths.h \
    ioLib.h sockLib.h hostLib.h shlwapi.h sys/synch.h db.h ndbm.h])

AC_CHECK_HEADERS([sys/mount.h], [], [],
[AC_INCLUDES_DEFAULT
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])

AC_CHECK_HEADERS([sys/sysctl.h], [], [],
[AC_INCLUDES_DEFAULT
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])

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

# checkpoint results
AC_CACHE_SAVE

##################################
# Types
##################################

opal_show_title "Type tests"

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

# SA_RESTART in signal.h

OPAL_VAR_SCOPE_PUSH([MSG])
AC_MSG_CHECKING([if SA_RESTART defined in signal.h])
AC_EGREP_CPP(yes, [
#include <signal.h>
#ifdef SA_RESTART
    yes
#endif ], [MSG=yes VALUE=1], [MSG=no VALUE=0])
AC_DEFINE_UNQUOTED(OPAL_HAVE_SA_RESTART, $VALUE,
    [Whether we have SA_RESTART in <signal.h> or not])
AC_MSG_RESULT([$MSG])
OPAL_VAR_SCOPE_POP

AC_CHECK_MEMBERS([struct sockaddr.sa_len], [], [], [
#include <sys/types.h>
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])

AC_CHECK_MEMBERS([struct dirent.d_type], [], [], [
#include <sys/types.h>
#include <dirent.h>])

AC_CHECK_MEMBERS([siginfo_t.si_fd],,,[#include <signal.h>])

AC_CHECK_MEMBERS([siginfo_t.si_band],,,[#include <signal.h>])

#
# Checks for struct member names in struct statfs
#

AC_CHECK_MEMBERS([struct statfs.f_type], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#endif
])

AC_CHECK_MEMBERS([struct statfs.f_fstypename], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#endif
])

#
# Checks for struct member names in struct statvfs
#

AC_CHECK_MEMBERS([struct statvfs.f_basetype], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
])

AC_CHECK_MEMBERS([struct statvfs.f_fstypename], [], [], [
AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
])

#
# Check for ptrdiff type.  Yes, there are platforms where
# sizeof(void*) != sizeof(long) (64 bit Windows, apparently).
#

AC_MSG_CHECKING([for pointer diff type])
if test $ac_cv_type_ptrdiff_t = yes ; then
    opal_ptrdiff_t="ptrdiff_t"
    opal_ptrdiff_size=$ac_cv_sizeof_ptrdiff_t
elif test $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long ; then
    opal_ptrdiff_t="long"
    opal_ptrdiff_size=$ac_cv_sizeof_long
elif test $ac_cv_type_long_long = yes && test $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long_long ; then
    opal_ptrdiff_t="long long"
    opal_ptrdiff_size=$ac_cv_sizeof_long_long
else
    AC_MSG_ERROR([Could not find datatype to emulate ptrdiff_t.  Cannot continue])
fi

AC_DEFINE_UNQUOTED([OPAL_PTRDIFF_TYPE], [$opal_ptrdiff_t],
    [type to use for ptrdiff_t])
AC_MSG_RESULT([$opal_ptrdiff_t (size: $opal_ptrdiff_size)])

#
# Find corresponding types for MPI_Aint, MPI_Count, and MPI_Offset.
# And if relevant, find the corresponding MPI_ADDRESS_KIND,
# MPI_COUNT_KIND, and MPI_OFFSET_KIND.
#

# checkpoint results
AC_CACHE_SAVE

##################################
# Linker characteristics
##################################

AC_MSG_CHECKING([the linker for support for the -fini option])
OPAL_VAR_SCOPE_PUSH([LDFLAGS_save])
LDFLAGS_save=$LDFLAGS
LDFLAGS="$LDFLAGS_save -Wl,-fini -Wl,finalize"
AC_TRY_LINK([void finalize (void) {}], [], [AC_MSG_RESULT([yes])
        opal_ld_have_fini=1], [AC_MSG_RESULT([no])
        opal_ld_have_fini=0])
LDFLAGS=$LDFLAGS_save
OPAL_VAR_SCOPE_POP

##################################
# Libraries
##################################

opal_show_title "Library and Function tests"

# Darwin doesn't need -lutil, as it's something other than this -lutil.
OPAL_SEARCH_LIBS_CORE([openpty], [util])

OPAL_SEARCH_LIBS_CORE([gethostbyname], [nsl])

OPAL_SEARCH_LIBS_CORE([socket], [socket])

# Solaris has sched_yield in -lrt, usually in libc
OPAL_SEARCH_LIBS_CORE([sched_yield], [rt])

# IRIX and CentOS have dirname in -lgen, usually in libc
OPAL_SEARCH_LIBS_CORE([dirname], [gen])

# Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
OPAL_SEARCH_LIBS_CORE([ceil], [m])

# -lrt might be needed for clock_gettime
OPAL_SEARCH_LIBS_CORE([clock_gettime], [rt])

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf openpty isatty getpwuid fork waitpid execve pipe ptsname setsid mmap tcgetpgrp posix_memalign strsignal sysconf syslog vsyslog regcmp regexec regfree _NSGetEnviron socketpair strncpy_s usleep mkfifo dbopen dbm_open statfs statvfs setpgid setenv __malloc_initialize_hook])

# Sanity check: ensure that we got at least one of statfs or statvfs.

if test $ac_cv_func_statfs = no && test $ac_cv_func_statvfs = no; then
    AC_MSG_WARN([neither statfs() and statvfs() were found])
    AC_MSG_ERROR([Cannot continue])
fi

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
])], [ompi_cv_htonl_define=yes], [ompi_cv_htonl_define=no])])

AC_CHECK_FUNC([htonl], [ompi_have_htonl=yes], [ompi_have_htonl=no])

AS_IF([test "$ompi_cv_htonl_define" = "yes" || test "$ompi_have_htonl" = "yes"],
    [AC_DEFINE_UNQUOTED([HAVE_UNIX_BYTESWAP], [1],
        [whether unix byteswap routines -- htonl, htons, nothl, ntohs -- are available])])

#
# Make sure we can copy va_lists (need check declared, not linkable)
#

AC_CHECK_DECL(va_copy, OPAL_HAVE_VA_COPY=1, OPAL_HAVE_VA_COPY=0,
    [#include <stdarg.h>])

AC_DEFINE_UNQUOTED(OPAL_HAVE_VA_COPY, $OPAL_HAVE_VA_COPY,
    [Whether we have va_copy or not])

AC_CHECK_DECL(__va_copy, OPAL_HAVE_UNDERSCORE_VA_COPY=1,
    OPAL_HAVE_UNDERSCORE_VA_COPY=0, [#include <stdarg.h>])

AC_DEFINE_UNQUOTED(OPAL_HAVE_UNDERSCORE_VA_COPY, $OPAL_HAVE_UNDERSCORE_VA_COPY,
    [Whether we have __va_copy or not])

AC_CHECK_DECLS(__func__)

# checkpoint results
AC_CACHE_SAVE

##################################
# System-specific tests
##################################

opal_show_title "System-specific tests"

# Do we have _SC_NPROCESSORS_ONLN? (only going to pass if we also have
# <unistd.h> and sysconf(), which is ok) OS X 10.4 has <unistd.h> and
# sysconf(), but does not have _SC_NPROCESSORS_ONLN.  Doh!

AC_CACHE_CHECK([for _SC_NPROCESSORS_ONLN],
  [ompi_cv_have__SC_NPROCESSORS_ONLN],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT
#include <unistd.h>
],
     [int i = _SC_NPROCESSORS_ONLN;])],
     [ompi_cv_have__SC_NPROCESSORS_ONLN="yes"],
     [ompi_cv_have__SC_NPROCESSORS_ONLN="no"])])

AS_IF([test "$ompi_cv_have__SC_NPROCESSORS_ONLN" = "yes"],
      [result=1], [result=0])

AC_DEFINE_UNQUOTED([OPAL_HAVE__SC_NPROCESSORS_ONLN], [$result],
  [Define to 1 ifyou have the declaration of _SC_NPROCESSORS_ONLN, and to 0 otherwise])

# all: endian
AC_C_BIGENDIAN

OPAL_CHECK_BROKEN_QSORT

#
# Check out what thread support we have
#

OPAL_CONFIG_THREADS

CFLAGS="$CFLAGS $THREAD_CFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS="$LIBS $THREAD_LIBS"

OPAL_WRAPPER_FLAGS_ADD([CFLAGS], [$THREAD_CFLAGS])
OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$THREAD_LDFLAGS])

# no need to update WRAPPER_EXTRA_LIBS - we'll get it from LT later
#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S
AC_PROG_GREP
AC_PROG_EGREP

#
# We need as and lex
#

AM_PROG_AS
AM_PROG_LEX

# If we don't have Flex and we don't have a generated .c file
# (distribution tarballs will have the .c file included, but git
# clones will not), then error.  Must have Flex -- other versions of
# Lex are not workable (all things being equal, since this is *only*
# required for developers, we decided that it really was not worth it
# to be portable between different versions of lex ;-).

if test -z "$LEX" || \
   test -n "`echo $LEX | $GREP missing`" || \
   test "`basename $LEX`" != "flex"; then
    if test ! -f "$srcdir/opal/util/show_help_lex.c"; then
        AC_MSG_WARN([*** Could not find Flex on your system.])
        AC_MSG_WARN([*** Flex is required for developer builds of Open MPI.])
        AC_MSG_WARN([*** Other versions of Lex are not supported.])
        AC_MSG_WARN([*** YOU DO NOT NEED FLEX WHEN BUILDING DISTRIBUTION TARBALLS!])
        AC_MSG_ERROR([Cannot continue])
    fi
fi

#
# Look for ps command and arguments
#
OPAL_PS_FLAVOR_CHECK

#
# File system case sensitivity
#

OPAL_CASE_SENSITIVE_FS_SETUP

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

# checkpoint results
AC_CACHE_SAVE

##################################
# MCA
##################################

opal_show_title "Modular Component Architecture (MCA) setup"

AC_MSG_CHECKING([for subdir args])
OPAL_CONFIG_SUBDIR_ARGS([opal_subdir_args])
AC_MSG_RESULT([$opal_subdir_args])

OPAL_MCA

# checkpoint results
AC_CACHE_SAVE

##################################
# Visibility
##################################
# Check the visibility declspec at the end to avoid problem with
# the previous tests that are not necessarily prepared for
# the visibility feature.

opal_show_title "Symbol visibility feature"

OPAL_CHECK_VISIBILITY

############################################################################
# Final top-level PMIx Reference Server configuration
############################################################################

opal_show_title "Final top-level PMIx Reference Server configuration"

############################################################################
# Libtool: part two
# (after C compiler setup = no compiler/linker tests after this)
############################################################################
opal_show_subtitle "Libtool configuration"

# Use the undocumented solaris_use_stlport4 libtool variable to turn off any
# Cstd/stlport4 linkage. This allows Open MPI to be C++ STL agnostic.
if test "x$opal_cv_c_compiler_vendor" = "xsun"; then
    solaris_use_stlport4="yes"
fi

LT_INIT([dlopen win32-dll])


# What's the suffix of shared libraries?  Inspired by generated
# Libtool code (even though we don't support several of these
# platforms, there didn't seem to be any harm in leaving in some of
# them, alhtough I did remove some that we have never/will never
# support, like OS/2).
OPAL_DYN_LIB_PREFIX=lib
case $host_os in
cygwin*)
    OPAL_DYN_LIB_PREFIX=cyg
    OPAL_DYN_LIB_SUFFIX=dll
    ;;
mingw* | pw32* | cegcc*)
    OPAL_DYN_LIB_SUFFIX=dll
    ;;
darwin* | rhapsody*)
    OPAL_DYN_LIB_SUFFIX=dylib
    ;;
hpux9* | hpux10* | hpux11*)
    case $host_cpu in
        ia64*)
        OPAL_DYN_LIB_SUFFIX=so
        ;;
    *)
        OPAL_DYN_LIB_SUFFIX=sl
        ;;
    esac
    ;;
*)
   OPAL_DYN_LIB_SUFFIX=so
   ;;
esac

AC_SUBST(OPAL_DYN_LIB_PREFIX)
AC_SUBST(OPAL_DYN_LIB_SUFFIX)

# Need the libtool binary before the rpathify stuff
LT_OUTPUT

############################################################################
# final compiler config
############################################################################
opal_show_subtitle "Compiler flags"

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory.  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#

# Because opal_config.h is created by AC_CONFIG_HEADERS, we
# don't need to -I the builddir for opal/include. However, we do
# need to add it for orte as it doesn't have an AC_CONFIG_HEADERS that
# will install it for us. If we VPATH building, we do need to include the
# source directories, however.
#

if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    # Note the embedded m4 directives here -- we must embed them
    # rather than have successive assignments to these shell
    # variables, lest the $(foo) names try to get evaluated here.
    # Yuck!
    CPPFLAGS='-I$(top_srcdir) -I$(top_builddir) -I$(top_srcdir)/opal/include -I$(top_srcdir)/orte/include -I$(top_builddir)/orte/include'" $CPPFLAGS"
else
    CPPFLAGS='-I$(top_srcdir) -I$(top_srcdir)/orte/include'" $CPPFLAGS"
fi

# ORTE wants some additional processing of the flags (e.g., get
# versions without optimization for debugger modules).
ORTE_SETUP_DEBUGGER_FLAGS

#
# Delayed the substitution of CFLAGS and CXXFLAGS until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)

#
# Aggregate MCA parameters directory
#
AC_SUBST([AMCA_PARAM_SETS_DIR], ['$(opaldatadir)/amca-param-sets'])

############################################################################
# final wrapper compiler config
############################################################################
opal_show_subtitle "Wrapper compiler final setup"

# The ORTE and PMIx wrapper scripts (i.e., not the C-compiled
# executables) need perl.
AC_PATH_PROG(PERL, perl, perl)

OPAL_SETUP_WRAPPER_FINAL

# If there is a local hook for each project, call it.
m4_ifdef([opal_CONFIG_LOCAL], [opal_CONFIG_LOCAL])
m4_ifdef([orte_CONFIG_LOCAL], [orte_CONFIG_LOCAL])

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

# opaldatadir, opallibdir, and opalinclude are essentially the same as
# pkg*dir, but will always be */pmixsrvr.

opaldatadir='${datadir}/pmixsrvr'
opallibdir='${libdir}/pmixsrvr'
opalincludedir='${includedir}/pmixsrvr'
AC_SUBST(opaldatadir)
AC_SUBST(opallibdir)
AC_SUBST(opalincludedir)

OPAL_SET_MCA_PREFIX([PMCA_])
OPAL_SET_MCA_CMD_LINE_ID([pmca])

ortedatadir="$opaldatadir"
AC_SUBST(ortedatadir)
ortelibdir="$opallibdir"
AC_SUBST(ortelibdir)
orteincludedir="$opalincludedir"
AC_SUBST(orteincludedir)

# turn some things off
AC_DEFINE_UNQUOTED(OPAL_ENABLE_FT, 0, [Turn off])
AC_DEFINE_UNQUOTED(OPAL_ENABLE_FT_CR, 0, [Turn off])
AC_DEFINE_UNQUOTED(OPAL_ENABLE_FT_THREAD, 0, [Turn off])


opal_show_subtitle "Final output"

AC_CONFIG_FILES([
    Makefile
    config/Makefile
    contrib/Makefile
])


OPAL_CONFIG_FILES
ORTE_CONFIG_FILES

OPAL_CHECK_LIBNL_SUMMARY

AC_OUTPUT

OPAL_SUMMARY_PRINT
