# -*- shell-script -*-
#
# $HEADER$
#


############################################################################
# Initialization, version number, and other random setup/init stuff
############################################################################

# Init autoconf

AC_INIT(./src/mpi/c/datatype_get_name.c)
AC_PREREQ(2.52)
AC_CONFIG_AUX_DIR(./config)

# Get the version of LAM that we are installing

LAM_GET_VERSION($srcdir/config, $srcdir/VERSION, LAM)

AC_DEFINE_UNQUOTED(LAM_MAJOR_VERSION, $LAM_MAJOR_VERSION, 
    [Major LAM version])
AC_DEFINE_UNQUOTED(LAM_MINOR_VERSION, $LAM_MINOR_VERSION, 
    [Minor LAM version])
AC_DEFINE_UNQUOTED(LAM_RELEASE_VERSION, $LAM_RELEASE_VERSION, 
    [Release LAM version])
AC_DEFINE_UNQUOTED(LAM_ALPHA_VERSION, $LAM_ALPHA_VERSION, 
    [Alpha LAM version])
AC_DEFINE_UNQUOTED(LAM_BETA_VERSION, $LAM_BETA_VERSION, 
    [Beta LAM version])
AC_DEFINE_UNQUOTED(LAM_CVS_VERSION, $LAM_CVS_VERSION, 
    [CVS LAM version])
AC_DEFINE_UNQUOTED(LAM_VERSION, "$LAM_VERSION", 
    [Overall LAM version number])

AC_SUBST(LAM_MAJOR_VERSION)
AC_SUBST(LAM_MINOR_VERSION)
AC_SUBST(LAM_RELEASE_VERSION)
AC_SUBST(LAM_ALPHA_VERSION)
AC_SUBST(LAM_BETA_VERSION)
AC_SUBST(LAM_CVS_VERSION)
AC_SUBST(LAM_VERSION)

#
# Start it up
#

LAM_CONFIGURE_SETUP
lam_show_title "Configuring LAM version $LAM_VERSION"
lam_show_subtitle "Initialization, setup"

#
# Init automake
# The third argument to AM_INIT_AUTOMAKE surpresses the PACKAGE and
# VERSION macors
#

AM_INIT_AUTOMAKE(lam, $LAM_VERSION, 'no')

LAM_TOP_BUILDDIR="`pwd`"
AC_SUBST(LAM_TOP_BUILDDIR)
cd "$srcdir"
LAM_TOP_SRCDIR="`pwd`"
AC_SUBST(LAM_TOP_SRCDIR)
cd "$LAM_TOP_BUILDDIR"

AC_MSG_NOTICE([builddir: $LAM_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $LAM_TOP_SRCDIR])
if test "$LAM_TOP_BUILDDIR" != "$LAM_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# Setup the top of the src/include/lam_config.h file

AH_TOP([/* -*- c -*-
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration 
 */

#ifndef LAM_CONFIG_H
#define LAM_CONFIG_H
])
AH_BOTTOM([
#include <lam_config_bottom.h>
#endif /* LAM_CONFIG_H */
])

# What kind of machine are we on?

AC_CANONICAL_HOST


############################################################################
# Configuration options
############################################################################

LAM_CONFIGURE_OPTIONS


############################################################################
# Libtool: part one
# (before C compiler setup)
############################################################################

#
# Part one of libtool magic.  Enable static so that we have the --with
# tests done up here and can check for OS.  Save the values of
# $enable_static and $enable_shared before setting the defaults,
# because if the user specified --[en|dis]able-[static|shared] on the
# command line, they'll already be set.  In this way, we can tell if
# the user requested something or if the default was set here.
#

lam_enable_shared="$enable_shared"
lam_enable_static="$enable_static"
AM_DISABLE_SHARED
AM_ENABLE_STATIC


############################################################################
# Check for compilers and preprocessors
############################################################################

##################################
# C compiler characteristics
##################################

LAM_SETUP_CC

# force ANSI prototypes
# check for STDC
# check for some types
AC_CHECK_TYPES(long long)
AC_CHECK_TYPES(int8_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(int16_t)
AC_CHECK_TYPES(uint16_t)
AC_CHECK_TYPES(int32_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES(int64_t)
AC_CHECK_TYPES(uint64_t)
AC_CHECK_TYPES(intptr_t)
AC_CHECK_TYPES(uintptr_t)
# check for type sizes
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(void *)
# check for type alignments

AC_C_INLINE
AC_CHECK_HEADERS(stdbool.h)
LAM_C_WEAK_SYMBOLS

# If we want the profiling layer:
# - If the C compiler has weak symbols, use those.
# - If not, then set to compile the code again with #define's in a
#   separate directory.

if test "$WANT_WEAK_SYMBOLS" = "0"; then
    LAM_C_HAVE_WEAK_SYMBOLS=1
fi
if test "$WANT_MPI_PROFILING" = "1"; then
    if test "$LAM_C_HAVE_WEAK_SYMBOLS" = "1"; then
	LAM_PROFILING_COMPILE_SEPARATELY=0
    else
	LAM_PROFILING_COMPILE_SEPARATELY=1
    fi
else
    LAM_PROFILING_COMPILE_SEPARATELY=0
fi
AM_CONDITIONAL(COMPILE_PROFILING_SEPARATELY, 
    test "$LAM_PROFILING_COMPILE_SEPARATELY" = 1)
AC_DEFINE_UNQUOTED(LAM_WANT_MPI_PROFILING, $WANT_MPI_PROFILING,
    [Whether we want MPI profiling or not])
AC_DEFINE_UNQUOTED(LAM_HAVE_WEAK_SYMBOLS, $LAM_C_HAVE_WEAK_SYMBOLS,
    [Wehther we have weak symbols or not])


##################################
# C++ compiler characteristics
##################################

LAM_SETUP_CXX

# check for STL
# check for bool (and corresponding C type)
# check for true/false
# check for type sizes
# check for type alignments
# check for template repository


##################################
# Fortran
##################################

LAM_SETUP_F77
LAM_SETUP_F90


##################################
# Header files
##################################

# snprintf declaration
# gethostname declaration
# headers:
#   stropts.h grh.h netinet/tcp.h sys/select.h sys/resource.h pty.h util.h
#   rpc/types.h rpc/xdr.h sched.h strings.h

# SA_RESTART in signal.h
# sa_len in struct sockaddr
# union semun in sys/sem.h


##################################
# Libraries
##################################

# -lsocket
# -lnsl
# -lutil (openpty)
# openpty
# atexit
# getcwd
# snprintf
# atoll
# strtoll
# yield
# sched_yield
# vscanf
# va_copy

##################################
# System-specific tests
##################################

# all: endian
# all: SYSV semaphores
# all: SYSV shared memory
# all: thread flavor
# all: size of FD_SET
# all: FD passing (or not!!)
# all: BSD vs. SYSV ptys
# all: sizeof struct stat members
# all: type of getsockopt optlen
# all: type of recvfrom optlen
# all: file system case sensitivity

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy


############################################################################
# Libtool: part two
# (after C compiler setup)
############################################################################

lam_show_title "Libtool configuration"

AM_PROG_LIBTOOL


############################################################################
# final wrapper compiler config
############################################################################

lam_show_title "Final top-level LAM configuration"

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory (don't know why automake doesn't do this
# automatically).  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#

CPPFLAGS='-I$(top_srcdir)/src'" $CPPFLAGS"
CXXCPPFLAGS='-I$(top_srcdir)/src'" $CXXCPPFLAGS"

#
# Delayed the substitution of CFLAGS and CXXFLAGS until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CXXCPPFLAGS)
AC_SUBST(FFLAGS)


############################################################################
# Party on
############################################################################

lam_show_subtitle "Final output" 

AM_CONFIG_HEADER([src/include/lam_config.h])
AC_CONFIG_FILES([
    Makefile

    config/Makefile

    src/Makefile
    src/include/Makefile

    src/lam/Makefile
    src/lam/ctnetwork/Makefile
    src/lam/lfc/Makefile
    src/lam/mem/Makefile

    src/lam/os/Makefile
    src/lam/os/cygwin/Makefile
    src/lam/os/darwin/Makefile
    src/lam/os/darwin/ppc_32/Makefile
    src/lam/os/darwin/ppc_64/Makefile
    src/lam/os/irix/Makefile
    src/lam/os/irix/sn0/Makefile
    src/lam/os/linux/Makefile
    src/lam/os/linux/alpha/Makefile
    src/lam/os/linux/i686/Makefile
    src/lam/os/linux/ia64/Makefile
    src/lam/os/linux/x86_64/Makefile
    src/lam/os/tru64/Makefile

    src/lam/threads/Makefile
    src/lam/util/Makefile

    src/mpi/Makefile
    src/mpi/c/Makefile
    src/mpi/c/profile/Makefile
    src/mpi/cxx/Makefile
    src/mpi/f77/Makefile
    src/mpi/f77/profile/Makefile
    src/mpi/f90/Makefile

    src/mca/Makefile
    src/mca/common/Makefile
    src/mca/lam/Makefile
    src/mca/mpi/Makefile
    src/mca/mpi/coll/Makefile
    src/mca/mpi/io/Makefile
    src/mca/mpi/one/Makefile
    src/mca/mpi/pml/Makefile
    src/mca/mpi/ptl/Makefile
    src/mca/mpi/topo/Makefile

    src/tools/Makefile
    src/tools/wrappers/Makefile

    test/Makefile
    test/unit/Makefile
    test/unit/mpi/Makefile
    test/unit/mpi/communicator/Makefile
    test/unit/mpi/datatype/Makefile
])
AC_OUTPUT
