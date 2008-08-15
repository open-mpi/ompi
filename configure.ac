# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


############################################################################
# Initialization, version number, and other random setup/init stuff
############################################################################

# Init autoconf

# We don't have the version number to put in here yet, and we can't
# call OMPI_GET_VERSION (etc.) before AC_INIT.  So use the shell
# version.

AC_INIT([Open MPI], 
        [m4_normalize(esyscmd([config/ompi_get_version.sh VERSION --base]))],
        [http://www.open-mpi.org/community/help/], [openmpi])
AC_PREREQ(2.58)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

# Get our platform support file.  This has to be done very, very early
# because it twiddles random bits of autoconf
OMPI_LOAD_PLATFORM

#
# Init automake - if we are using AC.260 or higher, require AM 1.10 or higher
#
# XXX - FIX ME - when we drop support for 2.58 and 2.59, 
# remove this test and require 1.10 or higher all the time.
m4_if(m4_version_compare(m4_defn([m4_PACKAGE_VERSION]), [2.60]), -1,
  [AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define])],
  [AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.10])])

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

#
# Start it up
#

OMPI_CONFIGURE_SETUP
ompi_show_title "Configuring Open MPI"

ompi_show_subtitle "Checking versions"

# Get the version of OMPI that we are installing

OMPI_SAVE_VERSION([OMPI], [Open MPI], [$srcdir/VERSION],
                  [ompi/include/ompi/version.h])

OMPI_SAVE_VERSION([ORTE], [Open Run-Time Environment], [$srcdir/VERSION],
                  [orte/include/orte/version.h])

OMPI_SAVE_VERSION([OPAL], [Open Portable Access Layer], [$srcdir/VERSION],
                  [opal/include/opal/version.h])

# override/fixup the version numbers set by AC_INIT, since on
# developer builds, there's no good way to know what the version is
# before running configure :(.  We only use the base version number
# (ie, no svn r numbers) for the version set in AC_INIT.  This will
# always match reality because we add the VERSION file (the only way
# to change the major.minor.release{greek}) into the configure
# dependencies.  PACKAGE_VERSION the AC_DEFINE doesn't change once set
# the first time -- AC_INIT's input (so it doesn't have an r number in
# it).  PACKAGE_VERSION the AC_SUBST can be rewritten along the way,
# and we'd like it to have the r number in it so that it shows up in
# the tarball name, so it is set to the full version here.
PACKAGE_VERSION="$OPAL_VERSION"
PACKAGE_STRING="${PACKAGE_NAME} ${PACKAGE_VERSION}"
VERSION="${PACKAGE_VERSION}"

ompi_show_subtitle "Initialization, setup"

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
])
AH_BOTTOM([
#include "opal_config_bottom.h"
#endif /* OPAL_CONFIG_H */
])

# Other basic setup stuff (shared with components)

OMPI_BASIC_SETUP

top_ompi_srcdir="$OMPI_TOP_SRCDIR"
AC_SUBST(top_ompi_srcdir)
top_ompi_builddir="$OMPI_TOP_BUILDDIR"
AC_SUBST(top_ompi_builddir)

############################################################################
# Configuration options
############################################################################

OMPI_CONFIGURE_OPTIONS

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

ompi_enable_shared="$enable_shared"
ompi_enable_static="$enable_static"
AM_ENABLE_SHARED
AM_DISABLE_STATIC

OMPI_SETUP_WRAPPER_INIT


############################################################################
# Check for compilers and preprocessors
############################################################################
ompi_show_title "Compiler and preprocessor tests"

##################################
# C compiler characteristics
##################################

OMPI_SETUP_CC

# If we build on a windows environment with the windows compiler and linker
# then we need some translation functions from the opal/win32 directory.
AM_CONDITIONAL(OMPI_NEED_WINDOWS_REPLACEMENTS,
               test "$ompi_cv_c_compiler_vendor" = "microsoft" )

#
# Check for some types
#

AC_CHECK_TYPES(long long)
AC_CHECK_TYPES(long double)
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
AC_CHECK_TYPES(mode_t)
AC_CHECK_TYPES(ssize_t)
AC_CHECK_TYPES(ptrdiff_t)

#
# Check for type sizes
#

AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
if test $ac_cv_type_long_long = yes; then
    AC_CHECK_SIZEOF(long long)
fi
if test $ac_cv_type_long_double = yes; then
    AC_CHECK_SIZEOF(long double)
fi
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(size_t)
if test $ac_cv_type_ssize_t = yes ; then
    AC_CHECK_SIZEOF(ssize_t)
fi
if test $ac_cv_type_ptrdiff_t = yes; then
    AC_CHECK_SIZEOF(ptrdiff_t)
fi

#
# Check for type alignments
#

OMPI_C_GET_ALIGNMENT(char, OMPI_ALIGNMENT_CHAR)
OMPI_C_GET_ALIGNMENT(short, OMPI_ALIGNMENT_SHORT)
OMPI_C_GET_ALIGNMENT(wchar_t, OMPI_ALIGNMENT_WCHAR)
OMPI_C_GET_ALIGNMENT(int, OMPI_ALIGNMENT_INT)
OMPI_C_GET_ALIGNMENT(long, OMPI_ALIGNMENT_LONG)
if test $ac_cv_type_long_long = yes; then
    OMPI_C_GET_ALIGNMENT(long long, OMPI_ALIGNMENT_LONG_LONG)
fi
OMPI_C_GET_ALIGNMENT(float, OMPI_ALIGNMENT_FLOAT)
OMPI_C_GET_ALIGNMENT(double, OMPI_ALIGNMENT_DOUBLE)
if test $ac_cv_type_long_double = yes; then
    OMPI_C_GET_ALIGNMENT(long double, OMPI_ALIGNMENT_LONG_DOUBLE)
fi
OMPI_C_GET_ALIGNMENT(void *, OMPI_ALIGNMENT_VOID_P)

#
# Does the C compiler native support "bool"? (i.e., without
# <stdbool.h> or any other help)
#

AC_MSG_CHECKING(for C bool type)
AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[
AC_INCLUDES_DEFAULT]],
[[bool bar, foo = true; bar = foo;]]),
[OMPI_NEED_C_BOOL=0 MSG=yes],[OMPI_NEED_C_BOOL=1 MSG=no])
AC_DEFINE_UNQUOTED(OMPI_NEED_C_BOOL, $OMPI_NEED_C_BOOL,
    [Whether the C compiler supports "bool" without any other help (such as <stdbool.h>)])
AC_MSG_RESULT([$MSG])

#
# Check for other compiler characteristics
#

if test "$GCC" = "yes"; then

    # gcc 2.96 will emit oodles of warnings if you use "inline" with
    # -pedantic (which we do in developer builds).  However,
    # "__inline__" is ok.  So we have to force gcc to select the
    # right one.  If you use -pedantic, the AC_C_INLINE test will fail
    # (because it names a function foo() -- without the (void)).  So
    # we turn off all the picky flags, turn on -ansi mode (which is
    # implied by -pedantic), and set warnings to be errors.  Hence,
    # this does the following (for 2.96):
    #
    # - causes the check for "inline" to emit a warning, which then
    # fails
    # - checks for __inline__, which then emits no error, and works
    #
    # This also works nicely for gcc 3.x because "inline" will work on
    # the first check, and all is fine.  :-)

    CFLAGS_save="$CFLAGS"
    CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY -Werror -ansi"
fi
AC_C_INLINE
# Microsoft compilers support 2 versions of restrict. One for functions, and
# one for variables. The problem is that they don't have an equivalent
# syntax, and the autoconf restrict detection is unable to detect them
# correctly. It detect the restrict keyword as __restrict which break the
# rules for function syntax which is declspec(restrict).
if test "x$ompi_cv_c_compiler_vendor" != "xmicrosoft"; then
  AC_C_RESTRICT
fi
OMPI_C_WEAK_SYMBOLS
if test "$GCC" = "yes"; then
    CFLAGS="$CFLAGS_save"
fi

if test "x$CC" = "xicc"; then
  OMPI_CHECK_ICC_VARARGS
fi

# If we want the profiling layer:
# - If the C compiler has weak symbols, use those.
# - If not, then set to compile the code again with #define's in a
#   separate directory.

if test "$WANT_WEAK_SYMBOLS" = "0"; then
    OMPI_C_HAVE_WEAK_SYMBOLS=0
fi
if test "$WANT_MPI_PROFILING" = "1"; then
    if test "$OMPI_C_HAVE_WEAK_SYMBOLS" = "1"; then
	OMPI_PROFILING_COMPILE_SEPARATELY=0
    else
	OMPI_PROFILING_COMPILE_SEPARATELY=1
    fi
else
    OMPI_PROFILING_COMPILE_SEPARATELY=0
fi

#
# There are 2 layers to the MPI Language binidings One layer generates
# MPI_* bindings. The other layer generates PMPI_* bindings. The
# following conditions determine whether each (or both) these layers
# are built.
# 1. MPI_* bindings are needed if:
#	- Profiling is not required
#   - Profiling is required but weak symbols are not
#	  supported
# 2. PMPI_* bindings are needed if profiling is required.  Hence we
# define 2 conditionals which tell us whether each of these layers
# need to be built or NOT
#

AM_CONDITIONAL(WANT_MPI_BINDINGS_LAYER,
	test "$WANT_MPI_PROFILING" = 0 -o "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1)

AM_CONDITIONAL(WANT_PMPI_BINDINGS_LAYER,
	test "$WANT_MPI_PROFILING" = 1)
AM_CONDITIONAL(COMPILE_PROFILING_SEPARATELY, 
    test "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1)
AC_DEFINE_UNQUOTED(OMPI_ENABLE_MPI_PROFILING, $WANT_MPI_PROFILING,
    [Whether we want MPI profiling or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_WEAK_SYMBOLS, $OMPI_C_HAVE_WEAK_SYMBOLS,
    [Wehther we have weak symbols or not])


##################################
# C++ compiler characteristics
##################################

OMPI_SETUP_CXX

# check for type sizes

AC_LANG_PUSH(C++)
AC_CHECK_SIZEOF(bool)

# check for type alignments

OMPI_C_GET_ALIGNMENT(bool, OMPI_ALIGNMENT_CXX_BOOL)
AC_LANG_POP(C++)

# check if we want C++ support                                                  

AM_CONDITIONAL(WANT_MPI_CXX_BINDINGS,
    test "$WANT_MPI_CXX_SUPPORT" = 1)
AC_DEFINE_UNQUOTED(OMPI_WANT_CXX_BINDINGS, $WANT_MPI_CXX_SUPPORT,
    [Whether we want MPI cxx support or not])
    

##################################
# Assembler Configuration
##################################

ompi_show_subtitle "Assembler" 

AM_PROG_AS
OMPI_CONFIG_ASM


##################################
# Fortran
##################################

#-----------
# Fortran 77
#-----------

OMPI_SETUP_F77

# This allows us to mark bogus types, but still have them be a valid
# [sentinel] value

AC_DEFINE([ompi_fortran_bogus_type_t], [int],
          [A bogus type that allows us to have sentinel type values that are still valid])

# We want to set the #define's for all of these, so invoke the macros
# regardless of whether we have F77 support or not.

OMPI_F77_CHECK([LOGICAL], [yes], 
               [char, int32_t, int, int64_t, long long, long], [-1])
OMPI_F77_CHECK([INTEGER], [yes], 
               [int32_t, int, int64_t, long long, long], [-1])
OMPI_F77_CHECK([INTEGER*1], [no], 
               [char, int8_t, short, int, int64_t, long long, long], [1])
OMPI_F77_CHECK([INTEGER*2], [no],
               [short, int16_t, int32_t, int, int64_t, long long, long], [2])
OMPI_F77_CHECK([INTEGER*4], [no],
               [int32_t, int, int64_t, long long, long], [4])
OMPI_F77_CHECK([INTEGER*8], [no],
               [int, int64_t, long long, long], [8])
OMPI_F77_CHECK([INTEGER*16], [no],
               [int, int64_t, long long, long], [16])

OMPI_F77_CHECK([REAL], [yes],
               [float, double, long double], [-1])
OMPI_F77_CHECK([REAL*4], [no],
               [float, double, long double], [4])
OMPI_F77_CHECK([REAL*8], [no],
               [float, double, long double], [8])
OMPI_F77_CHECK([REAL*16], [no],
               [float, double, long double], [16])
OMPI_F77_CHECK([DOUBLE PRECISION], [yes],
               [float, double, long double], [-1])

OMPI_F77_CHECK([COMPLEX], [yes], [], [-1])

# The complex*N tests are a bit different (note: the complex tests are
# the same as all the rest, because complex is a composite of two
# reals, which we *have* to have.  It's only the complex*N tests that
# are different).  The fortran complex types are composites of the
# real*(N/2) types.  So for us to support complex*N, two conditions
# must be true:
#
# a) we must support real*(N/2) (i.e., compiler supports it and we
#    have a back-end C type for it)
# b) compiler supports complex*N

OMPI_F77_CHECK([COMPLEX*8], [no], [], [8])
OMPI_F77_CHECK([COMPLEX*16], [no], [], [16])
OMPI_F77_CHECK([COMPLEX*32], [no], [], [32])

# Regardless of whether we have fortran bindings, or even a fortran
# compiler, get the max value for a fortran MPI handle (this macro
# handles the case where we don't have a fortran compiler).

OMPI_F77_GET_FORTRAN_HANDLE_MAX

#
# Check for Fortran compilers value of TRUE and for the correct assumption
# on LOGICAL for conversion into what C considers to be a true value
#
OMPI_F77_GET_VALUE_TRUE
OMPI_F77_CHECK_LOGICAL_ARRAY

#
# There are 2 layers to the MPI f77 layer. The only extra thing that
# determine f77 bindings is that fortran can be disabled by user. In
# such cases, we need to not build the target at all.  One layer
# generates MPI_f77* bindings. The other layer generates PMPI_f77*
# bindings. The following conditions determine whether each (or both)
# these layers are built.
#
# Superceeding clause:
#   - fortran77 bindings should be enabled, else everything is
#     disabled
# 1. MPI_f77* bindings are needed if:
#   - Profiling is not required
#   - Profiling is required but weak symbols are not
#     supported
# 2. PMPI_* bindings are needed if profiling is required.  Hence we
# define 2 conditionals which tell us whether each of these layers
# need to be built or NOT
#
AM_CONDITIONAL(WANT_MPI_F77_BINDINGS_LAYER,
               test \( "$WANT_MPI_PROFILING" = 0 -o "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1 \) -a "$OMPI_WANT_F77_BINDINGS" = 1)
                    
AM_CONDITIONAL(WANT_PMPI_F77_BINDINGS_LAYER,
               test "$OMPI_WANT_F77_BINDINGS" = 1 -a "$WANT_MPI_PROFILING" = 1)

#-----------
# Fortran 90
#-----------

OMPI_SETUP_F90

# Look for the fortran module compiler flag
OMPI_F90_FIND_MODULE_INCLUDE_FLAG

# Look for all the types
OMPI_F90_IKINDS=""
OMPI_F90_RKINDS=""
OMPI_F90_CKINDS=""

# LOGICAL and friends
OMPI_F90_CHECK([LOGICAL])

# INTEGER and friends
OMPI_F90_CHECK([INTEGER])
OMPI_F90_CHECK([INTEGER*1], [1])
OMPI_F90_CHECK([INTEGER*2], [2])
OMPI_F90_CHECK([INTEGER*4], [4])
OMPI_F90_CHECK([INTEGER*8], [8])
OMPI_F90_CHECK([INTEGER*16], [16])

# REAL, DOUBLE PRECISION, REAL*4, *8, *16
OMPI_F90_CHECK([REAL])
OMPI_F90_CHECK([REAL*4], [4], [1])
OMPI_F90_CHECK([REAL*8], [8], [1])
OMPI_F90_CHECK([REAL*16], [16], [1])
OMPI_F90_CHECK([DOUBLE PRECISION])

# COMPLEX, DOUBLE COMPLEX, COMPLEX*8, *16, *32
OMPI_F90_CHECK([COMPLEX])
OMPI_F90_CHECK([COMPLEX*8], [8])
OMPI_F90_CHECK([COMPLEX*16], [16])
OMPI_F90_CHECK([COMPLEX*32], [32])
OMPI_F90_CHECK([DOUBLE COMPLEX])

AC_SUBST(OMPI_F90_IKINDS)
AC_SUBST(OMPI_F90_RKINDS)
AC_SUBST(OMPI_F90_CKINDS)

# get kind value for Fortran MPI_INTEGER_KIND (corresponding to
# whatever is the same size as a F77 INTEGER -- for the
# most-likely-will-never-occur case where F77 INTEGER is smaller than
# an F90 INTEGER; see MPI-2 4.12.6.5
if test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "2"; then
    OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 4, OMPI_MPI_INTEGER_KIND)
elif test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "4"; then
    OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 9, OMPI_MPI_INTEGER_KIND)
elif test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "8"; then
    OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 18, OMPI_MPI_INTEGER_KIND)
elif test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "16"; then
    OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 19, OMPI_MPI_INTEGER_KIND)
    AC_MSG_ERROR([Cannot support Fortran MPI_INTEGER_KIND!])
fi
AC_SUBST(OMPI_MPI_INTEGER_KIND)

# get kind value for Fortran MPI_ADDRESS_KIND (corresponding to
# whatever is big enough to hold (void*))
if test "$ac_cv_sizeof_void_p" = "2"; then
    OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 4, OMPI_MPI_ADDRESS_KIND)
elif test "$ac_cv_sizeof_void_p" = "4"; then
    OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 9, OMPI_MPI_ADDRESS_KIND)
elif test "$ac_cv_sizeof_void_p" = "8"; then
    OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 18, OMPI_MPI_ADDRESS_KIND)
elif test "$ac_cv_sizeof_void_p" = "16"; then
    OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 19, OMPI_MPI_ADDRESS_KIND)
else
    AC_MSG_ERROR([Cannot support Fortran MPI_ADDRESS_KIND!])
fi
AC_SUBST(OMPI_MPI_ADDRESS_KIND)

# get kind value for Fortran MPI_OFFSET_KIND (corresponding to INTEGER*8)
OMPI_F90_GET_INT_KIND(MPI_OFFSET_KIND, 18, OMPI_MPI_OFFSET_KIND)
AC_SUBST(OMPI_MPI_OFFSET_KIND)

# checkpoint results
AC_CACHE_SAVE

##################################
# Header files
##################################

ompi_show_title "Header file tests"

AC_CHECK_HEADERS([alloca.h aio.h arpa/inet.h dirent.h \
    dlfcn.h execinfo.h err.h fcntl.h grp.h inttypes.h libgen.h \
    libutil.h netdb.h netinet/in.h netinet/tcp.h \
    poll.h pthread.h pty.h pwd.h sched.h stdint.h \
    string.h strings.h stropts.h sys/fcntl.h sys/ipc.h \
    sys/ioctl.h sys/mman.h  sys/param.h sys/queue.h \
    sys/resource.h sys/select.h sys/socket.h sys/sockio.h \
    stdarg.h sys/stat.h sys/statvfs.h sys/time.h sys/tree.h \
    sys/types.h sys/uio.h sys/utsname.h sys/wait.h syslog.h \
    time.h termios.h ulimit.h unistd.h util.h utmp.h malloc.h \
    crt_externs.h])

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

# Note that sometimes we have <stdbool.h>, but it doesn't work (e.g.,
# have both Portland and GNU installed; using pgcc will find GNU's
# <stdbool.h>, which all it does -- by standard -- is define "bool" to
# "_Bool" [see
# http://www.opengroup.org/onlinepubs/009695399/basedefs/stdbool.h.html],
# and Portland has no idea what to do with _Bool).

# So first figure out if we have <stdbool.h> (i.e., check the value of
# the macro HAVE_STDBOOL_H from the result of AC_CHECK_HEADERS,
# above).  If we do have it, then check to see if it actually works.
# Define OMPI_USE_STDBOOL_H as approrpaite.
AC_CHECK_HEADERS([stdbool.h], [have_stdbool_h=1], [have_stdbool_h=0])
AC_MSG_CHECKING([if <stdbool.h> works])
if test "$have_stdbool_h" = "1"; then
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[
AC_INCLUDES_DEFAULT
#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif]], 
[[bool bar, foo = true; bar = foo;]]),
[OMPI_USE_STDBOOL_H=1 MSG=yes],[OMPI_USE_STDBOOL_H=0 MSG=no])
else
    OMPI_USE_STDBOOL_H=0
    MSG="no (don't have <stdbool.h>)"
fi
AC_DEFINE_UNQUOTED(OMPI_USE_STDBOOL_H, $OMPI_USE_STDBOOL_H,
    [Whether to use <stdbool.h> or not])
AC_MSG_RESULT([$MSG])

# checkpoint results
AC_CACHE_SAVE


##################################
# Types
##################################

ompi_show_title "Type tests"

# Size of pid_t
AC_CHECK_SIZEOF(pid_t)

AC_CHECK_TYPES([socklen_t], [AC_DEFINE([HAVE_SOCKLEN_T], [1], 
                                       [Whether we have socklen_t or not])],
               [], [AC_INCLUDES_DEFAULT
#include <sys/socket.h>])

AC_CHECK_TYPES([struct sockaddr_in], [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

# Do we have IPv6 support?
AC_CHECK_TYPES([struct sockaddr_in6], [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

# SA_RESTART in signal.h
AC_MSG_CHECKING([if SA_RESTART defined in signal.h])
AC_EGREP_CPP(yes, [
#include <signal.h>
#ifdef SA_RESTART
    yes
#endif ], [MSG=yes VALUE=1], [MSG=no VALUE=0])
AC_DEFINE_UNQUOTED(OMPI_HAVE_SA_RESTART, $VALUE, 
    [Whether we have SA_RESTART in <signal.h> or not])
AC_MSG_RESULT([$MSG])

# sa_len in struct sockaddr
AC_MSG_CHECKING([for sa_len in struct sockaddr])
AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/socket.h>], [struct sockaddr s; s.sa_len;],
[MSG=yes VALUE=1], [MSG=no VALUE=0])
AC_DEFINE_UNQUOTED(OMPI_HAVE_SA_LEN, $VALUE,
    [Whether we have the sa_len struct in <sys/socket.h> or not])
AC_MSG_RESULT([$MSG])

AC_CHECK_MEMBERS([struct dirent.d_type], [], [], [
#include <sys/types.h>
#include <dirent.h>])

AC_CHECK_MEMBERS([siginfo_t.si_fd],,,[#include <signal.h>])
AC_CHECK_MEMBERS([siginfo_t.si_band],,,[#include <signal.h>])


# checkpoint results
AC_CACHE_SAVE


##################################
# Libraries
##################################

ompi_show_title "Library and Function tests"

# Darwin doesn't need -lutil, as it's something other than this -lutil.
OMPI_CHECK_FUNC_LIB([openpty], [util])

AC_CHECK_LIB([nsl], [gethostbyname])

AC_CHECK_LIB([socket], [socket])

# Solaris has sched_yeild in -lrt, usually in libc
OMPI_CHECK_FUNC_LIB([sched_yield], [rt])

# IRIX has dirname in -lgen, usually in libc
OMPI_CHECK_FUNC_LIB([dirname], [gen])

# Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
OMPI_CHECK_FUNC_LIB([ceil], [m])

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf openpty isatty htonl ntohl htons ntohs getpwuid fork waitpid execve pipe ptsname setsid mmap mallopt tcgetpgrp posix_memalign strsignal _NSGetEnviron])

#
# Make sure we can copy va_lists (need check declared, not linkable)
#

AC_CHECK_DECL(va_copy, OMPI_HAVE_VA_COPY=1, OMPI_HAVE_VA_COPY=0, 
	      [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(OMPI_HAVE_VA_COPY, $OMPI_HAVE_VA_COPY,
    [Whether we have va_copy or not])

AC_CHECK_DECL(__va_copy, OMPI_HAVE_UNDERSCORE_VA_COPY=1,
	      OMPI_HAVE_UNDERSCORE_VA_COPY=0, [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(OMPI_HAVE_UNDERSCORE_VA_COPY, $OMPI_HAVE_UNDERSCORE_VA_COPY,
    [Whether we have __va_copy or not])

AC_CHECK_DECLS(__func__)

# checkpoint results
AC_CACHE_SAVE


##################################
# System-specific tests
##################################

ompi_show_title "System-specific tests"

#
# Test to determine type of MPI_Offset. This is searched in the following order
# int64_t, long long, long, int. If none of these are 8 bytes, then we should
# search for int32_t, long long, long, int.
#
MPI_OFFSET_TYPE="not found"
MPI_OFFSET_DATATYPE="not found"
AC_MSG_CHECKING([checking for type of MPI_Offset])
if test "$ac_cv_type_long_long" = "yes" -a "$ac_cv_sizeof_long_long" = 8; then
	MPI_OFFSET_TYPE="long long"
        MPI_OFFSET_DATATYPE=MPI_LONG_LONG
        MPI_OFFSET_SIZE=8
elif test "$ac_cv_type_long" = "yes" -a "$ac_cv_sizeof_long" = 8; then
	MPI_OFFSET_TYPE="long"
        MPI_OFFSET_DATATYPE=MPI_LONG
        MPI_OFFSET_SIZE=8
elif test "ac_cv_sizeof_int" = 8; then
	MPI_OFFSET_TYPE=int
        MPI_OFFSET_DATATYPE=MPI_INT
        MPI_OFFSET_SIZE=8
elif test "$ac_cv_type_long_long" = "yes" -a "$ac_cv_sizeof_long_long" = 4; then
	MPI_OFFSET_TYPE="long long"
        MPI_OFFSET_DATATYPE=MPI_LONG_LONG
        MPI_OFFSET_SIZE=4
elif test "$ac_cv_type_long" = "yes" -a "$ac_cv_sizeof_long" = 4; then
	MPI_OFFSET_TYPE="long"
        MPI_OFFSET_DATATYPE=MPI_LONG
        MPI_OFFSET_SIZE=4
elif test "ac_cv_sizeof_int" = 4; then
	MPI_OFFSET_TYPE=int
        MPI_OFFSET_DATATYPE=MPI_INT
        MPI_OFFSET_SIZE=4
fi
AC_MSG_RESULT([$MPI_OFFSET_TYPE])
if test "$MPI_OFFSET_TYPE" = "not found"; then
	AC_MSG_WARN([*** Unable to find the right definition for MPI_Offset])
	AC_MSG_ERROR([Cannot continue])
fi
AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_TYPE, $MPI_OFFSET_TYPE, [Type of MPI_Offset -- has to be defined here and typedef'ed later because mpi.h does not get AC SUBST's])

#
# Check for MPI_Aint type.  Yes, there are platforms where
# sizeof(void*) != sizeof(long) (64 bit Windows, apparently).
#
if test $ac_cv_type_ptrdiff_t = yes ; then
    ompi_ptrdiff_t="ptrdiff_t"
elif test $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long ; then
    ompi_ptrdiff_t="long"
elif test $ac_cv_type_long_long = yes -a $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long_long ; then
    ompi_ptrdiff_t="long long"
else
    AC_MSG_ERROR([Could not find datatype to emulate ptrdiff_t.  Cannot continue])
fi
AC_DEFINE_UNQUOTED([OMPI_PTRDIFF_TYPE], [$ompi_ptrdiff_t],
    [type to use for ptrdiff_t])

#
# If we haven't already, figure out an MPI datatype that corresponds
# to the back-end C type of MPI_Offset.
#
AC_MSG_CHECKING([checking for an MPI datatype for MPI_Offset])
AC_MSG_RESULT([$MPI_OFFSET_DATATYPE])
if test "$MPI_OFFSET_DATATYPE" = "not found"; then
	AC_MSG_WARN([*** Unable to find an MPI datatype corresponding to MPI_Offset])
	AC_MSG_ERROR([Cannot continue])
fi
AC_DEFINE_UNQUOTED(OMPI_OFFSET_DATATYPE, $MPI_OFFSET_DATATYPE, [MPI datatype corresponding to MPI_Offset])

# all: endian

AC_C_BIGENDIAN

OMPI_CHECK_BROKEN_QSORT

AC_CACHE_CHECK([if word-sized integers must be word-size aligned],
    [ompi_cv_c_word_size_align],
    [AC_LANG_PUSH(C)
     AC_RUN_IFELSE([AC_LANG_PROGRAM([dnl
#include <stdlib.h>], [[    long data[2] = {0, 0};
    long *lp;
    int *ip;
    ip = (int*) data;
    ip++;
    lp = (long*) ip;
    return lp[0]; ]])],
        [ompi_cv_c_word_size_align=no],
        [ompi_cv_c_word_size_align=yes],
        [ompi_cv_c_word_size_align=yes])])
AS_IF([test $ompi_cv_c_word_size_align = yes], [results=1], [results=0])
AC_DEFINE_UNQUOTED([OMPI_ALIGN_WORD_SIZE_INTEGERS], [$results],
    [set to 1 if word-size integers must be aligned to word-size padding to prevent bus errors])

# all: SYSV semaphores
# all: SYSV shared memory
# all: size of FD_SET
# all: sizeof struct stat members
# all: type of getsockopt optlen
# all: type of recvfrom optlen

#
# Check out what thread support we have
#

OMPI_CONFIG_THREADS
CFLAGS="$CFLAGS $THREAD_CFLAGS"
CXXFLAGS="$CXXFLAGS $THREAD_CXXFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
CXXCPPFLAGS="$CXXCPPFLAGS $THREAD_CXXCPPFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS="$LIBS $THREAD_LIBS"


WRAPPER_EXTRA_CPPFLAGS="$WRAPPER_EXTRA_CPPFLAGS $THREAD_CPPFLAGS"
WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $THREAD_CFLAGS"
WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS $THREAD_CXXFLAGS"
WRAPPER_EXTRA_FFLAGS="$WRAPPER_EXTRA_FFLAGS $THREAD_FFLAGS"
WRAPPER_EXTRA_FCFLAGS="$WRAPPER_EXTRA_FCFLAGS $THREAD_FFLAGS"
WRAPPER_EXTRA_LDFLAGS="$WRAPPER_EXTRA_LDFLAGS $THREAD_LDFLAGS"
# no need to update WRAPPER_EXTRA_LIBS - we'll get it from LT later

#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S

#
# We need as and lex
#
AM_PROG_AS
AM_PROG_LEX

# If we don't have GNU Flex and we don't have a generated .c file
# (distribution tarballs will have the .c file included, but SVN
# checkouts will not), then error.  Must have GNU Flex -- other
# versions of Lex are not workable (all things being equal, since this
# is *only* required for developers, we decided that it really was not
# worth it to be portable between different versions of lex ;-).

if test -z "$LEX" -o -n "`echo $LEX | grep missing`" -o \
    "`basename $LEX`" != "flex"; then
    if test ! -f "$srcdir/opal/util/show_help_lex.c"; then
        AC_MSG_WARN([*** Could not find GNU Flex on your system.])
        AC_MSG_WARN([*** GNU Flex required for developer builds of Open MPI.])
        AC_MSG_WARN([*** Other versions of Lex are not supported.])
        AC_MSG_WARN([*** YOU DO NOT NEED FLEX FOR DISTRIBUTION TARBALLS!])
        AC_MSG_WARN([*** If you absolutely cannot install GNU Flex on this system])
        AC_MSG_WARN([*** consider using a distribution tarball, or generate the])
        AC_MSG_WARN([*** following files on another system (using Flex) and])
        AC_MSG_WARN([*** copy them here:])
        for lfile in `find . -name \*.l -print`; do
            cfile="`echo $lfile | cut -d. -f-2`"
            AC_MSG_WARN([***     $cfile.c])
        done
        AC_MSG_ERROR([Cannot continue])
    fi
fi

#
# File system case sensitivity
#

OMPI_CASE_SENSITIVE_FS_SETUP

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy

# checkpoint results
AC_CACHE_SAVE


##################################
# MCA
##################################

ompi_show_title "Modular Component Architecture (MCA) setup" 

AC_MSG_CHECKING([for subdir args])
OMPI_CONFIG_SUBDIR_ARGS([ompi_subdir_args])
AC_MSG_RESULT([$ompi_subdir_args])

OMPI_MCA

# checkpoint results
AC_CACHE_SAVE


############################################################################
# Final top-level OMPI configuration
############################################################################

ompi_show_title "Final top-level OMPI configuration"

############################################################################
# Libevent setup
############################################################################

ompi_show_subtitle "Libevent 3rd party event handler"
CFLAGS_save="$CFLAGS"
CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY"
OMPI_SETUP_LIBEVENT
CFLAGS="$CFLAGS_save"


############################################################################
# Libtool: part two
# (after C compiler setup)
############################################################################

ompi_show_subtitle "Libtool configuration"

# Use convenience libltdl for the moment, because we need to force the
# use of the newest libltdl (i.e., the one that ships with libtool
# 1.5) because it has support for a lot more things than older
# versions of libtool (which are generally installed by default).
AS_IF([test "$OMPI_ENABLE_DLOPEN_SUPPORT" = "1"], 
       [AC_LIBLTDL_CONVENIENCE(opal/libltdl)
        AC_LIBTOOL_DLOPEN])
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
AM_PROG_LIBTOOL

# AC_CONFIG_SUBDIRS appears to be broken for non-gcc compilers (i.e.,
# passing precious variables down to the sub-configure).  
#
# Note that we also did some magic scripty-foo in autogen.sh to rename
# the output libtool library "libompi_ltdl", so add -lompi_ltdl here.
# This is because so many systems have older versions of libltdl
# installed very early in ld.so's search path; if we installed our own
# libltdl in some non-standard path (e.g., $HOME/local or even
# /usr/local), the libltdl in /usr/lib might get found first.  And if
# it's older -- or just not matching the version that we need, Bad
# Things happen.  [sigh]
#
# Finally, make libompi_ltdl follow the same shared/static convention
# that was user for the main OMPI libraries.  So manually examine
# $enable_shared and $enable_static and pass down the corresponding
# flags.

ompi_show_subtitle "GNU libltdl setup" 

if test "$OMPI_ENABLE_DLOPEN_SUPPORT" = "1" ; then
    ompi_subdir_args="$ompi_subdir_args --enable-ltdl-convenience --disable-ltdl-install"
    if test "$enable_shared" = "yes"; then
        ompi_subdir_args="$ompi_subdir_args --enable-shared"
    else
        ompi_subdir_args="$ompi_subdir_args --disable-shared"
    fi
    if test "$enable_static" = "yes"; then
        ompi_subdir_args="$ompi_subdir_args --enable-static"
    else
        ompi_subdir_args="$ompi_subdir_args --disable-static"
    fi

    CFLAGS_save="$CFLAGS"
    CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY"
    OMPI_CONFIG_SUBDIR(opal/libltdl, [$ompi_subdir_args], [HAPPY=1], [HAPPY=0])
    if test "$HAPPY" = "1"; then
        LIBLTDL_SUBDIR=libltdl
        LIBLTDL_LTLIB=libltdl/libltdlc.la
        WANT_LIBLTDL=1

        # Arrgh.  This is gross.  But I can't think of any other way to do
        # it.  :-(

        flags="`egrep ^LIBADD_DL opal/libltdl/Makefile | cut -d= -f2-`"
        OMPI_CHECK_LINKER_FLAGS([opal/libltdl/libtool], [-export-dynamic $flags])
        WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS $extra_ldflags"
        LDFLAGS="-export-dynamic $LDFLAGS"
    else
        AC_MSG_ERROR([Failed to build GNU libltdl.  This usually means that something
is incorrectly setup with your environment.  There may be useful information in
opal/libltdl/config.log.  You can also disable GNU libltdl (which will disable
dynamic shared object loading) by configuring with --disable-dlopen.])
    fi
    CFLAGS="$CFLAGS_save"
    OPAL_LTDL_CPPFLAGS='-I$(top_srcdir)/opal/libltdl'
else
    AC_MSG_WARN([libltdl support disabled (by --disable-dlopen)])

    LIBLTDL_SUBDIR=
    LIBLTDL_LTLIB=
    WANT_LIBLTDL=0
    OPAL_LTDL_CPPFLAGS=

    # append instead of prepend, since LIBS are going to be system
    # type things needed by everyone.  Normally, libltdl will push
    # these into LIBS, but since we've disabled libltdl, we need
    # to do it here.
    WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS $LIBS"
fi

AC_SUBST(LIBLTDL_SUBDIR)
AC_SUBST(LIBLTDL_LTLIB)
# Have a variable that can be used to indicate where ltdl.h lives.  Do
# this specifically so that we get the included ltdl.h (which we know
# will work properly with the compiled code, including libltdl), not
# the system one (which is likely to be far too old and buggy).
AC_SUBST(OPAL_LTDL_CPPFLAGS)

AM_CONDITIONAL(WANT_LIBLTDL, test "$WANT_LIBLTDL" = "1")
AC_DEFINE_UNQUOTED(OMPI_WANT_LIBLTDL, $WANT_LIBLTDL,
    [Whether to include support for libltdl or not])


############################################################################
# final compiler config
############################################################################

ompi_show_subtitle "Compiler flags"

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory.  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#
# Because opal_config.h, orte_config.h, and ompi_config.h are all
# created by AC_CONFIG_HEADERS, we don't need to -I the builddir for
# <project>/include.  If we VPATH building, we do need to include the
# source directories, however.
#
if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    CPPFLAGS='-I$(top_srcdir) -I$(top_builddir) -I$(top_srcdir)/opal/include -I$(top_srcdir)/orte/include -I$(top_srcdir)/ompi/include'" $CPPFLAGS"
    CXXCPPFLAGS='-I$(top_srcdir) -I$(top_builddir) -I$(top_srcdir)/opal/include -I$(top_srcdir)/orte/include -I$(top_srcdir)/ompi/include'" $CXXCPPFLAGS"
else
    CPPFLAGS='-I$(top_srcdir)'" $CPPFLAGS"
    CXXCPPFLAGS='-I$(top_srcdir)'" $CXXCPPFLAGS"
fi

#
# Do a final process of the CFLAGS to make a WITHOUT_OPTFLAGS version.
# We need this so that we can guarantee to build the TotalView stuff
# with -g and nothing else.
#

OMPI_MAKE_STRIPPED_FLAGS($CFLAGS)
CFLAGS_WITHOUT_OPTFLAGS="$s_result"
if test "$with_tv_debug_flags" != ""; then
    TOTALVIEW_DEBUG_FLAGS="$with_tv_debug_flags"
else
    TOTALVIEW_DEBUG_FLAGS="-g"
fi
AC_MSG_CHECKING([which of CFLAGS are ok for TotalView modules])
AC_MSG_RESULT([$CFLAGS_WITHOUT_OPTFLAGS])
AC_MSG_CHECKING([extra CFLAGS for TotalView modules])
AC_MSG_RESULT([$TOTALVIEW_DEBUG_FLAGS])

AC_SUBST(CFLAGS_WITHOUT_OPTFLAGS)
AC_SUBST(TOTALVIEW_DEBUG_FLAGS)


#
# Delayed the substitution of CFLAGS and CXXFLAGS until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CXXCPPFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(FCFLAGS)

############################################################################
# final wrapper compiler config
############################################################################

ompi_show_subtitle "Wrapper compiler flags"
OMPI_SETUP_WRAPPER_FINAL

# Recreate some defines prefixed with OMPI_ so that there are no bare
# autoconf macro defines in mpi.h.  Since AC sometimes changes whether
# things are defined as null tokens or an integer result, two projects
# with different versions of AC can cause problems.
if test $ac_cv_header_stdc = yes; then
  AC_DEFINE(OMPI_STDC_HEADERS, 1,
	    [Do not use outside of mpi.h.  Define to 1 if you have the ANSI C header files.])
fi
if test $ac_cv_header_sys_time_h = yes ; then
  AC_DEFINE(OMPI_HAVE_SYS_TIME_H, 1,
	    [Do not use outside of mpi.h.  Define to 1 if you have the <sys/time.h> header file.])
fi
if test $ac_cv_type_long_long = yes ; then
  AC_DEFINE(OMPI_HAVE_LONG_LONG, 1,
	    [Do not use outside of mpi.h.  Define to 1 if the system has the type `long long'.]) dnl `
fi
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_BOOL, $ac_cv_sizeof_bool,
          [Do not use outside of mpi.h.  The size of a `bool', as computed by sizeof.]) dnl `
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_INT, $ac_cv_sizeof_int,
          [Do not use outside of mpi.h.  The size of a `int', as computed by sizeof.]) dnl `

############################################################################
# Party on
############################################################################

ompi_show_subtitle "Final output" 

AM_CONFIG_HEADER([opal/include/opal_config.h orte/include/orte_config.h ompi/include/ompi_config.h ompi/include/mpi.h])
AC_CONFIG_FILES([
    Makefile

    config/Makefile

    contrib/Makefile

    opal/Makefile
    opal/etc/Makefile
    opal/include/Makefile
    opal/asm/Makefile
    opal/event/Makefile
    opal/event/compat/Makefile
    opal/event/compat/sys/Makefile
    opal/util/Makefile
    opal/util/keyval/Makefile
    opal/mca/base/Makefile
    opal/tools/wrappers/Makefile
    opal/tools/wrappers/opalcc-wrapper-data.txt
    opal/tools/wrappers/opalc++-wrapper-data.txt

    orte/Makefile
    orte/include/Makefile
    orte/etc/Makefile

    orte/tools/orted/Makefile
    orte/tools/orterun/Makefile
    orte/tools/wrappers/Makefile
    orte/tools/wrappers/ortecc-wrapper-data.txt
    orte/tools/wrappers/ortec++-wrapper-data.txt

    ompi/Makefile
    ompi/etc/Makefile
    ompi/include/Makefile
    ompi/include/mpif.h
    ompi/include/mpif-config.h

    ompi/datatype/Makefile
    ompi/debuggers/Makefile

    ompi/mpi/Makefile
    ompi/mpi/c/Makefile
    ompi/mpi/c/profile/Makefile
    ompi/mpi/cxx/Makefile
    ompi/mpi/f77/Makefile
    ompi/mpi/f77/profile/Makefile
    ompi/mpi/f90/Makefile
    ompi/mpi/f90/fortran_kinds.sh
    ompi/mpi/f90/fortran_sizes.h
    ompi/mpi/f90/scripts/Makefile

    ompi/tools/ompi_info/Makefile
    ompi/tools/wrappers/Makefile
    ompi/tools/wrappers/mpicc-wrapper-data.txt
    ompi/tools/wrappers/mpic++-wrapper-data.txt
    ompi/tools/wrappers/mpif77-wrapper-data.txt
    ompi/tools/wrappers/mpif90-wrapper-data.txt
    ompi/tools/ortetools/Makefile

    test/Makefile
    test/event/Makefile
    test/asm/Makefile
    test/class/Makefile
    test/memory/Makefile
    test/support/Makefile
    test/threads/Makefile
    test/peruse/Makefile
    test/datatype/Makefile
])
AC_OUTPUT
