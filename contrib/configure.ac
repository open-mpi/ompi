# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
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

AC_INIT(./src/mpi/c/init.c)
AC_PREREQ(2.58)
AC_CONFIG_AUX_DIR(./config)

# Get the version of OMPI that we are installing

OMPI_GET_VERSION($srcdir/VERSION, OMPI)

AC_SUBST(OMPI_MAJOR_VERSION)
AC_SUBST(OMPI_MINOR_VERSION)
AC_SUBST(OMPI_RELEASE_VERSION)
AC_SUBST(OMPI_ALPHA_VERSION)
AC_SUBST(OMPI_BETA_VERSION)
AC_SUBST(OMPI_SVN_VERSION)
AC_SUBST(OMPI_VERSION)

#
# Start it up
#

OMPI_CONFIGURE_SETUP
ompi_show_title "Configuring OMPI version $OMPI_VERSION"
ompi_show_subtitle "Initialization, setup"

#
# Init automake
# The third argument to AM_INIT_AUTOMAKE surpresses the PACKAGE and
# VERSION macors
#

AM_INIT_AUTOMAKE(openmpi, $OMPI_VERSION, 'no')

#
# GNU C and autotools are inconsistent about whether this is defined
# so let's make it true everywhere for now...
#
AC_GNU_SOURCE

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

# Setup the top of the include/ompi_config.h file

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

#ifndef OMPI_CONFIG_H
#define OMPI_CONFIG_H
])
AH_BOTTOM([
#include "ompi_config_bottom.h"
#endif /* OMPI_CONFIG_H */
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


############################################################################
# Check for compilers and preprocessors
############################################################################
ompi_show_title "Compiler and preprocessor tests"

##################################
# C compiler characteristics
##################################

OMPI_SETUP_CC

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
AC_C_RESTRICT
OMPI_C_WEAK_SYMBOLS
if test "$GCC" = "yes"; then
    CFLAGS="$CFLAGS_save"
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

AC_LANG_SAVE
AC_LANG_CPLUSPLUS
#AC_CHECK_SIZEOF(BOOL)

# check for type alignments

#OMPI_C_GET_ALIGNMENT(BOOL, OMPI_ALIGNMENT_CXX_BOOL)
AC_LANG_RESTORE

# check if we want C++ support                                                  

AM_CONDITIONAL(WANT_MPI_CXX_BINDINGS,
    test "$WANT_MPI_CXX_SUPPORT" = 1)
AC_DEFINE_UNQUOTED(OMPI_WANT_CXX_BINDINGS, $WANT_MPI_CXX_SUPPORT,
    [Whether we want MPI cxx support or not])
    

##################################
# Fortran
##################################

OMPI_HAVE_FORTRAN_INTEGER1=0
OMPI_HAVE_FORTRAN_INTEGER2=0
OMPI_HAVE_FORTRAN_INTEGER4=0
OMPI_HAVE_FORTRAN_INTEGER8=0
OMPI_HAVE_FORTRAN_INTEGER16=0
OMPI_HAVE_FORTRAN_REAL4=0
OMPI_HAVE_FORTRAN_REAL8=0
OMPI_HAVE_FORTRAN_REAL16=0
OMPI_HAVE_FORTRAN_COMPLEX8=0
OMPI_HAVE_FORTRAN_COMPLEX16=0
OMPI_HAVE_FORTRAN_COMPLEX32=0

OMPI_SIZEOF_FORTRAN_INT=0
OMPI_SIZEOF_FORTRAN_REAL=0
OMPI_SIZEOF_FORTRAN_DBLPREC=0
OMPI_SIZEOF_FORTRAN_COMPLEX=0
OMPI_SIZEOF_FORTRAN_DBLCOMPLEX=0

OMPI_ALIGNMENT_FORTRAN_INT=0
OMPI_ALIGNMENT_FORTRAN_REAL=0
OMPI_ALIGNMENT_FORTRAN_DBLPREC=0
OMPI_ALIGNMENT_FORTRAN_COMPLEX=0
OMPI_ALIGNMENT_FORTRAN_DBLCOMPLEX=0

OMPI_SETUP_F77
OMPI_F77_FIND_EXT_SYMBOL_CONVENTION($OMPI_F77)

# Even if we don't want fortran support, we have to have a size for
# INTEGER because it's needed to define MPI_Fint, which is part of
# mpi.h.  Hence, if we don't want fortran support, we set the size of
# Fortran INTEGER's to sizeof(int).
#
# Similarly, we need C types for LOGICAL, REAL, and DOUBLE PRECISION
# for the MPI_Op reduction back-end routines.

if test "$OMPI_WANT_F77_BINDINGS" = "0" ; then
    OMPI_SIZEOF_FORTRAN_LOGICAL=$ac_cv_sizeof_int
    OMPI_SIZEOF_FORTRAN_INT=$ac_cv_sizeof_int
    OMPI_SIZEOF_FORTRAN_REAL=$ac_cv_sizeof_float
    OMPI_SIZEOF_FORTRAN_DBLPREC=$ac_cv_sizeof_double
else
    # If we want fortran support, then get the sizes and alignments of
    # all the rest of the fortran types

    OMPI_F77_CHECK_TYPE(INTEGER*1, OMPI_HAVE_FORTRAN_INTEGER1)
    OMPI_F77_CHECK_TYPE(INTEGER*2, OMPI_HAVE_FORTRAN_INTEGER2)
    OMPI_F77_CHECK_TYPE(INTEGER*4, OMPI_HAVE_FORTRAN_INTEGER4)
    OMPI_F77_CHECK_TYPE(INTEGER*8, OMPI_HAVE_FORTRAN_INTEGER8)
    OMPI_F77_CHECK_TYPE(INTEGER*16, OMPI_HAVE_FORTRAN_INTEGER16)

    OMPI_F77_CHECK_TYPE(REAL*4, OMPI_HAVE_FORTRAN_REAL4)
    OMPI_F77_CHECK_TYPE(REAL*8, OMPI_HAVE_FORTRAN_REAL8)
    OMPI_F77_CHECK_TYPE(REAL*16, OMPI_HAVE_FORTRAN_REAL16)

    OMPI_F77_CHECK_TYPE(COMPLEX*8, OMPI_HAVE_FORTRAN_COMPLEX8)
    OMPI_F77_CHECK_TYPE(COMPLEX*16, OMPI_HAVE_FORTRAN_COMPLEX16)
    OMPI_F77_CHECK_TYPE(COMPLEX*32, OMPI_HAVE_FORTRAN_COMPLEX32)

    OMPI_F77_GET_SIZEOF(LOGICAL, OMPI_SIZEOF_FORTRAN_LOGICAL)
    OMPI_F77_GET_SIZEOF(INTEGER, OMPI_SIZEOF_FORTRAN_INT)
    OMPI_F77_GET_SIZEOF(REAL, OMPI_SIZEOF_FORTRAN_REAL)
    OMPI_F77_GET_SIZEOF(DOUBLE PRECISION, OMPI_SIZEOF_FORTRAN_DBLPREC)
    OMPI_F77_GET_SIZEOF(COMPLEX, OMPI_SIZEOF_FORTRAN_COMPLEX)
    OMPI_F77_GET_SIZEOF(DOUBLE COMPLEX, OMPI_SIZEOF_FORTRAN_DBLCOMPLEX)

    OMPI_F77_GET_ALIGNMENT(LOGICAL, OMPI_ALIGNMENT_FORTRAN_LOGICAL)
    OMPI_F77_GET_ALIGNMENT(INTEGER, OMPI_ALIGNMENT_FORTRAN_INT)
    OMPI_F77_GET_ALIGNMENT(REAL, OMPI_ALIGNMENT_FORTRAN_REAL)
    OMPI_F77_GET_ALIGNMENT(DOUBLE PRECISION, OMPI_ALIGNMENT_FORTRAN_DBLPREC)
    OMPI_F77_GET_ALIGNMENT(COMPLEX, OMPI_ALIGNMENT_FORTRAN_COMPLEX)
    OMPI_F77_GET_ALIGNMENT(DOUBLE COMPLEX, OMPI_ALIGNMENT_FORTRAN_DBLCOMPLEX)
fi

AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_INTEGER1, $OMPI_HAVE_FORTRAN_INTEGER1,
    [Whether we have FORTRAN INTEGER1 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_INTEGER2, $OMPI_HAVE_FORTRAN_INTEGER2,
    [Whether we have FORTRAN INTEGER2 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_INTEGER4, $OMPI_HAVE_FORTRAN_INTEGER4,
    [Whether we have FORTRAN INTEGER4 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_INTEGER8, $OMPI_HAVE_FORTRAN_INTEGER8,
    [Whether we have FORTRAN INTEGER8 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_INTEGER16, $OMPI_HAVE_FORTRAN_INTEGER16,
    [Whether we have FORTRAN INTEGER16 or not])

AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_REAL4, $OMPI_HAVE_FORTRAN_REAL4,
    [Whether we have FORTRAN REAL4 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_REAL8, $OMPI_HAVE_FORTRAN_REAL8,
    [Whether we have FORTRAN REAL8 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_REAL16, $OMPI_HAVE_FORTRAN_REAL16,
    [Whether we have FORTRAN REAL16 or not])

AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_COMPLEX8, $OMPI_HAVE_FORTRAN_COMPLEX8,
    [Whether we have FORTRAN COMPLEX8 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_COMPLEX16, $OMPI_HAVE_FORTRAN_COMPLEX16,
    [Whether we have FORTRAN COMPLEX16 or not])
AC_DEFINE_UNQUOTED(OMPI_HAVE_FORTRAN_COMPLEX32, $OMPI_HAVE_FORTRAN_COMPLEX32,
    [Whether we have FORTRAN COMPLEX32 or not])

AC_DEFINE_UNQUOTED(OMPI_SIZEOF_FORTRAN_LOGICAL, $OMPI_SIZEOF_FORTRAN_LOGICAL,
    [Size of fortran logical])
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_FORTRAN_INT, $OMPI_SIZEOF_FORTRAN_INT,
    [Size of fortran integer])
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_FORTRAN_REAL, $OMPI_SIZEOF_FORTRAN_REAL,
    [Size of fortran real])
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_FORTRAN_DBLPREC, $OMPI_SIZEOF_FORTRAN_DBLPREC,
    [Size of fortran double precision])
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_FORTRAN_COMPLEX, $OMPI_SIZEOF_FORTRAN_COMPLEX,
    [Size of fortran complex])
AC_DEFINE_UNQUOTED(OMPI_SIZEOF_FORTRAN_DBLCOMPLEX, 
    $OMPI_SIZEOF_FORTRAN_DBLCOMPLEX,
    [Size of fortran double complex])

AC_DEFINE_UNQUOTED(OMPI_ALIGNMENT_FORTRAN_LOGICAL, 
    $OMPI_ALIGNMENT_FORTRAN_LOGICAL,
    [Alignment of fortran logical])
AC_DEFINE_UNQUOTED(OMPI_ALIGNMENT_FORTRAN_INT, $OMPI_ALIGNMENT_FORTRAN_INT,
    [Alignment of fortran integer])
AC_DEFINE_UNQUOTED(OMPI_ALIGNMENT_FORTRAN_REAL, $OMPI_ALIGNMENT_FORTRAN_REAL,
    [alignment of fortran real])
AC_DEFINE_UNQUOTED(OMPI_ALIGNMENT_FORTRAN_DBLPREC, 
    $OMPI_ALIGNMENT_FORTRAN_DBLPREC,
    [Alignment of fortran double precision])
AC_DEFINE_UNQUOTED(OMPI_ALIGNMENT_FORTRAN_COMPLEX, 
    $OMPI_ALIGNMENT_FORTRAN_COMPLEX,
    [Alignment of fortran complex])
AC_DEFINE_UNQUOTED(OMPI_ALIGNMENT_FORTRAN_DBLCOMPLEX, 
    $OMPI_ALIGNMENT_FORTRAN_DBLCOMPLEX,
    [Alignment of fortran double complex])

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

#
# Fortran 90 setup
#

##################################
# Header files
##################################

ompi_show_title "Header file tests"

AC_CHECK_HEADERS([alloca.h aio.h arpa/inet.h dirent.h dlfcn.h \
    inttypes.h libgen.h net/if.h netdb.h netinet/in.h netinet/tcp.h \
    poll.h pthread.h pwd.h sched.h stdint.h strings.h stropts.h \
    sys/ipc.h sys/mman.h sys/resource.h sys/select.h sys/socket.h \
    sys/statvfs.h sys/time.h sys/uio.h sys/utsname.h sys/wait.h \
    syslog.h termios.h ulimit.h unistd.h])

# snprintf declaration
# gethostname declaration
# headers:
#   stropts.h grh.h netinet/tcp.h sys/select.h sys/resource.h pty.h util.h
#   rpc/types.h rpc/xdr.h sched.h strings.h

# SA_RESTART in signal.h
# sa_len in struct sockaddr
# union semun in sys/sem.h

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


##################################
# Libraries
##################################

ompi_show_title "Library and Function tests"

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

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf])

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


##################################
# System-specific tests
##################################

ompi_show_title "System-specific tests"

#
# Determine corresponding C types for Fortran: LOGICAL, INTEGER, REAL,
# DOUBLE PRECISION.  If we don't have a fortran compiler, assume some
# sensible defaults (although the values don't really matter).  We
# need these types for the back-end MPI_Op reduction functions,
# regardless of whether we have a fortran compiler or not.
#

if test "$OMPI_F77" != "none"; then
        OMPI_FIND_TYPE(Fortran LOGICAL, FORTRAN_LOGICAL,
            char int int32_t)
        OMPI_FIND_TYPE(Fortran INTEGER, FORTRAN_INT, 
            int32_t int intr64_t long:long long)
        OMPI_FIND_TYPE(Fortran REAL, FORTRAN_REAL,
            float double long:double)
        OMPI_FIND_TYPE(Fortran DOUBLE PRECISION, FORTRAN_DBLPREC,
            float double long:double)
else
        MPI_FORTRAN_LOGICAL_TYPE=char
	MPI_FORTRAN_INT_TYPE=int
        MPI_FORTRAN_REAL_TYPE=float
        MPI_FORTRAN_DBLPREC_TYPE=double
	AC_MSG_WARN([*** WARNING: Did not detect a f77 compiler.  Assuming default corresponding types])
        AC_MSG_WARN([*** Fortran LOGICAL = C $MPI_FORTRAN_LOGICAL_TYPE])
        AC_MSG_WARN([*** Fortran INTEGER = C $MPI_FORTRAN_INT_TYPE])
        AC_MSG_WARN([*** Fortran REAL = C $MPI_FORTRAN_REAL_TYPE])
        AC_MSG_WARN([*** Fortran DOUBLE PRECISION = C $MPI_FORTRAN_DBLPREC_TYPE])
fi
AC_DEFINE_UNQUOTED(MPI_Fint, $MPI_FORTRAN_INT_TYPE, 
    [C type corresponding to Fortran INTEGER])

AC_DEFINE_UNQUOTED(ompi_fortran_logical_t, $MPI_FORTRAN_LOGICAL_TYPE, 
    [C type corresponding to Fortran LOGICAL])
AC_DEFINE_UNQUOTED(ompi_fortran_integer_t, $MPI_FORTRAN_INT_TYPE, 
    [C type corresponding to Fortran LOGICAL])
AC_DEFINE_UNQUOTED(ompi_fortran_real_t, $MPI_FORTRAN_REAL_TYPE, 
    [C type corresponding to Fortran REAL])
AC_DEFINE_UNQUOTED(ompi_fortran_dblprec_t, $MPI_FORTRAN_DBLPREC_TYPE, 
    [C type corresponding to Fortran DOUBLE PRECISION])

#
# Test to determine type of MPI_Offset. This is searched in the following order
# int64_t, long long, long, int. If none of these are 8 bytes, then we should
# search for int32_t, long long, long, int.
#
MPI_OFFSET_TYPE="not found"
AC_MSG_CHECKING([checking for type of MPI_Offset])
if test "$ac_cv_type_int64_t" == "yes"; then
	MPI_OFFSET_TYPE=int64_t
elif test "$ac_cv_type_long_long_t" == "yes" -a "$ac_cv_sizeof_long_long" == 8; then
	MPI_OFFSET_TYPE="long long"
elif test "$ac_cv_type_long_t" == "yes" -a "$ac_cv_sizeof_long" == 8; then
	MPI_OFFSET_TYPE="long"
elif test "ac_cv_sizeof_int" == 8; then
	MPI_OFFSET_TYPE=int
elif test "$ac_cv_type_int32_t" == "yes"; then
	MPI_OFFSET_TYPE=int32_t
elif test "$ac_cv_type_long_long_t" == "yes" -a "$ac_cv_sizeof_long_long" == 4; then
	MPI_OFFSET_TYPE="long long"
elif test "$ac_cv_type_long_t" == "yes" -a "$ac_cv_sizeof_long" == 4; then
	MPI_OFFSET_TYPE="long"
elif test "ac_cv_sizeof_int" == 4; then
	MPI_OFFSET_TYPE=int
fi
AC_MSG_RESULT([$MPI_OFFSET_TYPE])
if test "$MPI_FINT_TYPE" = "not found"; then
	AC_MSG_WARN([*** WARNING: Unable to find the right definition for MPI_Offset])
	AC_MSG_ERROR([Cannot continue])
fi
AC_DEFINE_UNQUOTED(MPI_Offset, $MPI_OFFSET_TYPE, [Type of MPI_Offset])


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

WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $THREAD_CFLAGS"
WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS $THREAD_CXXFLAGS"
WRAPPER_EXTRA_FFLAGS="$WRAPPER_EXTRA_FFLAGS $THREAD_FFLAGS"
WRAPPER_EXTRA_FCFLAGS="$WRAPPER_EXTRA_FCFLAGS $THREAD_FFLAGS"
WRAPPER_EXTRA_LDFLAGS="$WRAPPER_EXTRA_LDFLAGS $THREAD_LDFLAGS"
WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS $THREAD_LIBS"

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
    if test ! -f "$srcdir/src/util/show_help_lex.c"; then
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


##################################
# MCA
##################################

ompi_show_title "MPI Component Architecture (MCA) setup -- not required now " 

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

ompi_show_subtitle "Libtool configuration skipped"

WANT_LIBLTDL = 0;
AC_DEFINE_UNQUOTED(OMPI_WANT_LIBLTDL, $WANT_LIBLTDL,
    [Whether to include support for libltdl or not])


############################################################################
# final wrapper compiler config
############################################################################

ompi_show_subtitle "Wrapper compiler flags"

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory (don't know why automake doesn't do this
# automatically).  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#

CPPFLAGS='-I$(top_srcdir)/src -I$(top_srcdir)/include -I$(top_builddir)/src'" $CPPFLAGS"
CXXCPPFLAGS='-I$(top_srcdir)/src  -I$(top_srcdir)/include -I$(top_builddir)/src'" $CXXCPPFLAGS"

#
# Adding WRAPPER_* flags so that extra flags needed for wrappper compilers
#
# WRAPPER_EXTRA_CFLAGS
#
AC_MSG_CHECKING([for mpicc CFLAGS])
OMPI_UNIQ(WRAPPER_EXTRA_CFLAGS)
AC_SUBST(WRAPPER_EXTRA_CFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$WRAPPER_EXTRA_CFLAGS",
    [Additional CFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_CFLAGS])

#
# WRAPPER_EXTRA_CXXFLAGS
#
AC_MSG_CHECKING([for mpiCC CXXFLAGS])
OMPI_UNIQ(WRAPPER_EXTRA_CXXFLAGS)
AC_SUBST(WRAPPER_EXTRA_CXXFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS, "$WRAPPER_EXTRA_CXXFLAGS",
    [Additional CXXFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_CXXFLAGS])

#
# WRAPPER_EXTRA_FFLAGS
#
AC_MSG_CHECKING([for mpif77/mpif90 FFLAGS])
OMPI_UNIQ(WRAPPER_EXTRA_FFLAGS)
AC_SUBST(WRAPPER_EXTRA_FFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FFLAGS, "$WRAPPER_EXTRA_FFLAGS",
    [Additional FFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_FFLAGS])

#
# WRAPPER_EXTRA_FCFLAGS
#
AC_MSG_CHECKING([for mpif77/mpif90 FCFLAGS])
OMPI_UNIQ(WRAPPER_EXTRA_FCFLAGS)
AC_SUBST(WRAPPER_EXTRA_FCFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FCFLAGS, "$WRAPPER_EXTRA_FCFLAGS",
    [Additional FCFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_FCFLAGS])

#
# WRAPPER_EXTRA_LDFLAGS
#
AC_MSG_CHECKING([for wrapper compiler LDFLAGS])
OMPI_UNIQ(WRAPPER_EXTRA_LDFLAGS)
AC_SUBST(WRAPPER_EXTRA_LDFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LDFLAGS, "$WRAPPER_EXTRA_LDFLAGS",
    [Additional LDFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_LDFLAGS])

#
# WRAPPER_EXTRA_LIBS
#
AC_MSG_CHECKING([for wrapper compiler LIBS])
AC_SUBST(WRAPPER_EXTRA_LIBS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LIBS, "$WRAPPER_EXTRA_LIBS",
    [Additional LIBS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_LIBS])

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
# Party on
############################################################################

ompi_show_subtitle "Final output" 

AM_CONFIG_HEADER([include/ompi_config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
