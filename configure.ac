# -*- shell-script -*-
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
AC_DEFINE_UNQUOTED(LAM_SVN_VERSION, $LAM_SVN_VERSION, 
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

# Setup the top of the include/lam_config.h file

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
#include "lam_config_bottom.h"
#endif /* LAM_CONFIG_H */
])

# Other basic setup stuff (shared with modules)

LAM_BASIC_SETUP

top_lam_srcdir="$LAM_TOP_SRCDIR"
AC_SUBST(top_lam_srcdir)
top_lam_builddir="$LAM_TOP_BUILDDIR"
AC_SUBST(top_lam_builddir)

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

#
# Check for some types
#

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

#
# Check for type sizes
#

AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(void *)

#
# Check for type alignments
#

LAM_C_GET_ALIGNMENT(char, LAM_ALIGNMENT_CHAR)
LAM_C_GET_ALIGNMENT(short, LAM_ALIGNMENT_SHORT)
LAM_C_GET_ALIGNMENT(wchar_t, LAM_ALIGNMENT_WCHAR)
LAM_C_GET_ALIGNMENT(int, LAM_ALIGNMENT_INT)
LAM_C_GET_ALIGNMENT(long, LAM_ALIGNMENT_LONG)
LAM_C_GET_ALIGNMENT(long long, LAM_ALIGNMENT_LONG_LONG)
LAM_C_GET_ALIGNMENT(float, LAM_ALIGNMENT_FLOAT)
LAM_C_GET_ALIGNMENT(double, LAM_ALIGNMENT_DOUBLE)
LAM_C_GET_ALIGNMENT(long double, LAM_ALIGNMENT_LONG_DOUBLE)
LAM_C_GET_ALIGNMENT(void *, LAM_ALIGNMENT_VOID_P)

#
# Check for other compiler characteristics
#

AC_C_INLINE
AC_C_RESTRICT
AC_CHECK_HEADERS(stdbool.h)
LAM_C_WEAK_SYMBOLS

# If we want the profiling layer:
# - If the C compiler has weak symbols, use those.
# - If not, then set to compile the code again with #define's in a
#   separate directory.

if test "$WANT_WEAK_SYMBOLS" = "0"; then
    LAM_C_HAVE_WEAK_SYMBOLS=0
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
#
# There are 2 layers to the MPI Language binidings
# One layer generates MPI_* bindings. The other layer
# generates PMPI_* bindings. The following conditions
# determine whether each (or both) these layers are 
# built.
# 1. MPI_* bindings are needed if:
#	- Profiling is not required
#   - Profiling is required but weak symbols are not
#	  supported
# 2. PMPI_* bindings are needed if profiling is required.
# Hence we define 2 AM_CONDITIONALs which tell us whether
# each of these layers need to be built or NOT
#
AM_CONDITIONAL(WANT_MPI_BINDINGS_LAYER,
	test "$WANT_MPI_PROFILING" = 0 -o "$LAM_PROFILING_COMPILE_SEPARATELY" = 1)

AM_CONDITIONAL(WANT_PMPI_BINDINGS_LAYER,
	test "$WANT_MPI_PROFILING" = 1)
AM_CONDITIONAL(COMPILE_PROFILING_SEPARATELY, 
    test "$LAM_PROFILING_COMPILE_SEPARATELY" = 1)
AC_DEFINE_UNQUOTED(LAM_ENABLE_MPI_PROFILING, $WANT_MPI_PROFILING,
    [Whether we want MPI profiling or not])
AC_DEFINE_UNQUOTED(LAM_HAVE_WEAK_SYMBOLS, $LAM_C_HAVE_WEAK_SYMBOLS,
    [Wehther we have weak symbols or not])


##################################
# C++ compiler characteristics
##################################

LAM_SETUP_CXX

# check for type sizes

AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_CHECK_SIZEOF(bool)

# check for type alignments

LAM_C_GET_ALIGNMENT(bool, LAM_ALIGNMENT_CXX_BOOL)
AC_LANG_RESTORE


##################################
# Fortran
##################################

LAM_SIZEOF_FORTRAN_INT=0
LAM_SIZEOF_FORTRAN_REAL=0
LAM_SIZEOF_FORTRAN_DBLPREC=0
LAM_SIZEOF_FORTRAN_COMPLEX=0
LAM_SIZEOF_FORTRAN_DBLCOMPLEX=0

LAM_ALIGNMENT_FORTRAN_INTEGER=0
LAM_ALIGNMENT_FORTRAN_REAL=0
LAM_ALIGNMENT_FORTRAN_DBLPREC=0
LAM_ALIGNMENT_FORTRAN_COMPLEX=0
LAM_ALIGNMENT_FORTRAN_DBLCOMPLEX=0

LAM_SETUP_F77
if test "$F77" != "none" ; then
    LAM_F77_FIND_EXT_SYMBOL_CONVENTION($F77)

    # checking the sizeof fortran interger. This is needed to define 
    # MPI_Fint. This is needed for C bindings and hence there is no
    # bearing of --enable-fortran flag on this test.

    LAM_F77_GET_SIZEOF(INTEGER, LAM_SIZEOF_FORTRAN_INT)
fi

# If we want fortran support, then get the sizes and alignments of all
# the rest of the fortran types

if test "$F77" != "none" -a "$WANT_MPI_F77" = "1"; then
    # INTEGER is already done, per above
    LAM_F77_GET_SIZEOF(REAL, LAM_SIZEOF_FORTRAN_REAL)
    LAM_F77_GET_SIZEOF(DOUBLE PRECISION, LAM_SIZEOF_FORTRAN_DBLPREC)
    LAM_F77_GET_SIZEOF(COMPLEX, LAM_SIZEOF_FORTRAN_COMPLEX)
    LAM_F77_GET_SIZEOF(DOUBLE COMPLEX, LAM_SIZEOF_FORTRAN_DBLCOMPLEX)

    LAM_F77_GET_ALIGNMENT(INTEGER, LAM_ALIGNMENT_FORTRAN_INT)
    LAM_F77_GET_ALIGNMENT(REAL, LAM_ALIGNMENT_FORTRAN_REAL)
    LAM_F77_GET_ALIGNMENT(DOUBLE PRECISION, LAM_ALIGNMENT_FORTRAN_DBLPREC)
    LAM_F77_GET_ALIGNMENT(COMPLEX, LAM_ALIGNMENT_FORTRAN_COMPLEX)
    LAM_F77_GET_ALIGNMENT(DOUBLE COMPLEX, LAM_ALIGNMENT_FORTRAN_DBLCOMPLEX)
fi

AC_DEFINE_UNQUOTED(LAM_SIZEOF_FORTRAN_INT, $LAM_SIZEOF_FORTRAN_INT,
    [Size of fortran integer])
AC_DEFINE_UNQUOTED(LAM_SIZEOF_FORTRAN_REAL, $LAM_SIZEOF_FORTRAN_REAL,
    [Size of fortran real])
AC_DEFINE_UNQUOTED(LAM_SIZEOF_FORTRAN_DBLPREC, $LAM_SIZEOF_FORTRAN_DBLPREC,
    [Size of fortran double precision])
AC_DEFINE_UNQUOTED(LAM_SIZEOF_FORTRAN_COMPLEX, $LAM_SIZEOF_FORTRAN_COMPLEX,
    [Size of fortran complex])
AC_DEFINE_UNQUOTED(LAM_SIZEOF_FORTRAN_DBLCOMPLEX, 
    $LAM_SIZEOF_FORTRAN_DBLCOMPLEX,
    [Size of fortran double complex])

AC_DEFINE_UNQUOTED(LAM_ALIGNMENT_FORTRAN_INT, $LAM_ALIGNMENT_FORTRAN_INT,
    [Size of fortran integer])
AC_DEFINE_UNQUOTED(LAM_ALIGNMENT_FORTRAN_REAL, $LAM_ALIGNMENT_FORTRAN_REAL,
    [Size of fortran real])
AC_DEFINE_UNQUOTED(LAM_ALIGNMENT_FORTRAN_DBLPREC, 
    $LAM_ALIGNMENT_FORTRAN_DBLPREC,
    [Size of fortran double precision])
AC_DEFINE_UNQUOTED(LAM_ALIGNMENT_FORTRAN_COMPLEX, 
    $LAM_ALIGNMENT_FORTRAN_COMPLEX,
    [Size of fortran complex])
AC_DEFINE_UNQUOTED(LAM_ALIGNMENT_FORTRAN_DBLCOMPLEX, 
    $LAM_ALIGNMENT_FORTRAN_DBLCOMPLEX,
    [Size of fortran double complex])

#
# There are 2 layers to the MPI f77 layer. The only extra thing
# that determine f77 bindings is that fortran can be disabled by
# user. In such cases, we need to not build the target at all.
# One layer generates MPI_f77* bindings. The other layer
# generates PMPI_f77* bindings. The following conditions
# determine whether each (or both) these layers are 
# built.
# Superceeding clause:
#   - fortran77 bindings should be enabled, else everything is 
#     disabled
# 1. MPI_f77* bindings are needed if:
#	- Profiling is not required
#   - Profiling is required but weak symbols are not
#	  supported
# 2. PMPI_* bindings are needed if profiling is required.
# Hence we define 2 AM_CONDITIONALs which tell us whether
# each of these layers need to be built or NOT
#
AM_CONDITIONAL(WANT_MPI_F77_BINDINGS_LAYER,
    test "$WANT_MPI_PROFILING" = 0 -o "$LAM_PROFILING_COMPILE_SEPARATELY" = 1 -a "$WANT_MPI_F77" = 1)
AM_CONDITIONAL(WANT_PMPI_F77_BINDINGS_LAYER,
	test "$WANT_MPI_F77" = 1 -a "$WANT_MPI_PROFILING" = 1)
AM_CONDITIONAL(COMPILE_PROFILING_SEPARATELY, 
    test "$LAM_PROFILING_COMPILE_SEPARATELY" = 1)
#
# Fortran 90 setup
#

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

#
# Make sure we can copy va_lists (need check declared, not linkable)
#

AC_CHECK_DECL(va_copy, LAM_HAVE_VA_COPY=1, LAM_HAVE_VA_COPY=0, 
	      [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(LAM_HAVE_VA_COPY, $LAM_HAVE_VA_COPY,
    [Whether we have va_copy or not])

AC_CHECK_DECL(__va_copy, LAM_HAVE_UNDERSCORE_VA_COPY=1,
	      LAM_HAVE_UNDERSCORE_VA_COPY=0, [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(LAM_HAVE_UNDERSCORE_VA_COPY, $LAM_HAVE_UNDERSCORE_VA_COPY,
    [Whether we have __va_copy or not])


##################################
# System-specific tests
##################################

lam_show_title "System-specific tests"
#
# Determine what MPI_Fint shout be defined as. If the size of 
# fortran integer is 4 then it is defined to either
# int32_t or int. Similarly if the size of fortran integer
# is 8 bytes then it is defined to either int64_t or int.
# ac_cv_type_int32_t=yes implies that unint_32 was present.
# similary ac_cv_type_int64_t=yes. Similarly the values 
# of ac_cv_sizeof_int will contain the size of int and later
# this will be #defined to SIZEOF_INT.
# NOTE:
# This test should be carried out ONLY if there is a f77 compiler
# available. Else we should default to MPI_Fint being an int.
#
if test "$F77" != "none"; then
	MPI_FINT_TYPE="not found"
	AC_MSG_CHECKING([checking for type of MPI_Fint])
	if test "$LAM_SIZEOF_FORTRAN_INT" = 4 ; then
            if test "$ac_cv_type_int32_t" = "yes"; then
        	MPI_FINT_TYPE=int32_t
            elif test "$ac_cv_sizeof_int" = 4 ; then
        	MPI_FINT_TYPE=int
            fi
	elif test "$LAM_SIZEOF_FORTRAN_INT" = 8 ; then
            if test "$ac_cv_type_int64_t" = "yes"; then
                MPI_FINT_TYPE=int64_t
            elif test "$ac_cv_type_long_long" = "yes" -a "$ac_cv_sizeof_long_long" = 8; then
        	MPI_FINT_TYPE="long long"
            elif test "$ac_cv_sizeof_long" = 8; then
        	MPI_FINT_TYPE="long"
            elif test "$ac_cv_sizeof_int" = 8 ; then
        	MPI_FINT_TYPE=int
            fi
	fi
	AC_MSG_RESULT([$MPI_FINT_TYPE])
	if test "$MPI_FINT_TYPE" = "not found"; then
            AC_MSG_WARN([*** WARNING: Unable to find the right definition for MPI_Fint])
            AC_MSG_ERROR([Cannot continue])
	fi
	AC_DEFINE_UNQUOTED(MPI_Fint, $MPI_FINT_TYPE, [Type of MPI_Fint])
else
	AC_MSG_WARN([*** WARNING: Could not detect a f77 compiler. MPI_Fint will be defined as an int]) 
	MPI_FINT_TYPE=int
	AC_DEFINE_UNQUOTED(MPI_Fint, $MPI_FINT_TYPE, [Type of MPI_Fint])
fi

#
# Test to determine type of MPI_Offset. This is searched in the following order
# int64_t, long long, long, int. If none of these are 8 bytes, then we should
# search for int32_t, long long, long, int.
#
MPI_OFFSET_TYPE="not found"
AC_MSG_CHECKING([checking for type of MPI_Offset])
if test "$ac_cv_type_int64_t" == "yes"; then
	MPI_OFFSET_TYPE=int64_t
elif test "$ac_cv_type_long_long_t" == "yes" -a "$ac_cv_sizeof_long_long" = 8; then
	MPI_OFFSET_TYPE="long long"
elif test "$ac_cv_type_long_t" == "yes" -a "$ac_cv_sizeof_long" = 8; then
	MPI_OFFSET_TYPE="long"
elif test "ac_cv_sizeof_int" = 8; then
	MPI_OFFSET_TYPE=int
elif test "$ac_cv_type_int32_t" == "yes"; then
	MPI_OFFSET_TYPE=int32_t
elif test "$ac_cv_type_long_long_t" == "yes" -a "$ac_cv_sizeof_long_long" = 4; then
	MPI_OFFSET_TYPE="long long"
elif test "$ac_cv_type_long_t" == "yes" -a "$ac_cv_sizeof_long" = 4; then
	MPI_OFFSET_TYPE="long"
elif test "ac_cv_sizeof_int" = 4; then
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

LAM_CONFIG_THREADS
CFLAGS="$CFLAGS $THREAD_CFLAGS"
CXXFLAGS="$CXXFLAGS $THREAD_CXXFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
CXXCPPFLAGS="$CXXCPPFLAGS $THREAD_CXXCPPFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS="$LIBS $THREAD_LIBS"

WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $THREAD_CFLAGS"
WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS $THREAD_CXXFLAGS"
WRAPPER_EXTRA_FFLAGS="$WRAPPER_EXTRA_FFLAGS $THREAD_FFLAGS"
WRAPPER_EXTRA_LDFLAGS="$WRAPPER_EXTRA_LDFLAGS $THREAD_LDFLAGS"
WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS $THREAD_LIBS"

#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S

#
# File system case sensitivity
#

LAM_CASE_SENSITIVE_FS_SETUP

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy


##################################
# MCA
##################################

lam_show_title "MPI Component Architecture (MCA) setup" 

AC_MSG_CHECKING([for subdir args])
LAM_CONFIG_SUBDIR_ARGS([lam_subdir_args])
AC_MSG_RESULT([$lam_subdir_args])

LAM_MCA


############################################################################
# Final top-level LAM configuration
############################################################################

lam_show_title "Final top-level LAM configuration"

############################################################################
# Libevent setup
############################################################################

lam_show_subtitle "Libevent 3rd party event handler"
CFLAGS_save="$CFLAGS"
CFLAGS="$LAM_CFLAGS_BEFORE_PICKY"
LAM_SETUP_LIBEVENT
CFLAGS="$CFLAGS_save"


############################################################################
# Libtool: part two
# (after C compiler setup)
############################################################################

lam_show_subtitle "Libtool configuration"

# Use convenience libltdl for the moment, because we need to force the
# use of the newest libltdl (i.e., the one that ships with libtool
# 1.5) because it has support for a lot more things than older
# versions of libtool (which are generally installed by default).

AC_LIBLTDL_CONVENIENCE(src/libltdl)
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL

# AC_CONFIG_SUBDIRS appears to be broken for non-gcc compilers (i.e.,
# passing precious variables down to the sub-configure).  
#
# Note that we also did some magic scripty-foo in autogen.sh to rename
# the output libtool library "liblamltdl", so add -llamltdl here.
# This is because so many systems have older versions of libltdl
# installed very early in ld.so's search path; if we installed our own
# libltdl in some non-standard path (e.g., $HOME/local or even
# /usr/local), the libltdl in /usr/lib might get found first.  And if
# it's older -- or just not matching the version that we need, Bad
# Things happen.  [sigh]
#
# Finally, make liblamltdl follow the same shared/static convention
# that was user for the main LAM libraries.  So manually examine
# $enable_shared and $enable_static and pass down the corresponding
# flags.

lam_show_subtitle "GNU libltdl setup" 

lam_subdir_args="$lam_subdir_args --enable-ltdl-convenience --disable-ltdl-install"
if test "$enable_shared" = "yes"; then
    lam_subdir_args="$lam_subdir_args --enable-shared"
else
    lam_subdir_args="$lam_subdir_args --disable-shared"
fi
if test "$enable_static" = "yes"; then
    lam_subdir_args="$lam_subdir_args --enable-static"
else
    lam_subdir_args="$lam_subdir_args --disable-static"
fi

CFLAGS_save="$CFLAGS"
CFLAGS="$LAM_CFLAGS_BEFORE_PICKY"
LAM_CONFIG_SUBDIR(src/libltdl, [$lam_subdir_args], [HAPPY=1], [HAPPY=0])
if test "$HAPPY" = "1"; then
    LIBLTDL_SUBDIR=libltdl
    LIBLTDL_LTLIB=libltdl/libltdlc.la
    WANT_LIBLTDL=1

    # Arrgh.  This is gross.  But I can't think of any other way to do
    # it.  :-(

    flags="`egrep ^LIBADD_DL src/libltdl/Makefile | cut -d= -f2-`"
    LAM_CHECK_LINKER_FLAGS([src/libltdl/libtool], [-export-dynamic $flags])
    WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS $extra_ldflags"
    LDFLAGS="-export-dynamic $LDFLAGS"
else
    LIBLTDL_SUBDIR=
    LIBLTDL_LTLIB=
    WANT_LIBLTDL=0
fi
CFLAGS="$CFLAGS_save"
AC_SUBST(LIBLTDL_SUBDIR)
AC_SUBST(LIBLTDL_LTLIB)
AM_CONDITIONAL(WANT_LIBLTDL, test "$WANT_LIBLTDL" = "1")
AC_DEFINE_UNQUOTED(LAM_WANT_LIBLTDL, $WANT_LIBLTDL,
    [Whether to include support for libltdl or not])


############################################################################
# final wrapper compiler config
############################################################################

lam_show_subtitle "Wrapper compiler flags"

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
LAM_UNIQ(WRAPPER_EXTRA_CFLAGS)
AC_SUBST(WRAPPER_EXTRA_CFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$WRAPPER_EXTRA_CFLAGS",
    [Additional CFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_CFLAGS])

#
# WRAPPER_EXTRA_CXXFLAGS
#
AC_MSG_CHECKING([for mpiCC CXXFLAGS])
LAM_UNIQ(WRAPPER_EXTRA_CXXFLAGS)
AC_SUBST(WRAPPER_EXTRA_CXXFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS, "$WRAPPER_EXTRA_CXXFLAGS",
    [Additional CXXFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_CXXFLAGS])

#
# WRAPPER_EXTRA_FFLAGS
#
AC_MSG_CHECKING([for mpif77/mpif90 FFLAGS])
LAM_UNIQ(WRAPPER_EXTRA_FFLAGS)
AC_SUBST(WRAPPER_EXTRA_FFLAGS)
AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FFLAGS, "$WRAPPER_EXTRA_FFLAGS",
    [Additional FFLAGS to pass through the wrapper compilers])
AC_MSG_RESULT([$WRAPPER_EXTRA_FFLAGS])

#
# WRAPPER_EXTRA_LDFLAGS
#
AC_MSG_CHECKING([for wrapper compiler LDFLAGS])
LAM_UNIQ(WRAPPER_EXTRA_LDFLAGS)
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


############################################################################
# Party on
############################################################################

lam_show_subtitle "Final output" 

AM_CONFIG_HEADER([include/lam_config.h])
AC_CONFIG_FILES([
    Makefile

    config/Makefile

    include/Makefile

    src/Makefile
    src/include/Makefile

    src/ctnetwork/Makefile
    src/lfc/Makefile
    src/mem/Makefile
    src/os/Makefile
    src/os/cygwin/Makefile
    src/os/darwin/Makefile
    src/os/darwin/ppc_32/Makefile
    src/os/darwin/ppc_64/Makefile
    src/os/irix/Makefile
    src/os/irix/sn0/Makefile
    src/os/linux/Makefile
    src/os/linux/alpha/Makefile
    src/os/linux/i686/Makefile
    src/os/linux/ia64/Makefile
    src/os/linux/x86_64/Makefile
    src/os/tru64/Makefile
    src/runtime/Makefile
    src/threads/Makefile
    src/util/Makefile

    src/event/Makefile
    src/event/compat/Makefile
    src/event/compat/sys/Makefile

    src/attribute/Makefile
    src/communicator/Makefile
    src/datatype/Makefile
    src/errhandler/Makefile
    src/group/Makefile
    src/info/Makefile
    src/op/Makefile
    src/proc/Makefile
    src/request/Makefile
    src/win/Makefile

    src/mca/Makefile
    src/mca/base/Makefile
    src/mca/common/Makefile
    src/mca/common/base/Makefile

    src/mca/oob/Makefile
    src/mca/oob/base/Makefile
    src/mca/pcm/Makefile
    src/mca/pcm/base/Makefile
    src/mca/registry/Makefile
    src/mca/registry/base/Makefile

    src/mca/coll/Makefile
    src/mca/coll/base/Makefile
    src/mca/io/Makefile
    src/mca/io/base/Makefile
    src/mca/one/Makefile
    src/mca/one/base/Makefile
    src/mca/pml/Makefile
    src/mca/pml/base/Makefile
    src/mca/ptl/Makefile
    src/mca/ptl/base/Makefile
    src/mca/topo/Makefile
    src/mca/topo/base/Makefile

    src/mpi/Makefile
    src/mpi/c/Makefile
    src/mpi/c/profile/Makefile
    src/mpi/cxx/Makefile
    src/mpi/f77/Makefile
    src/mpi/f77/profile/Makefile
    src/mpi/f90/Makefile

    src/tools/Makefile
    src/tools/laminfo/Makefile
    src/tools/mpirun/Makefile
    src/tools/wrappers/Makefile

    test/Makefile
    test/support/Makefile
    test/lam/Makefile
    test/lam/lfc/Makefile
    test/lam/threads/Makefile
    test/lam/util/Makefile
    test/mpi/Makefile
    test/mpi/environment/Makefile
    test/unit/Makefile
    test/unit/lam/Makefile
    test/unit/lam/oob_cofs/Makefile
    test/unit/lam/util/Makefile
    test/unit/mpi/Makefile
    test/unit/mpi/communicator/Makefile
    test/unit/mpi/datatype/Makefile
])
AC_OUTPUT
