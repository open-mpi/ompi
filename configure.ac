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

# Get the version of OMPI that we are installing

OMPI_GET_VERSION($srcdir/config, $srcdir/VERSION, OMPI)

AC_DEFINE_UNQUOTED(OMPI_MAJOR_VERSION, $OMPI_MAJOR_VERSION, 
    [Major OMPI version])
AC_DEFINE_UNQUOTED(OMPI_MINOR_VERSION, $OMPI_MINOR_VERSION, 
    [Minor OMPI version])
AC_DEFINE_UNQUOTED(OMPI_RELEASE_VERSION, $OMPI_RELEASE_VERSION, 
    [Release OMPI version])
AC_DEFINE_UNQUOTED(OMPI_ALPHA_VERSION, $OMPI_ALPHA_VERSION, 
    [Alpha OMPI version])
AC_DEFINE_UNQUOTED(OMPI_BETA_VERSION, $OMPI_BETA_VERSION, 
    [Beta OMPI version])
AC_DEFINE_UNQUOTED(OMPI_SVN_VERSION, $OMPI_SVN_VERSION, 
    [CVS OMPI version])
AC_DEFINE_UNQUOTED(OMPI_VERSION, "$OMPI_VERSION", 
    [Overall OMPI version number])

AC_SUBST(OMPI_MAJOR_VERSION)
AC_SUBST(OMPI_MINOR_VERSION)
AC_SUBST(OMPI_RELEASE_VERSION)
AC_SUBST(OMPI_ALPHA_VERSION)
AC_SUBST(OMPI_BETA_VERSION)
AC_SUBST(OMPI_CVS_VERSION)
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

AM_INIT_AUTOMAKE(ompi, $OMPI_VERSION, 'no')

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

# Other basic setup stuff (shared with modules)

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
AM_DISABLE_SHARED
AM_ENABLE_STATIC


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

AC_C_INLINE
AC_C_RESTRICT
OMPI_C_WEAK_SYMBOLS

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
AC_CHECK_SIZEOF(bool)

# check for type alignments

OMPI_C_GET_ALIGNMENT(bool, OMPI_ALIGNMENT_CXX_BOOL)
AC_LANG_RESTORE


##################################
# Fortran
##################################

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
if test "$OMPI_F77" != "none" ; then
    OMPI_F77_FIND_EXT_SYMBOL_CONVENTION($OMPI_F77)

    # checking the sizeof fortran interger. This is needed to define 
    # MPI_Fint. This is needed for C bindings and hence there is no
    # bearing of --enable-fortran flag on this test.

    OMPI_F77_GET_SIZEOF(INTEGER, OMPI_SIZEOF_FORTRAN_INT)
fi

# If we want fortran support, then get the sizes and alignments of all
# the rest of the fortran types

if test "$OMPI_WANT_F77_BINDINGS" = "1" ; then
    # INTEGER is already done, per above
    OMPI_F77_GET_SIZEOF(REAL, OMPI_SIZEOF_FORTRAN_REAL)
    OMPI_F77_GET_SIZEOF(DOUBLE PRECISION, OMPI_SIZEOF_FORTRAN_DBLPREC)
    OMPI_F77_GET_SIZEOF(COMPLEX, OMPI_SIZEOF_FORTRAN_COMPLEX)
    OMPI_F77_GET_SIZEOF(DOUBLE COMPLEX, OMPI_SIZEOF_FORTRAN_DBLCOMPLEX)

    OMPI_F77_GET_ALIGNMENT(INTEGER, OMPI_ALIGNMENT_FORTRAN_INT)
    OMPI_F77_GET_ALIGNMENT(REAL, OMPI_ALIGNMENT_FORTRAN_REAL)
    OMPI_F77_GET_ALIGNMENT(DOUBLE PRECISION, OMPI_ALIGNMENT_FORTRAN_DBLPREC)
    OMPI_F77_GET_ALIGNMENT(COMPLEX, OMPI_ALIGNMENT_FORTRAN_COMPLEX)
    OMPI_F77_GET_ALIGNMENT(DOUBLE COMPLEX, OMPI_ALIGNMENT_FORTRAN_DBLCOMPLEX)
fi

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
#   - Profiling is not required
#   - Profiling is required but weak symbols are not
#     supported
# 2. PMPI_* bindings are needed if profiling is required.
# Hence we define 2 AM_CONDITIONALs which tell us whether
# each of these layers need to be built or NOT
#
AM_CONDITIONAL(WANT_MPI_F77_BINDINGS_LAYER,
               test "$WANT_MPI_PROFILING" = 0 -o "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1  -a "$OMPI_WANT_F77_BINDINGS" = 1)
                    
AM_CONDITIONAL(WANT_PMPI_F77_BINDINGS_LAYER,
               test "$OMPI_WANT_F77_BINDINGS" = 1 -a "$WANT_MPI_PROFILING" = 1)

#
# Fortran 90 setup
#

OMPI_SETUP_F90


##################################
# Header files
##################################

ompi_show_title "Header file tests"

# snprintf declaration
# gethostname declaration
# headers:
#   stropts.h grh.h netinet/tcp.h sys/select.h sys/resource.h pty.h util.h
#   rpc/types.h rpc/xdr.h sched.h strings.h

# SA_RESTART in signal.h
# sa_len in struct sockaddr
# union semun in sys/sem.h
AC_CHECK_HEADERS([stdbool.h alloca.h])


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
if test "$OMPI_F77" != "none"; then
	MPI_FINT_TYPE="not found"
	AC_MSG_CHECKING([checking for type of MPI_Fint])
	if test "$OMPI_SIZEOF_FORTRAN_INT" = 4 ; then
            if test "$ac_cv_type_int32_t" = "yes"; then
        	MPI_FINT_TYPE=int32_t
            elif test "$ac_cv_sizeof_int" = 4 ; then
        	MPI_FINT_TYPE=int
            fi
	elif test "$OMPI_SIZEOF_FORTRAN_INT" = 8 ; then
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
# File system case sensitivity
#

OMPI_CASE_SENSITIVE_FS_SETUP

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy


##################################
# MCA
##################################

ompi_show_title "MPI Component Architecture (MCA) setup" 

AC_MSG_CHECKING([for subdir args])
OMPI_CONFIG_SUBDIR_ARGS([ompi_subdir_args])
AC_MSG_RESULT([$ompi_subdir_args])

OMPI_MCA


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

AC_LIBLTDL_CONVENIENCE(src/libltdl)
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
AC_LIBTOOL_DLOPEN
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
OMPI_CONFIG_SUBDIR(src/libltdl, [$ompi_subdir_args], [HAPPY=1], [HAPPY=0])
if test "$HAPPY" = "1"; then
    LIBLTDL_SUBDIR=libltdl
    LIBLTDL_LTLIB=libltdl/libltdlc.la
    WANT_LIBLTDL=1

    # Arrgh.  This is gross.  But I can't think of any other way to do
    # it.  :-(

    flags="`egrep ^LIBADD_DL src/libltdl/Makefile | cut -d= -f2-`"
    OMPI_CHECK_LINKER_FLAGS([src/libltdl/libtool], [-export-dynamic $flags])
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
AC_CONFIG_FILES([
    Makefile

    config/Makefile

    include/Makefile
    include/mpif.h

    src/Makefile
    src/include/Makefile

    src/ctnetwork/Makefile
    src/class/Makefile
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
    src/file/Makefile
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
    src/mca/topo/Makefile
    src/mca/topo/base/Makefile
    src/mca/io/Makefile
    src/mca/io/base/Makefile
    src/mca/one/Makefile
    src/mca/one/base/Makefile
    src/mca/pml/Makefile
    src/mca/pml/base/Makefile
    src/mca/ptl/Makefile
    src/mca/ptl/base/Makefile
    src/mca/gpr/Makefile

    src/mpi/Makefile
    src/mpi/c/Makefile
    src/mpi/c/profile/Makefile
    src/mpi/cxx/Makefile
    src/mpi/f77/Makefile
    src/mpi/f77/profile/Makefile
    src/mpi/f90/Makefile

    src/tools/Makefile
    src/tools/ompi_info/Makefile
    src/tools/mpirun/Makefile
    src/tools/wrappers/Makefile

    src/rte/universe/Makefile

    test/Makefile
    test/support/Makefile
    test/class/Makefile
    test/threads/Makefile
    test/util/Makefile
    test/rte/Makefile

])
AC_OUTPUT
