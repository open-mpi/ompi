# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2018 The University of Tennessee and The University
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
# Copyright (c) 2011-2026 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2022 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016-2022 IBM Corporation.  All rights reserved.
# Copyright (c) 2018-2021 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2019      Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
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
        [https://www.open-mpi.org/community/help/], [project_name_short])
AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

OAC_PUSH_PREFIX([OPAL])

OPAL_CAPTURE_CONFIGURE_CLI([OPAL_CONFIGURE_CLI])

# Get our platform support file.  This has to be done very, very early
# because it twiddles random bits of autoconf
OPAL_LOAD_PLATFORM

# Start a list of packages / modules / etc. that want to disable "make dist".
OPAL_MAKEDIST_DISABLE=""

#
# Start it up
#

OPAL_CONFIGURE_SETUP
opal_show_title "Configuring project_name_long"
opal_show_subtitle "Prerequisites"

AC_PROG_SED

AC_CHECK_PROG([PERL],[perl],[perl],[no])
AS_IF([test "X$PERL" = "Xno"],
      [AC_MSG_ERROR(["Open MPI requires perl. Aborting"])])

#
# Setup some things that must be done before AM-INIT-AUTOMAKE
#

opal_show_subtitle "Startup tests"
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_DEFINE_UNQUOTED(OPAL_ARCH, "$target", [OMPI architecture string])
AS_IF([test "$host" != "$target"],
      [AC_MSG_WARN([Cross-compile detected])
       AC_MSG_WARN([Cross-compiling is only partially supported])
       AC_MSG_WARN([Proceed at your own risk!])])


#
# Init automake
#
AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.13.4 tar-pax])

# SILENT_RULES is new in AM 1.11, but we require 1.11 or higher via
# autogen.  Limited testing shows that calling SILENT_RULES directly
# works in more cases than adding "silent-rules" to INIT_AUTOMAKE
# (even though they're supposed to be identical).  Shrug.
AM_SILENT_RULES([yes])

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

# Sanity checks
AC_DEFUN([OMPI_CHECK_DIR_FOR_SPACES],[
    dir="$1"
    article="$2"
    label="$3"

    AC_MSG_CHECKING([directory of $label])
    AC_MSG_RESULT([$dir])
    AS_IF([test -n "`echo $dir | grep ' '`"],
          [AC_MSG_WARN([This version of Open MPI does not support $article $label])
           AC_MSG_WARN([with a path that contains spaces])
           AC_MSG_ERROR([Cannot continue.])])
])

ompi_dir=`pwd`
OMPI_CHECK_DIR_FOR_SPACES([$ompi_dir], [a], [build tree])
OMPI_CHECK_DIR_FOR_SPACES([$srcdir], [a], [source tree])
OMPI_CHECK_DIR_FOR_SPACES([$prefix], [a], [prefix])

opal_show_subtitle "Checking versions"

# Get the version of OMPI that we are installing

m4_ifdef([project_ompi],
         [OPAL_SAVE_VERSION([OMPI], [Open MPI], [$srcdir/VERSION],
                            [ompi/include/ompi/version.h])
          OPAL_SAVE_MPI_STANDARD_VERSION([$srcdir/VERSION])])

m4_ifdef([project_oshmem],
         [OPAL_SAVE_VERSION([OSHMEM], [Open SHMEM],
                            [$srcdir/VERSION],
                            [oshmem/include/oshmem/version.h])])

OPAL_SAVE_VERSION([OPAL], [Open Portable Access Layer], [$srcdir/VERSION],
                  [opal/include/opal/version.h])

# Get shared library version numbers

. $srcdir/VERSION
m4_ifdef([project_ompi],
         [AC_SUBST(libmpi_so_version)
          AC_SUBST(libmpi_mpifh_so_version)
          AC_SUBST(libmpi_usempi_tkr_so_version)
          AC_SUBST(libmpi_usempi_ignore_tkr_so_version)
          AC_SUBST(libmpi_usempif08_so_version)
          AC_SUBST(libmpi_java_so_version)
          AC_SUBST(libompitrace_so_version)])
m4_ifdef([project_oshmem],
         [AC_SUBST(liboshmem_so_version)])
AC_SUBST(libopen_pal_so_version)
# It's icky that we have to hard-code the names of the
# common components here.  :-( This could probably be done
# transparently by adding some intelligence in autogen.pl
# and/or opal_mca.m4, but I don't have the cycles to do this
# right now.
AC_SUBST(libmca_opal_common_ofi_so_version)
AC_SUBST(libmca_opal_common_cuda_so_version)
AC_SUBST(libmca_opal_common_sm_so_version)
AC_SUBST(libmca_opal_common_ugni_so_version)
AC_SUBST(libmca_ompi_common_ompio_so_version)
AC_SUBST(libmca_ompi_common_monitoring_so_version)
AC_SUBST(libmca_opal_common_ucx_so_version)

#
# Get the versions of the autotools that were used to bootstrap us
# (helpful for debugging reports)
#
AC_MSG_CHECKING([for bootstrap Autoconf version])
acversion=`grep "Generated by GNU Autoconf" $0 | head -n 1 | awk '{ print $6 }'`
AC_MSG_RESULT([$acversion])
AC_MSG_CHECKING([for bootstrap Automake version])
AC_MSG_RESULT([$am__api_version])
AC_MSG_CHECKING([for bootstrap Libtool version])
ltversion=`grep VERSION= $srcdir/config/ltmain.sh | head -n 1 | cut -d= -f2`
AC_MSG_RESULT([$ltversion])

# List header files to generate

AC_CONFIG_HEADERS([opal/include/opal_config.h])
m4_ifdef([project_ompi],
         [AC_CONFIG_HEADERS([ompi/include/mpi.h])])
m4_ifdef([project_oshmem],
         [AC_CONFIG_HEADERS([oshmem/include/shmem.h])])

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

# Check for deprecated/deleted options
OMPI_CHECK_DELETED_OPTIONS

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

m4_ifdef([project_oshmem],
         [OSHMEM_TOP_SRCDIR="$OMPI_TOP_SRCDIR"
          OSHMEM_TOP_BUILDDIR="$OMPI_TOP_BUILDDIR"
          AC_SUBST(OSHMEM_TOP_SRCDIR)
          AC_SUBST(OSHMEM_TOP_BUILDDIR)])

############################################################################
# Configuration options
############################################################################

OPAL_CONFIGURE_OPTIONS

###########################
# Open MPI options
m4_ifdef([project_ompi], [OMPI_CONFIGURE_OPTIONS])

# Enable/Disable Software-Based Performance Counters Capability
AC_MSG_CHECKING([if want software-based performance counters (SPC)])
AC_ARG_ENABLE([spc],
    [AS_HELP_STRING([--enable-spc],
                   [Enable software-based performance counters capability (default: disabled)])])
if test "$enable_spc" = "yes"; then
    AC_MSG_RESULT([yes])
    SPC_ENABLE=1
else
    AC_MSG_RESULT([no])
    SPC_ENABLE=0
fi
AC_DEFINE_UNQUOTED([SPC_ENABLE],
                   [$SPC_ENABLE],
                   [If the software-based performance counters capability should be enabled.])
AM_CONDITIONAL(SPC_ENABLE, test "$SPC_ENABLE" = "1")

AS_IF([test "$enable_spc" != "no"], [project_spc_amc=true], [project_spc_amc=false])

if test "$enable_binaries" = "no" && test "$enable_dist" = "yes"; then
    AC_MSG_WARN([--disable-binaries is incompatible with --enable dist])
    AC_MSG_ERROR([Cannot continue])
fi

# The library prefixes must be set before we call OPAL MCA.  Here is
# as good a place as any.
m4_ifdef([project_opal],
         [OPAL_SET_LIB_NAME([open-pal])])

m4_ifdef([project_ompi],
         [OMPI_SET_LIB_NAME([])])

###########################
# OSHMEM options
m4_ifdef([project_oshmem], [OSHMEM_CONFIGURE_OPTIONS])

# Set up project specific AM_CONDITIONALs
AS_IF([test "$enable_ompi" != "no"], [project_ompi_amc=true], [project_ompi_amc=false])
m4_ifndef([project_ompi], [project_ompi_amc=false])

AS_IF([test "$enable_oshmem" != "no"], [project_oshmem_amc=true], [project_oshmem_amc="no (disabled)"])
m4_ifndef([project_oshmem], [project_oshmem_amc="no (not available)"])

############################################################################
# Libtool: part one
# (before C compiler setup)
############################################################################

#
# Part one of libtool magic.  Default to: enable shared, disable static.
#

AC_ENABLE_SHARED
AC_DISABLE_STATIC

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

# Do all Interix detections if necessary
OMPI_INTERIX

#
# Check for some types
#

# 'short float' support of the Intel C++ Compiler (group of C and C++
# compilers), at least versions 18.0 and 19.0, is half-baked. It can
# compile declarations of 'short float' variables and expressions of
# 'sizeof(short float)' but cannot compile casts and operations of
# 'short float' variables. In this situation, 'AC_CHECK_TYPES(short float)'
# defines 'HAVE_SHORT_FLOAT' as 1 and compilation errors occur in
# ompi/mca/op/base/op_base_functions.c. To avoid this error, check it
# using 'AC_COMPILE_IFELSE' and set Autoconf cache variables before
# 'AC_CHECK_TYPES(short float)'. This check can be removed when all
# OMPI-supported Intel C++ Compilers support 'short float' completely
# (or drop it completely).
if test "$opal_cv_c_compiler_vendor" = "intel"; then
    AC_MSG_CHECKING([if Intel compiler supports "short float" properly])
    AC_COMPILE_IFELSE(
        [AC_LANG_SOURCE([float f(short float a, short float b) { return (float)(a + b); }])],
        [AC_MSG_RESULT([yes])],
        [ac_cv_type_short_float="no"
         ac_cv_type_short_float__Complex="no"]
         AC_MSG_RESULT([no]))
fi

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

AC_CHECK_TYPES(_Float128)
AC_CHECK_TYPES(__float128)
AC_CHECK_TYPES(short float)
AC_CHECK_TYPES(long double)
# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
AC_CHECK_HEADERS(complex.h)
AC_CHECK_TYPES(short float _Complex)
AC_CHECK_TYPES(float _Complex)
AC_CHECK_TYPES(double _Complex)
AC_CHECK_TYPES(long double _Complex)
AC_CHECK_TYPES(_Float128 _Complex)
AC_CHECK_TYPES(__float128 _Complex)

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
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(long long)
if test "$ac_cv_type_short_float" = yes; then
    AC_CHECK_SIZEOF(short float)
fi
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
if test "$ac_cv_type__Float128" = yes; then
    AC_CHECK_SIZEOF(_Float128)
fi
if test "$ac_cv_type___float128" = yes; then
    AC_CHECK_SIZEOF(__float128)
fi
# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_short_float__Complex" = yes; then
    AC_CHECK_SIZEOF(short float _Complex)
fi
AC_CHECK_SIZEOF(float _Complex)
AC_CHECK_SIZEOF(double _Complex)
AC_CHECK_SIZEOF(long double _Complex)
if test "$ac_cv_type__Float128__Complex" = yes; then
    AC_CHECK_SIZEOF(_Float128 _Complex)
fi
if test "$ac_cv_type___float128__Complex" = yes; then
    AC_CHECK_SIZEOF(__float128 _Complex)
fi

AC_CHECK_SIZEOF(void *)
AS_IF([test "$ac_cv_sizeof_void_p" -eq 4],
      [AC_MSG_WARN([Open MPI no longer supports 32 bit builds.])
       AC_MSG_WARN([Please use Open MPI v4.x or earlier if you need 32 bit support.])
       AC_MSG_ERROR([Cannot continue])])
AC_CHECK_SIZEOF(size_t)
if test "$ac_cv_type_ssize_t" = yes ; then
    AC_CHECK_SIZEOF(ssize_t)
fi
if test "$ac_cv_type_ptrdiff_t" = yes; then
    AC_CHECK_SIZEOF(ptrdiff_t)
else
    AC_MSG_ERROR([ptrdiff_t type is not available, this is required by C99 standard.  Cannot continue])
fi
AC_CHECK_SIZEOF(wchar_t)

AC_CHECK_SIZEOF(pid_t)

# Check sizes of atomic types so we can define fixed-width types in OPAL
AC_CHECK_SIZEOF(atomic_short, [],[[#include <stdatomic.h>]])
AC_CHECK_SIZEOF(atomic_int,[],[[#include <stdatomic.h>]])
AC_CHECK_SIZEOF(atomic_long,[],[[#include <stdatomic.h>]])
AC_CHECK_SIZEOF(atomic_llong,[],[[#include <stdatomic.h>]])

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
OPAL_C_GET_ALIGNMENT(long long, OPAL_ALIGNMENT_LONG_LONG)
if test "$ac_cv_type_short_float" = yes; then
    OPAL_C_GET_ALIGNMENT(short float, OPAL_ALIGNMENT_SHORT_FLOAT)
fi
OPAL_C_GET_ALIGNMENT(float, OPAL_ALIGNMENT_FLOAT)
OPAL_C_GET_ALIGNMENT(double, OPAL_ALIGNMENT_DOUBLE)
OPAL_C_GET_ALIGNMENT(long double, OPAL_ALIGNMENT_LONG_DOUBLE)
if test "$ac_cv_type__Float128" = yes; then
    OPAL_C_GET_ALIGNMENT(_Float128, OPAL_ALIGNMENT__FLOAT128)
fi
if test "$ac_cv_type___float128" = yes; then
    OPAL_C_GET_ALIGNMENT(__float128, OPAL_ALIGNMENT___FLOAT128)
fi


# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_short_float__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(short float _Complex, OPAL_ALIGNMENT_SHORT_FLOAT_COMPLEX)
fi
OPAL_C_GET_ALIGNMENT(float _Complex, OPAL_ALIGNMENT_FLOAT_COMPLEX)
OPAL_C_GET_ALIGNMENT(double _Complex, OPAL_ALIGNMENT_DOUBLE_COMPLEX)
OPAL_C_GET_ALIGNMENT(long double _Complex, OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX)
if test "$ac_cv_type__Float128__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(_Float128 _Complex, OPAL_ALIGNMENT__FLOAT128_COMPLEX)
fi
if test "$ac_cv_type___float128__Complex" = yes; then
    OPAL_C_GET_ALIGNMENT(__float128 _Complex, OPAL_ALIGNMENT___FLOAT128_COMPLEX)
fi

OPAL_C_GET_ALIGNMENT(void *, OPAL_ALIGNMENT_VOID_P)
OPAL_C_GET_ALIGNMENT(size_t, OPAL_ALIGNMENT_SIZE_T)

#
# Check for an alternate type of C 'short float'
#

OPAL_CHECK_ALT_SHORT_FLOAT

# Check system alignment requirements
if test "$opal_want_heterogeneous" = 1; then
    ompi_cv_c_word_size_align=yes
else
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
fi
AS_IF([test $ompi_cv_c_word_size_align = yes], [results=1], [results=0])
AC_DEFINE_UNQUOTED([OPAL_ALIGN_WORD_SIZE_INTEGERS], [$results],
    [set to 1 if word-size integers must be aligned to word-size padding to prevent bus errors])

#
# Check for other compiler characteristics
#

OPAL_C_WEAK_SYMBOLS
OPAL_C_MACRO_WEAK_SYMBOLS

# If we want the profiling layer:
# - If the C compiler has weak symbols, use those.
# - If not, then set to compile the code again with #define's in a
#   separate directory.

if test "$WANT_WEAK_SYMBOLS" = "0"; then
    OPAL_C_HAVE_WEAK_SYMBOLS=0
fi
if test "$OPAL_C_HAVE_WEAK_SYMBOLS" = "1"; then
    OMPI_PROFILING_COMPILE_SEPARATELY=0
else
    OMPI_PROFILING_COMPILE_SEPARATELY=1
fi

# Check if we support the offsetof compiler directive

OPAL_CHECK_OFFSETOF


##################################
# C++ compiler characteristics
##################################

# We don't need C++ unless we're building Open MPI, because Open MPI
# supports an "mpicxx" wrapper compiler (there is no C++ code in Open
# MPI -- the MPI C++ bindings were removed in Open MPI v5.0 -- so we
# don't need a C++ compiler for compiling Open MPI itself).
m4_ifdef([project_ompi], [OMPI_SETUP_CXX])

##################################
# Only after setting up both
# C and C++ check compiler attributes.
##################################

opal_show_subtitle "Compiler characteristics"

OPAL_CHECK_ATTRIBUTES
OPAL_CHECK_COMPILER_VERSION_ID

# Open MPI only supports GCC >=v4.8.1.  Notes:
#
# 1. The default compiler that comes with RHEL 7 is v4.8.5 (version ID
#    264197).
# 2. We regularly test with GCC v4.8.1 (version ID 264193).
# 3. GCC 4.8.0 probably also works; we just haven't tested it.
#
# Since we regularly test with 4.8.1, that's what we check for.
AS_IF([test "$opal_cv_compiler_FAMILYNAME" = "GNU" && \
       test "$opal_cv_compiler_VERSION" -lt 264193],
      [AC_MSG_WARN([Open MPI no longer supports versions of the GNU compiler suite])
       AC_MSG_WARN([less than v4.8.1.])
       AC_MSG_WARN([Please upgrade your GNU compiler suite, or use])
       AC_MSG_WARN([a different compiler to build Open MPI.])
       AC_MSG_ERROR([Cannot continue])
      ])

##################################
# Java MPI Binding request
##################################
# Only needed for OMPI
m4_ifdef([project_ompi], [OMPI_SETUP_JAVA_BINDINGS])


##################################
# MPI / OpenSHMEM API profiling layer
##################################

# Setup profiling bindings (if we're building the relevant projects).
m4_ifdef([project_ompi], [OMPI_SETUP_MPI_PROFILING])
m4_ifdef([project_oshmem], [OSHMEM_SETUP_PROFILING])


##################################
# Assembler Configuration
##################################

opal_show_subtitle "Assembler"

AM_PROG_AS
OPAL_CONFIG_ASM


##################################
# Fortran
##################################

OMPI_BUILD_FORTRAN_BINDINGS=0

m4_ifdef([project_ompi], [OMPI_SETUP_MPI_FORTRAN], [ompi_fortran_happy=0])

# Used in Makefile.ompi-rules
AM_CONDITIONAL(MAN_PAGE_BUILD_MPIFH_BINDINGS,
              [test $OMPI_BUILD_FORTRAN_BINDINGS -gt $OMPI_FORTRAN_NO_BINDINGS])
AM_CONDITIONAL(MAN_PAGE_BUILD_USEMPIF08_BINDINGS,
              [test $OMPI_BUILD_FORTRAN_BINDINGS -ge $OMPI_FORTRAN_USEMPIF08_BINDINGS])

AM_CONDITIONAL(OSHMEM_BUILD_FORTRAN_BINDINGS,
    [test "$enable_oshmem" = "yes" && \
     test "$ompi_fortran_happy" = "1" && \
     test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" && \
     test "$enable_oshmem_fortran" != "no"])

# checkpoint results
AC_CACHE_SAVE


##################################
# Wrapper compilers.
#
# Must be called before MCA system
##################################
OPAL_SETUP_WRAPPER_INIT


##################################
# Header files
##################################

opal_show_title "Header file tests"

AC_CHECK_HEADERS([alloca.h aio.h arpa/inet.h dirent.h \
    dlfcn.h endian.h execinfo.h err.h fcntl.h grp.h libgen.h \
    libutil.h memory.h netdb.h netinet/in.h netinet/tcp.h \
    poll.h pthread.h pty.h pwd.h sched.h \
    strings.h stropts.h linux/ethtool.h linux/sockios.h \
    sys/fcntl.h sys/ipc.h sys/shm.h \
    sys/ioctl.h sys/mman.h sys/param.h sys/queue.h \
    sys/resource.h sys/select.h sys/socket.h sys/sockio.h \
    sys/stat.h sys/statfs.h sys/statvfs.h time.h sys/time.h sys/tree.h \
    sys/types.h sys/uio.h sys/un.h net/uio.h sys/utsname.h sys/vfs.h sys/wait.h syslog.h \
    termios.h ulimit.h unistd.h util.h utmp.h malloc.h \
    ifaddrs.h crt_externs.h regex.h mntent.h paths.h \
    ioLib.h sockLib.h hostLib.h shlwapi.h sys/synch.h db.h ndbm.h ieee754.h syslimits.h])

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

AC_CHECK_MEMBERS([struct timespec.tv_nsec],
               [], [], [AC_INCLUDES_DEFAULT
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif])

#
# Find corresponding types for MPI_Aint, MPI_Count, and MPI_Offset.
# And if relevant, find the corresponding MPI_ADDRESS_KIND,
# MPI_COUNT_KIND, and MPI_OFFSET_KIND.
#
m4_ifdef([project_ompi], [OMPI_FIND_MPI_AINT_COUNT_OFFSET])

# checkpoint results
AC_CACHE_SAVE

##################################
# Linker characteristics
##################################

AC_MSG_CHECKING([the linker for support for the -fini option])
OPAL_VAR_SCOPE_PUSH([LDFLAGS_save])
LDFLAGS_save=$LDFLAGS
LDFLAGS="$LDFLAGS_save -Wl,-fini -Wl,finalize"
AC_LINK_IFELSE([AC_LANG_PROGRAM([void finalize (void) {}], [])],
               [AC_MSG_RESULT([yes])
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

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf openpty isatty getpwuid fork waitpid execve pipe ptsname setsid mmap tcgetpgrp posix_memalign strsignal sysconf syslog vsyslog regcmp regexec regfree _NSGetEnviron socketpair usleep mkfifo dbopen dbm_open statfs statvfs setpgid setenv __malloc_initialize_hook __clear_cache on_exit])

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

##################################
OPAL_CHECK_OS_FLAVORS



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

# all: SYSV semaphores
# all: SYSV shared memory
# all: size of FD_SET
# all: sizeof struct stat members
# all: type of getsockopt optlen
# all: type of recvfrom optlen

#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S

AC_PROG_GREP
AC_PROG_EGREP

#
# We need as and flex
#
AM_PROG_AS

dnl Note that prior to AC v2.70, PROG_LEX did not take any arguments.
dnl But it is harmless to pass an argument to it ($1 will just be
dnl ignored).
AC_PROG_LEX([noyywrap])

# If we don't have Flex and we don't have a generated .c file
# (distribution tarballs will have the .c file included, but git
# clones will not), then error.  Must have Flex -- other versions of
# Lex are not workable (all things being equal, since this is *only*
# required for developers, we decided that it really was not worth it
# to be portable between different versions of lex ;-).
AS_IF([test -z "$LEX" || \
   test -n "`echo $LEX | $GREP missing`" || \
   test "`basename $LEX`" != "flex"],
    [AS_IF([test ! -f "$srcdir/opal/util/show_help_lex.c"],
       [AC_MSG_WARN([*** Could not find Flex on your system.])
        AC_MSG_WARN([*** Flex is required for developer builds of Open MPI.])
        AC_MSG_WARN([*** Other versions of Lex are not supported.])
        AC_MSG_WARN([*** NOTE: If you are building a tarball downloaded from www.open-mpi.org, you do not need Flex])
        AC_MSG_ERROR([Cannot continue])
       ])
    ])

#
# Setup HTML and man page processing
#

dnl Note that we have to double escape the URL below
dnl so that the # it contains doesn't confuse the Autotools
OAC_SETUP_SPHINX([$srcdir/docs/man/MPI_T.3],
                 [[https://docs.open-mpi.org/en/main/developers/prerequisites.html#sphinx-and-therefore-python]],
                 [$srcdir/docs/requirements.txt])

#
# File system case sensitivity
#

OPAL_CASE_SENSITIVE_FS_SETUP

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy

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
#        Fault Tolerance
# Part1: must happen before prte
##################################
OPAL_SETUP_FT_OPTIONS

##################################
# 3rd-party packages not called ROMIO
##################################
opal_show_title "3rd-party packages"

OPAL_3RDPARY_SUBDIRS=
OPAL_3RDPARY_DIST_SUBDIRS=
OPAL_3RDPARY_EXTRA_DIST=
OPAL_3RDPARY_DISTCLEAN_DIRS=

OPAL_CONFIG_LIBEVENT
OPAL_CONFIG_HWLOC
OPAL_CONFIG_PMIX
OMPI_SETUP_PRRTE

AC_SUBST(OPAL_3RDPARTY_SUBDIRS)
AC_SUBST(OPAL_3RDPARTY_DIST_SUBDIRS)
AC_SUBST(OPAL_3RDPARTY_EXTRA_DIST)
AC_SUBST(OPAL_3RDPARTY_DISTCLEAN_DIRS)

##################################
#        Fault Tolerance
# Part2: must happen after prte
##################################
OPAL_SETUP_FT

##################################
# MCA
##################################

opal_show_title "Modular Component Architecture (MCA) setup"

AC_MSG_CHECKING([for subdir args])
OPAL_CONFIG_SUBDIR_ARGS([opal_subdir_args])
AC_MSG_RESULT([$opal_subdir_args])

OPAL_MCA

#
# Now that we know how to support threads with wrappers, update
#
OPAL_WRAPPER_FLAGS_ADD([CFLAGS], [$THREAD_CFLAGS])
OPAL_WRAPPER_FLAGS_ADD([CXXFLAGS], [$THREAD_CXXFLAGS])
OPAL_WRAPPER_FLAGS_ADD([FCFLAGS], [$THREAD_FCFLAGS])
OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$THREAD_LDFLAGS])

m4_ifdef([project_ompi], [OMPI_REQUIRE_ENDPOINT_TAG_FINI])

# Last minute disable of OpenSHMEM if we didn't find any oshmem SPMLs
if test "$project_oshmem_amc" = "true" && test $OSHMEM_FOUND_WORKING_SPML -eq 0 ; then
    # We don't have an spml that will work, so oshmem wouldn't be able
    # to run an application.  Therefore, don't build the oshmem layer.
    if test "$enable_oshmem" != "no" && test -n "$enable_oshmem"; then
	AC_MSG_WARN([No spml found, so OpenSHMEM layer will be non functional.])
	AC_MSG_ERROR([Aborting because OpenSHMEM requested, but can not build.])
    else
	AC_MSG_WARN([No spml found.  Will not build OpenSHMEM layer.])
	project_oshmem_amc="false (no spml)"
	# now for the hard part, remove project from list that will
	# run.  This is a hack, but it works as long as the project
	# remains named "oshmem".
	MCA_PROJECT_SUBDIRS=`echo "$MCA_PROJECT_SUBDIRS" | sed -e 's/oshmem//'`
    fi
fi

# checkpoint results
AC_CACHE_SAVE

##################################
# MPI Extended Interfaces
##################################

m4_ifdef([project_ompi], [OMPI_SETUP_MPI_EXT])

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
# Final top-level OMPI configuration
############################################################################

opal_show_title "Final top-level OMPI configuration"

############################################################################
# Libtool: part two
# (after C compiler setup = no compiler/linker tests after this)
############################################################################

opal_show_subtitle "Libtool configuration"

# Due to this thread:
# https://www.open-mpi.org/community/lists/users/2013/02/21356.php and
# a helpful tip from Dave Goodell, set the precious variables for
# compilers to "no" that we don't want.  Libtool's m4 configry will
# interpret this as "I won't be using this language; don't bother
# setting it up."  Note that we do this only for Fortran; we *don't*
# do this for C++, because *do* still want to setup the mpicxx wrapper
# if we have a C++ compiler.
AS_IF([test "$OMPI_TRY_FORTRAN_BINDINGS" = "$OMPI_FORTRAN_NO_BINDINGS"],[F77=no FC=no])

LT_INIT

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

# Need the libtool executable before the rpathify stuff
LT_OUTPUT

############################################################################
# final compiler config
############################################################################

m4_ifdef([project_ompi], [opal_show_subtitle "Compiler flags"])

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory.  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#
# Because opal_config.h and mpi.h are created by AC_CONFIG_HEADERS, we
# don't need to -I the builddir for <opal,ompi>/include. If we VPATH
# building, we do need to include the source directories, however.
#
if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    # Note the embedded m4 directives here -- we must embed them
    # rather than have successive assignments to these shell
    # variables, lest the $(foo) names try to get evaluated here.
    # Yuck!
    cpp_includes='$(top_srcdir) $(top_builddir) $(top_srcdir)/opal/include m4_ifdef([project_ompi], [$(top_srcdir)/ompi/include]) m4_ifdef([project_oshmem], [$(top_srcdir)/oshmem/include])'
else
    cpp_includes='$(top_srcdir)'
fi

CPP_INCLUDES="$(echo $cpp_includes | $SED 's/[[^ \]]* */'"$opal_cc_iquote"'&/g')"
CXX_INCLUDES="$(echo $cpp_includes | $SED 's/[[^ \]]* */'"$opal_cxx_iquote"'&/g')"
CPPFLAGS="$CPP_INCLUDES $CPPFLAGS"
# C++ is only relevant if we're building OMPI
m4_ifdef([project_ompi],[CXXCPPFLAGS="$CXX_INCLUDES $CXXCPPFLAGS"])

# OMPI needs some additional processing of the flags (e.g., get
# versions without optimization for debugger modules).

m4_ifdef([project_ompi], [OMPI_SETUP_DEBUGGER_FLAGS])
#
# Delayed the substitution of CFLAGS and friends until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
m4_ifdef([project_ompi], [AC_SUBST(FFLAGS)
                          AC_SUBST(FCFLAGS)

                          AC_SUBST(OMPI_LIBMPI_EXTRA_LIBS)
                          AC_SUBST(OMPI_LIBMPI_EXTRA_LDFLAGS)])

#
# Aggregate MCA parameters directory
#
AC_SUBST([AMCA_PARAM_SETS_DIR], ['$(opaldatadir)/amca-param-sets'])

############################################################################
# final wrapper compiler config
############################################################################

opal_show_subtitle "Wrapper compiler final setup"
# The OMPI wrapper scripts (i.e., not the C-compiled
# executables) need perl.
AC_PATH_PROG(PERL, perl, perl)

OPAL_SETUP_WRAPPER_FINAL

# Recreate some defines prefixed with OMPI_ so that there are no bare
# autoconf macro defines in mpi.h.  Since AC sometimes changes whether
# things are defined as null tokens or an integer result, two projects
# with different versions of AC can cause problems.

# According to the autoconf 2.67 documentation the AC_HEADER_STDC macro,
# and therefore the ac_cv_header_stdc cache variable, is obsolescent, as
# current systems have conforming header files. Instead of removing the
# protection completely, let's just make sure it is always on.
AC_DEFINE(OPAL_STDC_HEADERS, 1,
          [Do not use outside of mpi.h.  Define to 1 if you have the ANSI C header files.])
if test $ac_cv_header_sys_time_h = yes ; then
    AC_DEFINE(OPAL_HAVE_SYS_TIME_H, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the <sys/time.h> header file.])
fi
if test $ac_cv_type_long_long = yes ; then
    AC_DEFINE(OPAL_HAVE_LONG_LONG, 1,
              [Do not use outside of mpi.h.  Define to 1 if the system has the type `long long'.]) dnl `
fi
if test $ac_cv_header_sys_synch_h = yes ; then
    AC_DEFINE(OPAL_HAVE_SYS_SYNCH_H, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the <sys/synch.h> header file.])
fi

# If there is a local hook for each project, call it.  This allows 3rd
# parties to add configuration steps to OPAL and/or OMPI simply
# by placing a file in [opal|ompi]/config/whatever.m4 that
# AC_DEFUN's the appropriate macro name -- no patching is necessary.
# If that macro is defined, we'll run it here.
#
# Unfortunately, aclocal is not smart enough to parse something like
# the following in opal_mca.m4 (when we're already m4 looping over the
# project list):
#
# m4_foreach(mca_project, [mca_project_list],
#           [m4_ifdef(mca_project[_CONFIG_LOCAL], mca_project[_CONFIG_LOCAL])])
#
# Meaning that aclocal doesn't see that, for example,
# "ompi_CONFIG_LOCAL" is actually invoked at the bottom and therefore
# go look for an .m4 file that contains it.  Instead, we have to
# manually list the macros here.  *Then* aclocal is smart enough to go
# look for an .m4 file containing each macro, and if found,
# automatically m4_include the corresponding in aclocal.m4.  Bummer.
# :-\

m4_ifdef([opal_CONFIG_LOCAL], [opal_CONFIG_LOCAL])
m4_ifdef([project_ompi],
         [m4_ifdef([ompi_CONFIG_LOCAL], [ompi_CONFIG_LOCAL])])

############################################################################
# Party on
############################################################################

# set projects good/no good AM_CONDITIONALS.  This is at the end so
# that the OSHMEM/OMPI projects can be disabled, if needed, based on
# MCA tests.  If a project is to be disabled, also remove it from
# MCA_PROJECT_SUBDIRS to actually disable building.
AM_CONDITIONAL([PROJECT_OMPI], [test "$project_ompi_amc" = "true"])
AM_CONDITIONAL([PROJECT_OSHMEM], [test "$project_oshmem_amc" = "true"])

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
# pkg*dir, but will always be */openmpi.  This is to make it a bit
# easier to deal with the problem of opal and ompi built from
# their own tarballs, with their own PACKAGE variables.
opaldatadir='${datadir}/openmpi'
opallibdir='${libdir}/openmpi'
opalincludedir='${includedir}/openmpi'
AC_SUBST(opaldatadir)
AC_SUBST(opallibdir)
AC_SUBST(opalincludedir)

OPAL_SET_MCA_PREFIX([OMPI_MCA_])
OPAL_SET_MCA_CMD_LINE_ID([mca])
m4_ifdef([project_ompi],
         [ompidatadir="$opaldatadir"
          AC_SUBST(ompidatadir)
          ompilibdir="$opallibdir"
          AC_SUBST(ompilibdir)
          ompiincludedir="$opalincludedir"
          AC_SUBST(ompiincludedir)])
m4_ifdef([project_oshmem],
         [oshmemdatadir="$opaldatadir"
          AC_SUBST(oshmemdatadir)
          oshmemlibdir="$opallibdir"
          AC_SUBST(oshmemlibdir)
          oshmemincludedir="$opalincludedir"
          AC_SUBST(oshmemincludedir)])

opal_show_subtitle "Final output"

AC_MSG_CHECKING([OMPI final CPPFLAGS])
AC_MSG_RESULT([$CPPFLAGS])
AC_MSG_CHECKING([OMPI final LDFLAGS])
AC_MSG_RESULT([$LDFLAGS])
AC_MSG_CHECKING([OMPI final LIBS])
AC_MSG_RESULT([$LIBS])

AC_CONFIG_FILES([
    Makefile

    docs/Makefile

    config/Makefile

    contrib/Makefile

    contrib/dist/mofed/debian/changelog
    contrib/dist/mofed/debian/control
    contrib/dist/mofed/debian/copyright:LICENSE

    3rd-party/Makefile

    test/Makefile
    test/event/Makefile
    test/asm/Makefile
    test/datatype/Makefile
    test/class/Makefile
    test/mpool/Makefile
    test/support/Makefile
    test/threads/Makefile
    test/util/Makefile
])

m4_ifdef([project_ompi], [AC_CONFIG_FILES([test/monitoring/Makefile test/spc/Makefile])])

AC_CONFIG_FILES([contrib/dist/mofed/debian/rules],
                [chmod +x contrib/dist/mofed/debian/rules])
AC_CONFIG_FILES([contrib/dist/mofed/compile_debian_mlnx_example],
                [chmod +x contrib/dist/mofed/compile_debian_mlnx_example])

AS_IF([test -n "$OPAL_MAKEDIST_DISABLE"],
      [AC_MSG_WARN(["make dist" will be disabled due to: $OPAL_MAKEDIST_DISABLE])])
AC_SUBST([OPAL_MAKEDIST_DISABLE])

OPAL_CONFIG_FILES
m4_ifdef([project_ompi], [OMPI_CONFIG_FILES])
m4_ifdef([project_oshmem], [OSHMEM_CONFIG_FILES])

OPAL_CHECK_LIBNL_SUMMARY

# checkpoint results
AC_CACHE_SAVE

AC_OUTPUT

OPAL_SUMMARY_PRINT
