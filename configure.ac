# -*- shell-script -*-
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
# 
# This file is part of the CMPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# CMPI source distribution.
#
# $Id: configure.ac,v 1.2 2003/11/22 16:40:48 jsquyres Exp $
#


############################################################################
# Initialization, version number, and other random setup/init stuff
############################################################################

# Init autoconf

AC_INIT(./src/mpi/datatype/d_get_name.c)
AC_PREREQ(2.52)
AC_CONFIG_AUX_DIR(./config)

# Get the version of CMPI that we are installing

CMPI_GET_VERSION($srcdir/config, $srcdir/VERSION, CMPI)

AC_DEFINE_UNQUOTED(CMPI_MAJOR_VERSION, $CMPI_MAJOR_VERSION, 
    [Major CMPI version])
AC_DEFINE_UNQUOTED(CMPI_MINOR_VERSION, $CMPI_MINOR_VERSION, 
    [Minor CMPI version])
AC_DEFINE_UNQUOTED(CMPI_RELEASE_VERSION, $CMPI_RELEASE_VERSION, 
    [Release CMPI version])
AC_DEFINE_UNQUOTED(CMPI_ALPHA_VERSION, $CMPI_ALPHA_VERSION, 
    [Alpha CMPI version])
AC_DEFINE_UNQUOTED(CMPI_BETA_VERSION, $CMPI_BETA_VERSION, 
    [Beta CMPI version])
AC_DEFINE_UNQUOTED(CMPI_CVS_VERSION, $CMPI_CVS_VERSION, 
    [CVS CMPI version])
AC_DEFINE_UNQUOTED(CMPI_VERSION, "$CMPI_VERSION", 
    [Overall CMPI version number])

AC_SUBST(CMPI_MAJOR_VERSION)
AC_SUBST(CMPI_MINOR_VERSION)
AC_SUBST(CMPI_RELEASE_VERSION)
AC_SUBST(CMPI_ALPHA_VERSION)
AC_SUBST(CMPI_BETA_VERSION)
AC_SUBST(CMPI_CVS_VERSION)
AC_SUBST(CMPI_VERSION)

#
# Start it up
#

CMPI_CONFIGURE_SETUP
cmpi_show_title "Configuring CMPI version $CMPI_VERSION"
cmpi_show_subtitle "Initialization, setup"

#
# Init automake
# The third argument to AM_INIT_AUTOMAKE surpresses the PACKAGE and
# VERSION macors
#

AM_INIT_AUTOMAKE(cmpi, $CMPI_VERSION, 'no')

CMPI_TOP_BUILDDIR="`pwd`"
AC_SUBST(CMPI_TOP_BUILDDIR)
cd "$srcdir"
CMPI_TOP_SRCDIR="`pwd`"
AC_SUBST(CMPI_TOP_SRCDIR)
cd "$CMPI_TOP_BUILDDIR"

AC_MSG_NOTICE([builddir: $CMPI_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $CMPI_TOP_SRCDIR])
if test "$CMPI_TOP_BUILDDIR" != "$CMPI_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# Setup the top of the src/include/cmpi_config.h file

AH_TOP([/* -*- c -*-
 *
 * Copyright (c) 2003 The Trustees of Indiana University.  
 *                    All rights reserved.
 * 
 * This file is part of the CMPI software package.  For license
 * information, see the LICENSE file in the top level directory of the
 * CMPI source distribution.
 *
 * Function: - OS, CPU and compiler dependent configuration 
 */

#ifndef CMPI_CONFIG_H
#define CMPI_CONFIG_H
])

# What kind of machine are we on?

AC_CANONICAL_HOST


############################################################################
# Configuration options
############################################################################

# --enable-purify
# --enable-dist
# --disable-profile
# --disable-f77
# ...?

# amorphous, seem-to-be-good-idea options
# --with-cmpi=maintainer_options
# --with-ssi-*
# ...?

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

cmpi_enable_shared="$enable_shared"
cmpi_enable_static="$enable_static"
AM_DISABLE_SHARED
AM_ENABLE_STATIC


############################################################################
# Check for compilers and preprocessors
############################################################################

##################################
# C compiler characteristics
##################################

cflags_save="$CFLAGS"
AC_PROG_CC
CFLAGS="$cflags_save"

# force ANSI prototypes
# check for STDC
# check for some types
# check for type sizes
# check for type alignments


##################################
# C++ compiler characteristics
##################################

cxx_flags_save="$CXXFLAGS"
AC_PROG_CXX
CXXFLAGS="$cxx_flags_save"

# check for STL
# check for bool (and corresponding C type)
# check for true/false
# check for type sizes
# check for type alignments
# check for template repository

##################################
# Fortran
##################################

if test "$with_f77" != "yes"; then
    cmpi_show_subtitle "Fortran Compiler -- skipped" 
    CMPI_WANT_FORTRAN=1
else
    cmpi_show_subtitle "Fortran Compiler"
    CMPI_WANT_FORTRAN=0
fi

if test "$CMPI_WANT_FORTRAN" = "1"; then
    # fortran linking style (necessary?)
    # check for types
    # check for type sizes
    # check for type alignments
    true
fi

AM_CONDITIONAL(WANT_FORTRAN, test "$CMPI_WANT_FORTRAN" = "1")


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

cmpi_show_title "Libtool configuration"

AM_PROG_LIBTOOL


############################################################################
# final wrapper compiler config
############################################################################

cmpi_show_title "Final top-level CMPI configuration"

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory (don't know why automake doesn't do this
# automatically).  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#

CPPFLAGS='-I$(top_srcdir)/share/include'" $CPPFLAGS"
CXXCPPFLAGS='-I$(top_srcdir)/share/include'" $CXXCPPFLAGS"

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

cmpi_show_subtitle "Final output" 

AM_CONFIG_HEADER([src/include/cmpi_config.h])
AC_CONFIG_FILES([
    Makefile

    config/Makefile

    src/Makefile
    src/include/Makefile
    src/mpi/Makefile
    src/mpi/communicator/Makefile
    src/mpi/datatype/Makefile
    src/tools/Makefile
    src/tools/wrappers/Makefile

    doc/Makefile

    test/Makefile
    test/unit/Makefile
    test/unit/mpi/Makefile
    test/unit/mpi/communicator/Makefile
    test/unit/mpi/datatype/Makefile
])
AC_OUTPUT
