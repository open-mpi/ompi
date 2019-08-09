# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2010 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2017 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2016-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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

# We don't have the version number to put in here yet, and we can't
# call PMIX_GET_VERSION (etc.) before AC_INIT.  So use the shell
# version.

AC_INIT([pmix],
        [m4_normalize(esyscmd([config/pmix_get_version.sh VERSION --tarball]))],
        [http://pmix.github.io/master], [pmix])
AC_PREREQ(2.69)
AC_CONFIG_AUX_DIR(./config)
# Note that this directory must *exactly* match what was specified via
# -I in ACLOCAL_AMFLAGS in the top-level Makefile.am.
AC_CONFIG_MACRO_DIR(./config)

# autotools expects to perform tests without interference
# from user-provided CFLAGS, particularly -Werror flags.
# Search for them here and cache any we find
PMIX_CFLAGS_cache=
PMIX_CFLAGS_pass=
for val in $CFLAGS; do
    if echo "$val" | grep -q -e "-W"; then
        PMIX_CFLAGS_cache="$PMIX_CFLAGS_cache $val";
    else
        PMIX_CFLAGS_pass="$PMIX_CFLAGS_pass $val";
    fi
done
CFLAGS=$PMIX_CFLAGS_pass

PMIX_CAPTURE_CONFIGURE_CLI([PMIX_CONFIGURE_CLI])

# Get our platform support file.  This has to be done very, very early
# because it twiddles random bits of autoconf
PMIX_LOAD_PLATFORM

PMIX_TOP_BUILDDIR="`pwd`"
AC_SUBST(PMIX_TOP_BUILDDIR)
top_buildir=`pwd`
cd "$srcdir"
PMIX_TOP_SRCDIR="`pwd`"
AC_SUBST(PMIX_TOP_SRCDIR)
cd "$PMIX_TOP_BUILDDIR"
top_srcdir=`pwd`

AC_MSG_NOTICE([builddir: $PMIX_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $PMIX_TOP_SRCDIR])
if test "$PMIX_TOP_BUILDDIR" != "$PMIX_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# setup configure options (e.g., show_title and friends)
PMIX_CONFIGURE_SETUP
pmix_show_title "Configuring PMIx"

# This must be before AM_INIT_AUTOMAKE
AC_CANONICAL_TARGET

# Init automake
AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.13.4 -Wall -Werror])

# SILENT_RULES is new in AM 1.11, but we require 1.13.4 or higher via
# autogen.  Limited testing shows that calling SILENT_RULES directly
# works in more cases than adding "silent-rules" to INIT_AUTOMAKE
# (even though they're supposed to be identical).  Shrug.
AM_SILENT_RULES([yes])

# set the language
AC_LANG([C])

# AC_USE_SYSTEM_EXTENSIONS will modify CFLAGS if nothing was in there
# beforehand.  We don't want that.  So if there was nothing in
# CFLAGS, put nothing back in there.
PMIX_VAR_SCOPE_PUSH([CFLAGS_save])
CFLAGS_save=$CFLAGS
AC_USE_SYSTEM_EXTENSIONS
AS_IF([test -z "$CFLAGS_save"], [CFLAGS=])
PMIX_VAR_SCOPE_POP

####################################################################
# Setup the configure header files
####################################################################

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
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * This file is automatically generated by configure.  Edits will be lost
 * the next time you run configure!
 */

#ifndef PMIX_CONFIG_H
#define PMIX_CONFIG_H

#include <src/include/pmix_config_top.h>

])
AH_BOTTOM([
#include <src/include/pmix_config_bottom.h>
#endif /* PMIX_CONFIG_H */
])

############################################################################
# Setup Libtool
############################################################################

# We want new Libtool.  None of that old stuff.  Pfft.
m4_ifdef([LT_PREREQ], [],
         [m4_fatal([libtool version 2.2.6 or higher is required], [63])])
LT_PREREQ([2.2.6])

#
# Enable static so that we have the --with tests done up here and can
# check for OS.  Save the values of $enable_static and $enable_shared
# before setting the defaults, because if the user specified
# --[en|dis]able-[static|shared] on the command line, they'll already
# be set.  In this way, we can tell if the user requested something or
# if the default was set here.
#

pmix_enable_shared="$enable_shared"
pmix_enable_static="$enable_static"
AS_IF([test ! -z "$enable_static" && test "$enable_static" = "yes"],
      [CFLAGS="$CFLAGS -fPIC"])

AM_ENABLE_SHARED
AM_DISABLE_STATIC

# This did not exist pre AM 1.11.x (where x is somewhere >0 and <3),
# but it is necessary in AM 1.12.x.
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AM_PROG_LEX

############################################################################
# Configuration options
############################################################################

# Set the MCA prefix
PMIX_SET_MCA_PREFIX([PMIX_MCA_])
PMIX_SET_MCA_CMD_LINE_ID([pmca])

# Define PMIx configure arguments
PMIX_DEFINE_ARGS
# Define some basic useful values
PMIX_BASIC_SETUP

# If debug mode, add -g
AS_IF([test "$pmix_debug" = "1"],
      [CFLAGS="$CFLAGS -g"])

LT_INIT()
LT_LANG([C])

############################################################################
# Setup the core
############################################################################

# Setup the pmix core
PMIX_SETUP_CORE()

# Run the AM_CONDITIONALs
PMIX_DO_AM_CONDITIONALS

####################################################################
# Setup C compiler
####################################################################

AC_ARG_VAR(CC_FOR_BUILD,[build system C compiler])
AS_IF([test -z "$CC_FOR_BUILD"],[
    AC_SUBST([CC_FOR_BUILD], [$CC])
])

# restore any user-provided Werror flags
AS_IF([test ! -z "$PMIX_CFLAGS_cache"], [CFLAGS="$CFLAGS $PMIX_CFLAGS_cache"])

# Delay setting pickyness until here so we
# don't break configure code tests
#if test "$WANT_PICKY_COMPILER" = "1"; then
#    CFLAGS="$CFLAGS -Wall -Wextra -Werror"
#fi

# Cleanup duplicate flags
PMIX_FLAGS_UNIQ(CPPFLAGS)
PMIX_FLAGS_UNIQ(LDFLAGS)
PMIX_FLAGS_UNIQ(LIBS)

#
# Delayed the substitution of CFLAGS and CXXFLAGS until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)

pmix_show_title "Final compiler flags"

AC_MSG_CHECKING([final CPPFLAGS])
AC_MSG_RESULT([$CPPFLAGS])

AC_MSG_CHECKING([final CFLAGS])
AC_MSG_RESULT([$CFLAGS])

AC_MSG_CHECKING([final LDFLAGS])
AC_MSG_RESULT([$LDFLAGS])

AC_MSG_CHECKING([final LIBS])
AC_MSG_RESULT([$LIBS])

####################################################################
# Version information
####################################################################

# PMIX_VERSION was setup by PMIX_SETUP_CORE above.

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

. $srcdir/VERSION
AC_SUBST([libpmix_so_version])
AC_SUBST([libpmi_so_version])
AC_SUBST([libpmi2_so_version])
AC_SUBST([libmca_common_dstore_so_version])

AC_CONFIG_FILES(pmix_config_prefix[contrib/Makefile]
                pmix_config_prefix[examples/Makefile]
                pmix_config_prefix[test/Makefile]
                pmix_config_prefix[test/simple/Makefile])

pmix_show_title "Configuration complete"

AC_OUTPUT

PMIX_SUMMARY_PRINT
