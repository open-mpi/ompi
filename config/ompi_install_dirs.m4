# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_INSTALL_DIRS(header_file)
# ------------------------------
# Write out the installation directories into a header file that is
# written by AC_SUBST so that files that depend on hard-coded paths
# can include it and be rebuilt when paths change.  We don't AC_DEFINE
# because we don't want to rebuild the entire tree just because
# someone changed the prefix.
AC_DEFUN([OMPI_INSTALL_DIRS], [
    ompi_exec_prefix_save="$exec_prefix"
    ompi_prefix_save="$prefix"

    m4_if(m4_version_compare(m4_defn([m4_PACKAGE_VERSION]), [2.60]), -1,
          [ # Previous to 2.60, everything was definied in terms
            # of datadir instead of datarootdir, so set datarootdir to
            # datadir in that situation....
           ompi_datadir_save="$datadir"
           datarootdir="${datadir}"
           datadir='${datarootdir}'])

    # need to temporarily expand this out as almost exactly as it will
    # be done later so that NONE doesn't show up in the
    # {exec_}prefix-based variables.
    test "x$prefix" = xNONE && prefix=$ac_default_prefix
    test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

    OPAL_PREFIX="$prefix"
    OPAL_EXEC_PREFIX="$exec_prefix"
    OPAL_BINDIR="$bindir"
    OPAL_SBINDIR="$sbindir"
    OPAL_LIBEXECDIR="$libexecdir"
    OPAL_DATAROOTDIR="$datarootdir"
    OPAL_DATADIR="$datadir"
    OPAL_SYSCONFDIR="$sysconfdir"
    OPAL_SHAREDSTATEDIR="$sharedstatedir"
    OPAL_LOCALSTATEDIR="$localstatedir"
    OPAL_LIBDIR="$libdir"
    OPAL_INCLUDEDIR="$includedir"
    OPAL_INFODIR="$infodir"
    OPAL_MANDIR="$mandir"

    AC_SUBST(OPAL_PREFIX)
    AC_SUBST(OPAL_EXEC_PREFIX)
    AC_SUBST(OPAL_BINDIR)
    AC_SUBST(OPAL_SBINDIR)
    AC_SUBST(OPAL_LIBEXECDIR)
    AC_SUBST(OPAL_DATAROOTDIR)
    AC_SUBST(OPAL_DATADIR)
    AC_SUBST(OPAL_SYSCONFDIR)
    AC_SUBST(OPAL_SHAREDSTATEDIR)
    AC_SUBST(OPAL_LOCALSTATEDIR)
    AC_SUBST(OPAL_LIBDIR)
    AC_SUBST(OPAL_INCLUDEDIR)
    AC_SUBST(OPAL_INFODIR)
    AC_SUBST(OPAL_MANDIR)

    prefix="$ompi_prefix_save"
    exec_prefix="$ompi_exec_prefix_save"
    m4_if(m4_version_compare(m4_defn([m4_PACKAGE_VERSION]), [2.60]), -1,
          [datadir="$ompi_datadir_save"])

    AC_CONFIG_FILES([$1])
])dnl
