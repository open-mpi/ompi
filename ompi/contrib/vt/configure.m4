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
# Copyright (c) 2007      Cisco, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_contrib_vt_CONFIG([action-if-can-compile], 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([OMPI_contrib_vt_CONFIG],[
    AC_ARG_WITH([contrib-vt-flags],
                [AC_HELP_STRING([--with-contrib-vt-flags=FLAGS],
                                [Pass FLAGS to the VampirTrace distribution configuration script])])

    AS_IF([test -n "$with_contrib_vt_flags" -a "$with_contrib_vt_flags" != "no"],
          [contrib_vt_flags="$with_contrib_vt_flags $contrib_vt_flags"],
          [contrib_vt_flags=])

    AS_IF([test -n "$prefix" -a "$prefix" != "NONE"],
          [contrib_vt_flags="$contrib_vt_flags --prefix=$prefix"])

    AS_IF([test "$cross_compiling" = "yes"],
        [AS_IF([test ! -z $build], [contrib_vt_flags="$contrib_vt_flags --build=$build"])
         AS_IF([test ! -z $host], [contrib_vt_flags="$contrib_vt_flags --host=$host"])
         AS_IF([test ! -z $target], [contrib_vt_flags="$contrib_vt_flags --target=$target"])])

    contrib_vt_flags="MPICC="'"'"$CC"'"'" --with-openmpi --with-mpi-inc-dir="'"'"$top_ompi_builddir/ompi/include"'"'" --with-mpi-status-size=5 --with-mpi-io --disable-config-titles --disable-config-summary $contrib_vt_flags"

    # Run VampirTrace's configure and see if it succeeded
    OMPI_CONFIG_SUBDIR([ompi/contrib/vt/vt],
                       [$contrib_vt_flags], 
                       [contrib_vt_happy=1], [contrib_vt_happy=0])

    # If VampirTrace configured itself successfully, setup OMPI-specific
    # VampirTrace glue code to build.
    AS_IF([test "$contrib_vt_happy" = "1"],
          [$1
           AC_CONFIG_FILES([
               ompi/contrib/vt/Makefile
               ompi/contrib/vt/wrappers/Makefile
               ompi/contrib/vt/wrappers/mpicc-vt-wrapper-data.txt
               ompi/contrib/vt/wrappers/mpic++-vt-wrapper-data.txt
               ompi/contrib/vt/wrappers/mpif77-vt-wrapper-data.txt
               ompi/contrib/vt/wrappers/mpif90-vt-wrapper-data.txt
           ])],
          [$2])
])dnl
