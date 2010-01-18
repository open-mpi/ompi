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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
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
    contrib_vt_args="--disable-option-checking --with-openmpi-inside"

    contrib_vt_skip=no
    eval "set x $ac_configure_args"; shift
    for contrib_vt_arg
    do
        if test "$contrib_vt_skip" = "yes"; then
            contrib_vt_skip=no
        else
            case $contrib_vt_arg in
            -with-contrib-vt-flags | --with-contrib-vt-flags)
                contrib_vt_skip=yes
                ;;
            -with-contrib-vt-flags=* | --with-contrib-vt-flags=*)
                ;;
            -with-platform | --with-platform)
                contrib_vt_skip=yes
                ;;
            -with-platform=* | --with-platform=*)
                ;;
            *)
                case $contrib_vt_arg in
                *\'*) contrib_vt_arg=`echo "$contrib_vt_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
                esac
                contrib_vt_args="$contrib_vt_args '$contrib_vt_arg'"
                ;;
            esac
        fi
    done

    AC_ARG_WITH([contrib-vt-flags],
                [AC_HELP_STRING([--with-contrib-vt-flags=FLAGS],
                                [Pass FLAGS to the VampirTrace distribution configuration script])])
    AS_IF([test "$with_contrib_vt_flags" != "yes" -a "$with_contrib_vt_flags" != "no"],
          [contrib_vt_args="$contrib_vt_args $with_contrib_vt_flags"])

    # Run VampirTrace's configure and see if it succeeded
    OMPI_CONFIG_SUBDIR([ompi/contrib/vt/vt],
                       [$contrib_vt_args], 
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
