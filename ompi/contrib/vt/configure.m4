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

    AC_ARG_WITH([contrib-vt-flags],
                [AC_HELP_STRING([--with-contrib-vt-flags=FLAGS],
                                [Pass FLAGS to the VampirTrace distribution configuration script])])
    AS_IF([test "$with_contrib_vt_flags" = "yes" -o "$with_contrib_vt_flags" = "no"],
          [with_contrib_vt_flags=""])

    contrib_vt_happy=1

    # In case of using the Oracle C++ compiler check whether its C++ STL
    # can be used to build VampirTrace.
    if test "x$ompi_cv_cxx_compiler_vendor" = "xsun"; then
        AC_MSG_CHECKING([whether C++ STL is suitable for vt])
        AC_LANG_PUSH(C++)
        AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM([[#include <map>]],
                             [[std::map<int, int> map;
                               // The following assignment implicitly converts an "iterator"
                               // to a "const_interator". Although this is allowed by the C++
                               // standard, using the default C++ STL (libCstd) will cause a
                               // compile error.
                               // Seen with Oracle Studio version 12.3 and Express 6/10.
                               std::pair<std::map<int, int>::const_iterator, bool> ret=
                               map.insert(std::pair<int, int>(123, 456));]])],
            [AC_MSG_RESULT([yes])],
            [AC_MSG_RESULT([no])
             AC_WARN([***********************************************************])
             AC_WARN([*** VampirTrace cannot be built due to an unsuitable])
             AC_WARN([*** C++ STL from Oracle Studio compiler.])
             AC_WARN([*** Please re-configure Open MPI to use the STLport4])
             AC_WARN([*** by adding the compiler flag -library=stlport4])
             AC_WARN([*** to CXXFLAGS.])
             if test -z $enable_vt; then
                 AC_WARN([*** Pausing to give you time to read this message...])
             fi
             AC_WARN([***********************************************************])
             if test -z $enable_vt; then
                 sleep 10
             fi
             contrib_vt_happy=0])
        AC_LANG_POP(C++)
    fi

    if test "$contrib_vt_happy" = "1"; then
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

        contrib_vt_args="$contrib_vt_args $with_contrib_vt_flags"

        # Run VampirTrace's configure and see if it succeeded
        OMPI_CONFIG_SUBDIR([ompi/contrib/vt/vt],
                           [$contrib_vt_args], 
                           [], [contrib_vt_happy=0])
    fi

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
