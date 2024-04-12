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
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_schizo_ompi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_prte_schizo_ompi_CONFIG],[
    AC_CONFIG_FILES([src/mca/schizo/ompi/Makefile])

    AC_ARG_ENABLE([ompi-support],
                  [AS_HELP_STRING([--disable-ompi-support],
                                  [Disable support for Open MPI (default: no)])],
                  [],
                  [enable_ompi_support=yes])

    AS_IF([test "$enable_ompi_support" = "yes"],
          [$1], [$2])

    PRTE_SUMMARY_ADD([Personalities], [OMPI], [], [$enable_ompi_support])

])dnl
