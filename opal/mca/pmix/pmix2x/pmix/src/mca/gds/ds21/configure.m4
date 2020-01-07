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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_gds_ds21_CONFIG([action-if-can-compile],
#                     [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_gds_ds21_CONFIG],[
    AC_CONFIG_FILES([src/mca/gds/ds21/Makefile])

    AS_IF([test "$pmix_pthread_mutexattr_setpshared" = "yes" && test "$pmix_pthread_process_shared" = "yes"],
          [$1], [$2])

])dnl
