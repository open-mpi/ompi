# -*- shell-script -*-
#
# Copyright (c) 2011-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2011-2015 INRIA.  All rights reserved.
# Copyright (c) 2011-2015 Universite Bordeaux 1
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2019      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_topo_treematch_CONFIG([action-if-can-compile],
#                                [action-if-cant-compile])
# -------------------------------------------
AC_DEFUN([MCA_ompi_topo_treematch_CONFIG], [
    OPAL_CONFIG_TREEMATCH([topo_treematch], [$1], [$2])
    AC_CONFIG_FILES([ompi/mca/topo/treematch/Makefile])

    AC_SUBST([topo_treematch_CPPFLAGS])
    AC_SUBST([topo_treematch_LDFLAGS])
    AC_SUBST([topo_treematch_LIBS])
])
