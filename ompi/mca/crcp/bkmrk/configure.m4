# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_crcp_bkmrk_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_crcp_bkmrk_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/crcp/bkmrk/Makefile])

    # If we don't want FT, don't compile this component
    AS_IF([test "$opal_want_ft_cr" = "1"],
        [$1],
        [$2])
])dnl
