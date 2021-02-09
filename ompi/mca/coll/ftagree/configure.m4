# -*- shell-script -*-
#
# Copyright (c) 2012-2020 The University of Tennessee and the University
#                         of Tennessee Research Foundation.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_coll_ftagree_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_coll_ftagree_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/ftagree/Makefile])

    # If we don't want FT, don't compile this component
    AS_IF([test "$opal_want_ft_mpi" = "1"],
        [$1],
        [$2])
])dnl
