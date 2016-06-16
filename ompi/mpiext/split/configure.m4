# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_split_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_split_CONFIG], [
    AC_CONFIG_FILES([ompi/mpiext/split/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/split/c/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/split/c/profile/Makefile])

    # This example can always build, so we just execute $1 if it was
    # requested.
    AS_IF([test "$ENABLE_split" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
])
