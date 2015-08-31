# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_cr_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_cr_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/cr/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/cr/c/Makefile])

    OPAL_VAR_SCOPE_PUSH([ompi_mpi_ext_cr_happy])

    # If we don't want FT, don't compile this extention
    AS_IF([test "$ENABLE_cr" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [ompi_mpi_ext_cr_happy=1],
          [ompi_mpi_ext_cr_happy=0])

    AS_IF([test "$ompi_mpi_ext_cr_happy" = "1" && \
           test "$opal_want_ft_cr" = "1"],
          [$1],
          [ # Error if the user specifically asked for this extension,
            # but we can't build it.
           AS_IF([test "$ENABLE_cr" = "1"],
                 [AC_MSG_WARN([Requested "cr" MPI extension, but cannot build it])
                  AC_MSG_WARN([because fault tolerance is not enabled.])
                  AC_MSG_WARN([Try again with --enable-ft])
                  AC_MSG_ERROR([Cannot continue])])
           $2])

    OPAL_VAR_SCOPE_POP
])
