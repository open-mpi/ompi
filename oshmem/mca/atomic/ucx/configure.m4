/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

# MCA_oshmem_atomic_ucx_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_atomic_ucx_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/atomic/ucx/Makefile])

    OMPI_CHECK_UCX([atomic_ucx],
                   [atomic_ucx_happy="yes"],
                   [atomic_ucx_happy="no"])

    AS_IF([test "$atomic_ucx_happy" = "yes"],
          [$1],
          [$2])


    # substitute in the things needed to build ucx 
    AC_SUBST([atomic_ucx_CFLAGS])
    AC_SUBST([atomic_ucx_CPPFLAGS])
    AC_SUBST([atomic_ucx_LDFLAGS])
    AC_SUBST([atomic_ucx_LIBS])
])dnl

