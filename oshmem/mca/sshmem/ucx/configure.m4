/*
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

# MCA_oshmem_sshmem_ucx_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_sshmem_ucx_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/sshmem/ucx/Makefile])

    OMPI_CHECK_UCX([sshmem_ucx],
                   [sshmem_ucx_happy="yes"],
                   [sshmem_ucx_happy="no"])

    AS_IF([test "$sshmem_ucx_happy" = "yes"],
          [$1],
          [$2])


    # substitute in the things needed to build ucx 
    AC_SUBST([sshmem_ucx_CFLAGS])
    AC_SUBST([sshmem_ucx_CPPFLAGS])
    AC_SUBST([sshmem_ucx_LDFLAGS])
    AC_SUBST([sshmem_ucx_LIBS])
])dnl

