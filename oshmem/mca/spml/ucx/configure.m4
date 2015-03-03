/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

# MCA_oshmem_spml_ucx_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_spml_ucx_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/spml/ucx/Makefile])

    OMPI_CHECK_UCX([spml_ucx],
                   [spml_ucx_happy="yes"],
                   [spml_ucx_happy="no"])

    AS_IF([test "$spml_ucx_happy" = "yes"],
          [$1],
          [$2])


    # substitute in the things needed to build ucx 
    AC_SUBST([spml_ucx_CFLAGS])
    AC_SUBST([spml_ucx_CPPFLAGS])
    AC_SUBST([spml_ucx_LDFLAGS])
    AC_SUBST([spml_ucx_LIBS])
])dnl

