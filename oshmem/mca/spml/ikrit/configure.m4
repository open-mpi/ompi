/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

# MCA_oshmem_mtl_mxm_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_spml_ikrit_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/spml/ikrit/Makefile])

    OMPI_CHECK_MXM([spml_ikrit],
                   [spml_ikrit_happy="yes"],
                   [spml_ikrit_happy="no"])

    AS_IF([test "$spml_ikrit_happy" = "yes"],
          [$1],
          [$2])


    # substitute in the things needed to build mxm
    AC_SUBST([spml_ikrit_CFLAGS])
    AC_SUBST([spml_ikrit_CPPFLAGS])
    AC_SUBST([spml_ikrit_LDFLAGS])
    AC_SUBST([spml_ikrit_LIBS])
])dnl

