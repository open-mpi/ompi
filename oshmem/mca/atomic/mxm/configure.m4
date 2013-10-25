/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

# MCA_oshmem_atomic_mxm_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_atomic_mxm_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/atomic/mxm/Makefile])
    OMPI_CHECK_MXM([atomic_mxm],
                   [save_CPPFLAGS="$CPPFLAGS"
                   save_LDFLAGS="$LDFLAGS"
                   save_LIBS="$LIBS"

                   CPPFLAGS="$CPPFLAGS -I$ompi_check_mxm_dir/include"
                   LDFLAGS="$LDFLAGS -L$ompi_check_mxm_dir/lib"
                   LIBS="$LIBS -lmxm"
                   AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
                                #include <mxm/api/mxm_api.h>
                                int main() {
                                if (mxm_get_version() < MXM_VERSION(1,5) )
                                    return 1;

                                /* if compiler sees these constansts then mxm has atomic support*/
                                int add_index = MXM_REQ_OP_ATOMIC_ADD;
                                int swap_index = MXM_REQ_OP_ATOMIC_SWAP;
                                return 0;
                                }]])],
                    [AC_DEFINE([OSHMEM_HAS_ATOMIC_MXM], [1], [mxm support is available]) atomic_mxm_happy="yes"],
                   [atomic_mxm_happy="no"],
                   [atomic_mxm_happy="no"])
                   CPPFLAGS=$save_CPPFLAGS
                   LDFLAGS=$save_LDFLAGS
                   LIBS=$save_LIBS
                   ],
                   [atomic_mxm_happy="no"])

    AS_IF([test "$atomic_mxm_happy" = "yes"],
          [atomic_mxm_WRAPPER_EXTRA_LDFLAGS="$atomic_mxm_LDFLAGS"
           atomic_mxm_WRAPPER_EXTRA_LIBS="$atomic_mxm_LIBS"
           $1],
          [$2])


    # substitute in the things needed to build mxm
    AC_SUBST([atomic_mxm_CFLAGS])
    AC_SUBST([atomic_mxm_CPPFLAGS])
    AC_SUBST([atomic_mxm_LDFLAGS])
    AC_SUBST([atomic_mxm_LIBS])

    AC_MSG_CHECKING([if oshmem/atomic/mxm component can be compiled])
    AC_MSG_RESULT([$atomic_mxm_happy])
])dnl

