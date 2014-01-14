# -*- shell-script -*-
#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_oshmem_memheap_CONFIG],[
    # configure all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    # this is a direct callable component, so set that up.
    MCA_SETUP_DIRECT_CALL($1, $2)

    OMPI_CHECK_OPENFABRICS([oshmem_verbs],
                           [oshmem_verbs_happy="yes"],
                           [oshmem_verbs_happy="no"])
    
     # substitute in the things needed to build MEMHEAP BASE
    AC_SUBST([oshmem_verbs_CFLAGS])
    AC_SUBST([oshmem_verbs_CPPFLAGS])
    AC_SUBST([oshmem_verbs_LDFLAGS])
    AC_SUBST([oshmem_verbs_LIBS])

    # If we have the oshmem_verbs stuff available, find out what we've got
    AS_IF(
        [test "$oshmem_verbs_happy" = "yes"],
        [
            OSHMEM_LIBSHMEM_EXTRA_LDFLAGS="$OSHMEM_LIBSHMEM_EXTRA_LDFLAGS $oshmem_verbs_LDFLAGS"
            OSHMEM_LIBSHMEM_EXTRA_LIBS="$OSHMEM_LIBSHMEM_EXTRA_LIBS $oshmem_verbs_LIBS"

            # ibv_reg_shared_mr was added in MOFED 1.8
            oshmem_have_mpage=0

            oshmem_verbs_save_CPPFLAGS="$CPPFLAGS"
            oshmem_verbs_save_LDFLAGS="$LDFLAGS"
            oshmem_verbs_save_LIBS="$LIBS"

            CPPFLAGS="$CPPFLAGS $oshmem_verbs_CPPFLAGS"
            LDFLAGS="$LDFLAGS $oshmem_verbs_LDFLAGS"
            LIBS="$LIBS $oshmem_verbs_LIBS"

            AC_CHECK_DECLS([IBV_ACCESS_ALLOCATE_MR,IBV_ACCESS_SHARED_MR_USER_READ],
                   [oshmem_have_mpage=2], [],
                   [#include <infiniband/verbs.h>])

            CPPFLAGS="$oshmem_verbs_save_CPPFLAGS"
            LDFLAGS="$oshmem_verbs_save_LDFLAGS"
            LIBS="$oshmem_verbs_save_LIBS"
    
            AC_DEFINE_UNQUOTED(MPAGE_ENABLE, $oshmem_have_mpage,
                [Whether we can use M-PAGE supported since MOFED 1.8])
        ])

   ])


