# -*- shell-script -*-
#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_mca_sshmem_verbs_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_sshmem_verbs_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/sshmem/verbs/Makefile])

    # do we have the verbs shm stuff?
    AC_MSG_CHECKING([if want verbs shared memory support])
    AC_ARG_ENABLE(verbs-sshmem,
        AC_HELP_STRING([--disable-verbs-sshmem],
                       [disable verbs shared memory support (default: enabled)]))
    AS_IF([test "$enable_verbs_sshmem" = "no"],
          [AC_MSG_RESULT([no])
           oshmem_verbs_sm_build_verbs=0],
          [AC_MSG_RESULT([yes])
           OPAL_CHECK_OPENFABRICS([oshmem_verbs],
                  [oshmem_verbs_sm_build_verbs=1],
                  [oshmem_verbs_sm_build_verbs=0])])

    # substitute in the things needed to build
    AC_SUBST([oshmem_verbs_CFLAGS])
    AC_SUBST([oshmem_verbs_CPPFLAGS])
    AC_SUBST([oshmem_verbs_LDFLAGS])
    AC_SUBST([oshmem_verbs_LIBS])

    # ibv_reg_shared_mr was added in MOFED 1.8
    oshmem_have_mpage=0
    # If we have the oshmem_verbs stuff available, find out what we've got
    AS_IF(
        [test "$oshmem_verbs_sm_build_verbs" = "1"],
        [
            OSHMEM_LIBSHMEM_EXTRA_LDFLAGS="$OSHMEM_LIBSHMEM_EXTRA_LDFLAGS $oshmem_verbs_LDFLAGS"
            OSHMEM_LIBSHMEM_EXTRA_LIBS="$OSHMEM_LIBSHMEM_EXTRA_LIBS $oshmem_verbs_LIBS"

            oshmem_verbs_save_CPPFLAGS="$CPPFLAGS"
            oshmem_verbs_save_LDFLAGS="$LDFLAGS"
            oshmem_verbs_save_LIBS="$LIBS"

            CPPFLAGS="$CPPFLAGS $oshmem_verbs_CPPFLAGS"
            LDFLAGS="$LDFLAGS $oshmem_verbs_LDFLAGS"
            LIBS="$LIBS $oshmem_verbs_LIBS"

            AC_CHECK_DECLS([IBV_ACCESS_ALLOCATE_MR,IBV_ACCESS_SHARED_MR_USER_READ],
                   [oshmem_have_mpage=2], [],
                   [#include <infiniband/verbs.h>])

            AC_CHECK_DECLS([IBV_EXP_ACCESS_ALLOCATE_MR,IBV_EXP_ACCESS_SHARED_MR_USER_READ],
                   [oshmem_have_mpage=3], [],
                   [#include <infiniband/verbs.h>])

            CPPFLAGS="$oshmem_verbs_save_CPPFLAGS"
            LDFLAGS="$oshmem_verbs_save_LDFLAGS"
            LIBS="$oshmem_verbs_save_LIBS"

		    if test "x$oshmem_have_mpage" = "x0"; then
		        oshmem_verbs_sm_build_verbs=0
		    fi
        ])
    AC_DEFINE_UNQUOTED(MPAGE_ENABLE, $oshmem_have_mpage, [Whether we can use M-PAGE supported since MOFED 1.8])

    exp_access_happy=0
    exp_reg_mr_happy=0
    AS_IF([test "$oshmem_have_mpage" = "3"],
            [
              AC_CHECK_MEMBER([struct ibv_exp_reg_shared_mr_in.exp_access],
                [exp_access_happy=1],
                [],
                [#include <infiniband/verbs_exp.h>])

                AC_CHECK_MEMBER([struct ibv_exp_reg_mr_in.create_flags],
                [exp_reg_mr_happy=1],
                [],
                [#include <infiniband/verbs_exp.h>])
         ])
    AC_DEFINE_UNQUOTED(MPAGE_HAVE_SMR_EXP_ACCESS, $exp_access_happy, [exp_access field is part of ibv_exp_reg_shared_mr_in])
    AC_DEFINE_UNQUOTED(MPAGE_HAVE_IBV_EXP_REG_MR_CREATE_FLAGS, $exp_reg_mr_happy, [create_flags field is part of ibv_exp_reg_mr_in])

    AS_IF([test "$enable_verbs_sshmem" = "yes" && test "$oshmem_verbs_sm_build_verbs" = "0"],
          [AC_MSG_WARN([VERBS shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$oshmem_verbs_sm_build_verbs" = "1"], [$1], [$2])

    AC_DEFINE_UNQUOTED([OSHMEM_SSHMEM_VERBS],
                       [$oshmem_verbs_sm_build_verbs],
                       [Whether we have shared memory support for verbs or not])
])dnl
