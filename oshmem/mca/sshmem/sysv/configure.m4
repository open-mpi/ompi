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

# MCA_mca_sshmem_sysv_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_sshmem_sysv_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/sshmem/sysv/Makefile])

    # do we have the sysv shm stuff?
    AC_MSG_CHECKING([if want SYSV shared memory support])
    AC_ARG_ENABLE(sysv-sshmem,
        AC_HELP_STRING([--disable-sysv-sshmem],
                       [disable sysv shared memory support (default: enabled)]))
    AS_IF([test "$enable_sysv_sshmem" = "no"],
          [AC_MSG_RESULT([no])
           oshmem_sysv_sm_build_sysv=0],
          [AC_MSG_RESULT([yes])
          AC_CHECK_FUNC(shmget,
                  [oshmem_sysv_sm_build_sysv=1],
                  [oshmem_sysv_sm_build_sysv=0])])
    AS_IF([test "$enable_sysv_sshmem" = "yes" && test "$oshmem_sysv_sm_build_sysv" = "0"],
          [AC_MSG_WARN([SYSV shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$oshmem_sysv_sm_build_sysv" = "1"], [$1], [$2])

    AC_DEFINE_UNQUOTED([OSHMEM_SSHMEM_SYSV],
                       [$oshmem_sysv_sm_build_sysv],
                       [Whether we have shared memory support for SYSV or not])
])dnl
