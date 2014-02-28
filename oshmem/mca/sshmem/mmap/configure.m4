# -*- shell-script -*-
#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_mca_sshmem_mmap_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_sshmem_mmap_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/sshmem/mmap/Makefile])

    # do we have the mmap shm stuff?
    AC_MSG_CHECKING([if want mmap shared memory support])
    AC_ARG_ENABLE(mmap-sshmem,
        AC_HELP_STRING([--disable-mmap-sshmem],
                       [disable mmap shared memory support (default: enabled)]))
    AS_IF([test "$enable_mmap_sshmem" = "no"],
          [AC_MSG_RESULT([no])
           oshmem_mmap_sm_build_mmap=0],
          [AC_MSG_RESULT([yes])
           AC_SEARCH_LIBS([mmap], [c],
                  [oshmem_mmap_sm_build_mmap=1],
                  [oshmem_mmap_sm_build_mmap=0])])
    AS_IF([test "$enable_mmap_sshmem" = "yes" -a "$oshmem_mmap_sm_build_mmap" = "0"],
          [AC_MSG_WARN([MMAP shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$oshmem_mmap_sm_build_mmap" = "1"], [$1], [$2])

    AC_DEFINE_UNQUOTED([OSHMEM_SSHMEM_MMAP],
                       [$oshmem_mmap_sm_build_mmap],
                       [Whether we have shared memory support for mmap or not])
])dnl
