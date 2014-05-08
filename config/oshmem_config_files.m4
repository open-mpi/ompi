# -*- shell-script -*-
#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OSHMEM_CONFIG_FILES],[
    AC_CONFIG_FILES([
    oshmem/Makefile
    oshmem/include/Makefile
    oshmem/shmem/c/Makefile
    oshmem/shmem/c/profile/Makefile

    oshmem/shmem/fortran/Makefile
    oshmem/shmem/fortran/profile/Makefile

    oshmem/tools/oshmem_info/Makefile
    oshmem/tools/wrappers/Makefile
    oshmem/tools/wrappers/shmemcc-wrapper-data.txt
    oshmem/tools/wrappers/shmemfort-wrapper-data.txt
    ])
])
