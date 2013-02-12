# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

dist_pkgdata_DATA += runtime/help-shmem-runtime.txt


headers += \
	runtime/runtime.h \
	runtime/params.h \
       runtime/oshmem_shmem_preconnect.h 

libshmem_la_SOURCES += \
	runtime/oshmem_shmem_init.c \
	runtime/oshmem_shmem_finalize.c \
       runtime/oshmem_shmem_abort.c \
       runtime/oshmem_shmem_params.c



