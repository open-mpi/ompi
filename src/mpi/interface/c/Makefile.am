# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
#
# LAM_PRPOFILING_DEFINES flag s enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory,
# we need it to be 0
#
AM_CPPFLAGS		= -DLAM_PROFILING_DEFINES=0

#
# The top directory always builds MPI_* bindings. The bottom directory
# always builds PMPI_* bindings. The cases where the top directory
# needs to be built are
# 1. When profiling is disabled.
# 2. When profiling is enabled but weak symbol support is absent.
#
if WANT_MPI_BINDINGS_LAYER
mpi_lib = libmpi_c_mpi.la
else
mpi_lib = 
endif

noinst_LTLIBRARIES = libmpi_c.la $(mpi_lib)

#
# The profile subdirectory always builds PMPI_* bindings. Hence, this
# directory needs to be built whenever profiling is enabled
#
if WANT_PMPI_BINDINGS_LAYER
	pmpi_lib = profile/libmpi_c_pmpi.la
else
	pmpi_lib = 
endif
#
# 
SUBDIRS = profile
#
# 
headers = bindings.h
#
# libmpi_c.la is the final library that will be built. This will try
# to snarf in mpi_lib and pmpi_lib. So, the right things will be done
# by default
#

libmpi_c_la_SOURCES	= 

libmpi_c_la_LIBADD	= \
					$(mpi_lib) \
					$(pmpi_lib)

libmpi_c_mpi_la_SOURCES = \
                abort.c \
                accumulate.c \
                add_error_class.c \
                add_error_code.c \
                add_error_string.c \
                address.c \
                allgather.c \
                allgatherv.c \
                alloc_mem.c \
                allreduce.c \
                alltoall.c \
                alltoallv.c \
                alltoallw.c \
                attr_delete.c \
                attr_get.c \
                attr_put.c \
                barrier.c \
                bcast.c \
                bsend_init.c \
                send.c \
                buffer_attach.c \
                buffer_detach.c \
                cancel.c \
                cart_coords.c \
                art_create.c \
                cart_get.c \
                cart_map.c \
                cart_rank.c \
                cart_shift.c \
                cart_sub.c \
                cartdim_get.c \
                close_port.c \
                comm_accept.c \
                comm_c2f.c \
                comm_call_errhandler.c \
                comm_compare.c \
                comm_connect.c \
                comm_create_errhandler.c \
                comm_create_keyval.c \
                comm_create.c \
                comm_delete_attr.c \
                comm_disconnect.c \
                comm_dup.c \
                comm_f2c.c \
                comm_free_keyval.c \
                comm_free.c \
                comm_get_attr.c \
                comm_get_errhandler.c \
                comm_get_name.c \
                comm_get_parent.c \
                comm_group.c \
                comm_join.c \
                comm_rank.c \
                comm_remote_group.c \
                comm_remote_size.c \
                comm_set_attribute.c \
                comm_set_errhandler.c \
                comm_set_name.c \
                comm_size.c \
                comm_spawn.c \
                comm_spawn_multiple.c \
                comm_split.c \
                comm_test_inter.c \
                finalize.c \
                free_mem.c \
                init.c \
                test_cancelled.c \
                test.c \
                testall.c \
                testany.c \
                testsome.c \
                topo_test.c \
                type_commit.c \
                type_contiguous.c \
                type_create_darray.c \
                type_create_f90_complex.c \
                type_create_f90_integer.c \
                type_create_f90_real.c \
                type_create_hindexed.c \
                type_create_hvector.c \
                type_create_indexed_block.c \
                type_create_keyval.c \
                type_create_resized.c \
                type_create_struct.c \
                type_create_subarray.c \
                type_delete_attr.c \
                type_dup.c \
                type_extent.c \
                type_free.c \
                type_free_keyval.c \
                type_get_attr.c \
                type_get_contents.c \
                type_get_envelope.c \
                type_get_extent.c \
                type_get_name.c \
                type_get_true_extent.c \
                type_hindexed.c \
                type_hvector.c \
                type_indexed.c \
                type_lb.c \
                type_match_size.c \
                type_set_attr.c \
                type_set_name.c \
                type_size.c \
                type_struct.c \
                type_ub.c \
                type_vector.c


EXTRA_DIST = $(libmpi_c_mpi_la_SOURCES) 
# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/c
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
