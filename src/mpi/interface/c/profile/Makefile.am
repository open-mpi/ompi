# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
#
# LAM_PROFILING_DEFINES flag s enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory
# we definately need it to be 1.
#
AM_CPPFLAGS	= -DLAM_PROFILING_DEFINES=1

#
# This build needs to go through only if profiling is required.
# Further, this build HAS to go through if profiling is required.
#

if WANT_PMPI_BINDINGS_LAYER
pmpi_lib = libmpi_c_pmpi.la
else
pmpi_lib = 
endif

noinst_LTLIBRARIES = $(pmpi_lib)

headers = defines.h

nodist_libmpi_c_pmpi_la_SOURCES = \
                pabort.c \
				paccumulate.c \
				padd_error_class.c \
				padd_error_code.c \
				padd_error_string.c \
				paddress.c \
				pallgather.c \
				pallgatherv.c \
		        palloc_mem.c \
				pallreduce.c \
				palltoall.c \
				palltoallv.c \
				palltoallw.c \
				pattr_delete.c \
				pattr_get.c \
				pattr_put.c \
				pbarrier.c \
				pbcast.c \
				pbsend_init.c \
				pbsend.c \
				pbuffer_attach.c \
				pbuffer_detach.c \
				pcancel.c \
				pcart_coords.c \
				pcart_create.c \
				pcart_get.c \
				pcart_map.c \
				pcart_rank.c \
				pcart_shift.c \
				pcart_sub.c \
				pcartdim_get.c \
				pclose_port.c \
				pcomm_accept.c \
				pcomm_c2f.c \
				pcomm_call_errhandler.c \
				pcomm_compare.c \
				pcomm_connect.c \
				pcomm_create_errhandler.c \
				pcomm_create_keyval.c \
				pcomm_create.c \
				pcomm_delete_attr.c \
				pcomm_disconnect.c \
				pcomm_dup.c \
				pcomm_f2c.c \
				pcomm_free_keyval.c \
				pcomm_free.c \
				pcomm_get_attr.c \
				pcomm_get_errhandler.c \
				pcomm_get_name.c \
				pcomm_get_parent.c \
				pcomm_group.c \
				pcomm_join.c \
				pcomm_rank.c \
				pcomm_remote_group.c \
				pcomm_remote_size.c \
				pcomm_set_attribute.c \
				pcomm_set_errhandler.c \
				pcomm_set_name.c \
				pcomm_size.c \
				pcomm_spawn.c \
				pcomm_spawn_multiple.c \
				pcomm_split.c \
				pcomm_test_inter.c \
                pfinalize.c \
                pfree_mem.c \
                pinit.c

bogus_stuff = \
                ptype_commit.c \
                ptype_contiguous.c \
                ptype_create_darray.c \
                ptype_create_f90_complex.c \
                ptype_create_f90_integer.c \
                ptype_create_f90_real.c \
                ptype_create_hindexed.c \
                ptype_create_hvector.c \
                ptype_create_indexed_block.c \
                ptype_create_keyval.c \
                ptype_create_resized.c \
                ptype_create_struct.c \
                ptype_create_subarray.c \
                ptype_delete_attr.c \
                ptype_dup.c \
                ptype_extent.c \
                ptype_free.c \
                ptype_free_keyval.c \
                ptype_get_attr.c \
                ptype_get_contents.c \
                ptype_get_envelope.c \
                ptype_get_extent.c \
                ptype_get_name.c \
                ptype_get_true_extent.c \
                ptype_hindexed.c \
                ptype_hvector.c \
                ptype_indexed.c \
                ptype_lb.c \
                ptype_match_size.c \
                ptype_set_attr.c \
                ptype_set_name.c \
                ptype_size.c \
                ptype_struct.c \
                ptype_ub.c \
                ptype_vector.c
#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_c_pmpi_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		ln -s $(top_srcdir)/src/mpi/interface/c/$$pname $@ ; \
	fi

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/c/profile
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_c_pmpi_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
