# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
#
# LAM_PRPOFILING_DEFINES flag is enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory,
# we need it to be 0
#
AM_CPPFLAGS		= -DLAM_PROFILE_LAYER=0
#
# The top directory always builds MPI_* bindings. The bottom directory
# always builds PMPI_* bindings. The cases where the top directory
# needs to be built are
# 1. When profiling is disabled.
# 2. When profiling is enabled but weak symbol support is absent.
#
if WANT_MPI_BINDINGS_LAYER
mpi_lib = libmpi_f77_mpi.la
else
mpi_lib = 
endif

noinst_LTLIBRARIES = libmpi_f77.la $(mpi_lib)

#
# The profile subdirectory always builds PMPI_* bindings. Hence, this
# directory needs to be built whenever profiling is enabled
#
if WANT_PMPI_BINDINGS_LAYER
	pmpi_lib = profile/libmpi_f77_pmpi.la
else
	pmpi_lib = 
endif
#
# 
SUBDIRS = profile
#
# 
headers = \
	bindings.h \
	prototypes_mpi.h
#
# libmpi_f77.la is the final library that will be built. This will try
# to snarf in mpi_lib and pmpi_lib. So, the right things will be done
# by default
#

libmpi_f77_la_SOURCES	= 

libmpi_f77_la_LIBADD	= \
					$(mpi_lib) \
					$(pmpi_lib)

libmpi_f77_mpi_la_SOURCES = \
        	alloc_mem_f.c 

bogus_stuff = \
			comm_get_name_f.c \
			comm_set_name_f.c \
			init_f.c \
			finalize_f.c \
        	free_mem_f.c
#
EXTRA_DIST = $(libmpi_f77_mpi_la_SOURCES)
#
# Conditionally install the header files
#
if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/f77
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
