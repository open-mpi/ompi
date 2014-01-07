#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-mpi-btl-smcuda.txt

libmca_btl_smcuda_la_sources = \
    btl_smcuda.c \
    btl_smcuda.h \
    btl_smcuda_component.c \
    btl_smcuda_endpoint.h \
    btl_smcuda_fifo.h \
    btl_smcuda_frag.c \
    btl_smcuda_frag.h 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_btl_smcuda_DSO
component_noinst =
component_install = mca_btl_smcuda.la
else
component_noinst = libmca_btl_smcuda.la
component_install =
endif

# See ompi/mca/common/cuda/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_smcuda_la_SOURCES = $(libmca_btl_smcuda_la_sources)
mca_btl_smcuda_la_LDFLAGS = -module -avoid-version
mca_btl_smcuda_la_LIBADD = \
    $(top_ompi_builddir)/ompi/mca/common/sm/libmca_common_sm.la
mca_btl_smcuda_la_CPPFLAGS = $(btl_smcuda_CPPFLAGS)
if OPAL_cuda_support
mca_btl_smcuda_la_LIBADD += \
    $(top_ompi_builddir)/ompi/mca/common/cuda/libmca_common_cuda.la
endif

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_smcuda_la_SOURCES = $(libmca_btl_smcuda_la_sources)
libmca_btl_smcuda_la_LDFLAGS = -module -avoid-version
libmca_btl_smcuda_la_CPPFLAGS = $(btl_smcuda_CPPFLAGS)
