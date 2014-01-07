# -*- indent-tabs-mode:nil -*-
#
# Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(btl_ugni_CPPFLAGS)

if MCA_BUILD_ompi_btl_ugni_DSO
component_noinst =
component_install = mca_btl_ugni.la
else
component_noinst = libmca_btl_ugni.la
component_install =
endif

ugni_SOURCES = \
        btl_ugni_component.c \
        btl_ugni_module.c \
        btl_ugni_add_procs.c \
        btl_ugni_endpoint.h \
        btl_ugni_endpoint.c \
        btl_ugni_frag.c \
        btl_ugni_frag.h \
        btl_ugni_rdma.h \
        btl_ugni_send.c \
        btl_ugni_put.c \
        btl_ugni_get.c \
        btl_ugni.h \
        btl_ugni_smsg.h \
        btl_ugni_smsg.c \
        btl_ugni_prepare.h

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_ugni_la_SOURCES = $(ugni_SOURCES)
nodist_mca_btl_ugni_la_SOURCES = $(ugni_nodist_SOURCES)
mca_btl_ugni_la_LIBADD = $(btl_ugni_LIBS) 
mca_btl_ugni_la_LDFLAGS = -module -avoid-version $(btl_ugni_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_ugni_la_SOURCES = $(ugni_SOURCES)
nodist_libmca_btl_ugni_la_SOURCES = $(ugni_nodist_SOURCES)
libmca_btl_ugni_la_LIBADD = $(btl_ugni_LIBS)
libmca_btl_ugni_la_LDFLAGS = -module -avoid-version $(btl_ugni_LDFLAGS)
