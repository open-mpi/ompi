#
# Copyright (c) 2015      Mellanox Technologies, Inc.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = 

AM_CPPFLAGS = $(spml_ucx_CPPFLAGS)

ucx_sources  = \
 spml_ucx_component.h \
 spml_ucx_component.c \
 spml_ucx.h \
 spml_ucx.c 


 
if MCA_BUILD_oshmem_spml_ucx_DSO
component_noinst =
component_install = mca_spml_ucx.la 
else
component_noinst = libmca_spml_ucx.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_spml_ucx_la_SOURCES = $(ucx_sources)
mca_spml_ucx_la_LIBADD = $(spml_ucx_LIBS)
mca_spml_ucx_la_LDFLAGS = -module -avoid-version $(spml_ucx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_spml_ucx_la_SOURCES = $(ucx_sources)
libmca_spml_ucx_la_LIBADD = $(spml_ucx_LIBS)
libmca_spml_ucx_la_LDFLAGS = -module -avoid-version $(spml_ucx_LDFLAGS)

