#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = \
	help-oshmem-spml-ikrit.txt

AM_CPPFLAGS = $(spml_ikrit_CPPFLAGS)

ikrit_sources  = \
	spml_ikrit.c \
	spml_ikrit.h \
	spml_ikrit_component.c \
	spml_ikrit_component.h 
 
if MCA_BUILD_oshmem_spml_ikrit_DSO
component_noinst =
component_install = mca_spml_ikrit.la 
else
component_noinst = libmca_spml_ikrit.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_spml_ikrit_la_SOURCES = $(ikrit_sources)
mca_spml_ikrit_la_LIBADD = $(spml_ikrit_LIBS)
mca_spml_ikrit_la_LDFLAGS = -module -avoid-version $(spml_ikrit_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_spml_ikrit_la_SOURCES = $(ikrit_sources)
libmca_spml_ikrit_la_LIBADD = $(spml_ikrit_LIBS)
libmca_spml_ikrit_la_LDFLAGS = -module -avoid-version $(spml_ikrit_LDFLAGS)

