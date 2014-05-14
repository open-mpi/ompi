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
	help-oshmem-spml-yoda.txt

EXTRA_DIST = post_configure.sh

AM_CFLAGS = $(btl_sm_CPPFLAGS)

yoda_sources  = \
	spml_yoda.c \
	spml_yoda.h \
	spml_yoda_component.c \
	spml_yoda_component.h \
	spml_yoda_rdmafrag.h \
	spml_yoda_putreq.c \
	spml_yoda_putreq.h \
	spml_yoda_getreq.c \
	spml_yoda_getreq.h 
 
if MCA_BUILD_ompi_pml_ob1_DSO
component_noinst =
component_install = mca_spml_yoda.la 
else
component_noinst = libmca_spml_yoda.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_spml_yoda_la_SOURCES = $(yoda_sources)
mca_spml_yoda_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_spml_yoda_la_SOURCES = $(yoda_sources)
libmca_spml_yoda_la_LDFLAGS = -module -avoid-version
