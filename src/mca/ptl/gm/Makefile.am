# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
	ptl_gm.c

if OMPI_BUILD_ptl_gm_LOADABLE_MODULE
module_noinst =
module_install = mca_ptl_gm.la
else
module_noinst = libmca_ptl_gm.la
module_install =
endif

mcamoduledir = $(libdir)/openmpi
mcamodule_LTLIBRARIES = $(module_install)
mca_ptl_gm_la_SOURCES = $(sources)
mca_ptl_gm_la_LIBADD = $(LIBOMPI_LA)
mca_ptl_gm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_ptl_gm_la_SOURCES = $(sources)
libmca_ptl_gm_la_LDFLAGS = -module -avoid-version

