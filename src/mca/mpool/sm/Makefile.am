# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
	mpool_sm.c \
	mpool_sm.h \
	mpool_sm_mmap.c \
	mpool_sm_mmap.h \
	mpool_sm_component.c

if OMPI_BUILD_mpool_sm_LOADABLE_MODULE
module_noinst =
module_install = mca_mpool_sm.la
else
module_noinst = libmca_mpool_sm.la
module_install =
endif

mcamoduledir = $(libdir)/openmpi
mcamodule_LTLIBRARIES = $(module_install)
mca_mpool_sm_la_SOURCES = $(sources)
mca_mpool_sm_la_LIBADD = $(LIBOMPI_LA)
mca_mpool_sm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_mpool_sm_la_SOURCES = $(sources)
libmca_mpool_sm_la_LDFLAGS = -module -avoid-version
