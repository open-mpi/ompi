#
# Copyright (c) 2018      DataDirect Networks. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if MCA_BUILD_ompi_fbtl_ime_DSO
component_noinst =
component_install = mca_fbtl_ime.la
else
component_noinst = libmca_fbtl_ime.la
component_install =
endif


# Source files

fbtl_ime_sources = \
        fbtl_ime.h \
        fbtl_ime.c \
        fbtl_ime_component.c \
        fbtl_ime_blocking_op.c \
        fbtl_ime_nonblocking_op.c

AM_CPPFLAGS = $(fbtl_ime_CPPFLAGS)

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fbtl_ime_la_SOURCES = $(fbtl_ime_sources)
mca_fbtl_ime_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(fbtl_ime_LIBS)
mca_fbtl_ime_la_LDFLAGS = -module -avoid-version $(fbtl_ime_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_fbtl_ime_la_SOURCES = $(fbtl_ime_sources)
libmca_fbtl_ime_la_LIBADD = $(fbtl_ime_LIBS)
libmca_fbtl_ime_la_LDFLAGS = -module -avoid-version $(fbtl_ime_LDFLAGS)