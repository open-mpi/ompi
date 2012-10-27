#
# Copyright (c) 2011      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(opal_memchecker_pin_CPPFLAGS)
sources = \
        .windows \
        memchecker_pin.h \
        memchecker_pin_component.c \
        memchecker_pin_module.c \
        MemPin.cpp \
		MemPin.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_memchecker_pin_DSO
component_noinst =
component_install = mca_memchecker_pin.la
else
component_noinst = libmca_memchecker_pin.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_memchecker_pin_la_SOURCES = $(sources)
mca_memchecker_pin_la_LDFLAGS = -module -avoid-version
mca_memchecker_pin_la_LIBADD = \
        $(top_ompi_builddir)/opal/libopal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_memchecker_pin_la_SOURCES =$(sources)
libmca_memchecker_pin_la_LDFLAGS = -module -avoid-version
