#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        pmix_native.h \
        pmix_native_component.c \
        pmix_native.c \
        usock.c \
        usock_sendrecv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_native_DSO
component_noinst =
component_install = mca_pmix_native.la
else
component_noinst = libmca_pmix_native.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_native_la_SOURCES = $(sources)
mca_pmix_native_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_native_la_SOURCES =$(sources)
libmca_pmix_native_la_LDFLAGS = -module -avoid-version
