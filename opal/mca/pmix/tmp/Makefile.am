#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        pmix_tmp.h \
        pmix_tmp_component.c \
        pmix_tmp.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_tmp_DSO
component_noinst =
component_install = mca_pmix_tmp.la
else
component_noinst = libmca_pmix_tmp.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_tmp_la_SOURCES = $(sources)
mca_pmix_tmp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_tmp_la_SOURCES =$(sources)
libmca_pmix_tmp_la_LDFLAGS = -module -avoid-version
