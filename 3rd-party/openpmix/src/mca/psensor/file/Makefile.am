#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_pmixdata_DATA = help-pmix-psensor-file.txt

sources = \
        psensor_file.c \
        psensor_file.h \
        psensor_file_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_psensor_file_DSO
component_noinst =
component_install = pmix_mca_psensor_file.la
else
component_noinst = libpmix_mca_psensor_file.la
component_install =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component_install)
pmix_mca_psensor_file_la_SOURCES = $(sources)
pmix_mca_psensor_file_la_LDFLAGS = -module -avoid-version
if NEED_LIBPMIX
pmix_mca_psensor_file_la_LIBADD = $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(component_noinst)
libpmix_mca_psensor_file_la_SOURCES =$(sources)
libpmix_mca_psensor_file_la_LDFLAGS = -module -avoid-version
