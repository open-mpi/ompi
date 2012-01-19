# -*- indent-tabs-mode:nil -*-
#
# Copyright (c) 2011      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(common_ugni_CPPFLAGS)

if MCA_BUILD_ompi_common_ugni_DSO
component_noinst =
component_install = mca_common_ugni.la
else
component_noinst = libmca_common_ugni.la
component_install =
endif

headers = common_ugni.h \
          common_ugni_ep.h

ugni_SOURCES = common_ugni.c \
               common_ugni_ep.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_common_ugni_la_SOURCES = $(headers) $(ugni_SOURCES)
nodist_mca_common_ugni_la_SOURCES = $(ugni_nodist_SOURCES)
mca_common_ugni_la_LIBADD = $(common_ugni_LIBS) 
mca_common_ugni_la_LDFLAGS = -module -avoid-version $(common_ugni_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_common_ugni_la_SOURCES = $(headers) $(ugni_SOURCES)
nodist_libmca_common_ugni_la_SOURCES = $(ugni_nodist_SOURCES)
libmca_common_ugni_la_LIBADD = $(common_ugni_LIBS)
libmca_common_ugni_la_LDFLAGS = -module -avoid-version $(common_ugni_LDFLAGS)
