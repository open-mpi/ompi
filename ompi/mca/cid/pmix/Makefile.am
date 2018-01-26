#
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
          cid_pmix_component.c \
          cid_pmix.h \
          cid_pmix.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_cid_pmix_DSO
component_noinst =
component_install = mca_cid_pmix.la
else
component_noinst = libmca_cid_pmix.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_cid_pmix_la_SOURCES = $(sources)
mca_cid_pmix_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_cid_pmix_la_SOURCES = $(sources)
libmca_cid_pmix_la_LDFLAGS = -module -avoid-version
