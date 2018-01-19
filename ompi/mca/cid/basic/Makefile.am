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
          cid_basic_component.c \
          cid_basic.h \
          cid_basic.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_cid_basic_DSO
component_noinst =
component_install = mca_cid_basic.la
else
component_noinst = libmca_cid_basic.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_cid_basic_la_SOURCES = $(sources)
mca_cid_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_cid_basic_la_SOURCES = $(sources)
libmca_cid_basic_la_LDFLAGS = -module -avoid-version
