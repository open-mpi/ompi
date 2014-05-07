#
# Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        dstore_pmi.h \
        dstore_pmi_component.c \
        dstore_pmi.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_dstore_pmi_DSO
component_noinst =
component_install = mca_dstore_pmi.la
else
component_noinst = libmca_dstore_pmi.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_dstore_pmi_la_SOURCES = $(sources)
mca_dstore_pmi_la_CPPFLAGS = $(dstore_pmi_CPPFLAGS)
mca_dstore_pmi_la_LDFLAGS = -module -avoid-version $(dstore_pmi_LDFLAGS)
mca_dstore_pmi_la_LIBADD = $(dstore_pmi_LIBS) \
	$(OPAL_TOP_BUILDDIR)/opal/mca/common/pmi/libmca_common_pmi.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_dstore_pmi_la_SOURCES =$(sources)
libmca_dstore_pmi_la_CPPFLAGS = $(dstore_pmi_CPPFLAGS)
libmca_dstore_pmi_la_LDFLAGS = -module -avoid-version $(dstore_pmi_LDFLAGS)
libmca_dstore_pmi_la_LIBADD = $(dstore_pmi_LIBS)
