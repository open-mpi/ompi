#
# Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        db_pmi.h \
        db_pmi_component.c \
        db_pmi.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_db_pmi_DSO
component_noinst =
component_install = mca_db_pmi.la
else
component_noinst = libmca_db_pmi.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_pmi_la_SOURCES = $(sources)
mca_db_pmi_la_CPPFLAGS = $(db_pmi_CPPFLAGS)
mca_db_pmi_la_LDFLAGS = -module -avoid-version $(db_pmi_LDFLAGS)
mca_db_pmi_la_LIBADD = $(db_pmi_LIBS) \
	$(top_ompi_builddir)/opal/mca/common/pmi/libmca_common_pmi.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_pmi_la_SOURCES =$(sources)
libmca_db_pmi_la_CPPFLAGS = $(db_pmi_CPPFLAGS)
libmca_db_pmi_la_LDFLAGS = -module -avoid-version $(db_pmi_LDFLAGS)
libmca_db_pmi_la_LIBADD = $(db_pmi_LIBS)
