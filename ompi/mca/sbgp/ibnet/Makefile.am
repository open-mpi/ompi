#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(sbgp_ibnet_CPPFLAGS) $(btl_openib_CPPFLAGS)

sources = \
        sbgp_ibnet.h \
        sbgp_ibnet_mca.h \
        sbgp_ibnet_mca.c \
        sbgp_ibnet_component.c  \
        sbgp_ibnet_module.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_sbgp_ibnet_DSO
component_install += mca_sbgp_ibnet.la
else
component_noinst += libmca_sbgp_ibnet.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sbgp_ibnet_la_SOURCES = $(sources)
mca_sbgp_ibnet_la_LDFLAGS = -module -avoid-version $(sbgp_ibnet_LDFLAGS) $(btl_openib_LDFLAGS) 
mca_sbgp_ibnet_la_LIBADD = $(sbgp_ibnet_LIBS) $(btl_openib_LIBS) \
    $(OMPI_TOP_BUILDDIR)/ompi/mca/common/verbs/libmca_common_verbs.la \
    $(OMPI_TOP_BUILDDIR)/ompi/mca/common/ofacm/libmca_common_ofacm.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_sbgp_ibnet_la_SOURCES =$(sources)
libmca_sbgp_ibnet_la_LDFLAGS = -module -avoid-version

$(OMPI_TOP_BUILDDIR)/ompi/mca/common/ofacm/libmca_common_ofacm.la: foo.c
	cd $(OMPI_TOP_BUILDDIR)/ompi/mca/common/ofacm && $(MAKE)

$(OMPI_TOP_BUILDDIR)/ompi/mca/common/ofautils/libmca_common_ofautils.la: foo.c
	cd $(OMPI_TOP_BUILDDIR)/ompi/mca/common/ofautils && $(MAKE)

foo.c:
