#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      UT-Battelle, LLC
# Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(ras_alps_CPPFLAGS)

dist_prtedata_DATA = help-ras-alps.txt \
		  ras-alps-command.sh

install-data-hook:
	chmod +x $(DESTDIR)$(prtedatadir)/ras-alps-command.sh

sources = \
        ras_alps.h \
        ras_alps_component.c \
        ras_alps_module.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_ras_alps_DSO
lib =
lib_sources =
component = prte_mca_ras_alps.la
component_sources = $(sources)
else
lib = libprtemca_ras_alps.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component)
prte_mca_ras_alps_la_SOURCES = $(component_sources)
prte_mca_ras_alps_la_LDFLAGS = -module -avoid-version $(ras_alps_LDFLAGS)
prte_mca_ras_alps_la_LIBADD = $(ras_alps_LIBS) \
                         $(top_builddir)/src/libprrte.la
prte_mca_ras_alps_la_CPPFLAGS = $(ras_alps_CPPFLAGS)

noinst_LTLIBRARIES = $(lib)
libprtemca_ras_alps_la_SOURCES = $(lib_sources)
libprtemca_ras_alps_la_LDFLAGS = -module -avoid-version $(ras_alps_LDFLAGS)
libprtemca_ras_alps_la_LIBADD = $(ras_alps_LIBS)
libprtemca_ras_alps_la_CPPFLAGS = $(ras_alps_CPPFLAGS)
