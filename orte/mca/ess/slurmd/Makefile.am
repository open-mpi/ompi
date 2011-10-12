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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(ess_slurmd_pmi_CPPFLAGS)

dist_pkgdata_DATA = help-ess-slurmd.txt

sources = \
        ess_slurmd.h \
        ess_slurmd_component.c \
        ess_slurmd_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_ess_slurmd_DSO
component_noinst =
component_install = mca_ess_slurmd.la
else
component_noinst = libmca_ess_slurmd.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ess_slurmd_la_SOURCES = $(sources)
mca_ess_slurmd_la_LDFLAGS = -module -avoid-version $(ess_slurmd_LDFLAGS) $(ess_slurmd_pmi_LDFLAGS)
mca_ess_slurmd_la_LIBADD = $(ess_slurmd_LIBS) $(ess_slurmd_pmi_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ess_slurmd_la_SOURCES =$(sources)
libmca_ess_slurmd_la_LDFLAGS = -module -avoid-version $(ess_slurmd_LDFLAGS) $(ess_slurmd_pmi_LDFLAGS)
libmca_ess_slurmd_la_LIBADD = $(ess_slurmd_LIBS) $(ess_slurmd_pmi_LIBS)
