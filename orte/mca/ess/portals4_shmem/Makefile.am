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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(ess_portals4_shmem_CPPFLAGS)

sources = \
        ess_portals4_shmem.h \
        ess_portals4_shmem_component.c \
        ess_portals4_shmem_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_ess_portals4_shmem_DSO
component_noinst =
component_install = mca_ess_portals4_shmem.la
else
component_noinst = libmca_ess_portals4_shmem.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ess_portals4_shmem_la_SOURCES = $(sources)
mca_ess_portals4_shmem_la_LIBADD = $(ess_portals4_shmem_LIBS)
mca_ess_portals4_shmem_la_LDFLAGS = -module -avoid-version $(ess_portals4_shmem_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ess_portals4_shmem_la_SOURCES =$(sources)
libmca_ess_portals4_shmem_la_LIBADD = $(ess_portals4_shmem_LIBS)
libmca_ess_portals4_shmem_la_LDFLAGS = -module -avoid-version $(ess_portals4_shmem_LDFLAGS)
