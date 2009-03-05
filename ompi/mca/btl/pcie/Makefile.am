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
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_pcie_CPPFLAGS)

dist_pkgdata_DATA = \
	help-mpi-btl-pcie.txt

sources = \
    btl_pcie.c \
    btl_pcie.h \
    btl_pcie_component.c \
    btl_pcie_endpoint.c \
    btl_pcie_endpoint.h \
    btl_pcie_fifo.c \
    btl_pcie_fifo.h \
    btl_pcie_frag.c \
    btl_pcie_frag.h \
    btl_pcie_proc.c \
    btl_pcie_proc.h \
    btl_pcie_lex.c \
    btl_pcie_lex.h \
    btl_pcie_cfg.c \
    btl_pcie_ddriver.h \
    btl_pcie_ddriver.c

EXTRA_DIST = btl_pcie_lex.l


if OMPI_BUILD_btl_pcie_DSO
lib =
lib_sources =
component = mca_btl_pcie.la
component_sources = $(sources)
else
lib = libmca_btl_pcie.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_pcie_la_SOURCES = $(component_sources)
mca_btl_pcie_la_LDFLAGS = -module -avoid-version $(btl_pcie_LDFLAGS) 
mca_btl_pcie_la_LIBADD = $(btl_pcie_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_pcie_la_SOURCES = $(lib_sources)
libmca_btl_pcie_la_LDFLAGS= -module -avoid-version $(btl_pcie_LDFLAGS) 
libmca_btl_pcie_la_LIBADD = $(btl_pcie_LIBS)


ompi_sysconfdir = $(OMPI_SYSCONFDIR)
ompi_sysconf_DATA = \
    mca-btl-pcie-local-resources.cfg \
    mca-btl-pcie-remote-resources.cfg

