# -*- makefile -*-
#
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

AM_FCFLAGS = -I$(top_builddir)/ompi/include \
             -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/$(OMPI_FORTRAN_USEMPI_DIR) \
             $(OMPI_FC_MODULE_FLAG). \
             $(OMPI_FC_MODULE_FLAG)../mod \
             -I$(top_srcdir) $(FCFLAGS_f90)

MOSTLYCLEANFILES = *.mod

CLEANFILES += *.i90

###########################################################################

module_sentinel_file = \
        libforce_usempif08_internal_bindings_to_be_built.la

noinst_LTLIBRARIES = $(module_sentinel_file)

# f08 support modules

libforce_usempif08_internal_bindings_to_be_built_la_SOURCES = \
        mpi-f-interfaces-bind.h \
        ompi-mpifh-bindings.F90

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

ompi-mpifh-bindings.lo: mpi-f-interfaces-bind.h

distclean-local:
	rm -f *.mod

endif
