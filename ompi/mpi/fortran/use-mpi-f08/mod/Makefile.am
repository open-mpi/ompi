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
             -I$(top_srcdir) $(FCFLAGS_f90)

MOSTLYCLEANFILES = *.mod

CLEANFILES += *.i90

###########################################################################

module_sentinel_file = \
        libforce_usempif08_internal_modules_to_be_built.la

noinst_LTLIBRARIES = $(module_sentinel_file)

# f08 support modules

libforce_usempif08_internal_modules_to_be_built_la_SOURCES = \
        mpi-f08-types.F90 \
        mpi-f08-interfaces.F90 \
        mpi-f08-interfaces-callbacks.F90 \
        mpi-f08-callbacks.F90 \
        pmpi-f08-interfaces.F90

config_h = \
    $(top_builddir)/ompi/mpi/fortran/configure-fortran-output.h \
    $(top_srcdir)/ompi/mpi/fortran/configure-fortran-output-bottom.h

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

mpi-f08-types.lo: $(config_h)
mpi-f08-types.lo: mpi-f08-types.F90
mpi-f08-interfaces.lo: $(config_h)
mpi-f08-interfaces.lo: mpi-f08-interfaces.F90
mpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo
mpi-f08-interfaces-callbacks.lo: $(config_h)
mpi-f08-interfaces-callbacks.lo: mpi-f08-interfaces-callbacks.F90
mpi-f08-interfaces-callbacks.lo: mpi-f08-types.lo
mpi-f08-callbacks.lo: $(config_h)
mpi-f08-callbacks.lo: mpi-f08-callbacks.F90
mpi-f08-callbacks.lo: mpi-f08-types.lo
pmpi-f08-interfaces.lo: $(config_h)
pmpi-f08-interfaces.lo: pmpi-f08-interfaces.F90
pmpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo

###########################################################################

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(libdir); \
	  $(INSTALL) $$file $(DESTDIR)$(libdir); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(libdir)/$$file; \
	  rm -f $(DESTDIR)$(libdir)/$$file; \
	done

endif
