# -*- makefile -*-
#
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

AM_FCFLAGS = -I$(top_builddir)/ompi/include -I$(top_srcdir)/ompi/include \
             -I$(top_srcdir) $(FCFLAGS)

module_sentinel_file = \
        libforce_usempif08_internal_modules_to_be_built.la

noinst_LTLIBRARIES = $(module_sentinel_file)

# f08 support modules

libforce_usempif08_internal_modules_to_be_built_la_SOURCES = \
        mpi-f08-types.f90 \
        mpi-f08-interfaces.F90

#
# Clean up all F90 module files
#

MOSTLYCLEANFILES = *.mod

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

mpi-f08-types.lo: mpi-f08-types.f90
mpi-f08-interfaces.lo: mpi-f08-interfaces.F90 mpi-f08-types.lo


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

else

# Need to have empty targets because AM can't handle having an
# AM_CONDITIONAL was targets in the "if" statement but not in the
# "else".  :-(

install-exec-hook:
uninstall-local:

endif
