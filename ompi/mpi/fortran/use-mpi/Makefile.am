# -*- makefile -*-
#
# Copyright (c) 2006-2019 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2020 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# Note that Automake's Fortran-buidling rules uses CPPFLAGS and
# AM_CPPFLAGS.  This can cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253).  Let's just zero
# those out and rely on AM_FCFLAGS.
CPPFLAGS =
AM_CPPFLAGS =

# This Makefile is only relevant if we support TYPE(MPI_Status)
# MPI bindings.
if OMPI_FORTRAN_HAVE_TYPE_MPI_STATUS

AM_FCFLAGS = -I$(top_builddir)/ompi/include \
             -I$(top_srcdir)/ompi/include \
             -I$(top_srcdir) -I$(top_builddir) $(FCFLAGS_f90)

MOSTLYCLEANFILES = *.mod

CLEANFILES += *.i90

###########################################################################


noinst_LTLIBRARIES = libusempi_internal_modules.la

# Don't distribute mpi-types.F90,  it is generated.
nodist_libusempi_internal_modules_la_SOURCES = \
	mpi-types.F90

mpi-types.lo: mpi-types.F90

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR); \
	  $(INSTALL) $$file $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR)/$$file; \
	  rm -f $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR)/$$file; \
	done
endif
