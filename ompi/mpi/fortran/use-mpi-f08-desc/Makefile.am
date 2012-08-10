# -*- makefile -*-
#
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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

lib_LTLIBRARIES = libmpi_usempif08.la

#
# This list is a subset of the full MPI API used for testing Fortran
# descriptors usage in MPI-3
#
mpi_api_files = \
        comm_rank_f08.f90 \
        comm_size_f08.f90 \
        finalize_f08.f90 \
        init_f08.f90 \
        recv_f08_desc.f90 \
        send_f08_desc.f90 \
        type_commit_f08.f90 \
        type_contiguous_f08.f90 \
        type_vector_f08.f90

libmpi_usempif08_la_SOURCES = \
        $(mpi_api_files) \
        mpi-f08-types.f90 \
        mpi-f08-interfaces.F90 \
        mpi-f-interfaces-bind.h \
        mpi-f08.f90 \
        ISO_Fortran_binding.h \
        OMPI_Fortran_binding.f90 \
        OMPI_Fortran_binding_c.c \
        constants.h \
        constants.c

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
OMPI_Fortran_binding.lo: OMPI_Fortran_binding.f90 mpi-f08-types.lo

$(mpi_api_files): mpi-f08.lo

mpi-f08.lo: mpi-f08-types.lo 
mpi-f08.lo: OMPI_Fortran_binding.lo
mpi-f08.lo: mpi-f08-interfaces.lo
mpi-f08.lo: mpi-f-interfaces-bind.h
mpi-f08.lo: mpi-f08.f90 

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

else

# Need to have empty targets because AM can't handle having an
# AM_CONDITIONAL was targets in the "if" statement but not in the
# "else".  :-(

install-exec-hook:
uninstall-local:

endif
