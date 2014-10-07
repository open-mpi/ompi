# -*- makefile -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# THIS MAKEFILE IS OLD AND ONLY TO SUPPORT FORTRAN COMPILERS THAT DO
# NOT SUPPORT "IGNORE TKR" FUNCTIONALITY (i.e., gfortran before v4.9).
# All other Fortran compilers support ignore TKR and don't compile
# this directory -- instead, they compile
# ompi/fortran/use-mpi-ignore-tkr.

if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS

########################################################################

# Note the "-I." in the flags below.  This is because mpi.F90 includes
# mpi-f90-interfaces.h, which will be in the build tree (i.e., the
# current directory) because it is generated.

AM_FCFLAGS = -I$(top_builddir)/ompi/include -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG). -I$(srcdir) -I. \
             -I$(top_builddir)/ompi/mpi/fortran/use-mpi-tkr $(FCFLAGS_f90)

# Do different things if the top-level configure decided that we're
# going to build F90 bindings or not.

lib_LTLIBRARIES =

# Add the f90 library to the list of libraries to build

lib_LTLIBRARIES += libmpi_usempi.la

# Automake doesn't know how to do F90 dependency analysis, so manually
# list this here (i.e., "mpi-f90-interfaces.h" is included in
# mpi.F90).

mpi.lo: mpi.F90
mpi.lo: mpi-f90-interfaces.h
mpi.lo: $(top_builddir)/ompi/mpi/fortran/configure-fortran-output.h
mpi.lo: mpi-f90-cptr-interfaces.F90

# Per MPI-3 p610:34-41, if we're building a TKR mpi module, we should
# not build any interfaces for MPI subroutines that take a choice
# buffer dummy argument.  We therefore really only need to build a
# handful of subroutines.

libmpi_usempi_la_SOURCES = \
     mpi.F90 \
     mpi_comm_spawn_multiple_f90.f90 \
     mpi_testall_f90.f90 \
     mpi_testsome_f90.f90 \
     mpi_waitall_f90.f90 \
     mpi_waitsome_f90.f90 \
     mpi_wtick_f90.f90 \
     mpi_wtime_f90.f90
libmpi_usempi_la_LIBADD = \
     $(top_builddir)/ompi/mpi/fortran/mpif-h/libmpi_mpifh.la

# Don't distribute mpi-tkr-sizeof-*; they're generated.

nodist_libmpi_usempi_la_SOURCES =
if BUILD_FORTRAN_SIZEOF
nodist_libmpi_usempi_la_SOURCES += \
     mpi-tkr-sizeof.h \
     mpi-tkr-sizeof.f90
endif

# Set the library version
libmpi_usempi_la_LDFLAGS = \
        -version-info $(libmpi_usempi_tkr_so_version) \
        $(OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS)

#
# mpi-ignore-tkr-sizeof.* are generated based on some results from
# configure tests.
#

sizeof_pl=$(top_srcdir)/ompi/mpi/fortran/base/gen-mpi-sizeof.pl

mpi-tkr-sizeof.h: $(top_builddir)/config.status
mpi-tkr-sizeof.h: $(sizeof_pl)
mpi-tkr-sizeof.h:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --header=$@ --ierror=mandatory \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF)

mpi-tkr-sizeof.f90: $(top_builddir)/config.status
mpi-tkr-sizeof.f90: $(sizeof_pl)
mpi-tkr-sizeof.f90:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --impl=$@ --ierror=mandatory --mpi --pmpi \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF)

#
# Clean up all F90 module files and all generated files
#

CLEANFILES += mpi-tkr-sizeof.h mpi-tkr-sizeof.f90
MOSTLYCLEANFILES = *.mod
DISTCLEANFILES = $(nodist_libmpi_usempi_la_SOURCES)

#
# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(
#

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

# if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS
endif

# These files must be EXTRA_DIST'ed because they are included; they
# are not compiled directly (and we *can't* list the .F90 file in
# _SOURCES, because it would be compiled -- but it *must* have a .F90
# suffix so that the Fortran compiler will interpret the #if
# directives properly).

EXTRA_DIST = \
     mpi-f90-interfaces.h \
     mpi-f90-cptr-interfaces.F90

